{-|
Module: Squeal.PostgreSQL.Manipulation
Description: Squeal data manipulation language
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal data manipulation language.
-}

{-# LANGUAGE
    DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PatternSynonyms
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Manipulation
  ( -- * Manipulation
    Manipulation_
  , Manipulation (..)
  , queryStatement
    -- * Insert
  , insertInto
  , insertInto_
    -- * Update
  , update
  , update_
    -- * Delete
  , deleteFrom
  , deleteFrom_
    -- * Clauses
  , Optional (..)
  , QueryClause (..)
  , pattern Values_
  , LiteralColumn (..)
  , literalColumns
  , inline
  , inlineMany
  , ReturningClause (..)
  , pattern Returning_
  , ConflictClause (..)
  , ConflictTarget (..)
  , ConflictAction (..)
  , UsingClause (..)
  ) where

import Control.DeepSeq
import Data.ByteString hiding (foldr)
import Data.Kind (Type)
import Data.Quiver.Functor
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Literal
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import Data.Int
-- >>> import Data.Time

{- |
A `Manipulation` is a statement which may modify data in the database,
but does not alter its db. Examples are inserts, updates and deletes.
A `Query` is also considered a `Manipulation` even though it does not modify data.

The general `Manipulation` type is parameterized by

* @commons :: FromType@ - scope for all `common` table expressions,
* @db :: SchemasType@ - scope for all `table`s and `view`s,
* @params :: [NullityType]@ - scope for all `Squeal.Expression.Parameter.parameter`s,
* @row :: RowType@ - return type of the `Query`.
-}
newtype Manipulation
  (commons :: FromType)
  (db :: SchemasType)
  (params :: [NullityType])
  (columns :: RowType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Manipulation commons db params columns) where
  renderSQL = renderManipulation
instance With Manipulation where
  with Done manip = manip
  with ctes manip = UnsafeManipulation $
    "WITH" <+> commaSeparated (qtoList renderSQL ctes) <+> renderSQL manip

{- |
The top level `Manipulation_` type is parameterized by a @db@ `SchemasType`,
against which the query is type-checked, an input @params@ Haskell `Type`,
and an ouput row Haskell `Type`.

A top-level `Manipulation_` can be run
using `Squeal.PostgreSQL.PQ.manipulateParams`, or if @params = ()@
using `Squeal.PostgreSQL.PQ.manipulate`.

Generally, @params@ will be a Haskell tuple or record whose entries
may be referenced using positional
`Squeal.PostgreSQL.Expression.Parameter.param`s and @row@ will be a
Haskell record, whose entries will be targeted using overloaded labels.

>>> :set -XDeriveAnyClass -XDerivingStrategies
>>> :{
data Row a b = Row { col1 :: a, col2 :: b }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
:}

simple insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'Null 'PGint4, "col2" ::: 'Def :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () ()
  manipulation =
    insertInto_ #tab (Values_ (Set 2 `as` #col1 :* Default `as` #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (2, DEFAULT)

out-of-line parameterized insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Manipulation_ (Public Schema) (Int32, Int32) ()
  manipulation =
    insertInto_ #tab (Values_ (Set (param @1) `as` #col1 :* Set (param @2) `as` #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (($1 :: int4), ($2 :: int4))

in-line parameterized insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Row Int32 Int32 -> Manipulation_ (Public Schema) () ()
  manipulation row =
    insertInto_ #tab (inline row)
in printSQL (manipulation (Row 1 2))
:}
INSERT INTO "tab" ("col1", "col2") VALUES (1, 2)

returning insert:

>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () (Only Int32)
  manipulation =
    insertInto #tab (Values_ (Set 2 `as` #col1 :* Set 3 `as` #col2))
      OnConflictDoRaise (Returning (#col1 `as` #fromOnly))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (2, 3) RETURNING "col1" AS "fromOnly"

upsert:

>>> type CustomersColumns = '["name" ::: 'NoDef :=> 'NotNull 'PGtext, "email" ::: 'NoDef :=> 'NotNull 'PGtext]
>>> type CustomersConstraints = '["uq" ::: 'Unique '["name"]]
>>> type CustomersSchema = '["customers" ::: 'Table (CustomersConstraints :=> CustomersColumns)]
>>> :{
let
  manipulation :: Manipulation_ (Public CustomersSchema) () ()
  manipulation =
    insertInto #customers
      (Values_ (Set "John Smith" `as` #name :* Set "john@smith.com" `as` #email))
      (OnConflict (OnConstraint #uq)
        (DoUpdate (Set (#excluded ! #email <> "; " <> #customers ! #email) `as` #email) []))
      (Returning_ Nil)
in printSQL manipulation
:}
INSERT INTO "customers" ("name", "email") VALUES (E'John Smith', E'john@smith.com') ON CONFLICT ON CONSTRAINT "uq" DO UPDATE SET "email" = ("excluded"."email" || (E'; ' || "customers"."email"))

query insert:

>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () ()
  manipulation = insertInto_ #tab (Subquery (select Star (from (table #tab))))
in printSQL manipulation
:}
INSERT INTO "tab" SELECT * FROM "tab" AS "tab"

update:

>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () ()
  manipulation = update_ #tab (Set 2 `as` #col1) (#col1 ./= #col2)
in printSQL manipulation
:}
UPDATE "tab" SET "col1" = 2 WHERE ("col1" <> "col2")

delete:

>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () (Row Int32 Int32)
  manipulation = deleteFrom #tab NoUsing (#col1 .== #col2) (Returning Star)
in printSQL manipulation
:}
DELETE FROM "tab" WHERE ("col1" = "col2") RETURNING *

delete and using clause:

>>> :{
type Schema3 =
  '[ "tab" ::: 'Table ('[] :=> Columns)
   , "other_tab" ::: 'Table ('[] :=> Columns)
   , "third_tab" ::: 'Table ('[] :=> Columns) ]
:}

>>> :{
let
  manipulation :: Manipulation_ (Public Schema3) () ()
  manipulation =
    deleteFrom #tab (Using (table #other_tab & also (table #third_tab)))
    ( (#tab ! #col2 .== #other_tab ! #col2)
    .&& (#tab ! #col2 .== #third_tab ! #col2) )
    (Returning_ Nil)
in printSQL manipulation
:}
DELETE FROM "tab" USING "other_tab" AS "other_tab", "third_tab" AS "third_tab" WHERE (("tab"."col2" = "other_tab"."col2") AND ("tab"."col2" = "third_tab"."col2"))

with manipulation:

>>> type ProductsColumns = '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]
>>> type ProductsSchema = '["products" ::: 'Table ('[] :=> ProductsColumns), "products_deleted" ::: 'Table ('[] :=> ProductsColumns)]
>>> :{
let
  manipulation :: Manipulation_ (Public ProductsSchema) (Only Day) ()
  manipulation = with
    (deleteFrom #products NoUsing (#date .< param @1) (Returning Star) `as` #del)
    (insertInto_ #products_deleted (Subquery (select Star (from (common #del)))))
in printSQL manipulation
:}
WITH "del" AS (DELETE FROM "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" SELECT * FROM "del" AS "del"
-}
type family Manipulation_ (db :: SchemasType) (params :: Type) (row :: Type) where
  Manipulation_ db params row = Manipulation '[] db (TuplePG params) (RowPG row)

-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query '[] commons db params columns
  -- ^ `Query` to embed as a `Manipulation`
  -> Manipulation commons db params columns
queryStatement q = UnsafeManipulation $ renderSQL q

{-----------------------------------------
INSERT statements
-----------------------------------------}

{- |
When a table is created, it contains no data. The first thing to do
before a database can be of much use is to insert data. Data is
conceptually inserted one row at a time. Of course you can also insert
more than one row, but there is no way to insert less than one row.
Even if you know only some column values, a complete row must be created.
-}
insertInto
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , SOP.SListI (TableToColumns table)
     , SOP.SListI row )
  => QualifiedAlias sch tab
  -- ^ table
  -> QueryClause commons db params (TableToColumns table)
  -- ^ what to insert
  -> ConflictClause tab commons db params table
  -- ^ what to do in case of conflict
  -> ReturningClause commons db params '[tab ::: TableToRow table] row
  -- ^ what to return
  -> Manipulation commons db params row
insertInto tab qry conflict ret = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderSQL tab
  <+> renderSQL qry
  <> renderSQL conflict
  <> renderSQL ret

-- | Like `insertInto` but with `OnConflictDoRaise` and no `ReturningClause`.
insertInto_
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , SOP.SListI (TableToColumns table) )
  => QualifiedAlias sch tab
  -- ^ table
  -> QueryClause commons db params (TableToColumns table)
  -- ^ what to insert
  -> Manipulation commons db params '[]
insertInto_ tab qry =
  insertInto tab qry OnConflictDoRaise (Returning_ Nil)

-- | A `QueryClause` describes what to `insertInto` a table.
data QueryClause commons db params columns where
  Values
    :: SOP.SListI columns
    => NP (Aliased (Optional (Expression '[] commons 'Ungrouped db params '[]))) columns
    -- ^ row of values
    -> [NP (Aliased (Optional (Expression '[] commons 'Ungrouped db params '[]))) columns]
    -- ^ additional rows of values
    -> QueryClause commons db params columns
  Select
    :: SOP.SListI columns
    => NP (Aliased (Optional (Expression '[] commons grp db params from))) columns
    -- ^ row of values
    -> TableExpression '[] commons grp db params from
    -- ^ from a table expression
    -> QueryClause commons db params columns
  Subquery
    :: ColumnsToRow columns ~ row
    => Query '[] commons db params row
    -- ^ subquery to insert
    -> QueryClause commons db params columns

instance RenderSQL (QueryClause commons db params columns) where
  renderSQL = \case
    Values row0 rows ->
      parenthesized (renderCommaSeparated renderSQLPart row0)
      <+> "VALUES"
      <+> commaSeparated
            ( parenthesized
            . renderCommaSeparated renderValuePart <$> row0 : rows )
    Select row0 tab ->
      parenthesized (renderCommaSeparatedMaybe renderSQLPartMaybe row0)
      <+> "SELECT"
      <+> renderCommaSeparatedMaybe renderValuePartMaybe row0
      <+> renderSQL tab
    Subquery qry -> renderQuery qry
    where
      renderSQLPartMaybe, renderValuePartMaybe
        :: Aliased (Optional (Expression '[] commons grp db params from)) column
        -> Maybe ByteString
      renderSQLPartMaybe = \case
        Default `As` _ -> Nothing
        Set _ `As` name -> Just $ renderSQL name
      renderValuePartMaybe = \case
        Default `As` _ -> Nothing
        Set value `As` _ -> Just $ renderExpression value
      renderSQLPart, renderValuePart
        :: Aliased (Optional (Expression '[] commons grp db params from)) column
        -> ByteString
      renderSQLPart (_ `As` name) = renderSQL name
      renderValuePart (value `As` _) = renderSQL value

-- | `Values_` describes a single `NP` list of `Aliased` `Optional` `Expression`s
-- whose `ColumnsType` must match the tables'.
pattern Values_
  :: SOP.SListI columns
  => NP (Aliased (Optional (Expression '[] commons 'Ungrouped db params '[]))) columns
  -- ^ row of values
  -> QueryClause commons db params columns
pattern Values_ vals = Values vals []

-- | `Optional` is either `Default` or a value, parameterized by an appropriate
-- `ColumnConstraint`.
data Optional expr ty where
  -- | Use the `Default` value for a column.
  Default :: Optional expr ('Def :=> ty)
  -- | `Set` a value for a column.
  Set :: expr ty -> Optional expr (def :=> ty)

instance (forall x. RenderSQL (expr x)) => RenderSQL (Optional expr ty) where
  renderSQL = \case
    Default -> "DEFAULT"
    Set expr -> renderSQL expr

-- | Lifts `Literal` to a column entry
class LiteralColumn field column where
  -- | Haskell record field as a literal column
  literalColumn
    :: SOP.P field
    -> Aliased ( Optional
      ( Expression outer commons grp db params from
      ) ) column
instance (Literal hask, column ~ (def :=> NullPG hask), KnownSymbol alias)
  => LiteralColumn (alias ::: hask) (alias ::: column) where
    literalColumn (SOP.P hask) = Set (literal hask) `as` (Alias @alias)
instance (KnownSymbol alias, column ~ ('Def :=> ty))
  => LiteralColumn (alias ::: ()) (alias ::: column) where
    literalColumn _ = Default `as` (Alias @alias)

-- | Use a Haskell record as a literal list of columns
literalColumns
  :: ( SOP.IsRecord hask xs
     , SOP.AllZip LiteralColumn xs columns )
  => hask -- ^ record
  -> NP (Aliased (Optional (
      Expression '[] commons 'Ungrouped db params '[]
      ) ) ) columns
literalColumns
  = SOP.htrans (SOP.Proxy @LiteralColumn) literalColumn
  . SOP.toRecord

-- | `inline` a Haskell record in `insertInto`.
inline
  :: ( SOP.IsRecord hask xs
     , SOP.AllZip LiteralColumn xs columns )
  => hask -- ^ record
  -> QueryClause commons db params columns
inline = Values_ . literalColumns

-- | `inlineMany` Haskell records in `insertInto`.
inlineMany
  :: ( SOP.IsRecord hask xs
     , SOP.AllZip LiteralColumn xs columns )
  => hask -- ^ record
  -> [hask] -- ^ more
  -> QueryClause commons db params columns
inlineMany hask hasks = Values (literalColumns hask) (literalColumns <$> hasks)

-- | A `ReturningClause` computes and return value(s) based
-- on each row actually inserted, updated or deleted. This is primarily
-- useful for obtaining values that were supplied by defaults, such as a
-- serial sequence number. However, any expression using the table's columns
-- is allowed. Only rows that were successfully inserted or updated or
-- deleted will be returned. For example, if a row was locked
-- but not updated because an `OnConflict` `DoUpdate` condition was not satisfied,
-- the row will not be returned. `Returning` `Star` will return all columns
-- in the row. Use `Returning` `Nil` in the common case where no return
-- values are desired.
newtype ReturningClause commons db params from row =
  Returning (Selection '[] commons 'Ungrouped db params from row)

instance RenderSQL (ReturningClause commons db params from row) where
  renderSQL = \case
    Returning (List Nil) -> ""
    Returning selection -> " RETURNING" <+> renderSQL selection

-- | `Returning` a `List`
pattern Returning_
  :: SOP.SListI row
  => NP (Aliased (Expression '[] commons 'Ungrouped db params from)) row
  -- ^ row of values
  -> ReturningClause commons db params from row
pattern Returning_ list = Returning (List list)

-- | A `ConflictClause` specifies an action to perform upon a constraint
-- violation. `OnConflictDoRaise` will raise an error.
-- `OnConflict` `DoNothing` simply avoids inserting a row.
-- `OnConflict` `DoUpdate` updates the existing row that conflicts with the row
-- proposed for insertion.
data ConflictClause tab commons db params table where
  OnConflictDoRaise :: ConflictClause tab commons db params table
  OnConflict
    :: ConflictTarget table
    -- ^ conflict target
    -> ConflictAction tab commons db params table
    -- ^ conflict action
    -> ConflictClause tab commons db params table

-- | Render a `ConflictClause`.
instance SOP.SListI (TableToColumns table)
  => RenderSQL (ConflictClause tab commons db params table) where
    renderSQL = \case
      OnConflictDoRaise -> ""
      OnConflict target action -> " ON CONFLICT"
        <+> renderSQL target <+> renderSQL action

{- |
`ConflictAction` specifies an alternative `OnConflict` action.
It can be either `DoNothing`, or a `DoUpdate` clause specifying
the exact details of the `update` action to be performed in case of a conflict.
The `Set` and WHERE `Condition`s in `OnConflict` `DoUpdate` have access to the
existing row using the table's name, and to rows proposed
for insertion using the special @#excluded@ row.
`OnConflict` `DoNothing` simply avoids inserting a row as its alternative action.
`OnConflict` `DoUpdate` updates the existing row that conflicts
with the row proposed for insertion as its alternative action.
-}
data ConflictAction tab commons db params table where
  DoNothing :: ConflictAction tab commons db params table
  DoUpdate
    :: ( row ~ TableToRow table
       , from ~ '[tab ::: row, "excluded" ::: row]
       , Updatable table updates )
    => NP (Aliased (Optional (Expression '[] commons 'Ungrouped db params from))) updates
    -> [Condition '[] commons 'Ungrouped db params from]
       -- ^ WHERE `Condition`s
    -> ConflictAction tab commons db params table

instance RenderSQL (ConflictAction tab commons db params table) where
  renderSQL = \case
    DoNothing -> "DO NOTHING"
    DoUpdate updates whs'
      -> "DO UPDATE SET"
        <+> renderCommaSeparated renderUpdate updates
        <> case whs' of
          [] -> ""
          wh:whs -> " WHERE" <+> renderSQL (foldr (.&&) wh whs)

renderUpdate
  :: (forall x. RenderSQL (expr x))
  => Aliased (Optional expr) ty
  -> ByteString
renderUpdate (expr `As` col) = renderSQL col <+> "=" <+> renderSQL expr

-- | A `ConflictTarget` specifies the constraint violation that triggers a
-- `ConflictAction`.
data ConflictTarget table where
  OnConstraint
    :: Has con constraints constraint
    => Alias con
    -> ConflictTarget (constraints :=> columns)

-- | Render a `ConflictTarget`
instance RenderSQL (ConflictTarget constraints) where
  renderSQL (OnConstraint con) =
    "ON" <+> "CONSTRAINT" <+> renderSQL con

{-----------------------------------------
UPDATE statements
-----------------------------------------}

-- | An `update` command changes the values of the specified columns
-- in all rows that satisfy the condition.
update
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , Updatable table updates
     , SOP.SListI row )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (Aliased (Optional (Expression '[] '[] 'Ungrouped db params '[tab ::: TableToRow table]))) updates
  -- ^ modified values to replace old values
  -> Condition '[] commons 'Ungrouped db params '[tab ::: TableToRow table]
  -- ^ condition under which to perform update on a row
  -> ReturningClause commons db params '[tab ::: TableToRow table] row -- ^ results to return
  -> Manipulation commons db params row
update tab columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderSQL tab
  <+> "SET"
  <+> renderCommaSeparated renderUpdate columns
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

-- | Update a row returning `Nil`.
update_
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , Updatable table updates )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (Aliased (Optional (Expression '[] '[] 'Ungrouped db params '[tab ::: TableToRow table]))) updates
  -- ^ modified values to replace old values
  -> Condition '[] commons 'Ungrouped db params '[tab ::: TableToRow table]
  -- ^ condition under which to perform update on a row
  -> Manipulation commons db params '[]
update_ tab columns wh = update tab columns wh (Returning_ Nil)

{-----------------------------------------
DELETE statements
-----------------------------------------}

-- | Specify additional tables with `Using`
-- an `also` list of table expressions, allowing columns
-- from other tables to appear in the WHERE condition.
-- This is similar to the list of tables that can be specified
-- in the FROM Clause of a SELECT statement;
-- for example, an alias for the table name can be specified.
-- Do not repeat the target table in the `Using` list,
-- unless you wish to set up a self-join.
-- `NoUsing` if no additional tables are to be used.
data UsingClause commons db params from where
  NoUsing :: UsingClause commons db params '[]
  Using
    :: FromClause '[] commons db params from
    -- ^ what to use
    -> UsingClause commons db params from

-- | Delete rows from a table.
deleteFrom
  :: ( SOP.SListI row
     , Has sch db schema
     , Has tab schema ('Table table) )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> UsingClause commons db params from
  -> Condition '[] commons 'Ungrouped db params (tab ::: TableToRow table ': from)
  -- ^ condition under which to delete a row
  -> ReturningClause commons db params '[tab ::: TableToRow table] row
  -- ^ results to return
  -> Manipulation commons db params row
deleteFrom tab using wh returning = UnsafeManipulation $
  "DELETE FROM"
  <+> renderSQL tab
  <> case using of
    NoUsing -> ""
    Using tables -> " USING" <+> renderSQL tables
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

-- | Delete rows returning `Nil`.
deleteFrom_
  :: ( Has sch db schema
     , Has tab schema ('Table table) )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> Condition '[] commons 'Ungrouped db params '[tab ::: TableToRow table]
  -- ^ condition under which to delete a row
  -> Manipulation commons db params '[]
deleteFrom_ tab wh = deleteFrom tab NoUsing wh (Returning_ Nil)

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
    Manipulation (..)
  , Manipulation_
  , queryStatement
  , QueryClause (..)
  , pattern Values_
  , DefaultAliasable (..)
  , ColumnExpression (..)
  , ReturningClause (..)
  , pattern Returning_
  , ConflictClause (..)
  , ConflictTarget (..)
  , ConflictAction (..)
  , UsingClause (..)
    -- * Insert
  , insertInto
  , insertInto_
    -- * Update
  , update
  , update_
    -- * Delete
  , deleteFrom
  , deleteFrom_
  , also
  ) where

import Control.DeepSeq
import Data.ByteString hiding (foldr)
import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol)

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

{- |
A `Manipulation` is a statement which may modify data in the database,
but does not alter its schemas. Examples are inserts, updates and deletes.
A `Query` is also considered a `Manipulation` even though it does not modify data.

simple insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'Null 'PGint4, "col2" ::: 'Def :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[] '[]
  manipulation =
    insertInto_ #tab (Values_ (2 `as` #col1 :* defaultAs #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (2, DEFAULT)

parameterized insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[ 'NotNull 'PGint4, 'NotNull 'PGint4 ] '[]
  manipulation =
    insertInto_ #tab (Values_ (param @1 `as` #col1 :* param @2 `as` #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (($1 :: int4), ($2 :: int4))

returning insert:

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[] '["fromOnly" ::: 'NotNull 'PGint4]
  manipulation =
    insertInto #tab (Values_ (2 `as` #col1 :* 3 `as` #col2))
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
  manipulation :: Manipulation '[] (Public CustomersSchema) '[] '[]
  manipulation =
    insertInto #customers
      (Values_ ("John Smith" `as` #name :* "john@smith.com" `as` #email))
      (OnConflict (OnConstraint #uq)
        (DoUpdate (((#excluded ! #email) <> "; " <> (#customers ! #email)) `as` #email) []))
      (Returning_ Nil)
in printSQL manipulation
:}
INSERT INTO "customers" ("name", "email") VALUES (E'John Smith', E'john@smith.com') ON CONFLICT ON CONSTRAINT "uq" DO UPDATE SET "email" = ("excluded"."email" || (E'; ' || "customers"."email"))

query insert:

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[] '[]
  manipulation = insertInto_ #tab (Subquery (select Star (from (table #tab))))
in printSQL manipulation
:}
INSERT INTO "tab" SELECT * FROM "tab" AS "tab"

update:

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[] '[]
  manipulation = update_ #tab (2 `as` #col1) (#col1 ./= #col2)
in printSQL manipulation
:}
UPDATE "tab" SET "col1" = 2 WHERE ("col1" <> "col2")

delete:

>>> :{
let
  manipulation :: Manipulation '[] (Public Schema) '[]
    '[ "col1" ::: 'NotNull 'PGint4
     , "col2" ::: 'NotNull 'PGint4 ]
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
  manipulation :: Manipulation '[] (Public Schema3) '[] '[]
  manipulation =
    deleteFrom_ #tab (Using (table #other_tab & also (table #third_tab)))
    ( (#tab ! #col2 .== #other_tab ! #col2)
    .&& (#tab ! #col2 .== #third_tab ! #col2) )
in printSQL manipulation
:}
DELETE FROM "tab" USING "other_tab" AS "other_tab", "third_tab" AS "third_tab" WHERE (("tab"."col2" = "other_tab"."col2") AND ("tab"."col2" = "third_tab"."col2"))

with manipulation:

>>> type ProductsColumns = '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]
>>> type ProductsSchema = '["products" ::: 'Table ('[] :=> ProductsColumns), "products_deleted" ::: 'Table ('[] :=> ProductsColumns)]
>>> :{
let
  manipulation :: Manipulation '[] (Public ProductsSchema) '[ 'NotNull 'PGdate] '[]
  manipulation = with
    (deleteFrom #products NoUsing (#date .< param @1) (Returning Star) `as` #del)
    (insertInto_ #products_deleted (Subquery (select Star (from (common #del)))))
in printSQL manipulation
:}
WITH "del" AS (DELETE FROM "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" SELECT * FROM "del" AS "del"
-}

newtype Manipulation
  (commons :: FromType)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (columns :: RowType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Manipulation commons schemas params columns) where
  renderSQL = renderManipulation
instance With Manipulation where
  with Done manip = manip
  with ctes manip = UnsafeManipulation $
    "WITH" <+> renderSQL ctes <+> renderSQL manip

type family Manipulation_ (schemas :: SchemasType) (params :: Type) (row :: Type) where
  Manipulation_ schemas params row = Manipulation '[] schemas (TuplePG params) (RowPG row)

-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query '[] commons schemas params columns
  -> Manipulation commons schemas params columns
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
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , row0 ~ TableToRow table
     , SOP.SListI columns
     , SOP.SListI row1 )
  => QualifiedAlias sch tab
  -> QueryClause commons schemas params columns
  -> ConflictClause tab commons schemas params table
  -> ReturningClause commons schemas params '[tab ::: row0] row1
  -> Manipulation commons schemas params row1
insertInto tab qry conflict ret = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderSQL tab
  <+> renderSQL qry
  <> renderSQL conflict
  <> renderSQL ret

insertInto_
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , row ~ TableToRow table
     , SOP.SListI columns )
  => QualifiedAlias sch tab
  -> QueryClause commons schemas params columns
  -> Manipulation commons schemas params '[]
insertInto_ tab qry =
  insertInto tab qry OnConflictDoRaise (Returning_ Nil)

data QueryClause commons schemas params columns where
  Values
    :: SOP.SListI columns
    => NP (ColumnExpression 'Ungrouped schemas params '[]) columns
    -> [NP (ColumnExpression 'Ungrouped schemas params '[]) columns]
    -> QueryClause commons schemas params columns
  Select
    :: SOP.SListI columns
    => NP (ColumnExpression grp schemas params from) columns
    -> TableExpression '[] grp commons schemas params from
    -> QueryClause commons schemas params columns
  Subquery
    :: ColumnsToRow columns ~ row
    => Query '[] commons schemas params row
    -> QueryClause commons schemas params columns

instance RenderSQL (QueryClause commons schemas params columns) where
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
        :: ColumnExpression grp schemas params from column -> Maybe ByteString
      renderSQLPartMaybe = \case
        DefaultAs _ -> Nothing
        Specific (_ `As` name) -> Just $ renderSQL name
      renderValuePartMaybe = \case
        DefaultAs _ -> Nothing
        Specific (value `As` _) -> Just $ renderExpression value
      renderSQLPart, renderValuePart
        :: ColumnExpression grp schemas params from column -> ByteString
      renderSQLPart = \case
        DefaultAs name -> renderSQL name
        Specific (_ `As` name) -> renderSQL name
      renderValuePart = \case
        DefaultAs _ -> "DEFAULT"
        Specific (value `As` _) -> renderExpression value

pattern Values_
  :: SOP.SListI columns
  => NP (ColumnExpression 'Ungrouped schemas params '[]) columns
  -> QueryClause commons schemas params columns
pattern Values_ vals = Values vals []

data ColumnExpression grp schemas params from column where
  DefaultAs
    :: KnownSymbol col
    => Alias col
    -> ColumnExpression grp schemas params from (col ::: 'Def :=> ty)
  Specific
    :: Aliased (Expression '[] grp '[] schemas params from) (col ::: ty)
    -> ColumnExpression grp schemas params from (col ::: defness :=> ty)

instance (KnownSymbol col, column ~ (col ::: defness :=> ty))
  => Aliasable col
       (Expression '[] grp '[] schemas params from ty)
       (ColumnExpression grp schemas params from column) where
         expression `as` col = Specific (expression `As` col)
instance (KnownSymbol col, columns ~ '[col ::: defness :=> ty])
  => Aliasable col
       (Expression '[] grp '[] schemas params from ty)
       (NP (ColumnExpression grp schemas params from) columns) where
         expression `as` col = expression `as` col :* Nil
instance (KnownSymbol col, column ~ (col ::: 'Def :=> ty))
  => DefaultAliasable col (ColumnExpression grp schemas params from column) where
    defaultAs = DefaultAs
instance (KnownSymbol col, columns ~ '[col ::: 'Def :=> ty])
  => DefaultAliasable col (NP (ColumnExpression grp schemas params from) columns) where
    defaultAs col = defaultAs col :* Nil

instance (HasUnique tab from row, Has col row ty, column ~ (col ::: defness :=> ty))
  => IsLabel col (ColumnExpression 'Ungrouped schemas params from column) where
    fromLabel = fromLabel @col `as` Alias
instance (HasUnique tab from row, Has col row ty, columns ~ '[col ::: defness :=> ty])
  => IsLabel col
    (NP (ColumnExpression 'Ungrouped schemas params from) columns) where
    fromLabel = fromLabel @col `as` Alias

instance (Has tab from row, Has col row ty, column ~ (col ::: defness :=> ty))
  => IsQualified tab col (ColumnExpression 'Ungrouped schemas params from column) where
    tab ! col = tab ! col `as` col
instance (Has tab from row, Has col row ty, columns ~ '[col ::: defness :=> ty])
  => IsQualified tab col (NP (ColumnExpression 'Ungrouped schemas params from) columns) where
    tab ! col = tab ! col `as` col

instance
  ( HasUnique tab from row
  , Has col row ty
  , GroupedBy tab col bys
  , column ~ (col ::: defness :=> ty)
  ) => IsLabel col
    (ColumnExpression ('Grouped bys) schemas params from column) where
      fromLabel = fromLabel @col `as` Alias
instance
  ( HasUnique tab from row
  , Has col row ty
  , GroupedBy tab col bys
  , columns ~ '[col ::: defness :=> ty]
  ) => IsLabel col
    (NP (ColumnExpression ('Grouped bys) schemas params from) columns) where
      fromLabel = fromLabel @col `as` Alias

instance
  ( Has tab from row
  , Has col row ty
  , GroupedBy tab col bys
  , column ~ (col ::: defness :=> ty)
  ) => IsQualified tab col
    (ColumnExpression ('Grouped bys) schemas params from column) where
      tab ! col = tab ! col `as` col
instance
  ( Has tab from row
  , Has col row ty
  , GroupedBy tab col bys
  , columns ~ '[col ::: defness :=> ty]
  ) => IsQualified tab col
    (NP (ColumnExpression ('Grouped bys) schemas params from) columns) where
      tab ! col = tab ! col :* Nil

instance RenderSQL (ColumnExpression grp schemas params from column) where
  renderSQL = \case
    DefaultAs col ->
      renderSQL col <+> "=" <+> "DEFAULT"
    Specific (expression `As` col) ->
      renderSQL col <+> "=" <+> renderSQL expression

-- | A `ReturningClause` computes and return value(s) based
-- on each row actually inserted, updated or deleted. This is primarily
-- useful for obtaining values that were supplied by defaults, such as a
-- serial sequence number. However, any expression using the table's columns
-- is allowed. Only rows that were successfully inserted or updated or
-- deleted will be returned. For example, if a row was locked
-- but not updated because an `OnConflictDoUpdate` condition was not satisfied,
-- the row will not be returned. `ReturningStar` will return all columns
-- in the row. Use @Returning Nil@ in the common case where no return
-- values are desired.
newtype ReturningClause commons schemas params from row =
  Returning (Selection '[] 'Ungrouped commons schemas params from row)

instance RenderSQL (ReturningClause commons schemas params from row) where
  renderSQL = \case
    Returning (List Nil) -> ""
    Returning selection -> " RETURNING" <+> renderSQL selection

pattern Returning_
  :: SOP.SListI row
  => NP (Aliased (Expression '[] 'Ungrouped commons schemas params from)) row
  -> ReturningClause commons schemas params from row
pattern Returning_ list = Returning (List list)

-- | A `ConflictClause` specifies an action to perform upon a constraint
-- violation. `OnConflictDoRaise` will raise an error.
-- `OnConflictDoNothing` simply avoids inserting a row.
-- `OnConflictDoUpdate` updates the existing row that conflicts with the row
-- proposed for insertion.
data ConflictClause tab commons schemas params table where
  OnConflictDoRaise :: ConflictClause tab commons schemas params table
  OnConflict
    :: ConflictTarget constraints
    -> ConflictAction tab commons schemas params columns
    -> ConflictClause tab commons schemas params (constraints :=> columns)

-- | Render a `ConflictClause`.
instance SOP.SListI (TableToColumns table)
  => RenderSQL (ConflictClause tab commons schemas params table) where
    renderSQL = \case
      OnConflictDoRaise -> ""
      OnConflict target action -> " ON CONFLICT"
        <+> renderSQL target <+> renderSQL action

data ConflictAction tab commons schemas params columns where
  DoNothing :: ConflictAction tab commons schemas params columns
  DoUpdate
    :: ( row ~ ColumnsToRow columns
       , SOP.SListI columns
       , columns ~ (col0 ': cols)
       , SOP.All (HasIn columns) subcolumns
       , AllUnique subcolumns )
    => NP (ColumnExpression 'Ungrouped schemas params '[tab ::: row, "excluded" ::: row]) subcolumns
    -> [Condition '[] 'Ungrouped commons schemas params '[tab ::: row, "excluded" ::: row]]
    -> ConflictAction tab commons schemas params columns

instance RenderSQL (ConflictAction tab commons schemas params columns) where
  renderSQL = \case
    DoNothing -> "DO NOTHING"
    DoUpdate updates whs'
      -> "DO UPDATE SET"
        <+> renderCommaSeparated renderSQL updates
        <> case whs' of
          [] -> ""
          wh:whs -> " WHERE" <+> renderSQL (foldr (.&&) wh whs)

-- | A `ConflictTarget` specifies the constraint violation that triggers a
-- `ConflictAction`.
data ConflictTarget constraints where
  OnConstraint
    :: Has con constraints constraint
    => Alias con
    -> ConflictTarget constraints

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
  :: ( SOP.SListI columns
     , SOP.SListI row1
     , db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row0 ~ TableToRow table
     , columns ~ TableToColumns table
     , SOP.All (HasIn columns) subcolumns
     , AllUnique subcolumns )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (ColumnExpression 'Ungrouped schemas params '[tab ::: row0]) subcolumns
  -- ^ modified values to replace old values
  -> Condition '[] 'Ungrouped commons schemas params '[tab ::: row0]
  -- ^ condition under which to perform update on a row
  -> ReturningClause commons schemas params '[tab ::: row0] row1 -- ^ results to return
  -> Manipulation commons schemas params row1
update tab columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderSQL tab
  <+> "SET"
  <+> renderCommaSeparated renderSQL columns
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

-- | Update a row returning `Nil`.
update_
  :: ( SOP.SListI columns
     , db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table
     , SOP.All (HasIn columns) subcolumns
     , AllUnique subcolumns )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (ColumnExpression 'Ungrouped schemas params '[tab ::: row]) subcolumns
  -- ^ modified values to replace old values
  -> Condition '[] 'Ungrouped commons schemas params '[tab ::: row]
  -- ^ condition under which to perform update on a row
  -> Manipulation commons schemas params '[]
update_ tab columns wh = update tab columns wh (Returning_ Nil)

{-----------------------------------------
DELETE statements
-----------------------------------------}

data UsingClause commons schemas params from where
  NoUsing :: UsingClause commons schemas params '[]
  Using
    :: FromClause '[] commons schemas params from
    -> UsingClause commons schemas params from

-- | Delete rows from a table.
deleteFrom
  :: ( SOP.SListI row1
     , db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row0 ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> UsingClause commons schemas params from
  -> Condition '[] 'Ungrouped commons schemas params (tab ::: row0 ': from)
  -- ^ condition under which to delete a row
  -> ReturningClause commons schemas params '[tab ::: row0] row1 -- ^ results to return
  -> Manipulation commons schemas params row1
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
  :: ( db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> Condition '[] 'Ungrouped commons schemas params '[tab ::: row]
  -- ^ condition under which to delete a row
  -> Manipulation commons schemas params '[]
deleteFrom_ tab wh = deleteFrom tab NoUsing wh (Returning_ Nil)

-- | This has the behaviour of a cartesian product, taking all
-- possible combinations between @left@ and @right@ - exactly like a
-- CROSS JOIN. Used when no "CROSS JOIN" syntax is required but simply
-- a comma separated list of tables. Typical case is the `USING` clause
-- of a DELETE query.
also
  :: FromClause outer commons schemas params right
  -- ^ right
  -> FromClause outer commons schemas params left
  -- ^ left
  -> FromClause outer commons schemas params (Join left right)
also right left = UnsafeFromClause $
  renderSQL left <> "," <+> renderSQL right

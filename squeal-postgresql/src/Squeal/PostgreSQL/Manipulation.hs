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
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Manipulation
  ( -- * Manipulation
    Manipulation (..)
  , queryStatement
  , QueryClause (..)
  , pattern Values_
  , DefaultAliasable (..)
  , ColumnExpression (..)
  , ReturningClause (..)
  , ConflictClause (..)
  , ConflictTarget (..)
  , ConflictAction (..)
  , UsingClause (..)
    -- * Insert
  , insertInto
  , insertInto_
  , renderReturningClause
  , renderConflictClause
  , renderConflictTarget
  , renderConflictAction
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
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError)

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
  manipulation :: Manipulation (DBof (Public Schema)) '[] '[]
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
  manipulation :: Manipulation (DBof (Public Schema)) '[ 'NotNull 'PGint4, 'NotNull 'PGint4 ] '[]
  manipulation =
    insertInto_ #tab (Values_ (param @1 `as` #col1 :* param @2 `as` #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (($1 :: int4), ($2 :: int4))

returning insert:

>>> :{
let
  manipulation :: Manipulation (DBof (Public Schema)) '[] '["fromOnly" ::: 'NotNull 'PGint4]
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
  manipulation :: Manipulation (DBof (Public CustomersSchema)) '[] '[]
  manipulation =
    insertInto #customers
      (Values_ ("John Smith" `as` #name :* "john@smith.com" `as` #email))
      (OnConflict (OnConstraint #uq)
        (DoUpdate (((#excluded ! #email) <> "; " <> (#customers ! #email)) `as` #email) []))
      (Returning Nil)
in printSQL manipulation
:}
INSERT INTO "customers" ("name", "email") VALUES (E'John Smith', E'john@smith.com') ON CONFLICT ON CONSTRAINT "uq" DO UPDATE SET "email" = ("excluded"."email" || (E'; ' || "customers"."email"))

query insert:

>>> :{
let
  manipulation :: Manipulation (DBof (Public Schema)) '[] '[]
  manipulation = insertInto_ #tab (Subquery (selectStar (from (table #tab))))
in printSQL manipulation
:}
INSERT INTO "tab" SELECT * FROM "tab" AS "tab"

update:

>>> :{
let
  manipulation :: Manipulation (DBof (Public Schema)) '[] '[]
  manipulation = update_ #tab (2 `as` #col1) (#col1 ./= #col2)
in printSQL manipulation
:}
UPDATE "tab" SET "col1" = 2 WHERE ("col1" <> "col2")

delete:

>>> :{
let
  manipulation :: Manipulation (DBof (Public Schema)) '[]
    '[ "col1" ::: 'NotNull 'PGint4
     , "col2" ::: 'NotNull 'PGint4 ]
  manipulation = deleteFrom #tab NoUsing (#col1 .== #col2) ReturningStar
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
  manipulation :: Manipulation (DBof (Public Schema3)) '[] '[]
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
  manipulation :: Manipulation (DBof (Public ProductsSchema)) '[ 'NotNull 'PGdate] '[]
  manipulation = with
    (deleteFrom #products NoUsing (#date .< param @1) ReturningStar `as` #del)
    (insertInto_ #products_deleted (Subquery (selectStar (from (common #del)))))
in printSQL manipulation
:}
WITH "del" AS (DELETE FROM "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" SELECT * FROM "del" AS "del"
-}

newtype Manipulation
  (db :: DBType)
  (params :: [NullityType])
  (columns :: RowType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Manipulation db params columns) where
  renderSQL = renderManipulation
instance With Manipulation where
  with Done manip = manip
  with (cte :>> ctes) manip = UnsafeManipulation $
    "WITH" <+> renderCommonTableExpressions renderManipulation cte ctes
    <+> renderManipulation manip

-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query db params columns
  -> Manipulation db params columns
queryStatement q = UnsafeManipulation $ renderQuery q

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
  :: ( db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , row ~ TableToRow table
     , SOP.SListI columns
     , SOP.SListI result )
  => QualifiedAlias sch tab
  -> QueryClause db params columns
  -> ConflictClause tab db params table
  -> ReturningClause db params row result
  -> Manipulation db params result
insertInto tab qry conflict ret = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderQualifiedAlias tab
  <+> renderQueryClause qry
  <> renderConflictClause conflict
  <> renderReturningClause ret

insertInto_
  :: ( db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , row ~ TableToRow table
     , SOP.SListI columns )
  => QualifiedAlias sch tab
  -> QueryClause db params columns
  -> Manipulation db params '[]
insertInto_ tab qry =
  insertInto tab qry OnConflictDoRaise (Returning Nil)

data QueryClause db params columns where
  Values
    :: SOP.SListI columns
    => NP (ColumnExpression db '[] 'Ungrouped params) columns
    -> [NP (ColumnExpression db '[] 'Ungrouped params) columns]
    -> QueryClause db params columns
  Select
    :: SOP.SListI columns
    => NP (ColumnExpression db from grp params) columns
    -> TableExpression db params from grp
    -> QueryClause db params columns
  Subquery
    :: ColumnsToRow columns ~ row
    => Query db params row
    -> QueryClause db params columns

renderQueryClause
  :: QueryClause db params columns
  -> ByteString
renderQueryClause = \case
  Values row0 rows ->
    parenthesized (renderCommaSeparated renderAliasPart row0)
    <+> "VALUES"
    <+> commaSeparated
          ( parenthesized
          . renderCommaSeparated renderValuePart <$> row0 : rows )
  Select row0 tab ->
    parenthesized (renderCommaSeparatedMaybe renderAliasPartMaybe row0)
    <+> "SELECT"
    <+> renderCommaSeparatedMaybe renderValuePartMaybe row0
    <+> renderTableExpression tab
  Subquery qry -> renderQuery qry
  where
    renderAliasPartMaybe, renderValuePartMaybe
      :: ColumnExpression schema from grp params column -> Maybe ByteString
    renderAliasPartMaybe = \case
      DefaultAs _ -> Nothing
      Specific (_ `As` name) -> Just $ renderAlias name
    renderValuePartMaybe = \case
      DefaultAs _ -> Nothing
      Specific (value `As` _) -> Just $ renderExpression value
    renderAliasPart, renderValuePart
      :: ColumnExpression schema from grp params column -> ByteString
    renderAliasPart = \case
      DefaultAs name -> renderAlias name
      Specific (_ `As` name) -> renderAlias name
    renderValuePart = \case
      DefaultAs _ -> "DEFAULT"
      Specific (value `As` _) -> renderExpression value

pattern Values_
  :: SOP.SListI columns
  => NP (ColumnExpression schema '[] 'Ungrouped params) columns
  -> QueryClause schema params columns
pattern Values_ vals = Values vals []

data ColumnExpression schema from grp params column where
  DefaultAs
    :: KnownSymbol col
    => Alias col
    -> ColumnExpression schema from grp params (col ::: 'Def :=> ty)
  Specific
    :: Aliased (Expression schema from grp params) (col ::: ty)
    -> ColumnExpression schema from grp params (col ::: defness :=> ty)

instance (KnownSymbol alias, column ~ (alias ::: defness :=> ty))
  => Aliasable alias
       (Expression schema from grp params ty)
       (ColumnExpression schema from grp params column) where
         expression `as` alias = Specific (expression `As` alias)
instance (KnownSymbol alias, columns ~ '[alias ::: defness :=> ty])
  => Aliasable alias
       (Expression schema from grp params ty)
       (NP (ColumnExpression schema from grp params) columns) where
         expression `as` alias = expression `as` alias :* Nil
instance (KnownSymbol col, column ~ (col ::: 'Def :=> ty))
  => DefaultAliasable col (ColumnExpression schema from grp params column) where
    defaultAs = DefaultAs
instance (KnownSymbol col, columns ~ '[col ::: 'Def :=> ty])
  => DefaultAliasable col (NP (ColumnExpression schema from grp params) columns) where
    defaultAs col = defaultAs col :* Nil

instance (HasUnique table from columns, Has column columns ty, colty ~ (column ::: defness :=> ty))
  => IsLabel column (ColumnExpression schema from 'Ungrouped params colty) where
    fromLabel = fromLabel @column `as` Alias
instance (HasUnique table from columns, Has column columns ty, columnLabel ~ column, coltys ~ '[column ::: defness :=> ty])
  => IsLabel columnLabel
    (NP (ColumnExpression schema from 'Ungrouped params) coltys) where
    fromLabel = fromLabel @column `as` Alias

instance (Has table from columns, Has column columns ty, colty ~ (column ::: defness :=> ty))
  => IsQualified table column (ColumnExpression schema from 'Ungrouped params colty) where
    tab ! column = tab ! column `as` column
instance (Has table from columns, Has column columns ty, coltys ~ '[column ::: defness :=> ty])
  => IsQualified table column (NP (ColumnExpression schema from 'Ungrouped params) coltys) where
    tab ! column = tab ! column `as` column

instance
  ( HasUnique table from columns
  , Has column columns ty
  , GroupedBy table column bys
  , colty ~ (column ::: defness :=> ty)
  ) => IsLabel column
    (ColumnExpression schema from ('Grouped bys) params colty) where
      fromLabel = fromLabel @column `as` Alias
instance
  ( HasUnique table from columns
  , Has column columns ty
  , GroupedBy table column bys
  , coltys ~ '[column ::: defness :=> ty]
  ) => IsLabel column
    (NP (ColumnExpression schema from ('Grouped bys) params) coltys) where
      fromLabel = fromLabel @column `as` Alias

instance
  ( Has tab from row
  , Has col row ty
  , GroupedBy tab col bys
  , colty ~ (col ::: defness :=> ty)
  ) => IsQualified tab col
    (ColumnExpression db from ('Grouped bys) params colty) where
      tab ! col = tab ! col `as` col
instance
  ( Has tab from row
  , Has col row ty
  , GroupedBy tab col bys
  , coltys ~ '[col ::: defness :=> ty]
  ) => IsQualified tab col
    (NP (ColumnExpression db from ('Grouped bys) params) coltys) where
      tab ! col = tab ! col :* Nil

renderColumnExpression
  :: ColumnExpression db from grp params column
  -> ByteString
renderColumnExpression = \case
  DefaultAs col ->
    renderAlias col <+> "=" <+> "DEFAULT"
  Specific (expression `As` col) ->
    renderAlias col <+> "=" <+> renderExpression expression

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
data ReturningClause
  (db :: DBType)
  (params :: [NullityType])
  (row0 :: RowType)
  (row1 :: RowType)
  where
    ReturningStar
      :: ReturningClause db params row row
    Returning
      :: NP (Aliased (Expression db '[table ::: row0] 'Ungrouped params)) row1
      -> ReturningClause db params row0 row1

-- | Render a `ReturningClause`.
renderReturningClause
  :: SOP.SListI results
  => ReturningClause db params columns results
  -> ByteString
renderReturningClause = \case
  ReturningStar -> " RETURNING *"
  Returning Nil -> ""
  Returning results -> " RETURNING"
    <+> renderCommaSeparated (renderAliasedAs renderExpression) results

-- | A `ConflictClause` specifies an action to perform upon a constraint
-- violation. `OnConflictDoRaise` will raise an error.
-- `OnConflictDoNothing` simply avoids inserting a row.
-- `OnConflictDoUpdate` updates the existing row that conflicts with the row
-- proposed for insertion.
data ConflictClause tab db params table where
  OnConflictDoRaise :: ConflictClause tab db params table
  OnConflict
    :: ConflictTarget constraints
    -> ConflictAction tab db params columns
    -> ConflictClause tab db params (constraints :=> columns)

-- | Render a `ConflictClause`.
renderConflictClause
  :: SOP.SListI (TableToColumns table)
  => ConflictClause tab db params table
  -> ByteString
renderConflictClause = \case
  OnConflictDoRaise -> ""
  OnConflict target action -> " ON CONFLICT"
    <+> renderConflictTarget target <+> renderConflictAction action

data ConflictAction tab db params columns where
  DoNothing :: ConflictAction tab db params columns
  DoUpdate
    :: ( row ~ ColumnsToRow columns
       , SOP.SListI columns
       , columns ~ (col0 ': cols)
       , SOP.All (HasIn columns) subcolumns
       , AllUnique subcolumns )
    => NP (ColumnExpression db '[tab ::: row, "excluded" ::: row] 'Ungrouped params) subcolumns
    -> [Condition db '[tab ::: row, "excluded" ::: row] 'Ungrouped params]
    -> ConflictAction tab db params columns

renderConflictAction
  :: ConflictAction tab db params columns
  -> ByteString
renderConflictAction = \case
  DoNothing -> "DO NOTHING"
  DoUpdate updates whs'
    -> "DO UPDATE SET"
      <+> renderCommaSeparated renderColumnExpression updates
      <> case whs' of
        [] -> ""
        wh:whs -> " WHERE" <+> renderExpression (foldr (.&&) wh whs)

-- | A `ConflictTarget` specifies the constraint violation that triggers a
-- `ConflictAction`.
data ConflictTarget constraints where
  OnConstraint
    :: Has con constraints constraint
    => Alias con
    -> ConflictTarget constraints

-- | Render a `ConflictTarget`
renderConflictTarget
  :: ConflictTarget constraints
  -> ByteString
renderConflictTarget (OnConstraint con) =
  "ON" <+> "CONSTRAINT" <+> renderAlias con

{-----------------------------------------
UPDATE statements
-----------------------------------------}

-- In order to guide type inference, also enforces that a key appearing in
-- the common subset prefix of both sup and sub maps to the same value (type).
type family OrderedSubsetOf'
    (origSup :: [(Symbol, a)]) (origSub :: [(Symbol, a)])
    (sup     :: [(Symbol, a)]) (sub     :: [(Symbol, a)])
    :: Constraint where
  OrderedSubsetOf' _ _ _ '[] = ()
  OrderedSubsetOf' sup sub ('(xk, xv) ': xs) ('(xk, yv) ': ys) =
    (xv ~ yv, OrderedSubsetOf' sup sub xs ys)
  OrderedSubsetOf' sup sub (x ': xs) (y ': ys) =
    OrderedSubsetOf' sup sub xs (y ': ys)
  OrderedSubsetOf' sup sub '[] _ = TypeError
    (     'Text "The type"
    ':$$: 'ShowType sub
    ':$$: 'Text "is not an ordered subset of"
    ':$$: 'ShowType sup)

class    (OrderedSubsetOf' sup sub sup sub) => OrderedSubsetOf sup sub where
instance (OrderedSubsetOf' sup sub sup sub) => OrderedSubsetOf sup sub where

-- | An `update` command changes the values of the specified columns
-- in all rows that satisfy the condition.
update
  :: ( SOP.SListI columns
     , SOP.SListI results
     , db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table
     , SOP.All (HasIn columns) subcolumns
     , AllUnique subcolumns )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (ColumnExpression db '[tab ::: row] 'Ungrouped params) subcolumns
  -- ^ modified values to replace old values
  -> Condition db '[tab ::: row] 'Ungrouped params
  -- ^ condition under which to perform update on a row
  -> ReturningClause db params row results -- ^ results to return
  -> Manipulation db params results
update tab columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderQualifiedAlias tab
  <+> "SET"
  <+> renderCommaSeparated renderColumnExpression columns
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

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
  -> NP (ColumnExpression db '[tab ::: row] 'Ungrouped params) subcolumns
  -- ^ modified values to replace old values
  -> Condition db '[tab ::: row] 'Ungrouped params
  -- ^ condition under which to perform update on a row
  -> Manipulation db params '[]
update_ tab columns wh = update tab columns wh (Returning Nil)

{-----------------------------------------
DELETE statements
-----------------------------------------}

data UsingClause db params from where
  NoUsing :: UsingClause db params '[]
  Using
    :: FromClause db params from
    -> UsingClause db params from

-- | Delete rows from a table.
deleteFrom
  :: ( SOP.SListI results
     , db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> UsingClause db params from
  -> Condition db (tab ::: row ': from) 'Ungrouped params
  -- ^ condition under which to delete a row
  -> ReturningClause db params row results -- ^ results to return
  -> Manipulation db params results
deleteFrom tab using wh returning = UnsafeManipulation $
  "DELETE FROM"
  <+> renderQualifiedAlias tab
  <> case using of
    NoUsing -> ""
    Using tables -> " USING" <+> renderFromClause tables
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

-- | Delete rows returning `Nil`.
deleteFrom_
  :: ( db ~ (commons :=> schemas)
     , Has sch schemas schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> UsingClause db params from
  -> Condition db (tab ::: row ': from) 'Ungrouped params
  -- ^ condition under which to delete a row
  -> Manipulation db params '[]
deleteFrom_ tab uses wh = deleteFrom tab uses wh (Returning Nil)

-- | This has the behaviour of a cartesian product, taking all
-- possible combinations between @left@ and @right@ - exactly like a
-- CROSS JOIN. Used when no "CROSS JOIN" syntax is required but simply
-- a comma separated list of tables. Typical case is the `USING` clause
-- of a DELETE query.
also
  :: FromClause schema params right
  -- ^ right
  -> FromClause schema params left
  -- ^ left
  -> FromClause schema params (Join left right)
also right left = UnsafeFromClause $
  renderFromClause left <> "," <+> renderFromClause right

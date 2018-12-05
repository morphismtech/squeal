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
but does not alter the schema. Examples are inserts, updates and deletes.
A `Query` is also considered a `Manipulation` even though it does not modify data.

simple insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'Def :=> 'NotNull 'PGint4 ])] '[] '[]
  manipulation =
    insertInto_ #tab (Values_ (2 `as` #col1 :* defaultAs #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (2, DEFAULT)

parameterized insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4 ])]
    '[ 'NotNull 'PGint4, 'NotNull 'PGint4 ] '[]
  manipulation =
    insertInto_ #tab (Values_ (param @1 `as` #col1 :* param @2 `as` #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (($1 :: int4), ($2 :: int4))

returning insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'Def :=> 'NotNull 'PGint4 ])] '[]
    '["fromOnly" ::: 'NotNull 'PGint4]
  manipulation =
    insertInto #tab (Values_ (2 `as` #col1 :* defaultAs #col2))
      OnConflictDoRaise (Returning (#col1 `as` #fromOnly))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (2, DEFAULT) RETURNING "col1" AS "fromOnly"

upsert:

>>> :{
let
  manipulation :: Manipulation
    '[ "customers" ::: 'Table ('["uq" ::: 'Unique '["name"]] :=>
      '[ "name" ::: 'NoDef :=> 'NotNull 'PGtext
       , "email" ::: 'NoDef :=> 'NotNull 'PGtext ])]
    '[] '[]
  manipulation =
    insertInto #customers
      (Values_ ("John Smith" `as` #name :* "john@smith.com" `as` #email))
      (OnConflict (OnConstraint #uq)
        (DoUpdate (((#excluded ! #email) <> ";" <> (#customers ! #email)) `as` #email) []))
      (Returning Nil)
in printSQL manipulation
:}
INSERT INTO "customers" ("name", "email") VALUES (E'John Smith', E'john@smith.com') ON CONFLICT ON CONSTRAINT "uq" DO UPDATE SET "email" = ("excluded"."email" || (E';' || "customers"."email"))

query insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4
       ])
     , "other_tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4
       ])
     ] '[] '[]
  manipulation =
    insertInto_ #tab
      (Subquery (selectStar (from (table (#other_tab `as` #t)))))
in printSQL manipulation
:}
INSERT INTO "tab" SELECT * FROM "other_tab" AS "t"

update:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4 ])] '[] '[]
  manipulation = update_ #tab (2 `as` #col1) (#col1 ./= #col2)
in printSQL manipulation
:}
UPDATE "tab" SET "col1" = 2 WHERE ("col1" <> "col2")

delete:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4 ])] '[]
    '[ "col1" ::: 'NotNull 'PGint4
     , "col2" ::: 'NotNull 'PGint4 ]
  manipulation = deleteFrom #tab NoUsing (#col1 .== #col2) ReturningStar
in printSQL manipulation
:}
DELETE FROM "tab" WHERE ("col1" = "col2") RETURNING *

delete and using clause:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4
       ])
     , "other_tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4
       ])
     , "third_tab" ::: 'Table ('[] :=>
       '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
        , "col2" ::: 'NoDef :=> 'NotNull 'PGint4
        ])
     ] '[] '[]
  manipulation =
    deleteFrom #tab (Using (table #other_tab & also (table #third_tab)))
    ( (#tab ! #col2 .== #other_tab ! #col2)
    .&& (#tab ! #col2 .== #third_tab ! #col2) )
    (Returning Nil)
in printSQL manipulation
:}
DELETE FROM "tab" USING "other_tab" AS "other_tab", "third_tab" AS "third_tab" WHERE (("tab"."col2" = "other_tab"."col2") AND ("tab"."col2" = "third_tab"."col2"))

with manipulation:

>>> type ProductsTable = '[] :=> '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]

>>> :{
let
  manipulation :: Manipulation
    '[ "products" ::: 'Table ProductsTable
     , "products_deleted" ::: 'Table ProductsTable
     ] '[ 'NotNull 'PGdate] '[]
  manipulation = with
    (deleteFrom #products NoUsing (#date .< param @1) ReturningStar `as` #del)
    (insertInto_ #products_deleted (Subquery (selectStar (from (view #del)))))
in printSQL manipulation
:}
WITH "del" AS (DELETE FROM "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" SELECT * FROM "del" AS "del"
-}

newtype Manipulation
  (schema :: SchemaType)
  (params :: [NullityType])
  (columns :: RowType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Manipulation schema params columns) where
  renderSQL = renderManipulation
instance With Manipulation where
  with Done manip = manip
  with (cte :>> ctes) manip = UnsafeManipulation $
    "WITH" <+> renderCommonTableExpressions renderManipulation cte ctes
    <+> renderManipulation manip

-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query schema params columns
  -> Manipulation schema params columns
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
  :: ( Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , row ~ TableToRow table
     , SOP.SListI columns
     , SOP.SListI result )
  => Alias tab
  -> QueryClause schema params columns
  -> ConflictClause tab schema params table
  -> ReturningClause schema params row result
  -> Manipulation schema params result
insertInto tab qry conflict ret = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderAlias tab
  <+> renderQueryClause qry
  <> renderConflictClause conflict
  <> renderReturningClause ret

insertInto_
  :: ( Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , row ~ TableToRow table
     , SOP.SListI columns )
  => Alias tab
  -> QueryClause schema params columns
  -> Manipulation schema params '[]
insertInto_ tab qry =
  insertInto tab qry OnConflictDoRaise (Returning Nil)

data QueryClause schema params columns where
  Values
    :: SOP.SListI columns
    => NP (ColumnExpression schema '[] 'Ungrouped params) columns
    -> [NP (ColumnExpression schema '[] 'Ungrouped params) columns]
    -> QueryClause schema params columns
  Select
    :: SOP.SListI columns
    => NP (ColumnExpression schema from grp params) columns
    -> TableExpression schema params from grp
    -> QueryClause schema params columns
  Subquery
    :: ColumnsToRow columns ~ row
    => Query schema params row
    -> QueryClause schema params columns

renderQueryClause
  :: QueryClause schema params columns
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
  ( Has table from columns
  , Has column columns ty
  , GroupedBy table column bys
  , colty ~ (column ::: defness :=> ty)
  ) => IsQualified table column
    (ColumnExpression schema from ('Grouped bys) params colty) where
      tab ! column = tab ! column `as` column
instance
  ( Has table from columns
  , Has column columns ty
  , GroupedBy table column bys
  , coltys ~ '[column ::: defness :=> ty]
  ) => IsQualified table column
    (NP (ColumnExpression schema from ('Grouped bys) params) coltys) where
      tab ! column = tab ! column :* Nil

renderColumnExpression
  :: ColumnExpression schema from grp params column
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
  (schema :: SchemaType)
  (params :: [NullityType])
  (row0 :: RowType)
  (row1 :: RowType)
  where
    ReturningStar
      :: ReturningClause schema params row row
    Returning
      :: NP (Aliased (Expression schema '[table ::: row0] 'Ungrouped params)) row1
      -> ReturningClause schema params row0 row1

-- | Render a `ReturningClause`.
renderReturningClause
  :: SOP.SListI results
  => ReturningClause schema params columns results
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
data ConflictClause tab schema params table where
  OnConflictDoRaise :: ConflictClause tab schema params table
  OnConflict
    :: ConflictTarget constraints
    -> ConflictAction tab schema params columns
    -> ConflictClause tab schema params (constraints :=> columns)

-- | Render a `ConflictClause`.
renderConflictClause
  :: SOP.SListI (TableToColumns table)
  => ConflictClause tab schema params table
  -> ByteString
renderConflictClause = \case
  OnConflictDoRaise -> ""
  OnConflict target action -> " ON CONFLICT"
    <+> renderConflictTarget target <+> renderConflictAction action

data ConflictAction tab schema params columns where
  DoNothing :: ConflictAction tab schema params columns
  DoUpdate
    :: ( row ~ ColumnsToRow columns
       , SOP.SListI columns
       , columns ~ (col0 ': cols)
       , SOP.All (HasIn columns) subcolumns
       , AllUnique subcolumns )
    => NP (ColumnExpression schema '[tab ::: row, "excluded" ::: row] 'Ungrouped params) subcolumns
    -> [Condition schema '[tab ::: row, "excluded" ::: row] 'Ungrouped params]
    -> ConflictAction tab schema params columns

renderConflictAction
  :: ConflictAction tab schema params columns
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
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table
     , SOP.All (HasIn columns) subcolumns
     , AllUnique subcolumns )
  => Alias tab -- ^ table to update
  -> NP (ColumnExpression schema '[tab ::: row] 'Ungrouped params) subcolumns
  -- ^ modified values to replace old values
  -> Condition schema '[tab ::: row] 'Ungrouped params
  -- ^ condition under which to perform update on a row
  -> ReturningClause schema params row results -- ^ results to return
  -> Manipulation schema params results
update tab columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderAlias tab
  <+> "SET"
  <+> renderCommaSeparated renderColumnExpression columns
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

-- | Update a row returning `Nil`.
update_
  :: ( SOP.SListI columns
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table
     , SOP.All (HasIn columns) subcolumns
     , AllUnique subcolumns )
  => Alias tab -- ^ table to update
  -> NP (ColumnExpression schema '[tab ::: row] 'Ungrouped params) subcolumns
  -- ^ modified values to replace old values
  -> Condition schema '[tab ::: row] 'Ungrouped params
  -- ^ condition under which to perform update on a row
  -> Manipulation schema params '[]
update_ tab columns wh = update tab columns wh (Returning Nil)

{-----------------------------------------
DELETE statements
-----------------------------------------}

data UsingClause schema params from where
  NoUsing :: UsingClause schema params '[]
  Using
    :: FromClause schema params from
    -> UsingClause schema params from

-- | Delete rows from a table.
deleteFrom
  :: ( SOP.SListI results
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to delete from
  -> UsingClause schema params from
  -- ^-- tables to add to the scope - more tables can be added through `also`.
  -> Condition schema (tab ::: row ': from) 'Ungrouped params
  -- ^ condition under which to delete a row
  -> ReturningClause schema params row results -- ^ results to return
  -> Manipulation schema params results
deleteFrom tgt uses cond returning =
  let renderUsing NoUsing = mempty
      renderUsing (Using fc) = "USING" <+> renderFromClause fc <> " "
  in UnsafeManipulation $
    "DELETE FROM" <+> renderAlias tgt
    <+> renderUsing uses
    <> "WHERE" <+> renderExpression cond
    <> renderReturningClause returning

-- | Delete rows returning `Nil`.
deleteFrom_
  :: ( Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to delete from
  -> UsingClause schema params from
  -- ^-- tables to add to the scope - more tables can be added through `also`.
  -> Condition schema (tab ::: row ': from) 'Ungrouped params
  -- ^ condition under which to delete a row
  -> Manipulation schema params '[]
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

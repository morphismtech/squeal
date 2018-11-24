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
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , TypeInType
  , TypeOperators
#-}

module Squeal.PostgreSQL.Manipulation
  ( -- * Manipulation
    Manipulation (UnsafeManipulation, renderManipulation)
  , queryStatement
  , ColumnValue (..)
  , ReturningClause (ReturningStar, Returning)
  , ConflictClause (OnConflictDoRaise, OnConflictDoNothing, OnConflictDoUpdate)
    -- * Insert
  , insertRows
  , insertRow
  , insertRows_
  , insertRow_
  , insertQuery
  , insertQuery_
  , renderReturningClause
  , renderConflictClause
    -- * Update
  , update
  , update_
    -- * Delete
  , deleteFrom
  , deleteFrom_
  ) where

import Control.DeepSeq
import Data.ByteString hiding (foldr)

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

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'Def :=> 'NotNull 'PGint4 ])] '[] '[]
  manipulation =
    insertRow_ #tab (Set 2 `as` #col1 :* Default `as` #col2)
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
    insertRow_ #tab
      (Set (param @1) `as` #col1 :* Set (param @2) `as` #col2)
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
    insertRow #tab (Set 2 `as` #col1 :* Default `as` #col2)
      OnConflictDoRaise (Returning (#col1 `as` #fromOnly))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (2, DEFAULT) RETURNING "col1" AS "fromOnly"

upsert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: 'Table ('[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4 ])]
    '[] '[ "sum" ::: 'NotNull 'PGint4]
  manipulation =
    insertRows #tab
      (Set 2 `as` #col1 :* Set 4 `as` #col2)
      [Set 6 `as` #col1 :* Set 8 `as` #col2]
      (OnConflictDoUpdate
        (Set 2 `as` #col1 :* Same `as` #col2)
        [#col1 .== #col2])
      (Returning $ (#col1 + #col2) `as` #sum)
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (2, 4), (6, 8) ON CONFLICT DO UPDATE SET "col1" = 2 WHERE ("col1" = "col2") RETURNING ("col1" + "col2") AS "sum"

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
    insertQuery_ #tab
      (selectStar (from (table (#other_tab `as` #t))))
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
  manipulation =
    update_ #tab (Set 2 `as` #col1 :* Same `as` #col2)
      (#col1 ./= #col2)
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
  manipulation = deleteFrom #tab (#col1 .== #col2) ReturningStar
in printSQL manipulation
:}
DELETE FROM "tab" WHERE ("col1" = "col2") RETURNING *

with manipulation:

>>> type ProductsTable = '[] :=> '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]

>>> :{
let
  manipulation :: Manipulation
    '[ "products" ::: 'Table ProductsTable
     , "products_deleted" ::: 'Table ProductsTable
     ] '[ 'NotNull 'PGdate] '[]
  manipulation = with
    (deleteFrom #products (#date .< param @1) ReturningStar `as` #deleted_rows)
    (insertQuery_ #products_deleted (selectStar (from (view (#deleted_rows `as` #t)))))
in printSQL manipulation
:}
WITH "deleted_rows" AS (DELETE FROM "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" SELECT * FROM "deleted_rows" AS "t"
-}

newtype Manipulation
  (db :: DBType)
  (params :: [NullityType])
  (columns :: RowType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Manipulation db params columns) where
  renderSQL = renderManipulation
{--------------FIX WITH

instance With Manipulation where
  with Done manip = manip
  with (cte :>> ctes) manip = UnsafeManipulation $
    "WITH" <+> renderCommonTableExpressions renderManipulation cte ctes
    <+> renderManipulation manip
-}
-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query db params columns
  -> Manipulation db params columns
queryStatement q = UnsafeManipulation $ renderQuery q

{-----------------------------------------
INSERT statements
-----------------------------------------}

-- | Insert multiple rows.
--
-- When a table is created, it contains no data. The first thing to do
-- before a database can be of much use is to insert data. Data is
-- conceptually inserted one row at a time. Of course you can also insert
-- more than one row, but there is no way to insert less than one row.
-- Even if you know only some column values, a complete row must be created.
insertRows
  :: ( SOP.SListI columns
     , SOP.SListI results
     , Has sch db schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to insert into
  -> NP (Aliased (ColumnValue db '[] params)) columns -- ^ row to insert
  -> [NP (Aliased (ColumnValue db '[] params)) columns] -- ^ more rows to insert
  -> ConflictClause db table params
  -- ^ what to do in case of constraint conflict
  -> ReturningClause db params row results -- ^ results to return
  -> Manipulation db params results
insertRows tab rw rws conflict returning = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderQualifiedAlias tab
    <+> parenthesized (renderCommaSeparated renderAliasPart rw)
    <+> "VALUES"
    <+> commaSeparated
          ( parenthesized
          . renderCommaSeparated renderColumnValuePart <$> rw:rws )
    <> renderConflictClause conflict
    <> renderReturningClause returning
    where
      renderAliasPart, renderColumnValuePart
        :: Aliased (ColumnValue db '[] params) ty -> ByteString
      renderAliasPart (_ `As` name) = renderAlias name
      renderColumnValuePart (value `As` _) = case value of
        Default -> "DEFAULT"
        Set expression -> renderExpression expression

-- | Insert a single row.
insertRow
  :: ( SOP.SListI columns
     , SOP.SListI results
     , Has sch db schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to insert into
  -> NP (Aliased (ColumnValue db '[] params)) columns -- ^ row to insert
  -> ConflictClause db table params
  -- ^ what to do in case of constraint conflict
  -> ReturningClause db params row results -- ^ results to return
  -> Manipulation db params results
insertRow tab rw = insertRows tab rw []

-- | Insert multiple rows returning `Nil` and raising an error on conflicts.
insertRows_
  :: ( SOP.SListI columns
     , Has sch db schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to insert into
  -> NP (Aliased (ColumnValue db '[] params)) columns -- ^ row to insert
  -> [NP (Aliased (ColumnValue db '[] params)) columns] -- ^ more rows to insert
  -> Manipulation db params '[]
insertRows_ tab rw rws =
  insertRows tab rw rws OnConflictDoRaise (Returning Nil)

-- | Insert a single row returning `Nil` and raising an error on conflicts.
insertRow_
  :: ( SOP.SListI columns
     , Has sch db schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to insert into
  -> NP (Aliased (ColumnValue db '[] params)) columns -- ^ row to insert
  -> Manipulation db params '[]
insertRow_ tab rw = insertRow tab rw OnConflictDoRaise (Returning Nil)

-- | Insert a `Query`.
insertQuery
  :: ( SOP.SListI columns
     , SOP.SListI results
     , Has sch db schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to insert into
  -> Query db params (TableToRow table)
  -> ConflictClause db table params
  -- ^ what to do in case of constraint conflict
  -> ReturningClause db params row results -- ^ results to return
  -> Manipulation db params results
insertQuery tab query conflict returning = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderQualifiedAlias tab
    <+> renderQuery query
    <> renderConflictClause conflict
    <> renderReturningClause returning

-- | Insert a `Query` returning `Nil` and raising an error on conflicts.
insertQuery_
  :: ( SOP.SListI columns
     , Has sch db schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to insert into
  -> Query db params (TableToRow table)
  -> Manipulation db params '[]
insertQuery_ tab query =
  insertQuery tab query OnConflictDoRaise (Returning Nil)

-- | `ColumnValue`s are values to insert or update in a row.
-- `Same` updates with the same value.
-- `Default` inserts or updates with the @DEFAULT@ value.
-- `Set` sets a value to be an `Expression`, which can refer to
-- existing value in the row for an update.
data ColumnValue
  (db :: DBType)
  (columns :: RowType)
  (params :: [NullityType])
  (ty :: ColumnType)
  where
    Same :: ColumnValue db (column ': columns) params ty
    Default :: ColumnValue db columns params ('Def :=> ty)
    Set
      :: (forall table. Expression db '[table ::: columns] 'Ungrouped params ty)
      -> ColumnValue db columns params (constraint :=> ty)

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
data ConflictClause
  (db :: DBType)
  (table :: TableType)
  (params :: [NullityType]) where
    OnConflictDoRaise :: ConflictClause db table params
    OnConflictDoNothing :: ConflictClause db table params
    OnConflictDoUpdate
      :: (row ~ TableToRow table, columns ~ TableToColumns table)
      => NP (Aliased (ColumnValue db row params)) columns
      -> [Condition db '[t ::: row] 'Ungrouped params]
      -> ConflictClause db table params

-- | Render a `ConflictClause`.
renderConflictClause
  :: SOP.SListI (TableToColumns table)
  => ConflictClause db table params
  -> ByteString
renderConflictClause = \case
  OnConflictDoRaise -> ""
  OnConflictDoNothing -> " ON CONFLICT DO NOTHING"
  OnConflictDoUpdate updates whs'
    -> " ON CONFLICT DO UPDATE SET"
      <+> renderCommaSeparatedMaybe renderUpdate updates
      <> case whs' of
        [] -> ""
        wh:whs -> " WHERE" <+> renderExpression (foldr (.&&) wh whs)
      where
        renderUpdate
          :: Aliased (ColumnValue db columns params) column
          -> Maybe ByteString
        renderUpdate = \case
          Same `As` _ -> Nothing
          Default `As` column -> Just $
            renderAlias column <+> "=" <+> "DEFAULT"
          Set expression `As` column -> Just $
            renderAlias column <+> "=" <+> renderExpression expression

{-----------------------------------------
UPDATE statements
-----------------------------------------}

-- | An `update` command changes the values of the specified columns
-- in all rows that satisfy the condition.
update
  :: ( SOP.SListI columns
     , SOP.SListI results
     , Has sch db schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (Aliased (ColumnValue db row params)) columns
  -- ^ modified values to replace old values
  -> (forall t. Condition db '[t ::: row] 'Ungrouped params)
  -- ^ condition under which to perform update on a row
  -> ReturningClause db params row results -- ^ results to return
  -> Manipulation db params results
update tab columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderQualifiedAlias tab
  <+> "SET"
  <+> renderCommaSeparatedMaybe renderUpdate columns
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning
  where
    renderUpdate
      :: Aliased (ColumnValue db columns params) column
      -> Maybe ByteString
    renderUpdate = \case
      Same `As` _ -> Nothing
      Default `As` column -> Just $
        renderAlias column <+> "=" <+> "DEFAULT"
      Set expression `As` column -> Just $
        renderAlias column <+> "=" <+> renderExpression expression

-- | Update a row returning `Nil`.
update_
  :: ( SOP.SListI columns
     , Has sch db schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (Aliased (ColumnValue db row params)) columns
  -- ^ modified values to replace old values
  -> (forall t. Condition db '[t ::: row] 'Ungrouped params)
  -- ^ condition under which to perform update on a row
  -> Manipulation db params '[]
update_ tab columns wh = update tab columns wh (Returning Nil)

{-----------------------------------------
DELETE statements
-----------------------------------------}

-- | Delete rows of a table.
deleteFrom
  :: ( SOP.SListI results
     , Has sch db schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> Condition db '[tab ::: row] 'Ungrouped params
  -- ^ condition under which to delete a row
  -> ReturningClause db params row results -- ^ results to return
  -> Manipulation db params results
deleteFrom tab wh returning = UnsafeManipulation $
  "DELETE FROM" <+> renderQualifiedAlias tab
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

-- | Delete rows returning `Nil`.
deleteFrom_
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , row ~ TableToRow table
     , columns ~ TableToColumns table )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> (forall t. Condition db '[t ::: row] 'Ungrouped params)
  -- ^ condition under which to delete a row
  -> Manipulation db params '[]
deleteFrom_ tab wh = deleteFrom tab wh (Returning Nil)

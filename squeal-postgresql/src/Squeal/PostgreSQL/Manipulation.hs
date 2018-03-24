{-|
Module: Squeal.PostgreSQL.Manipulation
Description: Squeal data manipulation language
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal data manipulation language.
-}

{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , DeriveGeneric
  , FlexibleContexts
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , StandaloneDeriving
  , TypeInType
  , TypeOperators
#-}

module Squeal.PostgreSQL.Manipulation
  ( -- * Manipulation
    Manipulation (UnsafeManipulation, renderManipulation)
  , queryStatement
    -- * Insert
  , insertRows
  , insertRow
  , insertRows_
  , insertRow_
  , insertQuery
  , insertQuery_
  , ReturningClause (ReturningStar, Returning)
  , renderReturningClause
  , ConflictClause (OnConflictDoRaise, OnConflictDoNothing, OnConflictDoUpdate)
  , renderConflictClause
    -- * Update
  , update
  , update_
  , deleteFrom
  , deleteFrom_
  , ColumnValue (..)
  ) where

import Control.DeepSeq
import Data.ByteString hiding (foldr)
import Data.Monoid

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

-- | A `Manipulation` is a statement which may modify data in the database,
-- but does not alter the schema. Examples are `insertInto`, `update` and
-- `deleteFrom`. A `Query` is also considered a `Manipulation` even though
-- it does not modify data.
newtype Manipulation
  (schema :: TablesType)
  (params :: [NullityType])
  (columns :: RelationType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query schema params columns
  -> Manipulation schema params columns
queryStatement q = UnsafeManipulation $ renderQuery q <> ";"

{-----------------------------------------
INSERT statements
-----------------------------------------}

{- |
When a table is created, it contains no data. The first thing to do
before a database can be of much use is to insert data. Data is
conceptually inserted one row at a time. Of course you can also insert
more than one row, but there is no way to insert less than one row.
Even if you know only some column values, a complete row must be created.

simple insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: '[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'Def :=> 'NotNull 'PGint4 ]] '[] '[]
  manipulation =
    insertRow_ #tab (Set 2 `As` #col1 :* Default `As` #col2 :* Nil)
in renderManipulation manipulation
:}
"INSERT INTO tab (col1, col2) VALUES (2, DEFAULT);"

parameterized insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: '[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4 ]]
    '[ 'NotNull 'PGint4, 'NotNull 'PGint4 ] '[]
  manipulation =
    insertRow_ #tab
      (Set (param @1) `As` #col1 :* Set (param @2) `As` #col2 :* Nil)
in renderManipulation manipulation
:}
"INSERT INTO tab (col1, col2) VALUES (($1 :: int4), ($2 :: int4));"

returning insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: '[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'Def :=> 'NotNull 'PGint4 ]] '[]
    '["fromOnly" ::: 'NotNull 'PGint4]
  manipulation =
    insertRow #tab (Set 2 `As` #col1 :* Default `As` #col2 :* Nil)
      OnConflictDoRaise (Returning (#col1 `As` #fromOnly :* Nil))
in renderManipulation manipulation
:}
"INSERT INTO tab (col1, col2) VALUES (2, DEFAULT) RETURNING col1 AS fromOnly;"

upsert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: '[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4 ]]
    '[] '[ "sum" ::: 'NotNull 'PGint4]
  manipulation =
    insertRows #tab
      (Set 2 `As` #col1 :* Set 4 `As` #col2 :* Nil)
      [Set 6 `As` #col1 :* Set 8 `As` #col2 :* Nil]
      (OnConflictDoUpdate
        (Set 2 `As` #col1 :* Same `As` #col2 :* Nil)
        [#col1 .== #col2])
      (Returning $ (#col1 + #col2) `As` #sum :* Nil)
in renderManipulation manipulation
:}
"INSERT INTO tab (col1, col2) VALUES (2, 4), (6, 8) ON CONFLICT DO UPDATE SET col1 = 2 WHERE (col1 = col2) RETURNING (col1 + col2) AS sum;"
-}
insertRows
  :: ( SOP.SListI columns
     , SOP.SListI results
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to insert into
  -> NP (Aliased (ColumnValue '[] params)) columns
  -> [NP (Aliased (ColumnValue '[] params)) columns]
  -> ConflictClause columns params
  -- ^ what to do in case of constraint conflict
  -> ReturningClause columns params results -- ^ results to return
  -> Manipulation schema params results
insertRows tab row rows conflict returning = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderAlias tab
    <+> parenthesized (renderCommaSeparated renderAliasPart row)
    <+> "VALUES"
    <+> commaSeparated
          ( parenthesized
          . renderCommaSeparated renderColumnValuePart <$> row:rows )
    <> renderConflictClause conflict
    <> renderReturningClause returning
    where
      renderAliasPart, renderColumnValuePart
        :: Aliased (ColumnValue '[] params) ty -> ByteString
      renderAliasPart (_ `As` name) = renderAlias name
      renderColumnValuePart (value `As` _) = case value of
        Default -> "DEFAULT"
        Set expression -> renderExpression expression

-- | Insert a single row.
insertRow
  :: ( SOP.SListI columns
     , SOP.SListI results
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to insert into
  -> NP (Aliased (ColumnValue '[] params)) columns
  -> ConflictClause columns params
  -- ^ what to do in case of constraint conflict
  -> ReturningClause columns params results -- ^ results to return
  -> Manipulation schema params results
insertRow tab row = insertRows tab row []

-- | Insert multiple rows returning `Nil` and raising an error on conflicts.
insertRows_
  :: ( SOP.SListI columns
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to insert into
  -> NP (Aliased (ColumnValue '[] params)) columns
  -> [NP (Aliased (ColumnValue '[] params)) columns]
  -> Manipulation schema params '[]
insertRows_ tab row rows =
  insertRows tab row rows OnConflictDoRaise (Returning Nil)

-- | Insert a single row returning `Nil` and raising an error on conflicts.
insertRow_
  :: ( SOP.SListI columns
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to insert into
  -> NP (Aliased (ColumnValue '[] params)) columns
  -> Manipulation schema params '[]
insertRow_ tab row = insertRow tab row OnConflictDoRaise (Returning Nil)

{- | Insert a query.

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" ::: '[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4
       ]
     , "other_tab" ::: '[] :=>
      '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
       , "col2" ::: 'NoDef :=> 'NotNull 'PGint4
       ]
     ] '[] '[]
  manipulation = 
    insertQuery_ #tab
      (selectStar (from (table (#other_tab `As` #t))))
in renderManipulation manipulation
:}
"INSERT INTO tab SELECT * FROM other_tab AS t;"
-}
insertQuery
  :: ( SOP.SListI columns
     , SOP.SListI results
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to insert into
  -> Query schema params (ColumnsToRelation columns)
  -> ConflictClause columns params
  -- ^ what to do in case of constraint conflict
  -> ReturningClause columns params results -- ^ results to return
  -> Manipulation schema params results
insertQuery tab query conflict returning = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderAlias tab
    <+> renderQuery query
    <> renderConflictClause conflict
    <> renderReturningClause returning

-- | Insert a query returning `Nil` and raising an error on conflicts.
insertQuery_
  :: ( SOP.SListI columns
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to insert into
  -> Query schema params (ColumnsToRelation columns)
  -> Manipulation schema params '[]
insertQuery_ tab query =
  insertQuery tab query OnConflictDoRaise (Returning Nil)

-- | `ColumnValue`s are values to insert or update in a row
-- `Same` updates with the same value.
-- `Default` inserts or updates with the @DEFAULT@ value
-- `Set` a value to be an `Expression`, relative to the given
-- row for an update, and closed for an insert.
data ColumnValue
  (columns :: RelationType)
  (params :: [NullityType])
  (ty :: ColumnType)
  where
    Same :: ColumnValue (column ': columns) params ty
    Default :: ColumnValue columns params ('Def :=> ty)
    Set
      :: (forall table. Expression '[table ::: columns] 'Ungrouped params ty)
      -> ColumnValue columns params (constraint :=> ty)

-- | A `ReturningClause` computes and return value(s) based
-- on each row actually inserted, updated or deleted. This is primarily
-- useful for obtaining values that were supplied by defaults, such as a
-- serial sequence number. However, any expression using the table's columns
-- is allowed. Only rows that were successfully inserted or updated or
-- deleted will be returned. For example, if a row was locked
-- but not updated because an `OnConflictDoUpdate` condition was not satisfied,
-- the row will not be returned. `ReturningStar` will return all columns
-- in the row. Use `Returning Nil` in the common case where no return
-- values are desired.
data ReturningClause
  (columns :: ColumnsType)
  (params :: [NullityType])
  (results :: RelationType)
  where
    ReturningStar
      :: results ~ ColumnsToRelation columns
      => ReturningClause columns params results
    Returning
      :: rel ~ ColumnsToRelation columns
      => NP (Aliased (Expression '[table ::: rel] 'Ungrouped params)) results
      -> ReturningClause columns params results

-- | Render a `ReturningClause`.
renderReturningClause
  :: SOP.SListI results
  => ReturningClause params columns results
  -> ByteString
renderReturningClause = \case
  ReturningStar -> " RETURNING *;"
  Returning Nil -> ";"
  Returning results -> " RETURNING"
    <+> renderCommaSeparated (renderAliasedAs renderExpression) results <> ";"

-- | A `ConflictClause` specifies an action to perform upon a constraint
-- violation. `OnConflictDoRaise` will raise an error.
-- `OnConflictDoNothing` simply avoids inserting a row.
-- `OnConflictDoUpdate` updates the existing row that conflicts with the row
-- proposed for insertion.
data ConflictClause (columns :: ColumnsType) params where
  OnConflictDoRaise :: ConflictClause columns params
  OnConflictDoNothing :: ConflictClause columns params
  OnConflictDoUpdate
    :: NP (Aliased (ColumnValue (ColumnsToRelation columns) params)) columns
    -> [Condition '[table ::: ColumnsToRelation columns] 'Ungrouped params]
    -> ConflictClause columns params

-- | Render a `ConflictClause`.
renderConflictClause
  :: SOP.SListI columns
  => ConflictClause columns params
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
          :: Aliased (ColumnValue columns params) column
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
--
update
  :: ( SOP.SListI columns
     , SOP.SListI results
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to update
  -> NP (Aliased (ColumnValue (ColumnsToRelation columns) params)) columns
  -- ^ modified values to replace old values
  -> Condition '[tab ::: ColumnsToRelation columns] 'Ungrouped params
  -- ^ condition under which to perform update on a row
  -> ReturningClause columns params results -- ^ results to return
  -> Manipulation schema params results
update tab columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderAlias tab
  <+> "SET"
  <+> renderCommaSeparatedMaybe renderUpdate columns
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning
  where
    renderUpdate
      :: Aliased (ColumnValue columns params) column
      -> Maybe ByteString
    renderUpdate = \case
      Same `As` _ -> Nothing
      Default `As` column -> Just $
        renderAlias column <+> "=" <+> "DEFAULT"
      Set expression `As` column -> Just $
        renderAlias column <+> "=" <+> renderExpression expression

-- | Update a row returning `Nil`.
-- >>> :{
-- let
--   manipulation :: Manipulation
--     '[ "tab" ::: '[] :=>
--       '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
--        , "col2" ::: 'NoDef :=> 'NotNull 'PGint4 ]] '[] '[]
--   manipulation =
--     update_ #tab (Set 2 `As` #col1 :* Same `As` #col2 :* Nil)
--       (#col1 ./= #col2)
-- in renderManipulation manipulation
-- :}
-- "UPDATE tab SET col1 = 2 WHERE (col1 <> col2);"
update_
  :: ( SOP.SListI columns
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to update
  -> NP (Aliased (ColumnValue (ColumnsToRelation columns) params)) columns
  -- ^ modified values to replace old values
  -> Condition '[tab ::: ColumnsToRelation columns] 'Ungrouped params
  -- ^ condition under which to perform update on a row
  -> Manipulation schema params '[]
update_ tab columns wh = update tab columns wh (Returning Nil)

{-----------------------------------------
DELETE statements
-----------------------------------------}

-- | Delete rows of a table.
--
-- >>> :{
-- let
--   manipulation :: Manipulation
--     '[ "tab" ::: '[] :=>
--       '[ "col1" ::: 'NoDef :=> 'NotNull 'PGint4
--        , "col2" ::: 'NoDef :=> 'NotNull 'PGint4 ]] '[]
--     '[ "col1" ::: 'NotNull 'PGint4
--      , "col2" ::: 'NotNull 'PGint4 ]
--   manipulation = deleteFrom #tab (#col1 .== #col2) ReturningStar
-- in renderManipulation manipulation
-- :}
-- "DELETE FROM tab WHERE (col1 = col2) RETURNING *;"
deleteFrom
  :: ( SOP.SListI results
     , Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to delete from
  -> Condition '[tab ::: ColumnsToRelation columns] 'Ungrouped params
  -- ^ condition under which to delete a row
  -> ReturningClause columns params results -- ^ results to return
  -> Manipulation schema params results
deleteFrom tab wh returning = UnsafeManipulation $
  "DELETE FROM" <+> renderAlias tab
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

-- | Delete rows returning `Nil`.
deleteFrom_
  :: ( Has tab schema table
     , columns ~ TableToColumns table )
  => Alias tab -- ^ table to delete from
  -> Condition '[tab ::: ColumnsToRelation columns] 'Ungrouped params
  -- ^ condition under which to delete a row
  -> Manipulation schema params '[]
deleteFrom_ tab wh = deleteFrom tab wh (Returning Nil)

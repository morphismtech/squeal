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
  , insertInto
  , ValuesClause (Values, ValuesQuery)
  , renderValuesClause
  , ReturningClause (ReturningStar, Returning)
  , renderReturningClause
  , ConflictClause (OnConflictDoRaise, OnConflictDoNothing, OnConflictDoUpdate)
  , renderConflictClause
    -- * Update
  , update
  , UpdateExpression (Same, Set)
  , renderUpdateExpression
  , deleteFrom
  ) where

import Control.DeepSeq
import Data.ByteString
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
  (schema :: RelationType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
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
    '[ "tab" :::
      '[ "col1" ::: 'Required ('NotNull 'PGint4)
       , "col2" ::: 'Required ('NotNull 'PGint4) ]] '[] '[]
  manipulation =
    insertInto #tab (Values (2 `As` #col1 :* 4 `As` #col2 :* Nil) [])
      OnConflictDoRaise (Returning Nil)
in renderManipulation manipulation
:}
"INSERT INTO tab (col1, col2) VALUES (2, 4);"

parameterized insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" :::
      '[ "col1" ::: 'Required ('NotNull 'PGint4)
       , "col2" ::: 'Required ('NotNull 'PGint4) ]]
    '[ 'Required ('NotNull 'PGint4)
     , 'Required ('NotNull 'PGint4) ] '[]
  manipulation =
    insertInto #tab
      (Values (param @1 `As` #col1 :* param @2 `As` #col2 :* Nil) [])
      OnConflictDoRaise (Returning Nil)
in renderManipulation manipulation
:}
"INSERT INTO tab (col1, col2) VALUES (($1 :: int4), ($2 :: int4));"

returning insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" :::
      '[ "col1" ::: 'Required ('NotNull 'PGint4)
       , "col2" ::: 'Required ('NotNull 'PGint4) ]] '[]
    '["fromOnly" ::: 'Required ('NotNull 'PGint4)]
  manipulation =
    insertInto #tab (Values (2 `As` #col1 :* 4 `As` #col2 :* Nil) [])
      OnConflictDoRaise (Returning (#col1 `As` #fromOnly :* Nil))
in renderManipulation manipulation
:}
"INSERT INTO tab (col1, col2) VALUES (2, 4) RETURNING col1 AS fromOnly;"

query insert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" :::
      '[ "col1" ::: 'Required ('NotNull 'PGint4)
       , "col2" ::: 'Required ('NotNull 'PGint4)
       ]
     , "other_tab" :::
      '[ "col1" ::: 'Required ('NotNull 'PGint4)
       , "col2" ::: 'Required ('NotNull 'PGint4)
       ]
     ] '[] '[]
  manipulation = 
    insertInto #tab
      ( ValuesQuery $
        selectStar (from (Table (#other_tab `As` #t))) )
      OnConflictDoRaise (Returning Nil)
in renderManipulation manipulation
:}
"INSERT INTO tab SELECT * FROM other_tab AS t;"

upsert:

>>> :{
let
  manipulation :: Manipulation
    '[ "tab" :::
      '[ "col1" ::: 'Required ('NotNull 'PGint4)
       , "col2" ::: 'Required ('NotNull 'PGint4) ]]
    '[] '[ "sum" ::: 'Required ('NotNull 'PGint4)]
  manipulation =
    insertInto #tab
      (Values
        (2 `As` #col1 :* 4 `As` #col2 :* Nil)
        [6 `As` #col1 :* 8 `As` #col2 :* Nil])
      (OnConflictDoUpdate
        (Set 2 `As` #col1 :* Same `As` #col2 :* Nil)
        (Just (#col1 .== #col2)))
      (Returning $ (#col1 + #col2) `As` #sum :* Nil)
in renderManipulation manipulation
:}
"INSERT INTO tab (col1, col2) VALUES (2, 4), (6, 8) ON CONFLICT DO UPDATE SET col1 = 2 WHERE (col1 = col2) RETURNING (col1 + col2) AS sum;"
-}
insertInto
  :: (SOP.SListI columns, SOP.SListI results, HasTable table schema columns)
  => Alias table -- ^ table to insert into
  -> ValuesClause schema params columns -- ^ values to insert
  -> ConflictClause columns params
  -- ^ what to do in case of constraint conflict
  -> ReturningClause columns params results -- ^ results to return
  -> Manipulation schema params results
insertInto table insert conflict returning = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderAlias table
  <+> renderValuesClause insert
  <> renderConflictClause conflict
  <> renderReturningClause returning

-- | A `ValuesClause` lets you insert either values, free `Expression`s,
-- or the result of a `Query`.
data ValuesClause
  (schema :: RelationType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
    = Values
        (NP (Aliased (Expression '[] 'Ungrouped params)) columns)
        [NP (Aliased (Expression '[] 'Ungrouped params)) columns]
    -- ^ at least one row of values
    | ValuesQuery (Query schema params columns)

-- | Render a `ValuesClause`.
renderValuesClause
  :: SOP.SListI columns
  => ValuesClause schema params columns
  -> ByteString
renderValuesClause = \case
  Values row rows ->
    parenthesized (renderCommaSeparated renderAliasPart row)
    <+> "VALUES"
    <+> commaSeparated
      (parenthesized . renderCommaSeparated renderValuePart <$> row:rows)
    where
      renderAliasPart, renderValuePart
        :: Aliased (Expression '[] 'Ungrouped params) ty -> ByteString
      renderAliasPart (_ `As` name) = renderAlias name
      renderValuePart (value `As` _) = renderExpression value
  ValuesQuery q -> renderQuery q

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
  (params :: [ColumnType])
  (results :: ColumnsType)
  where
    ReturningStar :: ReturningClause columns params columns
    Returning
      :: NP
          (Aliased (Expression '[table ::: columns] 'Ungrouped params))
          results
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
    <+> renderCommaSeparated (renderAliased renderExpression) results <> ";"

-- | A `ConflictClause` specifies an action to perform upon a constraint
-- violation. `OnConflictDoRaise` will raise an error.
-- `OnConflictDoNothing` simply avoids inserting a row.
-- `OnConflictDoUpdate` updates the existing row that conflicts with the row
-- proposed for insertion.
data ConflictClause columns params where
  OnConflictDoRaise :: ConflictClause columns params
  OnConflictDoNothing :: ConflictClause columns params
  OnConflictDoUpdate
    :: NP (Aliased (UpdateExpression columns params)) columns
    -> Maybe (Condition '[table ::: columns] 'Ungrouped params)
    -> ConflictClause columns params

-- | Render a `ConflictClause`.
renderConflictClause
  :: SOP.SListI columns
  => ConflictClause columns params
  -> ByteString
renderConflictClause = \case
  OnConflictDoRaise -> ""
  OnConflictDoNothing -> " ON CONFLICT DO NOTHING"
  OnConflictDoUpdate updates whMaybe
    -> " ON CONFLICT DO UPDATE SET"
      <+> renderCommaSeparatedMaybe renderUpdateExpression updates
      <> case whMaybe of
        Nothing -> ""
        Just wh -> " WHERE" <+> renderExpression wh

{-----------------------------------------
UPDATE statements
-----------------------------------------}

-- | An `update` command changes the values of the specified columns
-- in all rows that satisfy the condition.
--
-- >>> :{
-- let
--   manipulation :: Manipulation
--     '[ "tab" :::
--       '[ "col1" ::: 'Required ('NotNull 'PGint4)
--        , "col2" ::: 'Required ('NotNull 'PGint4) ]] '[] '[]
--   manipulation =
--     update #tab (Set 2 `As` #col1 :* Same `As` #col2 :* Nil)
--       (#col1 ./= #col2) (Returning Nil)
-- in renderManipulation manipulation
-- :}
-- "UPDATE tab SET col1 = 2 WHERE (col1 <> col2);"
update
  :: (HasTable table schema columns, SOP.SListI columns, SOP.SListI results)
  => Alias table -- ^ table to update
  -> NP (Aliased (UpdateExpression columns params)) columns
  -- ^ modified values to replace old values
  -> Condition '[tab ::: columns] 'Ungrouped params
  -- ^ condition under which to perform update on a row
  -> ReturningClause columns params results -- ^ results to return
  -> Manipulation schema params results
update table columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderAlias table
  <+> "SET"
  <+> renderCommaSeparatedMaybe renderUpdateExpression columns
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

-- | Columns to be updated are mentioned with `Set`; columns which are to
-- remain the same are mentioned with `Same`.
data UpdateExpression columns params ty
  = Same
  -- ^ column to remain the same upon update
  | Set (forall table. Expression '[table ::: columns] 'Ungrouped params ty)
  -- ^ column to be updated
deriving instance Show (UpdateExpression columns params ty)
deriving instance Eq (UpdateExpression columns params ty)
deriving instance Ord (UpdateExpression columns params ty)

-- | Render an `UpdateExpression`.
renderUpdateExpression
  :: Aliased (UpdateExpression params columns) column
  -> Maybe ByteString
renderUpdateExpression = \case
  Same `As` _ -> Nothing
  Set expression `As` column -> Just $
    renderAlias column <+> "=" <+> renderExpression expression

{-----------------------------------------
DELETE statements
-----------------------------------------}

-- | Delete rows of a table.
--
-- >>> :{
-- let
--   manipulation :: Manipulation
--     '[ "tab" :::
--       '[ "col1" ::: 'Required ('NotNull 'PGint4)
--        , "col2" ::: 'Required ('NotNull 'PGint4) ]] '[]
--     '[ "col1" ::: 'Required ('NotNull 'PGint4)
--      , "col2" ::: 'Required ('NotNull 'PGint4) ]
--   manipulation = deleteFrom #tab (#col1 .== #col2) ReturningStar
-- in renderManipulation manipulation
-- :}
-- "DELETE FROM tab WHERE (col1 = col2) RETURNING *;"
deleteFrom
  :: (SOP.SListI results, HasTable table schema columns)
  => Alias table -- ^ table to delete from
  -> Condition '[table ::: columns] 'Ungrouped params
  -- ^ condition under which to delete a row
  -> ReturningClause columns params results -- ^ results to return
  -> Manipulation schema params results
deleteFrom table wh returning = UnsafeManipulation $
  "DELETE FROM" <+> renderAlias table
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

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
  ( -- * Data Manipulation Language
    Manipulation (UnsafeManipulation, renderManipulation)
  , queryStatement
  , insertInto
  , ValuesClause (Values, ValuesQuery)
  , renderValuesClause
  , ReturningClause (ReturningStar, Returning)
  , renderReturningClause
  , ConflictClause (Conflict, OnConflictDoNothing, OnConflictDoUpdate)
  , renderConflictClause
  , UpdateExpression (Same, Set)
  , renderUpdateExpression
  , update
  , deleteFrom
  ) where

-- import Control.Category
import Control.DeepSeq
import Data.ByteString
import Data.Data
import Data.Monoid
import Generics.SOP
-- import GHC.TypeLits
-- import Prelude hiding ((.), id)

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

newtype Manipulation
  (schema :: TablesType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

queryStatement
  :: Query schema params columns
  -> Manipulation schema params columns
queryStatement q = UnsafeManipulation $ renderQuery q <> ";"

{-----------------------------------------
INSERT statements
-----------------------------------------}

insertInto
  :: (SListI columns, SListI results, HasTable table schema columns)
  => Alias table
  -> ValuesClause schema params columns
  -> ConflictClause columns params
  -> ReturningClause columns params results
  -> Manipulation schema params results
insertInto table insert conflict returning = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderAlias table
  <+> renderValuesClause insert
  <> renderConflictClause conflict
  <> renderReturningClause returning

data ValuesClause
  (schema :: TablesType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
    = Values
        (NP (Aliased (Expression '[] 'Ungrouped params)) columns)
        [NP (Aliased (Expression '[] 'Ungrouped params)) columns]
    | ValuesQuery (Query schema params columns)

renderValuesClause
  :: SListI columns
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

renderReturningClause
  :: SListI results
  => ReturningClause params columns results
  -> ByteString
renderReturningClause = \case
  ReturningStar -> " RETURNING *;"
  Returning Nil -> ";"
  Returning results -> " RETURNING"
    <+> renderCommaSeparated (renderAliased renderExpression) results <> ";"

data ConflictClause columns params where
  Conflict :: ConflictClause columns params
  OnConflictDoNothing :: ConflictClause columns params
  OnConflictDoUpdate
    :: NP (Aliased (UpdateExpression columns params)) columns
    -> Maybe (Condition '[table ::: columns] 'Ungrouped params)
    -> ConflictClause columns params

renderConflictClause
  :: SListI columns
  => ConflictClause columns params
  -> ByteString
renderConflictClause = \case
  Conflict -> ""
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

data UpdateExpression columns params ty
  = Same
  | Set (forall table. Expression '[table ::: columns] 'Ungrouped params ty)
deriving instance Show (UpdateExpression columns params ty)
deriving instance Eq (UpdateExpression columns params ty)
deriving instance Ord (UpdateExpression columns params ty)

renderUpdateExpression
  :: Aliased (UpdateExpression params columns) column
  -> Maybe ByteString
renderUpdateExpression = \case
  Same `As` _ -> Nothing
  Set expression `As` column -> Just $
    renderAlias column <+> "=" <+> renderExpression expression

update
  :: (HasTable table schema columns, SListI columns, SListI results)
  => Alias table
  -> NP (Aliased (UpdateExpression columns params)) columns
  -> Condition '[tab ::: columns] 'Ungrouped params
  -> ReturningClause columns params results
  -> Manipulation schema params results
update table columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderAlias table
  <+> "SET"
  <+> renderCommaSeparatedMaybe renderUpdateExpression columns
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

{-----------------------------------------
DELETE statements
-----------------------------------------}

deleteFrom
  :: HasTable table schema columns
  => Alias table
  -> Condition '[table ::: columns] 'Ungrouped params
  -> Manipulation schema params '[]
deleteFrom table wh = UnsafeManipulation $
  "DELETE FROM" <+> renderAlias table
  <+> "WHERE" <+> renderExpression wh <> ";"

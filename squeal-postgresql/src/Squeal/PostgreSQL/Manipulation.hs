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
  -> ConflictClause params columns
  -> ReturningClause params columns results
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
        (NP (Aliased (Expression params '[] 'Ungrouped)) columns)
        [NP (Aliased (Expression params '[] 'Ungrouped)) columns]
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
        :: Aliased (Expression params '[] 'Ungrouped) ty -> ByteString
      renderAliasPart (_ `As` name) = renderAlias name
      renderValuePart (value `As` _) = renderExpression value
  ValuesQuery q -> renderQuery q

data ReturningClause
  (params :: [ColumnType])
  (columns :: ColumnsType)
  (results :: ColumnsType)
  where
    ReturningStar :: ReturningClause params columns columns
    Returning
      :: NP
          (Aliased (Expression params '[table ::: columns] 'Ungrouped))
          results
      -> ReturningClause params columns results

renderReturningClause
  :: SListI results
  => ReturningClause params columns results
  -> ByteString
renderReturningClause = \case
  ReturningStar -> " RETURNING *;"
  Returning Nil -> ";"
  Returning results -> " RETURNING"
    <+> renderCommaSeparated (renderAliased renderExpression) results <> ";"

data ConflictClause params columns where
  Conflict :: ConflictClause params columns
  OnConflictDoNothing :: ConflictClause params columns
  OnConflictDoUpdate
    :: NP (Aliased (UpdateExpression params columns)) columns
    -> Maybe (Condition params '[table ::: columns] 'Ungrouped)
    -> ConflictClause params columns

renderConflictClause
  :: SListI columns
  => ConflictClause params columns
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

data UpdateExpression params columns ty
  = Same
  | Set (forall table. Expression params '[table ::: columns] 'Ungrouped ty)
deriving instance Show (UpdateExpression params columns ty)
deriving instance Eq (UpdateExpression params columns ty)
deriving instance Ord (UpdateExpression params columns ty)

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
  -> NP (Aliased (UpdateExpression params columns)) columns
  -> Condition params '[tab ::: columns] 'Ungrouped
  -> ReturningClause params columns results
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
  -> Condition params '[table ::: columns] 'Ungrouped
  -> Manipulation schema params '[]
deleteFrom table wh = UnsafeManipulation $
  "DELETE FROM" <+> renderAlias table
  <+> "WHERE" <+> renderExpression wh <> ";"

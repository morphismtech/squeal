{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , DeriveGeneric
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Query
  ( -- * Queries
    Query (UnsafeQuery, renderQuery)
  , union
  , unionAll
  , intersect
  , intersectAll
  , except
  , exceptAll
    -- * SELECT queries
  , select
  , selectDistinct
  , selectStar
  , selectDistinctStar
  , selectDotStar
  , selectDistinctDotStar
    -- * FROM clauses
  , FromClause (..)
  , renderFromClause
    -- * Table Expressions
  , TableExpression (..)
  , renderTableExpression
  , from
  , where_
  , group
  , having
  , orderBy
  , limit
  , offset
  , By (By, By2)
  , renderBy
    -- * Grouping
  , GroupByClause (NoGroups, Group)
  , renderGroupByClause
  , HavingClause (NoHaving, Having)
  , renderHavingClause
  , GroupedBy (getGroup1, getGroup2)
    -- * Sorting
  , SortExpression (..)
  , renderSortExpression
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.Monoid
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.TypeLits

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Schema

newtype Query
  (schema :: TablesType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
    = UnsafeQuery { renderQuery :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

union, unionAll, intersect, intersectAll, except, exceptAll
  :: Query schema params columns
  -> Query schema params columns
  -> Query schema params columns
q1 `union` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "UNION"
  <+> parenthesized (renderQuery q2)
q1 `unionAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "UNION" <+> "ALL"
  <+> parenthesized (renderQuery q2)
q1 `intersect` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "INTERSECT"
  <+> parenthesized (renderQuery q2)
q1 `intersectAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "INTERSECT" <+> "ALL"
  <+> parenthesized (renderQuery q2)
q1 `except` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "EXCEPT"
  <+> parenthesized (renderQuery q2)
q1 `exceptAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "EXCEPT" <+> "ALL"
  <+> parenthesized (renderQuery q2)

{-----------------------------------------
SELECT queries
-----------------------------------------}

select
  :: SListI columns
  => NP (Aliased (Expression params tables grouping)) (column ': columns)
  -> TableExpression params schema tables grouping
  -> Query schema params (column ': columns)
select list tabs = UnsafeQuery $
  "SELECT"
  <+> renderCommaSeparated (renderAliased renderExpression) list
  <+> renderTableExpression tabs

selectDistinct
  :: SListI columns
  => NP (Aliased (Expression params tables grouping)) (column ': columns)
  -> TableExpression params schema tables grouping
  -> Query schema params (column ': columns)
selectDistinct list tabs = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderCommaSeparated (renderAliased renderExpression) list
  <+> renderTableExpression tabs

selectStar
  :: HasUnique table tables columns
  => TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectStar tabs = UnsafeQuery $ "SELECT" <+> "*" <+> renderTableExpression tabs

selectDistinctStar
  :: HasUnique table tables columns
  => TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectDistinctStar tabs = UnsafeQuery $
  "SELECT DISTINCT" <+> "*" <+> renderTableExpression tabs

selectDotStar
  :: HasTable table tables columns
  => Alias table
  -> TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectDotStar table tables = UnsafeQuery $
  "SELECT" <+> renderAlias table <> ".*" <+> renderTableExpression tables

selectDistinctDotStar
  :: HasTable table tables columns
  => Alias table
  -> TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectDistinctDotStar table tables = UnsafeQuery $
  "SELECT DISTINCT" <+> renderAlias table <> ".*"
  <+> renderTableExpression tables

{-----------------------------------------
FROM clauses
-----------------------------------------}

data FromClause params schema tables where
  Table
    :: Aliased (Table schema) table
    -> FromClause params schema '[table]
  Subquery
    :: Aliased (Query schema params) table
    -> FromClause params schema '[table]
  CrossJoin
    :: FromClause params schema right
    -> FromClause params schema left
    -> FromClause params schema (Join left right)
  InnerJoin
    :: FromClause params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> FromClause params schema left
    -> FromClause params schema (Join left right)
  LeftOuterJoin
    :: FromClause params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> FromClause params schema left
    -> FromClause params schema (Join left (NullifyTables right))
  RightOuterJoin
    :: FromClause params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> FromClause params schema left
    -> FromClause params schema (Join (NullifyTables left) right)
  FullOuterJoin
    :: FromClause params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> FromClause params schema left
    -> FromClause params schema
        (Join (NullifyTables left) (NullifyTables right))

renderFromClause :: FromClause params schema tables -> ByteString
renderFromClause = \case
  Table table -> renderAliased renderTable table
  Subquery selection -> renderAliased (parenthesized . renderQuery) selection
  CrossJoin right left ->
    renderFromClause left <+> "CROSS JOIN" <+> renderFromClause right
  InnerJoin right on left -> renderJoin "INNER JOIN" right on left
  LeftOuterJoin right on left -> renderJoin "LEFT OUTER JOIN" right on left
  RightOuterJoin right on left -> renderJoin "RIGHT OUTER JOIN" right on left
  FullOuterJoin right on left -> renderJoin "FULL OUTER JOIN" right on left
  where
    renderJoin op right on left =
      renderFromClause left <+> op <+> renderFromClause right
      <+> "ON" <+> renderExpression on

{-----------------------------------------
Table Expressions
-----------------------------------------}

data TableExpression
  (params :: [ColumnType])
  (schema :: TablesType)
  (tables :: TablesType)
  (grouping :: Grouping)
    = TableExpression
    { fromClause :: FromClause params schema tables
    , whereClause :: [Condition params tables 'Ungrouped]
    , groupByClause :: GroupByClause tables grouping
    , havingClause :: HavingClause params tables grouping
    , orderByClause :: [SortExpression params tables grouping]
    , limitClause :: [Word64]
    , offsetClause :: [Word64]
    }

renderTableExpression
  :: TableExpression params schema tables grouping
  -> ByteString
renderTableExpression
  (TableExpression tables whs' grps' hvs' srts' lims' offs') = mconcat
    [ "FROM ", renderFromClause tables
    , renderWheres whs'
    , renderGroupByClause grps'
    , renderHavingClause hvs'
    , renderSorts srts'
    , renderLimits lims'
    , renderOffsets offs'
    ]
    where
      renderWheres = \case
        [] -> ""
        wh:[] -> " WHERE" <+> renderExpression wh
        wh:whs -> " WHERE" <+> renderExpression (foldr (&&*) wh whs)
      renderSorts = \case
        [] -> ""
        srts -> " SORT BY"
          <+> commaSeparated (renderSortExpression <$> srts)
      renderLimits = \case
        [] -> ""
        lims -> " LIMIT" <+> fromString (show (minimum lims))
      renderOffsets = \case
        [] -> ""
        offs -> " OFFSET" <+> fromString (show (sum offs))

from
  :: FromClause params schema tables
  -> TableExpression params schema tables 'Ungrouped
from tables = TableExpression tables [] NoGroups NoHaving [] [] []

where_
  :: Condition params tables 'Ungrouped
  -> TableExpression params schema tables grouping
  -> TableExpression params schema tables grouping
where_ wh tables = tables {whereClause = wh : whereClause tables}

group
  :: SListI bys
  => NP (By tables) bys
  -> TableExpression params schema tables 'Ungrouped 
  -> TableExpression params schema tables ('Grouped bys)
group bys tables = TableExpression
  { fromClause = fromClause tables
  , whereClause = whereClause tables
  , groupByClause = Group bys
  , havingClause = Having []
  , orderByClause = []
  , limitClause = limitClause tables
  , offsetClause = offsetClause tables
  }

having
  :: Condition params tables ('Grouped bys)
  -> TableExpression params schema tables ('Grouped bys)
  -> TableExpression params schema tables ('Grouped bys)
having hv tables = tables
  { havingClause = case havingClause tables of Having hvs -> Having (hv:hvs) }

orderBy
  :: [SortExpression params tables grouping]
  -> TableExpression params schema tables grouping
  -> TableExpression params schema tables grouping
orderBy srts tables = tables {orderByClause = orderByClause tables ++ srts}

limit
  :: Word64
  -> TableExpression params schema tables grouping
  -> TableExpression params schema tables grouping
limit lim tables = tables {limitClause = lim : limitClause tables}

offset
  :: Word64
  -> TableExpression params schema tables grouping
  -> TableExpression params schema tables grouping
offset off tables = tables {offsetClause = off : offsetClause tables}

{-----------------------------------------
Grouping
-----------------------------------------}

data By
    (tables :: TablesType)
    (by :: (Symbol,Symbol)) where
    By
      :: (HasUnique table tables columns, HasColumn column columns ty)
      => Alias column
      -> By tables '(table, column)
    By2
      :: (HasTable table tables columns, HasColumn column columns ty)
      => (Alias table, Alias column)
      -> By tables '(table, column)
deriving instance Show (By tables by)
deriving instance Eq (By tables by)
deriving instance Ord (By tables by)
  
renderBy :: By tables tabcolty -> ByteString
renderBy = \case
  By column -> renderAlias column
  By2 (table, column) -> renderAlias table <> "." <> renderAlias column
  
data GroupByClause tables grouping where
  NoGroups :: GroupByClause tables 'Ungrouped
  Group
    :: SListI bys
    => NP (By tables) bys
    -> GroupByClause tables ('Grouped bys)
  
renderGroupByClause :: GroupByClause tables grouping -> ByteString
renderGroupByClause = \case
  NoGroups -> ""
  Group Nil -> ""
  Group bys -> " GROUP BY" <+> renderCommaSeparated renderBy bys
  
data HavingClause params tables grouping where
  NoHaving :: HavingClause params tables 'Ungrouped
  Having
    :: [Condition params tables ('Grouped bys)]
    -> HavingClause params tables ('Grouped bys)
deriving instance Show (HavingClause params tables grouping)
deriving instance Eq (HavingClause params tables grouping)
deriving instance Ord (HavingClause params tables grouping)
  
renderHavingClause :: HavingClause params tables grouping -> ByteString
renderHavingClause = \case
  NoHaving -> ""
  Having [] -> ""
  Having conditions ->
    " HAVING" <+> commaSeparated (renderExpression <$> conditions)

{-----------------------------------------
Sorting
-----------------------------------------}

data SortExpression params tables grouping where
    Asc
      :: Expression params tables grouping ('Required ('NotNull ty))
      -> SortExpression params tables grouping
    Desc
      :: Expression params tables grouping ('Required ('NotNull ty))
      -> SortExpression params tables grouping
    AscNullsFirst
      :: Expression params tables grouping ('Required ('Null ty))
      -> SortExpression params tables grouping
    AscNullsLast
      :: Expression params tables grouping ('Required ('Null ty))
      -> SortExpression params tables grouping
    DescNullsFirst
      :: Expression params tables grouping ('Required ('Null ty))
      -> SortExpression params tables grouping
    DescNullsLast
      :: Expression params tables grouping ('Required ('Null ty))
      -> SortExpression params tables grouping
deriving instance Show (SortExpression params tables grouping)
  
renderSortExpression :: SortExpression params tables grouping -> ByteString
renderSortExpression = \case
  Asc expression -> renderExpression expression <+> "ASC"
  Desc expression -> renderExpression expression <+> "DESC"
  AscNullsFirst expression -> renderExpression expression
    <+> "ASC NULLS FIRST"
  DescNullsFirst expression -> renderExpression expression
    <+> "DESC NULLS FIRST"
  AscNullsLast expression -> renderExpression expression <+> "ASC NULLS LAST"
  DescNullsLast expression -> renderExpression expression <+> "DESC NULLS LAST"

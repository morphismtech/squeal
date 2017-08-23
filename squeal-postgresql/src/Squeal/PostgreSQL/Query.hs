{-|
Module: Squeal.PostgreSQL.Query
Description: Squeal queries
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal queries.
-}

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
    -- * Grouping
  , By (By, By2)
  , renderBy
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
import Data.Monoid
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.TypeLits

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Schema

-- | The process of retrieving or the command to retrieve data from a database
-- is called a `Query`. The `select` command is used to specify queries.
newtype Query
  (schema :: TablesType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
    = UnsafeQuery { renderQuery :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | The results of two queries can be combined using the set operations
-- `union`, `intersect`, and `except` (set difference). Duplicates are
-- eliminated unless `unionAll`, `intersectAll` or `exceptAll` are used.
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
  => NP (Aliased (Expression tables grouping params)) (column ': columns)
  -> TableExpression schema params tables grouping
  -> Query schema params (column ': columns)
select list tabs = UnsafeQuery $
  "SELECT"
  <+> renderCommaSeparated (renderAliased renderExpression) list
  <+> renderTableExpression tabs

selectDistinct
  :: SListI columns
  => NP (Aliased (Expression tables 'Ungrouped params)) (column ': columns)
  -> TableExpression schema params tables 'Ungrouped
  -> Query schema params (column ': columns)
selectDistinct list tabs = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderCommaSeparated (renderAliased renderExpression) list
  <+> renderTableExpression tabs

selectStar, selectDistinctStar
  :: HasUnique table tables columns
  => TableExpression schema params tables 'Ungrouped
  -> Query schema params columns
selectStar tabs = UnsafeQuery $ "SELECT" <+> "*" <+> renderTableExpression tabs
selectDistinctStar tabs = UnsafeQuery $
  "SELECT DISTINCT" <+> "*" <+> renderTableExpression tabs

selectDotStar, selectDistinctDotStar
  :: HasTable table tables columns
  => Alias table
  -> TableExpression schema params tables 'Ungrouped
  -> Query schema params columns
selectDotStar table tables = UnsafeQuery $
  "SELECT" <+> renderAlias table <> ".*" <+> renderTableExpression tables
selectDistinctDotStar table tables = UnsafeQuery $
  "SELECT DISTINCT" <+> renderAlias table <> ".*"
  <+> renderTableExpression tables

{-----------------------------------------
FROM clauses
-----------------------------------------}

data FromClause schema params tables where
  Table
    :: Aliased (Table schema) table
    -> FromClause schema params '[table]
  Subquery
    :: Aliased (Query schema params) table
    -> FromClause schema params '[table]
  CrossJoin
    :: FromClause schema params right
    -> FromClause schema params left
    -> FromClause schema params (Join left right)
  InnerJoin
    :: FromClause schema params right
    -> Condition (Join left right) 'Ungrouped params
    -> FromClause schema params left
    -> FromClause schema params (Join left right)
  LeftOuterJoin
    :: FromClause schema params right
    -> Condition (Join left right) 'Ungrouped params
    -> FromClause schema params left
    -> FromClause schema params (Join left (NullifyTables right))
  RightOuterJoin
    :: FromClause schema params right
    -> Condition (Join left right) 'Ungrouped params
    -> FromClause schema params left
    -> FromClause schema params (Join (NullifyTables left) right)
  FullOuterJoin
    :: FromClause schema params right
    -> Condition (Join left right) 'Ungrouped params
    -> FromClause schema params left
    -> FromClause schema params
        (Join (NullifyTables left) (NullifyTables right))

renderFromClause :: FromClause schema params tables -> ByteString
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
  (schema :: TablesType)
  (params :: [ColumnType])
  (tables :: TablesType)
  (grouping :: Grouping)
    = TableExpression
    { fromClause :: FromClause schema params tables
    , whereClause :: [Condition tables 'Ungrouped params]
    , groupByClause :: GroupByClause tables grouping
    , havingClause :: HavingClause tables grouping params
    , orderByClause :: [SortExpression tables grouping params]
    , limitClause :: [Word64]
    , offsetClause :: [Word64]
    }

renderTableExpression
  :: TableExpression schema params tables grouping
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
        wh:whs -> " WHERE" <+> renderExpression (foldr (.&&) wh whs)
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
  :: FromClause schema params tables
  -> TableExpression schema params tables 'Ungrouped
from tables = TableExpression tables [] NoGroups NoHaving [] [] []

where_
  :: Condition tables 'Ungrouped params
  -> TableExpression schema params tables grouping
  -> TableExpression schema params tables grouping
where_ wh tables = tables {whereClause = wh : whereClause tables}

group
  :: SListI bys
  => NP (By tables) bys
  -> TableExpression schema params tables 'Ungrouped 
  -> TableExpression schema params tables ('Grouped bys)
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
  :: Condition tables ('Grouped bys) params
  -> TableExpression schema params tables ('Grouped bys)
  -> TableExpression schema params tables ('Grouped bys)
having hv tables = tables
  { havingClause = case havingClause tables of Having hvs -> Having (hv:hvs) }

orderBy
  :: [SortExpression tables grouping params]
  -> TableExpression schema params tables grouping
  -> TableExpression schema params tables grouping
orderBy srts tables = tables {orderByClause = orderByClause tables ++ srts}

limit
  :: Word64
  -> TableExpression schema params tables grouping
  -> TableExpression schema params tables grouping
limit lim tables = tables {limitClause = lim : limitClause tables}

offset
  :: Word64
  -> TableExpression schema params tables grouping
  -> TableExpression schema params tables grouping
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
  
data HavingClause tables grouping params where
  NoHaving :: HavingClause tables 'Ungrouped params
  Having
    :: [Condition tables ('Grouped bys) params]
    -> HavingClause tables ('Grouped bys) params
deriving instance Show (HavingClause tables grouping params)
deriving instance Eq (HavingClause tables grouping params)
deriving instance Ord (HavingClause tables grouping params)
  
renderHavingClause :: HavingClause tables grouping params -> ByteString
renderHavingClause = \case
  NoHaving -> ""
  Having [] -> ""
  Having conditions ->
    " HAVING" <+> commaSeparated (renderExpression <$> conditions)

{-----------------------------------------
Sorting
-----------------------------------------}

data SortExpression tables grouping params where
    Asc
      :: Expression tables grouping params ('Required ('NotNull ty))
      -> SortExpression tables grouping params
    Desc
      :: Expression tables grouping params ('Required ('NotNull ty))
      -> SortExpression tables grouping params
    AscNullsFirst
      :: Expression tables grouping params ('Required ('Null ty))
      -> SortExpression tables grouping params
    AscNullsLast
      :: Expression tables grouping params ('Required ('Null ty))
      -> SortExpression tables grouping params
    DescNullsFirst
      :: Expression tables grouping params ('Required ('Null ty))
      -> SortExpression tables grouping params
    DescNullsLast
      :: Expression tables grouping params ('Required ('Null ty))
      -> SortExpression tables grouping params
deriving instance Show (SortExpression tables grouping params)
  
renderSortExpression :: SortExpression tables grouping params -> ByteString
renderSortExpression = \case
  Asc expression -> renderExpression expression <+> "ASC"
  Desc expression -> renderExpression expression <+> "DESC"
  AscNullsFirst expression -> renderExpression expression
    <+> "ASC NULLS FIRST"
  DescNullsFirst expression -> renderExpression expression
    <+> "DESC NULLS FIRST"
  AscNullsLast expression -> renderExpression expression <+> "ASC NULLS LAST"
  DescNullsLast expression -> renderExpression expression <+> "DESC NULLS LAST"

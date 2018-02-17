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
  , FlexibleContexts
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
    -- * Select
  , select
  , selectDistinct
  , selectStar
  , selectDistinctStar
  , selectDotStar
  , selectDistinctDotStar
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
    -- * From
  , FromClause (..)
  , renderFromClause
    -- * Grouping
  , By (By, By2)
  , renderBy
  , GroupByClause (NoGroups, Group)
  , renderGroupByClause
  , HavingClause (NoHaving, Having)
  , renderHavingClause
    -- * Sorting
  , SortExpression (..)
  , renderSortExpression
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Monoid hiding (All)
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.TypeLits

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Schema

{- |
The process of retrieving or the command to retrieve data from a database
is called a `Query`. The `select`, `selectStar`, `selectDotStar`,
`selectDistinct`, `selectDistinctStar` and `selectDistinctDotStar` commands
are used to specify queries.

simple query:

>>> :{
let
  query :: Query '["tab" ::: '["col" ::: 'Required ('Null 'PGint4)]] '[]
    '["col" ::: 'Required ('Null 'PGint4)]
  query = selectStar (from (Table (#tab `As` #t)))
in renderQuery query
:}
"SELECT * FROM tab AS t"

restricted query:

>>> :{
let
  query :: Query
    '[ "tab" :::
       '[ "col1" ::: 'Required ('NotNull 'PGint4)
        , "col2" ::: 'Required ('NotNull 'PGint4) ]]
    '[]
    '[ "sum" ::: 'Required ('NotNull 'PGint4)
     , "col1" ::: 'Required ('NotNull 'PGint4) ]
  query = 
    select
      ((#col1 + #col2) `As` #sum :* #col1 `As` #col1 :* Nil)
      ( from (Table (#tab `As` #t))
        & where_ (#col1 .> #col2)
        & where_ (#col2 .> 0) )
in renderQuery query
:}
"SELECT (col1 + col2) AS sum, col1 AS col1 FROM tab AS t WHERE ((col1 > col2) AND (col2 > 0))"

subquery:

>>> :{
let
  query :: Query '["tab" ::: '["col" ::: 'Required ('Null 'PGint4)]] '[]
    '["col" ::: 'Required ('Null 'PGint4)]
  query =
    selectStar
      (from (Subquery (selectStar (from (Table (#tab `As` #t))) `As` #sub)))
in renderQuery query
:}
"SELECT * FROM (SELECT * FROM tab AS t) AS sub"

limits and offsets:

>>> :{
let
  query :: Query '["tab" ::: '["col" ::: 'Required ('Null 'PGint4)]] '[]
    '["col" ::: 'Required ('Null 'PGint4)]
  query = selectStar
    (from (Table (#tab `As` #t)) & limit 100 & offset 2 & limit 50 & offset 2)
in renderQuery query
:}
"SELECT * FROM tab AS t LIMIT 50 OFFSET 4"

parameterized query:

>>> :{
let
  query :: Query '["tab" ::: '["col" ::: 'Required ('NotNull 'PGfloat8)]]
    '[ 'Required ('NotNull 'PGfloat8)]
    '["col" ::: 'Required ('NotNull 'PGfloat8)]
  query = selectStar
    (from (Table (#tab `As` #t)) & where_ (#col .> param @1))
in renderQuery query
:}
"SELECT * FROM tab AS t WHERE (col > ($1 :: float8))"

aggregation query:

>>> :{
let
  query :: Query
    '[ "tab" :::
       '[ "col1" ::: 'Required ('NotNull 'PGint4)
        , "col2" ::: 'Required ('NotNull 'PGint4) ]]
    '[]
    '[ "sum" ::: 'Required ('NotNull 'PGint4)
     , "col1" ::: 'Required ('NotNull 'PGint4) ]
  query =
    select (sum_ #col2 `As` #sum :* #col1 `As` #col1 :* Nil)
    ( from (Table (#tab `As` #table1))
      & group (By #col1 :* Nil) 
      & having (#col1 + sum_ #col2 .> 1) )
in renderQuery query
:}
"SELECT sum(col2) AS sum, col1 AS col1 FROM tab AS table1 GROUP BY col1 HAVING ((col1 + sum(col2)) > 1)"

sorted query:

>>> :{
let
  query :: Query '["tab" ::: '["col" ::: 'Required ('Null 'PGint4)]] '[]
    '["col" ::: 'Required ('Null 'PGint4)]
  query = selectStar
    (from (Table (#tab `As` #t)) & orderBy [#col & AscNullsFirst])
in renderQuery query
:}
"SELECT * FROM tab AS t ORDER BY col ASC NULLS FIRST"

joins:

>>> :set -XFlexibleContexts
>>> :{
let
  query :: Query
    '[ "orders" :::
         '[ "id"    ::: 'Required ('NotNull 'PGint4)
          , "price"   ::: 'Required ('NotNull 'PGfloat4)
          , "customer_id" ::: 'Required ('NotNull 'PGint4)
          , "shipper_id"  ::: 'Required ('NotNull 'PGint4)
          ]
     , "customers" :::
         '[ "id" ::: 'Required ('NotNull 'PGint4)
          , "name" ::: 'Required ('NotNull 'PGtext)
          ]
     , "shippers" :::
         '[ "id" ::: 'Required ('NotNull 'PGint4)
          , "name" ::: 'Required ('NotNull 'PGtext)
          ]
     ]
    '[]
    '[ "order_price" ::: 'Required ('NotNull 'PGfloat4)
     , "customer_name" ::: 'Required ('NotNull 'PGtext)
     , "shipper_name" ::: 'Required ('NotNull 'PGtext)
     ]
  query = select
    ( #o ! #price `As` #order_price :*
      #c ! #name `As` #customer_name :*
      #s ! #name `As` #shipper_name :* Nil )
    ( from (Table (#orders `As` #o)
      & InnerJoin (Table (#customers `As` #c))
        (#o ! #customer_id .== #c ! #id)
      & InnerJoin (Table (#shippers `As` #s))
        (#o ! #shipper_id .== #s ! #id)) )
in renderQuery query
:}
"SELECT o.price AS order_price, c.name AS customer_name, s.name AS shipper_name FROM orders AS o INNER JOIN customers AS c ON (o.customer_id = c.id) INNER JOIN shippers AS s ON (o.shipper_id = s.id)"

self-join:

>>> :{
let
  query :: Query '["tab" ::: '["col" ::: 'Required ('Null 'PGint4)]] '[]
    '["col" ::: 'Required ('Null 'PGint4)]
  query = selectDotStar #t1
    (from (Table (#tab `As` #t1) & CrossJoin (Table (#tab `As` #t2))))
in renderQuery query
:}
"SELECT t1.* FROM tab AS t1 CROSS JOIN tab AS t2"

set operations:

>>> :{
let
  query :: Query '["tab" ::: '["col" ::: 'Required ('Null 'PGint4)]] '[]
    '["col" ::: 'Required ('Null 'PGint4)]
  query =
    selectStar (from (Table (#tab `As` #t)))
    `unionAll`
    selectStar (from (Table (#tab `As` #t)))
in renderQuery query
:}
"(SELECT * FROM tab AS t) UNION ALL (SELECT * FROM tab AS t)"
-}

newtype Query
  (schema :: TablesType)
  (params :: [NullityType])
  (columns :: RelationType)
    = UnsafeQuery { renderQuery :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | The results of two queries can be combined using the set operation
-- `union`. Duplicate rows are eliminated. 
union
  :: Query schema params columns
  -> Query schema params columns
  -> Query schema params columns
q1 `union` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "UNION"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `unionAll`, the disjoint union. Duplicate rows are retained.
unionAll
  :: Query schema params columns
  -> Query schema params columns
  -> Query schema params columns
q1 `unionAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "UNION" <+> "ALL"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `intersect`, the intersection. Duplicate rows are eliminated.
intersect
  :: Query schema params columns
  -> Query schema params columns
  -> Query schema params columns
q1 `intersect` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "INTERSECT"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `intersectAll`, the intersection. Duplicate rows are retained.
intersectAll
  :: Query schema params columns
  -> Query schema params columns
  -> Query schema params columns
q1 `intersectAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "INTERSECT" <+> "ALL"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `except`, the set difference. Duplicate rows are eliminated.
except
  :: Query schema params columns
  -> Query schema params columns
  -> Query schema params columns
q1 `except` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "EXCEPT"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `exceptAll`, the set difference. Duplicate rows are retained.
exceptAll
  :: Query schema params columns
  -> Query schema params columns
  -> Query schema params columns
q1 `exceptAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "EXCEPT" <+> "ALL"
  <+> parenthesized (renderQuery q2)

{-----------------------------------------
SELECT queries
-----------------------------------------}

-- | the `TableExpression` in the `select` command constructs an intermediate
-- virtual table by possibly combining tables, views, eliminating rows,
-- grouping, etc. This table is finally passed on to processing by
-- the select list. The select list determines which columns of
-- the intermediate table are actually output.
select
  :: SListI columns
  => NP (Aliased (Expression tables grouping params)) (column ': columns)
  -- ^ select list
  -> TableExpression schema params tables grouping
  -- ^ intermediate virtual table
  -> Query schema params (column ': columns)
select list tabs = UnsafeQuery $
  "SELECT"
  <+> renderCommaSeparated (renderAliasedAs renderExpression) list
  <+> renderTableExpression tabs

-- | After the select list has been processed, the result table can
-- be subject to the elimination of duplicate rows using `selectDistinct`.
selectDistinct
  :: SListI columns
  => NP (Aliased (Expression tables 'Ungrouped params)) (column ': columns)
  -- ^ select list
  -> TableExpression schema params tables 'Ungrouped
  -- ^ intermediate virtual table
  -> Query schema params (column ': columns)
selectDistinct list tabs = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderCommaSeparated (renderAliasedAs renderExpression) list
  <+> renderTableExpression tabs

-- | The simplest kind of query is `selectStar` which emits all columns
-- that the table expression produces.
selectStar
  :: HasUnique table tables columns
  => TableExpression schema params tables 'Ungrouped
  -- ^ intermediate virtual table
  -> Query schema params columns
selectStar tabs = UnsafeQuery $ "SELECT" <+> "*" <+> renderTableExpression tabs

-- | A `selectDistinctStar` emits all columns that the table expression
-- produces and eliminates duplicate rows.
selectDistinctStar
  :: HasUnique table tables columns
  => TableExpression schema params tables 'Ungrouped
  -- ^ intermediate virtual table
  -> Query schema params columns
selectDistinctStar tabs = UnsafeQuery $
  "SELECT DISTINCT" <+> "*" <+> renderTableExpression tabs

-- | When working with multiple tables, it can also be useful to ask
-- for all the columns of a particular table, using `selectDotStar`.
selectDotStar
  :: Has table tables columns
  => Alias table
  -- ^ particular virtual Table
  -> TableExpression schema params tables 'Ungrouped
  -- ^ intermediate virtual table
  -> Query schema params columns
selectDotStar table tables = UnsafeQuery $
  "SELECT" <+> renderAlias table <> ".*" <+> renderTableExpression tables

-- | A `selectDistinctDotStar` asks for all the columns of a particular table, 
-- and eliminates duplicate rows.
selectDistinctDotStar
  :: Has table tables columns
  => Alias table
  -- ^ particular virtual Table
  -> TableExpression schema params tables 'Ungrouped
  -- ^ intermediate virtual table
  -> Query schema params columns
selectDistinctDotStar table tables = UnsafeQuery $
  "SELECT DISTINCT" <+> renderAlias table <> ".*"
  <+> renderTableExpression tables

{-----------------------------------------
Table Expressions
-----------------------------------------}

-- | A `TableExpression` computes a table. The table expression contains
-- a `fromClause` that is optionally followed by a `whereClause`,
-- `groupByClause`, `havingClause`, `orderByClause`, `limitClause`
-- and `offsetClause`s. Trivial table expressions simply refer
-- to a table on disk, a so-called base table, but more complex expressions
-- can be used to modify or combine base tables in various ways.
data TableExpression
  (schema :: TablesType)
  (params :: [NullityType])
  (tables :: RelationsType)
  (grouping :: Grouping)
    = TableExpression
    { fromClause :: FromClause schema params tables
    -- ^ A table reference that can be a table name, or a derived table such
    -- as a subquery, a @JOIN@ construct, or complex combinations of these.
    , whereClause :: [Condition tables 'Ungrouped params]
    -- ^ optional search coditions, combined with `.&&`. After the processing
    -- of the `fromClause` is done, each row of the derived virtual table
    -- is checked against the search condition. If the result of the
    -- condition is true, the row is kept in the output table,
    -- otherwise it is discarded. The search condition typically references
    -- at least one column of the table generated in the `fromClause`;
    -- this is not required, but otherwise the WHERE clause will
    -- be fairly useless.
    , groupByClause :: GroupByClause tables grouping
    -- ^ The `groupByClause` is used to group together those rows in a table
    -- that have the same values in all the columns listed. The order in which
    -- the columns are listed does not matter. The effect is to combine each
    -- set of rows having common values into one group row that represents all
    -- rows in the group. This is done to eliminate redundancy in the output
    -- and/or compute aggregates that apply to these groups.
    , havingClause :: HavingClause tables grouping params
    -- ^ If a table has been grouped using `groupBy`, but only certain groups
    -- are of interest, the `havingClause` can be used, much like a
    -- `whereClause`, to eliminate groups from the result. Expressions in the
    -- `havingClause` can refer both to grouped expressions and to ungrouped
    -- expressions (which necessarily involve an aggregate function).
    , orderByClause :: [SortExpression tables grouping params]
    -- ^ The `orderByClause` is for optional sorting. When more than one
    -- `SortExpression` is specified, the later (right) values are used to sort
    -- rows that are equal according to the earlier (left) values.
    , limitClause :: [Word64]
    -- ^ The `limitClause` is combined with `min` to give a limit count
    -- if nonempty. If a limit count is given, no more than that many rows
    -- will be returned (but possibly fewer, if the query itself yields
    -- fewer rows).
    , offsetClause :: [Word64]
    -- ^ The `offsetClause` is combined with `+` to give an offset count
    -- if nonempty. The offset count says to skip that many rows before
    -- beginning to return rows. The rows are skipped before the limit count
    -- is applied.
    }

-- | Render a `TableExpression`
renderTableExpression
  :: TableExpression schema params tables grouping
  -> ByteString
renderTableExpression
  (TableExpression tables whs' grps' hvs' srts' lims' offs') = mconcat
    [ "FROM ", renderFromClause tables
    , renderWheres whs'
    , renderGroupByClause grps'
    , renderHavingClause hvs'
    , renderOrderByClause srts'
    , renderLimits lims'
    , renderOffsets offs'
    ]
    where
      renderWheres = \case
        [] -> ""
        wh:[] -> " WHERE" <+> renderExpression wh
        wh:whs -> " WHERE" <+> renderExpression (foldr (.&&) wh whs)
      renderOrderByClause = \case
        [] -> ""
        srts -> " ORDER BY"
          <+> commaSeparated (renderSortExpression <$> srts)
      renderLimits = \case
        [] -> ""
        lims -> " LIMIT" <+> fromString (show (minimum lims))
      renderOffsets = \case
        [] -> ""
        offs -> " OFFSET" <+> fromString (show (sum offs))

-- | A `from` generates a `TableExpression` from a table reference that can be
-- a table name, or a derived table such as a subquery, a JOIN construct,
-- or complex combinations of these. A `from` may be transformed by `where_`,
-- `group`, `having`, `orderBy`, `limit` and `offset`, using the `&` operator
-- to match the left-to-right sequencing of their placement in SQL.
from
  :: FromClause schema params tables -- ^ table reference
  -> TableExpression schema params tables 'Ungrouped
from tables = TableExpression tables [] NoGroups NoHaving [] [] []

-- | A `where_` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `whereClause`.
where_
  :: Condition tables 'Ungrouped params
  -> TableExpression schema params tables grouping
  -> TableExpression schema params tables grouping
where_ wh tables = tables {whereClause = wh : whereClause tables}

-- | A `group` is a transformation of `TableExpression`s which switches
-- its `Grouping` from `Ungrouped` to `Grouped`. Use @group Nil@ to perform
-- a "grand total" aggregation query.
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

-- | A `having` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `havingClause`.
having
  :: Condition tables ('Grouped bys) params
  -> TableExpression schema params tables ('Grouped bys)
  -> TableExpression schema params tables ('Grouped bys)
having hv tables = tables
  { havingClause = case havingClause tables of Having hvs -> Having (hv:hvs) }

-- | An `orderBy` is an endomorphism of `TableExpression`s which appends an
-- ordering to the right of the `orderByClause`.
orderBy
  :: [SortExpression tables grouping params]
  -> TableExpression schema params tables grouping
  -> TableExpression schema params tables grouping
orderBy srts tables = tables {orderByClause = orderByClause tables ++ srts}

-- | A `limit` is an endomorphism of `TableExpression`s which adds to the
-- `limitClause`.
limit
  :: Word64
  -> TableExpression schema params tables grouping
  -> TableExpression schema params tables grouping
limit lim tables = tables {limitClause = lim : limitClause tables}

-- | An `offset` is an endomorphism of `TableExpression`s which adds to the
-- `offsetClause`.
offset
  :: Word64
  -> TableExpression schema params tables grouping
  -> TableExpression schema params tables grouping
offset off tables = tables {offsetClause = off : offsetClause tables}

{-----------------------------------------
FROM clauses
-----------------------------------------}

{- |
A `FromClause` can be a table name, or a derived table such
as a subquery, a @JOIN@ construct, or complex combinations of these.

* A real `Table` is a table from the schema.

* `Subquery` derives a table from a `Query`.

* A joined table is a table derived from two other (real or derived) tables
according to the rules of the particular join type. `CrossJoin`, `InnerJoin`,
`LeftOuterJoin`, `RightOuterJoin` and `FullOuterJoin` are available and can
be nested using the `&` operator to match the left-to-right sequencing of
their placement in SQL.

    * @t1 & CrossJoin t2@. For every possible combination of rows from
    @t1@ and @t2@ (i.e., a Cartesian product), the joined table will contain
    a row consisting of all columns in @t1@ followed by all columns in @t2@.
    If the tables have @n@ and @m@ rows respectively, the joined table will
    have @n * m@ rows.

    * @t1 & InnerJoin t2 on@. For each row @r1@ of @t1@, the joined
    table has a row for each row in @t2@ that satisfies the @on@ condition
    with @r1@

    * @t1 & LeftOuterJoin t2 on@. First, an inner join is performed.
    Then, for each row in @t1@ that does not satisfy the @on@ condition with
    any row in @t2@, a joined row is added with null values in columns of @t2@.
    Thus, the joined table always has at least one row for each row in @t1@.

    * @t1 & RightOuterJoin t2 on@. First, an inner join is performed.
    Then, for each row in @t2@ that does not satisfy the @on@ condition with
    any row in @t1@, a joined row is added with null values in columns of @t1@.
    This is the converse of a left join: the result table will always
    have a row for each row in @t2@.

    * @t1 & FullOuterJoin t2 on@. First, an inner join is performed.
    Then, for each row in @t1@ that does not satisfy the @on@ condition with
    any row in @t2@, a joined row is added with null values in columns of @t2@.
    Also, for each row of @t2@ that does not satisfy the join condition
    with any row in @t1@, a joined row with null values in the columns of @t1@
    is added.
-}
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

-- | Renders a `FromClause`.
renderFromClause :: FromClause schema params tables -> ByteString
renderFromClause = \case
  Table table -> renderAliasedAs renderTable table
  Subquery selection -> renderAliasedAs (parenthesized . renderQuery) selection
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

-- joinRef
--   :: ( Has lefttable schema (constraints :=> leftcolumns)
--      , leftrel ~ ColumnsToRelation leftcolumns
--      , Has righttable schema right
--      , rightrel ~ ColumnsToRelation (TableToColumns right)
--      , Has ref constraints ('ForeignKey keys reftable refs)
--      , SListI keys, All KnownSymbol keys
--      , SListI refs, All KnownSymbol refs
--      , KnownSymbol leftalias, KnownSymbol rightalias
--      )
--   => Aliased Alias (rightalias ::: righttable)
--   -> Alias ref
--   -> Aliased Alias (leftalias ::: lefttable)
--   -> FromClause schema params '[leftalias ::: leftrel, rightalias ::: rightrel]
-- joinRef
--   ((_ :: Alias righttable) `As` rightalias)
--   ref
--   ((_ :: Alias lefttable) `As` leftalias) =
--   InnerJoin
--     (Table (fromLabel @righttable `As` rightalias))
--     refEq
--     (Table (fromLabel @lefttable `As` leftalias))
--   where
--     refEq = undefined

{-----------------------------------------
Grouping
-----------------------------------------}

-- | `By`s are used in `group` to reference a list of columns which are then
-- used to group together those rows in a table that have the same values
-- in all the columns listed. @By \#col@ will reference an unambiguous
-- column @col@; otherwise @By2 (\#tab \! \#col)@ will reference a table
-- qualified column @tab.col@.
data By
    (tables :: RelationsType)
    (by :: (Symbol,Symbol)) where
    By
      :: (HasUnique table tables columns, Has column columns ty)
      => Alias column
      -> By tables '(table, column)
    By2
      :: (Has table tables columns, Has column columns ty)
      => (Alias table, Alias column)
      -> By tables '(table, column)
deriving instance Show (By tables by)
deriving instance Eq (By tables by)
deriving instance Ord (By tables by)

-- | Renders a `By`.
renderBy :: By tables tabcolty -> ByteString
renderBy = \case
  By column -> renderAlias column
  By2 (table, column) -> renderAlias table <> "." <> renderAlias column

-- | A `GroupByClause` indicates the `Grouping` of a `TableExpression`.
-- A `NoGroups` indicates `Ungrouped` while a `Group` indicates `Grouped`.
-- @NoGroups@ is distinguised from @Group Nil@ since no aggregation can be
-- done on @NoGroups@ while all output `Expression`s must be aggregated
-- in @Group Nil@.
data GroupByClause tables grouping where
  NoGroups :: GroupByClause tables 'Ungrouped
  Group
    :: SListI bys
    => NP (By tables) bys
    -> GroupByClause tables ('Grouped bys)

-- | Renders a `GroupByClause`.
renderGroupByClause :: GroupByClause tables grouping -> ByteString
renderGroupByClause = \case
  NoGroups -> ""
  Group Nil -> ""
  Group bys -> " GROUP BY" <+> renderCommaSeparated renderBy bys

-- | A `HavingClause` is used to eliminate groups that are not of interest.
-- An `Ungrouped` `TableExpression` may only use `NoHaving` while a `Grouped`
-- `TableExpression` must use `Having` whose conditions are combined with
-- `.&&`.
data HavingClause tables grouping params where
  NoHaving :: HavingClause tables 'Ungrouped params
  Having
    :: [Condition tables ('Grouped bys) params]
    -> HavingClause tables ('Grouped bys) params
deriving instance Show (HavingClause tables grouping params)
deriving instance Eq (HavingClause tables grouping params)
deriving instance Ord (HavingClause tables grouping params)

-- | Render a `HavingClause`.
renderHavingClause :: HavingClause tables grouping params -> ByteString
renderHavingClause = \case
  NoHaving -> ""
  Having [] -> ""
  Having conditions ->
    " HAVING" <+> commaSeparated (renderExpression <$> conditions)

{-----------------------------------------
Sorting
-----------------------------------------}

-- | `SortExpression`s are used by `sortBy` to optionally sort the results
-- of a `Query`. `Asc` or `Desc` set the sort direction of a `NotNull` result
-- column to ascending or descending. Ascending order puts smaller values
-- first, where "smaller" is defined in terms of the `.<` operator. Similarly,
-- descending order is determined with the `.>` operator. `AscNullsFirst`,
-- `AscNullsLast`, `DescNullsFirst` and `DescNullsLast` options are used to
-- determine whether nulls appear before or after non-null values in the sort
-- ordering of a `Null` result column.
data SortExpression tables grouping params where
    Asc
      :: Expression tables grouping params ('NotNull ty)
      -> SortExpression tables grouping params
    Desc
      :: Expression tables grouping params ('NotNull ty)
      -> SortExpression tables grouping params
    AscNullsFirst
      :: Expression tables grouping params  ('Null ty)
      -> SortExpression tables grouping params
    AscNullsLast
      :: Expression tables grouping params  ('Null ty)
      -> SortExpression tables grouping params
    DescNullsFirst
      :: Expression tables grouping params  ('Null ty)
      -> SortExpression tables grouping params
    DescNullsLast
      :: Expression tables grouping params  ('Null ty)
      -> SortExpression tables grouping params
deriving instance Show (SortExpression tables grouping params)

-- | Render a `SortExpression`.
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

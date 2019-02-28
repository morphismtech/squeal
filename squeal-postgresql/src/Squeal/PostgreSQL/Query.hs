{-|
Module: Squeal.PostgreSQL.Query
Description: Squeal queries
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal queries.
-}

{-# LANGUAGE
    ConstraintKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , QuantifiedConstraints
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , RankNTypes
  , UndecidableInstances
  #-}

module Squeal.PostgreSQL.Query
  ( -- * Queries
    Query (..)
  , Query_
    -- ** Select
  , select
  , select_
  , selectDistinct
  , selectDistinct_
  , Selection (..)
    -- ** Values
  , values
  , values_
    -- ** Set Operations
  , union
  , unionAll
  , intersect
  , intersectAll
  , except
  , exceptAll
    -- ** With
  , With (..)
  , CommonTableExpression (..)
    -- ** Unnest
  , unnest
    -- ** Json
  , jsonEach
  , jsonbEach
  , jsonEachAsText
  , jsonbEachAsText
  , jsonObjectKeys
  , jsonbObjectKeys
  , jsonPopulateRecord
  , jsonbPopulateRecord
  , jsonPopulateRecordSet
  , jsonbPopulateRecordSet
  , jsonToRecord
  , jsonbToRecord
  , jsonToRecordSet
  , jsonbToRecordSet
    -- * Table Expressions
  , TableExpression (..)
  , from
  , where_
  , groupBy
  , having
  , limit
  , offset
    -- * From Clauses
  , FromClause (..)
  , table
  , subquery
  , view
  , common
  , crossJoin
  , innerJoin
  , leftOuterJoin
  , rightOuterJoin
  , fullOuterJoin
    -- * Grouping
  , By (..)
  , GroupByClause (..)
  , HavingClause (..)
    -- * Subquery Expressions
  , exists
  , in_
  , notIn
  , Operator
  , subAll
  , subAny
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.TypeLits

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

{- |
The process of retrieving or the command to retrieve data from a database
is called a `Query`. Let's see some examples of queries.

simple query:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = select Star (from (table #tab))
in printSQL query
:}
SELECT * FROM "tab" AS "tab"

restricted query:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["sum" ::: 'NotNull 'PGint4, "col1" ::: 'NotNull 'PGint4]
  query =
    select_ ((#col1 + #col2) `as` #sum :* #col1)
      ( from (table #tab)
        & where_ (#col1 .> #col2)
        & where_ (#col2 .> 0) )
in printSQL query
:}
SELECT ("col1" + "col2") AS "sum", "col1" AS "col1" FROM "tab" AS "tab" WHERE (("col1" > "col2") AND ("col2" > 0))

subquery:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = select Star (from (subquery (select Star (from (table #tab)) `as` #sub)))
in printSQL query
:}
SELECT * FROM (SELECT * FROM "tab" AS "tab") AS "sub"

limits and offsets:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = select Star (from (table #tab) & limit 100 & offset 2 & limit 50 & offset 2)
in printSQL query
:}
SELECT * FROM "tab" AS "tab" LIMIT 50 OFFSET 4

parameterized query:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[ 'NotNull 'PGint4] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = select Star (from (table #tab) & where_ (#col1 .> param @1))
in printSQL query
:}
SELECT * FROM "tab" AS "tab" WHERE ("col1" > ($1 :: int4))

aggregation query:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["sum" ::: 'NotNull 'PGint4, "col1" ::: 'NotNull 'PGint4 ]
  query =
    select_ (sum_ (All #col2) `as` #sum :* #col1)
    ( from (table (#tab `as` #table1))
      & groupBy #col1
      & having (#col1 + sum_ (Distinct #col2) .> 1) )
in printSQL query
:}
SELECT sum(ALL "col2") AS "sum", "col1" AS "col1" FROM "tab" AS "table1" GROUP BY "col1" HAVING (("col1" + sum(DISTINCT "col2")) > 1)

sorted query:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = select Star (from (table #tab) & orderBy [#col1 & Asc])
in printSQL query
:}
SELECT * FROM "tab" AS "tab" ORDER BY "col1" ASC

joins:

>>> :set -XFlexibleContexts
>>> :{
type OrdersColumns =
  '[ "id"         ::: 'NoDef :=> 'NotNull 'PGint4
  , "price"       ::: 'NoDef :=> 'NotNull 'PGfloat4
  , "customer_id" ::: 'NoDef :=> 'NotNull 'PGint4
  , "shipper_id"  ::: 'NoDef :=> 'NotNull 'PGint4
  ]
:}

>>> :{
type OrdersConstraints =
  '["pk_orders" ::: PrimaryKey '["id"]
  ,"fk_customers" ::: ForeignKey '["customer_id"] "customers" '["id"]
  ,"fk_shippers" ::: ForeignKey '["shipper_id"] "shippers" '["id"] ]
:}

>>> type NamesColumns = '["id" ::: 'NoDef :=> 'NotNull 'PGint4, "name" ::: 'NoDef :=> 'NotNull 'PGtext]
>>> type CustomersConstraints = '["pk_customers" ::: PrimaryKey '["id"]]
>>> type ShippersConstraints = '["pk_shippers" ::: PrimaryKey '["id"]]
>>> :{
type OrdersSchema =
  '[ "orders"   ::: 'Table (OrdersConstraints :=> OrdersColumns)
  , "customers" ::: 'Table (CustomersConstraints :=> NamesColumns)
  , "shippers" ::: 'Table (ShippersConstraints :=> NamesColumns) ]
:}

>>> :{
let
  query :: Query '[] '[] (Public OrdersSchema)
    '[]
    '[ "order_price" ::: 'NotNull 'PGfloat4
     , "customer_name" ::: 'NotNull 'PGtext
     , "shipper_name" ::: 'NotNull 'PGtext
     ]
  query = select_
    ( #o ! #price `as` #order_price :*
      #c ! #name `as` #customer_name :*
      #s ! #name `as` #shipper_name )
    ( from (table (#orders `as` #o)
      & innerJoin (table (#customers `as` #c))
        (#o ! #customer_id .== #c ! #id)
      & innerJoin (table (#shippers `as` #s))
        (#o ! #shipper_id .== #s ! #id)) )
in printSQL query
:}
SELECT "o"."price" AS "order_price", "c"."name" AS "customer_name", "s"."name" AS "shipper_name" FROM "orders" AS "o" INNER JOIN "customers" AS "c" ON ("o"."customer_id" = "c"."id") INNER JOIN "shippers" AS "s" ON ("o"."shipper_id" = "s"."id")

self-join:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = select (#t1 & DotStar) (from (table (#tab `as` #t1) & crossJoin (table (#tab `as` #t2))))
in printSQL query
:}
SELECT "t1".* FROM "tab" AS "t1" CROSS JOIN "tab" AS "t2"

value queries:

>>> :{
let
  query :: Query '[] commons schemas '[] '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
  query = values (1 `as` #foo :* true `as` #bar) [2 `as` #foo :* false `as` #bar]
in printSQL query
:}
SELECT * FROM (VALUES (1, TRUE), (2, FALSE)) AS t ("foo", "bar")

set operations:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = select Star (from (table #tab)) `unionAll` select Star (from (table #tab))
in printSQL query
:}
(SELECT * FROM "tab" AS "tab") UNION ALL (SELECT * FROM "tab" AS "tab")

with queries:

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = with (
    select Star (from (table #tab)) `as` #cte1 :>>
    select Star (from (common #cte1)) `as` #cte2
    ) (select Star (from (common #cte2)))
in printSQL query
:}
WITH "cte1" AS (SELECT * FROM "tab" AS "tab"), "cte2" AS (SELECT * FROM "cte1" AS "cte1") SELECT * FROM "cte2" AS "cte2"

window function queries

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] ["col1" ::: 'NotNull 'PGint4, "rank" ::: 'NotNull 'PGint8]
  query = select
    (#col1 & Also (rank `as` #rank `Over` (partitionBy #col1 & orderBy [#col2 & Asc])))
    (from (table #tab))
in printSQL query
:}
SELECT "col1" AS "col1", rank() OVER (PARTITION BY "col1" ORDER BY "col2" ASC) AS "rank" FROM "tab" AS "tab"

correlated subqueries

>>> :{
let
  query :: Query '[] '[] (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4]
  query =
    select #col1 (from (table (#tab `as` #t1))
    & where_ (exists (
      select Star (from (table (#tab `as` #t2))
      & where_ (#t2 ! #col2 .== #t1 ! #col1)))))
in printSQL query
:}
SELECT "col1" AS "col1" FROM "tab" AS "t1" WHERE EXISTS (SELECT * FROM "tab" AS "t2" WHERE ("t2"."col2" = "t1"."col1"))
-}
newtype Query
  (outer :: FromType)
  (commons :: FromType)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (row :: RowType)
    = UnsafeQuery { renderQuery :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Query outer commons schemas params row) where renderSQL = renderQuery

type family Query_ (schemas :: SchemasType) (params :: Type) (row :: Type) where
  Query_ schemas params row = Query '[] '[] schemas (TuplePG params) (RowPG row)

-- | The results of two queries can be combined using the set operation
-- `union`. Duplicate rows are eliminated.
union
  :: Query outer commons schemas params columns
  -> Query outer commons schemas params columns
  -> Query outer commons schemas params columns
q1 `union` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "UNION"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `unionAll`, the disjoint union. Duplicate rows are retained.
unionAll
  :: Query outer commons schemas params columns
  -> Query outer commons schemas params columns
  -> Query outer commons schemas params columns
q1 `unionAll` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "UNION" <+> "ALL"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `intersect`, the intersection. Duplicate rows are eliminated.
intersect
  :: Query outer commons schemas params columns
  -> Query outer commons schemas params columns
  -> Query outer commons schemas params columns
q1 `intersect` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "INTERSECT"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `intersectAll`, the intersection. Duplicate rows are retained.
intersectAll
  :: Query outer commons schemas params columns
  -> Query outer commons schemas params columns
  -> Query outer commons schemas params columns
q1 `intersectAll` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "INTERSECT" <+> "ALL"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `except`, the set difference. Duplicate rows are eliminated.
except
  :: Query outer commons schemas params columns
  -> Query outer commons schemas params columns
  -> Query outer commons schemas params columns
q1 `except` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "EXCEPT"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `exceptAll`, the set difference. Duplicate rows are retained.
exceptAll
  :: Query outer commons schemas params columns
  -> Query outer commons schemas params columns
  -> Query outer commons schemas params columns
q1 `exceptAll` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "EXCEPT" <+> "ALL"
  <+> parenthesized (renderSQL q2)

{-----------------------------------------
SELECT queries
-----------------------------------------}

data Selection outer grp commons schemas params from row where
  List
    :: SListI row
    => NP (Aliased (Expression outer grp commons schemas params from)) row
    -> Selection outer grp commons schemas params from row
  Star
    :: HasUnique tab from row
    => Selection outer 'Ungrouped commons schemas params from row
  DotStar
    :: Has tab from row
    => Alias tab
    -> Selection outer 'Ungrouped commons schemas params from row
  Also
    :: Selection outer grp commons schemas params from right
    -> Selection outer grp commons schemas params from left
    -> Selection outer grp commons schemas params from (Join left right)
  Over
    :: SListI row
    => NP (Aliased (WindowFunction outer grp commons schemas params from)) row
    -> WindowDefinition outer grp commons schemas params from
    -> Selection outer grp commons schemas params from row
instance (KnownSymbol col, row ~ '[col ::: ty])
  => Aliasable col
    (Expression outer grp commons schemas params from ty)
    (Selection outer grp commons schemas params from row) where
      expr `as` col = List (expr `as` col)
instance (Has tab (Join outer from) row0, Has col row0 ty, row1 ~ '[col ::: ty])
  => IsQualified tab col
    (Selection outer 'Ungrouped commons schemas params from row1) where
      tab ! col = tab ! col `as` col
instance
  ( Has tab (Join outer from) row0
  , Has col row0 ty
  , row1 ~ '[col ::: ty]
  , GroupedBy tab col bys )
  => IsQualified tab col
    (Selection outer ('Grouped bys) commons schemas params from row1) where
      tab ! col = tab ! col `as` col
instance (HasUnique tab (Join outer from) row0, Has col row0 ty, row1 ~ '[col ::: ty])
  => IsLabel col
    (Selection outer 'Ungrouped commons schemas params from row1) where
      fromLabel = fromLabel @col `as` Alias
instance
  ( HasUnique tab (Join outer from) row0
  , Has col row0 ty
  , row1 ~ '[col ::: ty]
  , GroupedBy tab col bys )
  => IsLabel col
    (Selection outer ('Grouped bys) commons schemas params from row1) where
      fromLabel = fromLabel @col `as` Alias

instance RenderSQL (Selection outer grp commons schemas params from row) where
  renderSQL = \case
    List list -> renderCommaSeparated (renderAliased renderSQL) list
    Star -> "*"
    DotStar tab -> renderSQL tab <> ".*"
    Also right left -> renderSQL left <> ", " <> renderSQL right
    Over winFns winDef ->
      let
        renderOver
          :: Aliased (WindowFunction outer grp commons schemas params from) field
          -> ByteString
        renderOver (winFn `As` col) = renderSQL winFn
          <+> "OVER" <+> parenthesized (renderSQL winDef)
          <+> "AS" <+> renderSQL col
      in
        renderCommaSeparated renderOver winFns

instance IsString
  (Selection outer grp commons schemas params from '["fromOnly" ::: 'NotNull 'PGtext]) where
    fromString str = fromString str `as` Alias

-- | the `TableExpression` in the `select` command constructs an intermediate
-- virtual table by possibly combining tables, views, eliminating rows,
-- grouping, etc. This table is finally passed on to processing by
-- the select list. The `Selection` determines which columns of
-- the intermediate table are actually output.
select
  :: (SListI row, row ~ (x ': xs))
  => Selection outer grp commons schemas params from row
  -- ^ select list
  -> TableExpression outer grp commons schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params row
select selection tabexpr = UnsafeQuery $
  "SELECT"
  <+> renderSQL selection
  <+> renderSQL tabexpr

select_
  :: (SListI row, row ~ (x ': xs))
  => NP (Aliased (Expression outer grp commons schemas params from)) row
  -> TableExpression outer grp commons schemas params from
  -> Query outer commons schemas params row
select_ list = select (List list)

-- | After the select list has been processed, the result table can
-- be subject to the elimination of duplicate rows using `selectDistinct`.
selectDistinct
  :: (SListI columns, columns ~ (col ': cols))
  => Selection outer 'Ungrouped commons schemas params from columns
  -- ^ select list
  -> TableExpression outer 'Ungrouped commons schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params columns
selectDistinct selection tabexpr = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderSQL selection
  <+> renderSQL tabexpr

selectDistinct_
  :: (SListI columns, columns ~ (col ': cols))
  => NP (Aliased (Expression outer 'Ungrouped commons schemas params from)) columns
  -- ^ select list
  -> TableExpression outer 'Ungrouped commons schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params columns
selectDistinct_ list = select (List list)

-- | `values` computes a row value or set of row values
-- specified by value expressions. It is most commonly used
-- to generate a “constant table” within a larger command,
-- but it can be used on its own.
--
-- >>> type Row = '["a" ::: 'NotNull 'PGint4, "b" ::: 'NotNull 'PGtext]
-- >>> let query = values (1 `as` #a :* "one" `as` #b) [] :: Query outer commons schemas '[] Row
-- >>> printSQL query
-- SELECT * FROM (VALUES (1, E'one')) AS t ("a", "b")
values
  :: SListI cols
  => NP (Aliased (Expression outer 'Ungrouped commons schemas params '[] )) cols
  -> [NP (Aliased (Expression outer 'Ungrouped commons schemas params '[] )) cols]
  -- ^ When more than one row is specified, all the rows must
  -- must have the same number of elements
  -> Query outer commons schemas params cols
values rw rws = UnsafeQuery $ "SELECT * FROM"
  <+> parenthesized (
    "VALUES"
    <+> commaSeparated
        ( parenthesized
        . renderCommaSeparated renderValuePart <$> rw:rws )
    ) <+> "AS t"
  <+> parenthesized (renderCommaSeparated renderAliasPart rw)
  where
    renderAliasPart, renderValuePart
      :: Aliased (Expression outer 'Ungrouped commons schemas params '[] ) ty -> ByteString
    renderAliasPart (_ `As` name) = renderSQL name
    renderValuePart (value `As` _) = renderSQL value

-- | `values_` computes a row value or set of row values
-- specified by value expressions.
values_
  :: SListI cols
  => NP (Aliased (Expression outer 'Ungrouped commons schemas params '[] )) cols
  -- ^ one row of values
  -> Query outer commons schemas params cols
values_ rw = values rw []

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
  (outer :: FromType)
  (grp :: Grouping)
  (commons :: FromType)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (from :: FromType)
    = TableExpression
    { fromClause :: FromClause outer commons schemas params from
    -- ^ A table reference that can be a table name, or a derived table such
    -- as a subquery, a @JOIN@ construct, or complex combinations of these.
    , whereClause :: [Condition outer 'Ungrouped commons schemas params from]
    -- ^ optional search coditions, combined with `.&&`. After the processing
    -- of the `fromClause` is done, each row of the derived virtual table
    -- is checked against the search condition. If the result of the
    -- condition is true, the row is kept in the output table,
    -- otherwise it is discarded. The search condition typically references
    -- at least one column of the table generated in the `fromClause`;
    -- this is not required, but otherwise the WHERE clause will
    -- be fairly useless.
    , groupByClause :: GroupByClause grp from
    -- ^ The `groupByClause` is used to group together those rows in a table
    -- that have the same values in all the columns listed. The order in which
    -- the columns are listed does not matter. The effect is to combine each
    -- set of rows having common values into one group row that represents all
    -- rows in the group. This is done to eliminate redundancy in the output
    -- and/or compute aggregates that apply to these groups.
    , havingClause :: HavingClause outer grp commons schemas params from
    -- ^ If a table has been grouped using `groupBy`, but only certain groups
    -- are of interest, the `havingClause` can be used, much like a
    -- `whereClause`, to eliminate groups from the result. Expressions in the
    -- `havingClause` can refer both to grouped expressions and to ungrouped
    -- expressions (which necessarily involve an aggregate function).
    , orderByClause :: [SortExpression outer grp commons schemas params from]
    -- ^ The `orderByClause` is for optional sorting. When more than one
    -- `SortExpression` is specified, the later (right) values are used to sort
    -- rows that are equal according to the earlier (left) values.
    , limitClause :: [Word64]
    -- ^ The `limitClause` is combined with `min` to give a limit count
    -- if nonempty. If a limit count is given, no more than that many rows
    -- will be returned (but possibly fewer, if the query itself yields
    -- fewer rows).
    , offsetClause :: [Word64]
    -- ^ The `offsetClause` is combined with `Prelude.+` to give an offset count
    -- if nonempty. The offset count says to skip that many rows before
    -- beginning to return rows. The rows are skipped before the limit count
    -- is applied.
    } deriving (GHC.Generic)

-- | Render a `TableExpression`
instance RenderSQL (TableExpression outer grp commons schemas params from) where
  renderSQL
    (TableExpression frm' whs' grps' hvs' srts' lims' offs') = mconcat
      [ "FROM ", renderSQL frm'
      , renderWheres whs'
      , renderSQL grps'
      , renderSQL hvs'
      , renderOrderByClause srts'
      , renderLimits lims'
      , renderOffsets offs' ]
      where
        renderWheres = \case
          [] -> ""
          wh:[] -> " WHERE" <+> renderSQL wh
          wh:whs -> " WHERE" <+> renderSQL (foldr (.&&) wh whs)
        renderOrderByClause = \case
          [] -> ""
          srts -> " ORDER BY"
            <+> commaSeparated (renderSQL <$> srts)
        renderLimits = \case
          [] -> ""
          lims -> " LIMIT" <+> fromString (show (minimum lims))
        renderOffsets = \case
          [] -> ""
          offs -> " OFFSET" <+> fromString (show (sum offs))

-- | A `from` generates a `TableExpression` from a table reference that can be
-- a table name, or a derived table such as a subquery, a JOIN construct,
-- or complex combinations of these. A `from` may be transformed by `where_`,
-- `groupBy`, `having`, `orderBy`, `limit` and `offset`, using the `&` operator
-- to match the left-to-right sequencing of their placement in SQL.
from
  :: FromClause outer commons schemas params from -- ^ table reference
  -> TableExpression outer 'Ungrouped commons schemas params from
from tab = TableExpression tab [] NoGroups NoHaving [] [] []

-- | A `where_` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `whereClause`.
where_
  :: Condition outer 'Ungrouped commons schemas params from -- ^ filtering condition
  -> TableExpression outer grp commons schemas params from
  -> TableExpression outer grp commons schemas params from
where_ wh rels = rels {whereClause = wh : whereClause rels}

-- | A `groupBy` is a transformation of `TableExpression`s which switches
-- its `Grouping` from `Ungrouped` to `Grouped`. Use @group Nil@ to perform
-- a "grand total" aggregation query.
groupBy
  :: SListI bys
  => NP (By from) bys -- ^ grouped columns
  -> TableExpression outer 'Ungrouped commons schemas params from
  -> TableExpression outer ('Grouped bys) commons schemas params from
groupBy bys rels = TableExpression
  { fromClause = fromClause rels
  , whereClause = whereClause rels
  , groupByClause = Group bys
  , havingClause = Having []
  , orderByClause = []
  , limitClause = limitClause rels
  , offsetClause = offsetClause rels
  }

-- | A `having` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `havingClause`.
having
  :: Condition outer ('Grouped bys) commons schemas params from -- ^ having condition
  -> TableExpression outer ('Grouped bys) commons schemas params from
  -> TableExpression outer ('Grouped bys) commons schemas params from
having hv rels = rels
  { havingClause = case havingClause rels of Having hvs -> Having (hv:hvs) }

instance OrderBy TableExpression where
  orderBy srts rels = rels {orderByClause = orderByClause rels ++ srts}

-- | A `limit` is an endomorphism of `TableExpression`s which adds to the
-- `limitClause`.
limit
  :: Word64 -- ^ limit parameter
  -> TableExpression outer grp commons schemas params from
  -> TableExpression outer grp commons schemas params from
limit lim rels = rels {limitClause = lim : limitClause rels}

-- | An `offset` is an endomorphism of `TableExpression`s which adds to the
-- `offsetClause`.
offset
  :: Word64 -- ^ offset parameter
  -> TableExpression outer grp commons schemas params from
  -> TableExpression outer grp commons schemas params from
offset off rels = rels {offsetClause = off : offsetClause rels}

{-----------------------------------------
JSON stuff
-----------------------------------------}

unsafeSetOfFunction
  :: ByteString
  -> Expression outer 'Ungrouped commons schemas params '[]  ty
  -> Query outer commons schemas params row
unsafeSetOfFunction fun expr = UnsafeQuery $
  "SELECT * FROM " <> fun <> "(" <> renderSQL expr <> ")"

unnest
  :: Expression outer 'Ungrouped commons schemas params '[] (nullity ('PGvararray ty))
  -> Query outer commons schemas params '[ "unnest" ::: ty ]
unnest = unsafeSetOfFunction "unnest"

-- | Expands the outermost JSON object into a set of key/value pairs.
jsonEach
  :: Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjson) -- ^ json object
  -> Query outer commons schemas params
      '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGjson]
jsonEach = unsafeSetOfFunction "json_each"

-- | Expands the outermost binary JSON object into a set of key/value pairs.
jsonbEach
  :: Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> Query outer commons schemas params
      '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGjsonb]
jsonbEach = unsafeSetOfFunction "jsonb_each"

-- | Expands the outermost JSON object into a set of key/value pairs.
jsonEachAsText
  :: Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjson) -- ^ json object
  -> Query outer commons schemas params
      '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGtext]
jsonEachAsText = unsafeSetOfFunction "json_each_text"

-- | Expands the outermost binary JSON object into a set of key/value pairs.
jsonbEachAsText
  :: Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> Query outer commons schemas params
    '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGtext]
jsonbEachAsText = unsafeSetOfFunction "jsonb_each_text"

-- | Returns set of keys in the outermost JSON object.
jsonObjectKeys
  :: Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjson) -- ^ json object
  -> Query outer commons schemas params '["json_object_keys" ::: 'NotNull 'PGtext]
jsonObjectKeys = unsafeSetOfFunction "json_object_keys"

-- | Returns set of keys in the outermost JSON object.
jsonbObjectKeys
  :: Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> Query outer commons schemas params '["jsonb_object_keys" ::: 'NotNull 'PGtext]
jsonbObjectKeys = unsafeSetOfFunction "jsonb_object_keys"

unsafePopulateFunction
  :: ByteString
  -> TypeExpression schemas (nullity ('PGcomposite row))
  -> Expression outer 'Ungrouped commons schemas params '[]  ty
  -> Query outer commons schemas params row
unsafePopulateFunction fun ty expr = UnsafeQuery $
  "SELECT * FROM " <> fun <> "("
    <> "null::" <> renderSQL ty <> ", "
    <> renderSQL expr <> ")"

-- | Expands the JSON expression to a row whose columns match the record
-- type defined by the given table.
jsonPopulateRecord
  :: TypeExpression schemas (nullity ('PGcomposite row)) -- ^ row type
  -> Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjson) -- ^ json object
  -> Query outer commons schemas params row
jsonPopulateRecord = unsafePopulateFunction "json_populate_record"

-- | Expands the binary JSON expression to a row whose columns match the record
-- type defined by the given table.
jsonbPopulateRecord
  :: TypeExpression schemas (nullity ('PGcomposite row)) -- ^ row type
  -> Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> Query outer commons schemas params row
jsonbPopulateRecord = unsafePopulateFunction "jsonb_populate_record"

-- | Expands the outermost array of objects in the given JSON expression to a
-- set of rows whose columns match the record type defined by the given table.
jsonPopulateRecordSet
  :: TypeExpression schemas (nullity ('PGcomposite row)) -- ^ row type
  -> Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjson) -- ^ json array
  -> Query outer commons schemas params row
jsonPopulateRecordSet = unsafePopulateFunction "json_populate_record_set"

-- | Expands the outermost array of objects in the given binary JSON expression
-- to a set of rows whose columns match the record type defined by the given
-- table.
jsonbPopulateRecordSet
  :: TypeExpression schemas (nullity ('PGcomposite row)) -- ^ row type
  -> Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjsonb) -- ^ jsonb array
  -> Query outer commons schemas params row
jsonbPopulateRecordSet = unsafePopulateFunction "jsonb_populate_record_set"

unsafeRecordFunction
  :: (SListI record, json `In` PGJsonType)
  => ByteString
  -> Expression outer 'Ungrouped commons schemas params '[]  (nullity json)
  -> NP (Aliased (TypeExpression schemas)) record
  -> Query outer commons schemas params record
unsafeRecordFunction fun expr types = UnsafeQuery $
  "SELECT * FROM " <> fun <> "("
    <> renderSQL expr <> ")"
    <+> "AS" <+> "x" <> parenthesized (renderCommaSeparated renderTy types)
    where
      renderTy :: Aliased (TypeExpression schemas) ty -> ByteString
      renderTy (ty `As` alias) =
        renderSQL alias <+> renderSQL ty

-- | Builds an arbitrary record from a JSON object.
jsonToRecord
  :: SListI record
  => Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjson) -- ^ json object
  -> NP (Aliased (TypeExpression schemas)) record -- ^ record types
  -> Query outer commons schemas params record
jsonToRecord = unsafeRecordFunction "json_to_record"

-- | Builds an arbitrary record from a binary JSON object.
jsonbToRecord
  :: SListI record
  => Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> NP (Aliased (TypeExpression schemas)) record -- ^ record types
  -> Query outer commons schemas params record
jsonbToRecord = unsafeRecordFunction "jsonb_to_record"

-- | Builds an arbitrary set of records from a JSON array of objects.
jsonToRecordSet
  :: SListI record
  => Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjson) -- ^ json array
  -> NP (Aliased (TypeExpression schemas)) record -- ^ record types
  -> Query outer commons schemas params record
jsonToRecordSet = unsafeRecordFunction "json_to_record_set"

-- | Builds an arbitrary set of records from a binary JSON array of objects.
jsonbToRecordSet
  :: SListI record
  => Expression outer 'Ungrouped commons schemas params '[]  (nullity 'PGjsonb) -- ^ jsonb array
  -> NP (Aliased (TypeExpression schemas)) record -- ^ record types
  -> Query outer commons schemas params record
jsonbToRecordSet = unsafeRecordFunction "jsonb_to_record_set"

{-----------------------------------------
FROM clauses
-----------------------------------------}

{- |
A `FromClause` can be a table name, or a derived table such
as a subquery, a @JOIN@ construct, or complex combinations of these.
-}
newtype FromClause outer commons schemas params from
  = UnsafeFromClause { renderFromClause :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (FromClause outer commons schemas params from) where
  renderSQL = renderFromClause

-- | A real `table` is a table from the database.
table
  :: (Has sch schemas schema, Has tab schema ('Table table))
  => Aliased (QualifiedAlias sch) (alias ::: tab)
  -> FromClause outer commons schemas params '[alias ::: TableToRow table]
table (tab `As` alias) = UnsafeFromClause $
  renderSQL tab <+> "AS" <+> renderSQL alias

-- | `subquery` derives a table from a `Query`.
subquery
  :: Aliased (Query outer commons schemas params) query
  -> FromClause outer commons schemas params '[query]
subquery = UnsafeFromClause . renderAliased (parenthesized . renderQuery)

-- | `view` derives a table from a `View`.
view
  :: (Has sch schemas schema, Has vw schema ('View view))
  => Aliased (QualifiedAlias sch) (alias ::: vw)
  -> FromClause outer commons schemas params '[alias ::: view]
view (vw `As` alias) = UnsafeFromClause $
  renderSQL vw <+> "AS" <+> renderSQL alias

common
  :: Has cte commons common
  => Aliased Alias (alias ::: cte)
  -> FromClause outer commons schemas params '[alias ::: common]
common (cte `As` alias) = UnsafeFromClause $
  renderSQL cte <+> "AS" <+> renderSQL alias

{- | @left & crossJoin right@. For every possible combination of rows from
    @left@ and @right@ (i.e., a Cartesian product), the joined table will contain
    a row consisting of all columns in @left@ followed by all columns in @right@.
    If the tables have @n@ and @m@ rows respectively, the joined table will
    have @n * m@ rows.
-}
crossJoin
  :: FromClause outer commons schemas params right
  -- ^ right
  -> FromClause outer commons schemas params left
  -- ^ left
  -> FromClause outer commons schemas params (Join left right)
crossJoin right left = UnsafeFromClause $
  renderSQL left <+> "CROSS JOIN" <+> renderSQL right

{- | @left & innerJoin right on@. The joined table is filtered by
the @on@ condition.
-}
innerJoin
  :: FromClause outer commons schemas params right
  -- ^ right
  -> Condition outer 'Ungrouped commons schemas params (Join left right)
  -- ^ @on@ condition
  -> FromClause outer commons schemas params left
  -- ^ left
  -> FromClause outer commons schemas params (Join left right)
innerJoin right on left = UnsafeFromClause $
  renderSQL left <+> "INNER JOIN" <+> renderSQL right
  <+> "ON" <+> renderSQL on

{- | @left & leftOuterJoin right on@. First, an inner join is performed.
    Then, for each row in @left@ that does not satisfy the @on@ condition with
    any row in @right@, a joined row is added with null values in columns of @right@.
    Thus, the joined table always has at least one row for each row in @left@.
-}
leftOuterJoin
  :: FromClause outer commons schemas params right
  -- ^ right
  -> Condition outer 'Ungrouped commons schemas params (Join left right)
  -- ^ @on@ condition
  -> FromClause outer commons schemas params left
  -- ^ left
  -> FromClause outer commons schemas params (Join left (NullifyFrom right))
leftOuterJoin right on left = UnsafeFromClause $
  renderSQL left <+> "LEFT OUTER JOIN" <+> renderSQL right
  <+> "ON" <+> renderSQL on

{- | @left & rightOuterJoin right on@. First, an inner join is performed.
    Then, for each row in @right@ that does not satisfy the @on@ condition with
    any row in @left@, a joined row is added with null values in columns of @left@.
    This is the converse of a left join: the result table will always
    have a row for each row in @right@.
-}
rightOuterJoin
  :: FromClause outer commons schemas params right
  -- ^ right
  -> Condition outer 'Ungrouped commons schemas params (Join left right)
  -- ^ @on@ condition
  -> FromClause outer commons schemas params left
  -- ^ left
  -> FromClause outer commons schemas params (Join (NullifyFrom left) right)
rightOuterJoin right on left = UnsafeFromClause $
  renderSQL left <+> "RIGHT OUTER JOIN" <+> renderSQL right
  <+> "ON" <+> renderSQL on

{- | @left & fullOuterJoin right on@. First, an inner join is performed.
    Then, for each row in @left@ that does not satisfy the @on@ condition with
    any row in @right@, a joined row is added with null values in columns of @right@.
    Also, for each row of @right@ that does not satisfy the join condition
    with any row in @left@, a joined row with null values in the columns of @left@
    is added.
-}
fullOuterJoin
  :: FromClause outer commons schemas params right
  -- ^ right
  -> Condition outer 'Ungrouped commons schemas params (Join left right)
  -- ^ @on@ condition
  -> FromClause outer commons schemas params left
  -- ^ left
  -> FromClause outer commons schemas params
      (Join (NullifyFrom left) (NullifyFrom right))
fullOuterJoin right on left = UnsafeFromClause $
  renderSQL left <+> "FULL OUTER JOIN" <+> renderSQL right
  <+> "ON" <+> renderSQL on

{-----------------------------------------
Grouping
-----------------------------------------}

-- | `By`s are used in `groupBy` to reference a list of columns which are then
-- used to group together those rows in a table that have the same values
-- in all the columns listed. @By \#col@ will reference an unambiguous
-- column @col@; otherwise @By2 (\#tab \! \#col)@ will reference a table
-- qualified column @tab.col@.
data By
    (from :: FromType)
    (by :: (Symbol,Symbol)) where
    By1
      :: (HasUnique table from columns, Has column columns ty)
      => Alias column
      -> By from '(table, column)
    By2
      :: (Has table from columns, Has column columns ty)
      => Alias table
      -> Alias column
      -> By from '(table, column)
deriving instance Show (By from by)
deriving instance Eq (By from by)
deriving instance Ord (By from by)
instance RenderSQL (By from by) where
  renderSQL = \case
    By1 column -> renderSQL column
    By2 rel column -> renderSQL rel <> "." <> renderSQL column

instance (HasUnique rel rels cols, Has col cols ty, by ~ '(rel, col))
  => IsLabel col (By rels by) where fromLabel = By1 fromLabel
instance (HasUnique rel rels cols, Has col cols ty, bys ~ '[ '(rel, col)])
  => IsLabel col (NP (By rels) bys) where fromLabel = By1 fromLabel :* Nil
instance (Has rel rels cols, Has col cols ty, by ~ '(rel, col))
  => IsQualified rel col (By rels by) where (!) = By2
instance (Has rel rels cols, Has col cols ty, bys ~ '[ '(rel, col)])
  => IsQualified rel col (NP (By rels) bys) where
    rel ! col = By2 rel col :* Nil

-- | A `GroupByClause` indicates the `Grouping` of a `TableExpression`.
-- A `NoGroups` indicates `Ungrouped` while a `Group` indicates `Grouped`.
-- @NoGroups@ is distinguised from @Group Nil@ since no aggregation can be
-- done on @NoGroups@ while all output `Expression`s must be aggregated
-- in @Group Nil@. In general, all output `Expression`s in the
-- complement of @bys@ must be aggregated in @Group bys@.
data GroupByClause grp from where
  NoGroups :: GroupByClause 'Ungrouped from
  Group
    :: SListI bys
    => NP (By from) bys
    -> GroupByClause ('Grouped bys) from

-- | Renders a `GroupByClause`.
instance RenderSQL (GroupByClause grp from) where
  renderSQL = \case
    NoGroups -> ""
    Group Nil -> ""
    Group bys -> " GROUP BY" <+> renderCommaSeparated renderSQL bys

-- | A `HavingClause` is used to eliminate groups that are not of interest.
-- An `Ungrouped` `TableExpression` may only use `NoHaving` while a `Grouped`
-- `TableExpression` must use `Having` whose conditions are combined with
-- `.&&`.
data HavingClause outer grp commons schemas params from where
  NoHaving :: HavingClause outer 'Ungrouped commons schemas params from
  Having
    :: [Condition outer ('Grouped bys) commons schemas params from]
    -> HavingClause outer ('Grouped bys) commons schemas params from
deriving instance Show (HavingClause outer grp commons schemas params from)
deriving instance Eq (HavingClause outer grp commons schemas params from)
deriving instance Ord (HavingClause outer grp commons schemas params from)

-- | Render a `HavingClause`.
instance RenderSQL (HavingClause outer grp commons schemas params from) where
  renderSQL = \case
    NoHaving -> ""
    Having [] -> ""
    Having conditions ->
      " HAVING" <+> commaSeparated (renderSQL <$> conditions)

{-
The argument of `exists` is an arbitrary subquery. The subquery is evaluated
to determine whether it returns any rows. If it returns at least one row,
the result of `exists` is `true`; if the subquery returns no rows,
the result of `exists` is `false`.

The subquery can refer to variables from the surrounding query,
which will act as constants during any one evaluation of the subquery.

The subquery will generally only be executed long enough to determine whether
at least one row is returned, not all the way to completion.
-}
exists
  :: Query (Join outer from) commons schemas params row
  -> Condition outer grp commons schemas params from
exists query = UnsafeExpression $ "EXISTS" <+> parenthesized (renderSQL query)

type Operator ty1 ty2 ty3
  =  forall outer grp commons schemas params from
  .  Expression outer grp commons schemas params from ty1
  -> Expression outer grp commons schemas params from ty2
  -> Expression outer grp commons schemas params from ty3

{- |
The right-hand side is a parenthesized subquery, which must return
exactly one column. The left-hand expression is evaluated and compared to each
row of the subquery result using the given `Operator`,
which must yield a Boolean result. The result of `subAll` is `true`
if all rows yield true (including the case where the subquery returns no rows).
The result is `false` if any `false` result is found.
The result is `null_` if no comparison with a subquery row returns `false`,
and at least one comparison returns `null_`.

>>> printSQL $ subAll true (.==) (values_ (true `as` #foo))
(TRUE = ALL (SELECT * FROM (VALUES (TRUE)) AS t ("foo")))
-}
subAll
  :: Expression outer grp commons schemas params from ty1 -- ^ expression
  -> Operator ty1 ty2 ('Null 'PGbool) -- ^ operator
  -> Query (Join outer from) commons schemas params '[col ::: ty2] -- ^ subquery
  -> Condition outer grp commons schemas params from
subAll expr (?) qry = expr ?
  (UnsafeExpression $ "ALL" <+> parenthesized (renderSQL qry))

{- |
The right-hand side is a parenthesized subquery, which must return exactly one column.
The left-hand expression is evaluated and compared to each row of the subquery result
using the given `Operator`, which must yield a Boolean result. The result of `subAny` is `true`
if any `true` result is obtained. The result is `false` if no true result is found
(including the case where the subquery returns no rows).

>>> printSQL $ subAll "foo" like (values_ ("foobar" `as` #foo))
(E'foo' LIKE ALL (SELECT * FROM (VALUES (E'foobar')) AS t ("foo")))
-}
subAny
  :: Expression outer grp commons schemas params from ty1 -- ^ expression
  -> Operator ty1 ty2 ('Null 'PGbool) -- ^ operator
  -> Query (Join outer from) commons schemas params '[col ::: ty2] -- ^ subquery
  -> Condition outer grp commons schemas params from
subAny expr (?) qry = expr ?
  (UnsafeExpression $ "ANY" <+> parenthesized (renderSQL qry))

{-
The result is `true` if the left-hand expression's result is equal
to any of the right-hand expressions.

>>> printSQL $ true `in_` [true, false, null_]
TRUE IN (TRUE, FALSE, NULL)
-}
in_
  :: Expression outer grp commons schemas params from ty -- ^ expression
  -> [Expression outer grp commons schemas params from ty]
  -> Condition outer grp commons schemas params from
expr `in_` exprs = UnsafeExpression $ renderSQL expr <+> "IN"
  <+> parenthesized (commaSeparated (renderSQL <$> exprs))

{-
The result is `true` if the left-hand expression's result is not equal
to any of the right-hand expressions.

>>> printSQL $ true `notIn` [false, null_]
TRUE NOT IN (FALSE, NULL)
-}
notIn
  :: Expression outer grp commons schemas params from ty -- ^ expression
  -> [Expression outer grp commons schemas params from ty]
  -> Condition outer grp commons schemas params from
expr `notIn` exprs = UnsafeExpression $ renderSQL expr <+> "NOT IN"
  <+> parenthesized (commaSeparated (renderSQL <$> exprs))

-- | A `CommonTableExpression` is an auxiliary statement in a `with` clause.
data CommonTableExpression statement
  (schemas :: SchemasType)
  (params :: [NullityType])
  (commons0 :: FromType)
  (commons1 :: FromType) where
  CommonTableExpression
    :: Aliased (statement commons schemas params) (cte ::: common)
    -> CommonTableExpression statement schemas params commons (cte ::: common ': commons)
instance
  ( KnownSymbol cte
  , commons1 ~ (cte ::: common ': commons)
  ) => Aliasable cte
    (statement commons schemas params common)
    (CommonTableExpression statement schemas params commons commons1) where
      statement `as` cte = CommonTableExpression (statement `as` cte)
instance
  ( KnownSymbol cte
  , commons1 ~ (cte ::: common ': commons)
  ) => Aliasable cte
    (statement commons schemas params common)
    (AlignedList (CommonTableExpression statement schemas params) commons commons1) where
      statement `as` cte = single (statement `as` cte)

instance (forall c s p r. RenderSQL (statement c s p r)) => RenderSQL
  (CommonTableExpression statement schemas params commons0 commons1) where
    renderSQL (CommonTableExpression (statement `As` cte)) =
      renderSQL cte <+> "AS" <+> parenthesized (renderSQL statement)

-- | `with` provides a way to write auxiliary statements for use in a larger query.
-- These statements, referred to as `CommonTableExpression`s, can be thought of as
-- defining temporary tables that exist just for one query.
class With statement where
  with
    :: AlignedList (CommonTableExpression statement schemas params) commons0 commons1
    -- ^ common table expressions
    -> statement commons1 schemas params row
    -- ^ larger query
    -> statement commons0 schemas params row
instance With (Query outer) where
  with Done query = query
  with ctes query = UnsafeQuery $
    "WITH" <+> renderSQL ctes <+> renderSQL query

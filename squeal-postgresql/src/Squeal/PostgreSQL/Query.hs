{-|
Module: Squeal.PostgreSQL.Query
Description: Squeal queries
Copyright: (c) Eitan Chatav, 2019
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
  , OverloadedLabels
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
    Query_
  , Query (..)
    -- ** Select
  , select
  , select_
  , selectDistinct
  , selectDistinct_
  , selectDistinctOn
  , selectDistinctOn_
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
  , withRecursive
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
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.TypeLits

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Sort
import Squeal.PostgreSQL.Expression.Window
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import Data.Int (Int32, Int64)
-- >>> import Data.Monoid (Sum (..))
-- >>> import Data.Text (Text)
-- >>> import qualified Generics.SOP as SOP

{- |
The process of retrieving or the command to retrieve data from
a database is called a `Query`.

The general `Query` type is parameterized by

* @outer :: FromType@ - outer scope for a correlated subquery,
* @commons :: FromType@ - scope for all `common` table expressions,
* @schemas :: SchemasType@ - scope for all `table`s and `view`s,
* @params :: [NullityType]@ - scope for all `Squeal.Expression.Parameter.parameter`s,
* @row :: RowType@ - return type of the `Query`.
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

{- |
The top level `Query_` type is parameterized by a @schemas@ `SchemasType`,
against which the query is type-checked, an input @parameters@ Haskell `Type`,
and an ouput row Haskell `Type`.

A top-level query can be run
using `Squeal.PostgreSQL.PQ.runQueryParams`, or if @parameters = ()@
using `Squeal.PostgreSQL.PQ.runQuery`.

Generally, @parameters@ will be a Haskell tuple or record whose entries
may be referenced using positional
`Squeal.PostgreSQL.Expression.Parameter.parameter`s and @row@ will be a
Haskell record, whose entries will be targeted using overloaded labels.

Let's see some examples of queries.

>>> :set -XDeriveAnyClass -XDerivingStrategies
>>> :{
data Row a b = Row { col1 :: a, col2 :: b }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
:}

simple query:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  query :: Query_ (Public Schema) () (Row Int32 Int32)
  query = select Star (from (table #tab))
in printSQL query
:}
SELECT * FROM "tab" AS "tab"

restricted query:

>>> :{
let
  query :: Query_ (Public Schema) () (Row Int32 Int32)
  query =
    select_ ((#col1 + #col2) `as` #col1 :* #col1 `as` #col2)
      ( from (table #tab)
        & where_ (#col1 .> #col2)
        & where_ (#col2 .> 0) )
in printSQL query
:}
SELECT ("col1" + "col2") AS "col1", "col1" AS "col2" FROM "tab" AS "tab" WHERE (("col1" > "col2") AND ("col2" > 0))

subquery:

>>> :{
let
  query :: Query_ (Public Schema) () (Row Int32 Int32)
  query = select Star (from (subquery (select Star (from (table #tab)) `as` #sub)))
in printSQL query
:}
SELECT * FROM (SELECT * FROM "tab" AS "tab") AS "sub"

limits and offsets:

>>> :{
let
  query :: Query_ (Public Schema) () (Row Int32 Int32)
  query = select Star (from (table #tab) & limit 100 & offset 2 & limit 50 & offset 2)
in printSQL query
:}
SELECT * FROM "tab" AS "tab" LIMIT 50 OFFSET 4

parameterized query:

>>> :{
let
  query :: Query_ (Public Schema) (Only Int32) (Row Int32 Int32)
  query = select Star (from (table #tab) & where_ (#col1 .> param @1))
in printSQL query
:}
SELECT * FROM "tab" AS "tab" WHERE ("col1" > ($1 :: int4))

aggregation query:

>>> :{
let
  query :: Query_ (Public Schema) () (Row Int64 Int32)
  query =
    select_ ((fromNull 0 (sum_ (All #col2))) `as` #col1 :* #col1 `as` #col2)
    ( from (table (#tab `as` #table1))
      & groupBy #col1
      & having (sum_ (Distinct #col2) .> 1) )
in printSQL query
:}
SELECT COALESCE(sum(ALL "col2"), 0) AS "col1", "col1" AS "col2" FROM "tab" AS "table1" GROUP BY "col1" HAVING (sum(DISTINCT "col2") > 1)

sorted query:

>>> :{
let
  query :: Query_ (Public Schema) () (Row Int32 Int32)
  query = select Star (from (table #tab) & orderBy [#col1 & Asc])
in printSQL query
:}
SELECT * FROM "tab" AS "tab" ORDER BY "col1" ASC

joins:

>>> :{
type OrdersColumns =
  '[ "id"         ::: 'NoDef :=> 'NotNull 'PGint4
   , "price"       ::: 'NoDef :=> 'NotNull 'PGfloat4
   , "customer_id" ::: 'NoDef :=> 'NotNull 'PGint4
   , "shipper_id"  ::: 'NoDef :=> 'NotNull 'PGint4  ]
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
data Order = Order
  { price :: Float
  , customerName :: Text
  , shipperName :: Text
  } deriving GHC.Generic
instance SOP.Generic Order
instance SOP.HasDatatypeInfo Order
:}

>>> :{
let
  query :: Query_ (Public OrdersSchema) () Order
  query = select_
    ( #o ! #price `as` #price :*
      #c ! #name `as` #customerName :*
      #s ! #name `as` #shipperName )
    ( from (table (#orders `as` #o)
      & innerJoin (table (#customers `as` #c))
        (#o ! #customer_id .== #c ! #id)
      & innerJoin (table (#shippers `as` #s))
        (#o ! #shipper_id .== #s ! #id)) )
in printSQL query
:}
SELECT "o"."price" AS "price", "c"."name" AS "customerName", "s"."name" AS "shipperName" FROM "orders" AS "o" INNER JOIN "customers" AS "c" ON ("o"."customer_id" = "c"."id") INNER JOIN "shippers" AS "s" ON ("o"."shipper_id" = "s"."id")

self-join:

>>> :{
let
  query :: Query_ (Public Schema) () (Row Int32 Int32)
  query = select
    (#t1 & DotStar)
    (from (table (#tab `as` #t1) & crossJoin (table (#tab `as` #t2))))
in printSQL query
:}
SELECT "t1".* FROM "tab" AS "t1" CROSS JOIN "tab" AS "t2"

value queries:

>>> :{
let
  query :: Query_ schemas () (Row String Bool)
  query = values
    ("true" `as` #col1 :* true `as` #col2)
    ["false" `as` #col1 :* false `as` #col2]
in printSQL query
:}
SELECT * FROM (VALUES (E'true', TRUE), (E'false', FALSE)) AS t ("col1", "col2")

set operations:

>>> :{
let
  query :: Query_ (Public Schema) () (Row Int32 Int32)
  query = select Star (from (table #tab)) `unionAll` select Star (from (table #tab))
in printSQL query
:}
(SELECT * FROM "tab" AS "tab") UNION ALL (SELECT * FROM "tab" AS "tab")

with queries:

>>> :{
let
  query :: Query_ (Public Schema) () (Row Int32 Int32)
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
  query :: Query_ (Public Schema) () (Row Int32 Int64)
  query = select
    (#col1 & Also (rank `as` #col2 `Over` (partitionBy #col1 & orderBy [#col2 & Asc])))
    (from (table #tab))
in printSQL query
:}
SELECT "col1" AS "col1", rank() OVER (PARTITION BY "col1" ORDER BY "col2" ASC) AS "col2" FROM "tab" AS "tab"

correlated subqueries

>>> :{
let
  query :: Query_ (Public Schema) () (Only Int32)
  query =
    select (#col1 `as` #fromOnly) (from (table (#tab `as` #t1))
    & where_ (exists (
      select Star (from (table (#tab `as` #t2))
      & where_ (#t2 ! #col2 .== #t1 ! #col1)))))
in printSQL query
:}
SELECT "col1" AS "fromOnly" FROM "tab" AS "t1" WHERE EXISTS (SELECT * FROM "tab" AS "t2" WHERE ("t2"."col2" = "t1"."col1"))

-}
type family Query_
  (schemas :: SchemasType)
  (parameters :: Type)
  (row :: Type) where
    Query_ schemas params row =
      Query '[] '[] schemas (TuplePG params) (RowPG row)

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

{- | The simplest kinds of `Selection` are `Star` and `DotStar` which
emits all columns that a `TableExpression` produces. A select `List`
is a list of `Expression`s. A `Selection` could be a list of
`WindowFunction`s `Over` `WindowDefinition`. `Additional` `Selection`s can
be selected with `Also`.
-}
data Selection outer commons grp schemas params from row where
  Star
    :: HasUnique tab from row
    => Selection outer commons 'Ungrouped schemas params from row
    -- ^ `HasUnique` table in the `FromClause`
  DotStar
    :: Has tab from row
    => Alias tab
       -- ^ `Has` table with `Alias`
    -> Selection outer commons 'Ungrouped schemas params from row
  List
    :: SListI row
    => NP (Aliased (Expression outer commons grp schemas params from)) row
       -- ^ `NP` list of `Aliased` `Expression`s
    -> Selection outer commons grp schemas params from row
  Over
    :: SListI row
    => NP (Aliased (WindowFunction outer commons grp schemas params from)) row
       -- ^ `NP` list of `Aliased` `WindowFunction`s
    -> WindowDefinition outer commons grp schemas params from
    -> Selection outer commons grp schemas params from row
  Also
    :: Selection outer commons grp schemas params from right
       -- ^ `Additional` `Selection`
    -> Selection outer commons grp schemas params from left
    -> Selection outer commons grp schemas params from (Join left right)
instance Additional (Selection outer commons grp schemas params from) where
  also = Also
instance (KnownSymbol col, row ~ '[col ::: ty])
  => Aliasable col
    (Expression outer commons grp schemas params from ty)
    (Selection outer commons grp schemas params from row) where
      expr `as` col = List (expr `as` col)
instance (Has tab (Join outer from) row0, Has col row0 ty, row1 ~ '[col ::: ty])
  => IsQualified tab col
    (Selection outer commons 'Ungrouped schemas params from row1) where
      tab ! col = tab ! col `as` col
instance
  ( Has tab (Join outer from) row0
  , Has col row0 ty
  , row1 ~ '[col ::: ty]
  , GroupedBy tab col bys )
  => IsQualified tab col
    (Selection outer commons ('Grouped bys) schemas params from row1) where
      tab ! col = tab ! col `as` col
instance (HasUnique tab (Join outer from) row0, Has col row0 ty, row1 ~ '[col ::: ty])
  => IsLabel col
    (Selection outer commons 'Ungrouped schemas params from row1) where
      fromLabel = fromLabel @col `as` Alias
instance
  ( HasUnique tab (Join outer from) row0
  , Has col row0 ty
  , row1 ~ '[col ::: ty]
  , GroupedBy tab col bys )
  => IsLabel col
    (Selection outer commons ('Grouped bys) schemas params from row1) where
      fromLabel = fromLabel @col `as` Alias

instance RenderSQL (Selection outer commons grp schemas params from row) where
  renderSQL = \case
    List list -> renderCommaSeparated (renderAliased renderSQL) list
    Star -> "*"
    DotStar tab -> renderSQL tab <> ".*"
    Also right left -> renderSQL left <> ", " <> renderSQL right
    Over winFns winDef ->
      let
        renderOver
          :: Aliased (WindowFunction outer commons grp schemas params from) field
          -> ByteString
        renderOver (winFn `As` col) = renderSQL winFn
          <+> "OVER" <+> parenthesized (renderSQL winDef)
          <+> "AS" <+> renderSQL col
      in
        renderCommaSeparated renderOver winFns

instance IsString
  (Selection outer commons grp schemas params from '["fromOnly" ::: 'NotNull 'PGtext]) where
    fromString str = fromString str `as` Alias

-- | the `TableExpression` in the `select` command constructs an intermediate
-- virtual table by possibly combining tables, views, eliminating rows,
-- grouping, etc. This table is finally passed on to processing by
-- the select list. The `Selection` determines which columns of
-- the intermediate table are actually output.
select
  :: (SListI row, row ~ (x ': xs))
  => Selection outer commons grp schemas params from row
  -- ^ selection
  -> TableExpression outer commons grp schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params row
select selection tabexpr = UnsafeQuery $
  "SELECT"
  <+> renderSQL selection
  <+> renderSQL tabexpr

-- | Like `select` but takes an `NP` list of `Expression`s instead
-- of a general `Selection`.
select_
  :: (SListI row, row ~ (x ': xs))
  => NP (Aliased (Expression outer commons grp schemas params from)) row
  -- ^ select list
  -> TableExpression outer commons grp schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params row
select_ = select . List

-- | After the select list has been processed, the result table can
-- be subject to the elimination of duplicate rows using `selectDistinct`.
selectDistinct
  :: (SListI columns, columns ~ (col ': cols))
  => Selection outer commons 'Ungrouped schemas params from columns
  -- ^ selection
  -> TableExpression outer commons 'Ungrouped schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params columns
selectDistinct selection tabexpr = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderSQL selection
  <+> renderSQL tabexpr

-- | Like `selectDistinct` but takes an `NP` list of `Expression`s instead
-- of a general `Selection`.
selectDistinct_
  :: (SListI columns, columns ~ (col ': cols))
  => NP (Aliased (Expression outer commons 'Ungrouped schemas params from)) columns
  -- ^ select list
  -> TableExpression outer commons 'Ungrouped schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params columns
selectDistinct_ = selectDistinct . List

{-|
`selectDistinctOn` keeps only the first row of each set of rows where
the given expressions evaluate to equal. The DISTINCT ON expressions are
interpreted using the same rules as for ORDER BY. ORDER BY is used to
ensure that the desired row appears first.
-}
selectDistinctOn
  :: (SListI columns, columns ~ (col ': cols))
  => [SortExpression outer commons 'Ungrouped schemas params from]
  -- ^ distinct on and return the first row in ordering
  -> Selection outer commons 'Ungrouped schemas params from columns
  -- ^ selection
  -> TableExpression outer commons 'Ungrouped schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params columns
selectDistinctOn distincts selection tab = UnsafeQuery $
  "SELECT DISTINCT ON"
  <+> parenthesized (commaSeparated (renderDistinctOn <$> distincts))
  <+> renderSQL selection
  <+> renderSQL (tab {orderByClause = distincts <> orderByClause tab})
  where
    renderDistinctOn = \case
      Asc expression -> renderSQL expression
      Desc expression -> renderSQL expression
      AscNullsFirst expression -> renderSQL expression
      DescNullsFirst expression -> renderSQL expression
      AscNullsLast expression -> renderSQL expression
      DescNullsLast expression -> renderSQL expression

-- | Like `selectDistinctOn` but takes an `NP` list of `Expression`s instead
-- of a general `Selection`.
selectDistinctOn_
  :: (SListI columns, columns ~ (col ': cols))
  => [SortExpression outer commons 'Ungrouped schemas params from]
  -- ^ distinct on and return the first row in ordering
  -> NP (Aliased (Expression outer commons 'Ungrouped schemas params from)) columns
  -- ^ selection
  -> TableExpression outer commons 'Ungrouped schemas params from
  -- ^ intermediate virtual table
  -> Query outer commons schemas params columns
selectDistinctOn_ distincts = selectDistinctOn distincts . List

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
  => NP (Aliased (Expression outer commons 'Ungrouped schemas params '[] )) cols
  -> [NP (Aliased (Expression outer commons 'Ungrouped schemas params '[] )) cols]
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
      :: Aliased (Expression outer commons 'Ungrouped schemas params '[] ) ty -> ByteString
    renderAliasPart (_ `As` name) = renderSQL name
    renderValuePart (value `As` _) = renderSQL value

-- | `values_` computes a row value or set of row values
-- specified by value expressions.
values_
  :: SListI cols
  => NP (Aliased (Expression outer commons 'Ungrouped schemas params '[] )) cols
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
  (commons :: FromType)
  (grp :: Grouping)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (from :: FromType)
    = TableExpression
    { fromClause :: FromClause outer commons schemas params from
    -- ^ A table reference that can be a table name, or a derived table such
    -- as a subquery, a @JOIN@ construct, or complex combinations of these.
    , whereClause :: [Condition outer commons 'Ungrouped schemas params from]
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
    , havingClause :: HavingClause outer commons grp schemas params from
    -- ^ If a table has been grouped using `groupBy`, but only certain groups
    -- are of interest, the `havingClause` can be used, much like a
    -- `whereClause`, to eliminate groups from the result. Expressions in the
    -- `havingClause` can refer both to grouped expressions and to ungrouped
    -- expressions (which necessarily involve an aggregate function).
    , orderByClause :: [SortExpression outer commons grp schemas params from]
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
instance RenderSQL (TableExpression outer commons grp schemas params from) where
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
  -> TableExpression outer commons 'Ungrouped schemas params from
from tab = TableExpression tab [] NoGroups NoHaving [] [] []

-- | A `where_` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `whereClause`.
where_
  :: Condition outer commons 'Ungrouped schemas params from -- ^ filtering condition
  -> TableExpression outer commons grp schemas params from
  -> TableExpression outer commons grp schemas params from
where_ wh rels = rels {whereClause = wh : whereClause rels}

-- | A `groupBy` is a transformation of `TableExpression`s which switches
-- its `Grouping` from `Ungrouped` to `Grouped`. Use @groupBy Nil@ to perform
-- a "grand total" aggregation query.
groupBy
  :: SListI bys
  => NP (By from) bys -- ^ grouped columns
  -> TableExpression outer commons 'Ungrouped schemas params from
  -> TableExpression outer commons ('Grouped bys) schemas params from
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
  :: Condition outer commons ('Grouped bys) schemas params from -- ^ having condition
  -> TableExpression outer commons ('Grouped bys) schemas params from
  -> TableExpression outer commons ('Grouped bys) schemas params from
having hv rels = rels
  { havingClause = case havingClause rels of Having hvs -> Having (hv:hvs) }

instance OrderBy TableExpression where
  orderBy srts rels = rels {orderByClause = orderByClause rels ++ srts}

-- | A `limit` is an endomorphism of `TableExpression`s which adds to the
-- `limitClause`.
limit
  :: Word64 -- ^ limit parameter
  -> TableExpression outer commons grp schemas params from
  -> TableExpression outer commons grp schemas params from
limit lim rels = rels {limitClause = lim : limitClause rels}

-- | An `offset` is an endomorphism of `TableExpression`s which adds to the
-- `offsetClause`.
offset
  :: Word64 -- ^ offset parameter
  -> TableExpression outer commons grp schemas params from
  -> TableExpression outer commons grp schemas params from
offset off rels = rels {offsetClause = off : offsetClause rels}

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
subquery = UnsafeFromClause . renderAliased (parenthesized . renderSQL)

-- | `view` derives a table from a `View`.
view
  :: (Has sch schemas schema, Has vw schema ('View view))
  => Aliased (QualifiedAlias sch) (alias ::: vw)
  -> FromClause outer commons schemas params '[alias ::: view]
view (vw `As` alias) = UnsafeFromClause $
  renderSQL vw <+> "AS" <+> renderSQL alias

-- | `common` derives a table from a common table expression.
common
  :: Has cte commons common
  => Aliased Alias (alias ::: cte)
  -> FromClause outer commons schemas params '[alias ::: common]
common (cte `As` alias) = UnsafeFromClause $
  renderSQL cte <+> "AS" <+> renderSQL alias

instance Additional (FromClause outer commons schemas params) where
  also right left = UnsafeFromClause $
    renderSQL left <> ", " <> renderSQL right

{- |
@left & crossJoin right@. For every possible combination of rows from
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
  -> Condition outer commons 'Ungrouped schemas params (Join left right)
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
  -> Condition outer commons 'Ungrouped schemas params (Join left right)
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
  -> Condition outer commons 'Ungrouped schemas params (Join left right)
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
  -> Condition outer commons 'Ungrouped schemas params (Join left right)
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
data HavingClause outer commons grp schemas params from where
  NoHaving :: HavingClause outer commons 'Ungrouped schemas params from
  Having
    :: [Condition outer commons ('Grouped bys) schemas params from]
    -> HavingClause outer commons ('Grouped bys) schemas params from
deriving instance Show (HavingClause outer commons grp schemas params from)
deriving instance Eq (HavingClause outer commons grp schemas params from)
deriving instance Ord (HavingClause outer commons grp schemas params from)

-- | Render a `HavingClause`.
instance RenderSQL (HavingClause outer commons grp schemas params from) where
  renderSQL = \case
    NoHaving -> ""
    Having [] -> ""
    Having conditions ->
      " HAVING" <+> commaSeparated (renderSQL <$> conditions)

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
    (Path (CommonTableExpression statement schemas params) commons commons1) where
      statement `as` cte = csingleton (statement `as` cte)

instance (forall c s p r. RenderSQL (statement c s p r)) => RenderSQL
  (CommonTableExpression statement schemas params commons0 commons1) where
    renderSQL (CommonTableExpression (statement `As` cte)) =
      renderSQL cte <+> "AS" <+> parenthesized (renderSQL statement)

-- | `with` provides a way to write auxiliary statements for use in a larger query.
-- These statements, referred to as `CommonTableExpression`s, can be thought of as
-- defining temporary tables that exist just for one query.
class With statement where
  with
    :: Path (CommonTableExpression statement schemas params) commons0 commons1
    -- ^ common table expressions
    -> statement commons1 schemas params row
    -- ^ larger query
    -> statement commons0 schemas params row
instance With (Query outer) where
  with Done query = query
  with ctes query = UnsafeQuery $
    "WITH" <+> commaSeparated (ctoList renderSQL ctes) <+> renderSQL query

{- |
>>> import Data.Monoid (Sum (..))
>>> import Data.Int (Int64)
>>> :{
  let
    query :: Query_ schema () (Sum Int64)
    query = withRecursive
      ( values_ ((1 & astype int) `as` #n)
        `unionAll`
        select_ ((#n + 1) `as` #n)
          (from (common #t) & where_ (#n .< 100)) `as` #t )
      ( select_ (fromNull 0 (sum_ (All #n)) `as` #getSum) (from (common #t) & groupBy Nil))
  in printSQL query
:}
WITH RECURSIVE "t" AS ((SELECT * FROM (VALUES ((1 :: int))) AS t ("n")) UNION ALL (SELECT ("n" + 1) AS "n" FROM "t" AS "t" WHERE ("n" < 100))) SELECT COALESCE(sum(ALL "n"), 0) AS "getSum" FROM "t" AS "t"
-}
withRecursive
  :: Aliased (Query outer (recursive ': commons) schemas params) recursive
  -> Query outer (recursive ': commons) schemas params row
  -> Query outer commons schemas params row
withRecursive (recursive `As` cte) query = UnsafeQuery $
  "WITH RECURSIVE" <+> renderSQL cte
    <+> "AS" <+> parenthesized (renderSQL recursive)
    <+> renderSQL query

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
  , DerivingStrategies
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
  ( -- * Query
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
    -- * Table Expression
  , TableExpression (..)
  , from
  , where_
  , groupBy
  , having
  , limit
  , offset
    -- * From Clause
  , FromClause (..)
  , table
  , subquery
  , view
  , common
  , cross
  , crossJoin
  , inner
  , innerJoin
  , leftOuter
  , leftOuterJoin
  , rightOuter
  , rightOuterJoin
  , fullOuter
  , fullOuterJoin
  , JoinItem (..)
  , SetOf
  , SetOfN
    -- * Grouping
  , By (..)
  , GroupByClause (..)
  , HavingClause (..)
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Quiver.Functor
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.TypeLits

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

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

* @lat :: FromType@ - lateral scope for a correlated subquery,
* @with :: FromType@ - scope for all `common` table expressions,
* @db :: SchemasType@ - scope for all `table`s and `view`s,
* @params :: [NullType]@ - scope for all `Squeal.Expression.Parameter.parameter`s,
* @row :: RowType@ - return type of the `Query`.
-}
newtype Query
  (lat :: FromType)
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (row :: RowType)
    = UnsafeQuery { renderQuery :: ByteString }
    deriving stock (GHC.Generic,Show,Eq,Ord)
    deriving newtype (NFData)
instance RenderSQL (Query lat with db params row) where renderSQL = renderQuery

{- |
The top level `Query_` type is parameterized by a @db@ `SchemasType`,
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

>>> :{
let
  query :: Query_ (Public OrdersSchema) () Order
  query = select_
    ( #o ! #price `as` #price :*
      #c ! #name `as` #customerName :*
      #s ! #name `as` #shipperName )
    ( from (table (#orders `as` #o)
      & (inner.JoinLateral) (select Star (from (table #customers)) `as` #c)
        (#o ! #customer_id .== #c ! #id)
      & (inner.JoinLateral) (select Star (from (table #shippers)) `as` #s)
        (#o ! #shipper_id .== #s ! #id)) )
in printSQL query
:}
SELECT "o"."price" AS "price", "c"."name" AS "customerName", "s"."name" AS "shipperName" FROM "orders" AS "o" INNER JOIN LATERAL (SELECT * FROM "customers" AS "customers") AS "c" ON ("o"."customer_id" = "c"."id") INNER JOIN LATERAL (SELECT * FROM "shippers" AS "shippers") AS "s" ON ("o"."shipper_id" = "s"."id")

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
  query :: Query_ db () (Row String Bool)
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
  (db :: SchemasType)
  (parameters :: Type)
  (row :: Type) where
    Query_ db params row =
      Query '[] '[] db (TuplePG params) (RowPG row)

-- | The results of two queries can be combined using the set operation
-- `union`. Duplicate rows are eliminated.
union
  :: Query lat with db params columns
  -> Query lat with db params columns
  -> Query lat with db params columns
q1 `union` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "UNION"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `unionAll`, the disjoint union. Duplicate rows are retained.
unionAll
  :: Query lat with db params columns
  -> Query lat with db params columns
  -> Query lat with db params columns
q1 `unionAll` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "UNION" <+> "ALL"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `intersect`, the intersection. Duplicate rows are eliminated.
intersect
  :: Query lat with db params columns
  -> Query lat with db params columns
  -> Query lat with db params columns
q1 `intersect` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "INTERSECT"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `intersectAll`, the intersection. Duplicate rows are retained.
intersectAll
  :: Query lat with db params columns
  -> Query lat with db params columns
  -> Query lat with db params columns
q1 `intersectAll` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "INTERSECT" <+> "ALL"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `except`, the set difference. Duplicate rows are eliminated.
except
  :: Query lat with db params columns
  -> Query lat with db params columns
  -> Query lat with db params columns
q1 `except` q2 = UnsafeQuery $
  parenthesized (renderSQL q1)
  <+> "EXCEPT"
  <+> parenthesized (renderSQL q2)

-- | The results of two queries can be combined using the set operation
-- `exceptAll`, the set difference. Duplicate rows are retained.
exceptAll
  :: Query lat with db params columns
  -> Query lat with db params columns
  -> Query lat with db params columns
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
data Selection lat with grp db params from row where
  Star
    :: HasUnique tab from row
    => Selection lat with 'Ungrouped db params from row
    -- ^ `HasUnique` table in the `FromClause`
  DotStar
    :: Has tab from row
    => Alias tab
       -- ^ `Has` table with `Alias`
    -> Selection lat with 'Ungrouped db params from row
  List
    :: SListI row
    => NP (Aliased (Expression lat with grp db params from)) row
       -- ^ `NP` list of `Aliased` `Expression`s
    -> Selection lat with grp db params from row
  Over
    :: SListI row
    => NP (Aliased (WindowFunction lat with grp db params from)) row
       -- ^ `NP` list of `Aliased` `WindowFunction`s
    -> WindowDefinition lat with grp db params from
    -> Selection lat with grp db params from row
  Also
    :: Selection lat with grp db params from right
       -- ^ `Additional` `Selection`
    -> Selection lat with grp db params from left
    -> Selection lat with grp db params from (Join left right)
instance Additional (Selection lat with grp db params from) where
  also = Also
instance (KnownSymbol col, row ~ '[col ::: ty])
  => Aliasable col
    (Expression lat with grp db params from ty)
    (Selection lat with grp db params from row) where
      expr `as` col = List (expr `as` col)
instance (Has tab (Join lat from) row0, Has col row0 ty, row1 ~ '[col ::: ty])
  => IsQualified tab col
    (Selection lat with 'Ungrouped db params from row1) where
      tab ! col = tab ! col `as` col
instance
  ( Has tab (Join lat from) row0
  , Has col row0 ty
  , row1 ~ '[col ::: ty]
  , GroupedBy tab col bys )
  => IsQualified tab col
    (Selection lat with ('Grouped bys) db params from row1) where
      tab ! col = tab ! col `as` col
instance (HasUnique tab (Join lat from) row0, Has col row0 ty, row1 ~ '[col ::: ty])
  => IsLabel col
    (Selection lat with 'Ungrouped db params from row1) where
      fromLabel = fromLabel @col `as` Alias
instance
  ( HasUnique tab (Join lat from) row0
  , Has col row0 ty
  , row1 ~ '[col ::: ty]
  , GroupedBy tab col bys )
  => IsLabel col
    (Selection lat with ('Grouped bys) db params from row1) where
      fromLabel = fromLabel @col `as` Alias

instance RenderSQL (Selection lat with grp db params from row) where
  renderSQL = \case
    List list -> renderCommaSeparated (renderAliased renderSQL) list
    Star -> "*"
    DotStar tab -> renderSQL tab <> ".*"
    Also right left -> renderSQL left <> ", " <> renderSQL right
    Over winFns winDef ->
      let
        renderOver
          :: Aliased (WindowFunction lat with grp db params from) field
          -> ByteString
        renderOver (winFn `As` col) = renderSQL winFn
          <+> "OVER" <+> parenthesized (renderSQL winDef)
          <+> "AS" <+> renderSQL col
      in
        renderCommaSeparated renderOver winFns

instance IsString
  (Selection lat with grp db params from '["fromOnly" ::: 'NotNull 'PGtext]) where
    fromString str = fromString str `as` Alias

-- | the `TableExpression` in the `select` command constructs an intermediate
-- virtual table by possibly combining tables, views, eliminating rows,
-- grouping, etc. This table is finally passed on to processing by
-- the select list. The `Selection` determines which columns of
-- the intermediate table are actually output.
select
  :: (SListI row, row ~ (x ': xs))
  => Selection lat with grp db params from row
  -- ^ selection
  -> TableExpression lat with grp db params from
  -- ^ intermediate virtual table
  -> Query lat with db params row
select selection tabexpr = UnsafeQuery $
  "SELECT"
  <+> renderSQL selection
  <+> renderSQL tabexpr

-- | Like `select` but takes an `NP` list of `Expression`s instead
-- of a general `Selection`.
select_
  :: (SListI row, row ~ (x ': xs))
  => NP (Aliased (Expression lat with grp db params from)) row
  -- ^ select list
  -> TableExpression lat with grp db params from
  -- ^ intermediate virtual table
  -> Query lat with db params row
select_ = select . List

-- | After the select list has been processed, the result table can
-- be subject to the elimination of duplicate rows using `selectDistinct`.
selectDistinct
  :: (SListI columns, columns ~ (col ': cols))
  => Selection lat with grp db params from columns
  -- ^ selection
  -> TableExpression lat with grp db params from
  -- ^ intermediate virtual table
  -> Query lat with db params columns
selectDistinct selection tabexpr = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderSQL selection
  <+> renderSQL tabexpr

-- | Like `selectDistinct` but takes an `NP` list of `Expression`s instead
-- of a general `Selection`.
selectDistinct_
  :: (SListI columns, columns ~ (col ': cols))
  => NP (Aliased (Expression lat with grp db params from)) columns
  -- ^ select list
  -> TableExpression lat with grp db params from
  -- ^ intermediate virtual table
  -> Query lat with db params columns
selectDistinct_ = selectDistinct . List

{-|
`selectDistinctOn` keeps only the first row of each set of rows where
the given expressions evaluate to equal. The DISTINCT ON expressions are
interpreted using the same rules as for ORDER BY. ORDER BY is used to
ensure that the desired row appears first.

The DISTINCT ON expression(s) must match the leftmost ORDER BY expression(s).
The ORDER BY clause will normally contain additional expression(s) that
determine the desired precedence of rows within each DISTINCT ON group.

In order to guarantee they match and reduce redundancy, this function
will prepend the The DISTINCT ON expressions to the ORDER BY clause.
-}
selectDistinctOn
  :: (SListI columns, columns ~ (col ': cols))
  => [SortExpression lat with grp db params from]
  -- ^ DISTINCT ON expression(s) and prepended to ORDER BY clause
  -> Selection lat with grp db params from columns
  -- ^ selection
  -> TableExpression lat with grp db params from
  -- ^ intermediate virtual table
  -> Query lat with db params columns
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
  => [SortExpression lat with grp db params from]
  -- ^ distinct on and return the first row in ordering
  -> NP (Aliased (Expression lat with grp db params from)) columns
  -- ^ selection
  -> TableExpression lat with grp db params from
  -- ^ intermediate virtual table
  -> Query lat with db params columns
selectDistinctOn_ distincts = selectDistinctOn distincts . List

-- | `values` computes a row value or set of row values
-- specified by value expressions. It is most commonly used
-- to generate a “constant table” within a larger command,
-- but it can be used on its own.
--
-- >>> type Row = '["a" ::: 'NotNull 'PGint4, "b" ::: 'NotNull 'PGtext]
-- >>> let query = values (1 `as` #a :* "one" `as` #b) [] :: Query lat with db '[] Row
-- >>> printSQL query
-- SELECT * FROM (VALUES (1, E'one')) AS t ("a", "b")
values
  :: SListI cols
  => NP (Aliased (Expression lat with 'Ungrouped db params '[] )) cols
  -> [NP (Aliased (Expression lat with 'Ungrouped db params '[] )) cols]
  -- ^ When more than one row is specified, all the rows must
  -- must have the same number of elements
  -> Query lat with db params cols
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
      :: Aliased (Expression lat with 'Ungrouped db params '[] ) ty -> ByteString
    renderAliasPart (_ `As` name) = renderSQL name
    renderValuePart (value `As` _) = renderSQL value

-- | `values_` computes a row value or set of row values
-- specified by value expressions.
values_
  :: SListI cols
  => NP (Aliased (Expression lat with 'Ungrouped db params '[] )) cols
  -- ^ one row of values
  -> Query lat with db params cols
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
  (lat :: FromType)
  (with :: FromType)
  (grp :: Grouping)
  (db :: SchemasType)
  (params :: [NullType])
  (from :: FromType)
    = TableExpression
    { fromClause :: FromClause lat with db params from
    -- ^ A table reference that can be a table name, or a derived table such
    -- as a subquery, a @JOIN@ construct, or complex combinations of these.
    , whereClause :: [Condition lat with 'Ungrouped db params from]
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
    , havingClause :: HavingClause lat with grp db params from
    -- ^ If a table has been grouped using `groupBy`, but only certain groups
    -- are of interest, the `havingClause` can be used, much like a
    -- `whereClause`, to eliminate groups from the result. Expressions in the
    -- `havingClause` can refer both to grouped expressions and to ungrouped
    -- expressions (which necessarily involve an aggregate function).
    , orderByClause :: [SortExpression lat with grp db params from]
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
instance RenderSQL (TableExpression lat with grp db params from) where
  renderSQL
    (TableExpression frm' whs' grps' hvs' srts' lims' offs') = mconcat
      [ "FROM ", renderSQL frm'
      , renderWheres whs'
      , renderSQL grps'
      , renderSQL hvs'
      , renderSQL srts'
      , renderLimits lims'
      , renderOffsets offs' ]
      where
        renderWheres = \case
          [] -> ""
          wh:[] -> " WHERE" <+> renderSQL wh
          wh:whs -> " WHERE" <+> renderSQL (foldr (.&&) wh whs)
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
  :: FromClause lat with db params from -- ^ table reference
  -> TableExpression lat with 'Ungrouped db params from
from tab = TableExpression tab [] noGroups NoHaving [] [] []

-- | A `where_` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `whereClause`.
where_
  :: Condition lat with 'Ungrouped db params from -- ^ filtering condition
  -> TableExpression lat with grp db params from
  -> TableExpression lat with grp db params from
where_ wh rels = rels {whereClause = wh : whereClause rels}

-- | A `groupBy` is a transformation of `TableExpression`s which switches
-- its `Grouping` from `Ungrouped` to `Grouped`. Use @groupBy Nil@ to perform
-- a "grand total" aggregation query.
groupBy
  :: SListI bys
  => NP (By from) bys -- ^ grouped columns
  -> TableExpression lat with 'Ungrouped db params from
  -> TableExpression lat with ('Grouped bys) db params from
groupBy bys rels = TableExpression
  { fromClause = fromClause rels
  , whereClause = whereClause rels
  , groupByClause = group bys
  , havingClause = Having []
  , orderByClause = []
  , limitClause = limitClause rels
  , offsetClause = offsetClause rels
  }

-- | A `having` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `havingClause`.
having
  :: Condition lat with ('Grouped bys) db params from -- ^ having condition
  -> TableExpression lat with ('Grouped bys) db params from
  -> TableExpression lat with ('Grouped bys) db params from
having hv rels = rels
  { havingClause = case havingClause rels of Having hvs -> Having (hv:hvs) }

instance OrderBy TableExpression where
  orderBy srts rels = rels {orderByClause = orderByClause rels ++ srts}

-- | A `limit` is an endomorphism of `TableExpression`s which adds to the
-- `limitClause`.
limit
  :: Word64 -- ^ limit parameter
  -> TableExpression lat with grp db params from
  -> TableExpression lat with grp db params from
limit lim rels = rels {limitClause = lim : limitClause rels}

-- | An `offset` is an endomorphism of `TableExpression`s which adds to the
-- `offsetClause`.
offset
  :: Word64 -- ^ offset parameter
  -> TableExpression lat with grp db params from
  -> TableExpression lat with grp db params from
offset off rels = rels {offsetClause = off : offsetClause rels}

{-----------------------------------------
FROM clauses
-----------------------------------------}

{- |
A `FromClause` can be a table name, or a derived table such
as a subquery, a @JOIN@ construct, or complex combinations of these.
-}
newtype FromClause
  (lat :: FromType)
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (from :: FromType)
  = UnsafeFromClause { renderFromClause :: ByteString }
  deriving stock (GHC.Generic,Show,Eq,Ord)
  deriving newtype (NFData)
instance RenderSQL (FromClause lat with db params from) where
  renderSQL = renderFromClause

-- | A real `table` is a table from the database.
table
  :: (Has sch db schema, Has tab schema ('Table table))
  => Aliased (QualifiedAlias sch) (alias ::: tab) -- ^ (renamable) table alias
  -> FromClause lat with db params '[alias ::: TableToRow table]
table (tab `As` alias) = UnsafeFromClause $
  renderSQL tab <+> "AS" <+> renderSQL alias

-- | `subquery` derives a table from a `Query`.
subquery
  :: Aliased (Query lat with db params) query
  -- ^ aliased `Query`
  -> FromClause lat with db params '[query]
subquery = UnsafeFromClause . renderAliased (parenthesized . renderSQL)

-- | `view` derives a table from a `View`.
view
  :: (Has sch db schema, Has vw schema ('View view))
  => Aliased (QualifiedAlias sch) (alias ::: vw) -- ^ (renamable) view alias
  -> FromClause lat with db params '[alias ::: view]
view (vw `As` alias) = UnsafeFromClause $
  renderSQL vw <+> "AS" <+> renderSQL alias

-- | `common` derives a table from a common table expression.
common
  :: Has cte with common
  => Aliased Alias (alias ::: cte) -- ^ (renamable) common table expression alias
  -> FromClause lat with db params '[alias ::: common]
common (cte `As` alias) = UnsafeFromClause $
  renderSQL cte <+> "AS" <+> renderSQL alias

instance Additional (FromClause lat with db params) where
  also right left = UnsafeFromClause $
    renderSQL left <> ", " <> renderSQL right

type SetOf db ty set
  =  forall lat with params
  .  Expression lat with 'Ungrouped db params '[] ty
     -- ^ input
  -> FromClause lat with db params '[set]
     -- ^ output

type SetOfN db tys set
  =  forall lat with params
  .  NP (Expression lat with 'Ungrouped db params '[]) tys
     -- ^ input
  -> FromClause lat with db params '[set]
     -- ^ output

data JoinItem
  (lat :: FromType)
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (left :: FromType)
  (right :: FromType) where
    Join
      :: FromClause lat with db params right
      -> JoinItem lat with db params left right
    JoinLateral
      :: Aliased (Query (Join lat left) with db params) query
      -> JoinItem lat with db params left '[query]
    JoinFunction
      :: SetOf db ty set
      -> Expression lat with 'Ungrouped db params left ty
      -> JoinItem lat with db params left '[set]
    JoinFunctionN
      :: SListI tys
      => SetOfN db tys set
      -> NP (Expression lat with 'Ungrouped db params left) tys
      -> JoinItem lat with db params left '[set]
instance RenderSQL (JoinItem lat with db params left right) where
  renderSQL = \case
    Join tab -> "JOIN" <+> renderSQL tab
    JoinLateral qry -> "JOIN LATERAL" <+>
      renderAliased (parenthesized . renderSQL) qry
    JoinFunction fun x -> "JOIN" <+>
      renderSQL (fun (UnsafeExpression (renderSQL x)))
    JoinFunctionN fun xs -> "JOIN" <+>
      renderSQL (fun (SOP.hmap (UnsafeExpression . renderSQL) xs))

{- |
@left & cross (Join right)@. For every possible combination of rows from
@left@ and @right@ (i.e., a Cartesian product), the joined table will contain
a row consisting of all columns in @left@ followed by all columns in @right@.
If the tables have @n@ and @m@ rows respectively, the joined table will
have @n * m@ rows.
-}
cross
  :: JoinItem lat with db params left right
  -> FromClause lat with db params left
  -> FromClause lat with db params (Join left right)
cross item tab = UnsafeFromClause $
  renderSQL tab <+> "CROSS" <+> renderSQL item

{- |
@left & crossJoin right@. For every possible combination of rows from
@left@ and @right@ (i.e., a Cartesian product), the joined table will contain
a row consisting of all columns in @left@ followed by all columns in @right@.
If the tables have @n@ and @m@ rows respectively, the joined table will
have @n * m@ rows.
-}
crossJoin
  :: FromClause lat with db params right
  -> FromClause lat with db params left
  -> FromClause lat with db params (Join left right)
crossJoin = cross . Join

{- | @left & inner (Join right) on@. The joined table is filtered by
the @on@ condition.
-}
inner
  :: JoinItem lat with db params left right
  -> Condition lat with 'Ungrouped db params (Join left right)
  -> FromClause lat with db params left
  -> FromClause lat with db params (Join left right)
inner item on tab = UnsafeFromClause $
  renderSQL tab <+> "INNER" <+> renderSQL item <+> "ON" <+> renderSQL on

{- | @left & innerJoin right on@. The joined table is filtered by
the @on@ condition.
-}
innerJoin
  :: FromClause lat with db params right
  -> Condition lat with 'Ungrouped db params (Join left right)
  -> FromClause lat with db params left
  -> FromClause lat with db params (Join left right)
innerJoin = inner . Join

{- | @left & leftOuter (Join right) on@. First, an inner join is performed.
Then, for each row in @left@ that does not satisfy the @on@ condition with
any row in @right@, a joined row is added with null values in columns of @right@.
Thus, the joined table always has at least one row for each row in @left@.
-}
leftOuter
  :: JoinItem lat with db params left right
  -> Condition lat with 'Ungrouped db params (Join left right)
  -> FromClause lat with db params left
  -> FromClause lat with db params (Join left (NullifyFrom right))
leftOuter item on tab = UnsafeFromClause $
  renderSQL tab <+> "LEFT OUTER" <+> renderSQL item <+> "ON" <+> renderSQL on

{- | @left & leftOuterJoin right on@. First, an inner join is performed.
Then, for each row in @left@ that does not satisfy the @on@ condition with
any row in @right@, a joined row is added with null values in columns of @right@.
Thus, the joined table always has at least one row for each row in @left@.
-}
leftOuterJoin
  :: FromClause lat with db params right
  -> Condition lat with 'Ungrouped db params (Join left right)
  -> FromClause lat with db params left
  -> FromClause lat with db params (Join left (NullifyFrom right))
leftOuterJoin = leftOuter . Join

{- | @left & rightOuter (Join right) on@. First, an inner join is performed.
Then, for each row in @right@ that does not satisfy the @on@ condition with
any row in @left@, a joined row is added with null values in columns of @left@.
This is the converse of a left join: the result table will always
have a row for each row in @right@.
-}
rightOuter
  :: JoinItem lat with db params left right
  -> Condition lat with 'Ungrouped db params (Join left right)
  -> FromClause lat with db params left
  -> FromClause lat with db params (Join (NullifyFrom left) right)
rightOuter item on tab = UnsafeFromClause $
  renderSQL tab <+> "RIGHT OUTER" <+> renderSQL item <+> "ON" <+> renderSQL on

{- | @left & rightOuterJoin right on@. First, an inner join is performed.
Then, for each row in @right@ that does not satisfy the @on@ condition with
any row in @left@, a joined row is added with null values in columns of @left@.
This is the converse of a left join: the result table will always
have a row for each row in @right@.
-}
rightOuterJoin
  :: FromClause lat with db params right
  -> Condition lat with 'Ungrouped db params (Join left right)
  -> FromClause lat with db params left
  -> FromClause lat with db params (Join (NullifyFrom left) right)
rightOuterJoin = rightOuter . Join

{- | @left & fullOuter (Join right) on@. First, an inner join is performed.
Then, for each row in @left@ that does not satisfy the @on@ condition with
any row in @right@, a joined row is added with null values in columns of @right@.
Also, for each row of @right@ that does not satisfy the join condition
with any row in @left@, a joined row with null values in the columns of @left@
is added.
-}
fullOuter
  :: JoinItem lat with db params left right
  -> Condition lat with 'Ungrouped db params (Join left right)
  -> FromClause lat with db params left
  -> FromClause lat with db params (NullifyFrom (Join left right))
fullOuter item on tab = UnsafeFromClause $
  renderSQL tab <+> "FULL OUTER" <+> renderSQL item <+> "ON" <+> renderSQL on

{- | @left & fullOuterJoin right on@. First, an inner join is performed.
Then, for each row in @left@ that does not satisfy the @on@ condition with
any row in @right@, a joined row is added with null values in columns of @right@.
Also, for each row of @right@ that does not satisfy the join condition
with any row in @left@, a joined row with null values in the columns of @left@
is added.
-}
fullOuterJoin
  :: FromClause lat with db params right
  -> Condition lat with 'Ungrouped db params (Join left right)
  -> FromClause lat with db params left
  -> FromClause lat with db params (NullifyFrom (Join left right))
fullOuterJoin = fullOuter . Join

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
newtype GroupByClause grp from = UnsafeGroupByClause
  { renderGroupByClause :: ByteString }
  deriving stock (GHC.Generic,Show,Eq,Ord)
  deriving newtype (NFData)
instance RenderSQL (GroupByClause grp from) where
  renderSQL = renderGroupByClause
noGroups :: GroupByClause 'Ungrouped from
noGroups = UnsafeGroupByClause ""
group
  :: SListI bys
  => NP (By from) bys
  -> GroupByClause ('Grouped bys) from
group bys = UnsafeGroupByClause $ case bys of
  Nil -> ""
  _ -> " GROUP BY" <+> renderCommaSeparated renderSQL bys

-- | A `HavingClause` is used to eliminate groups that are not of interest.
-- An `Ungrouped` `TableExpression` may only use `NoHaving` while a `Grouped`
-- `TableExpression` must use `Having` whose conditions are combined with
-- `.&&`.
data HavingClause lat with grp db params from where
  NoHaving :: HavingClause lat with 'Ungrouped db params from
  Having
    :: [Condition lat with ('Grouped bys) db params from]
    -> HavingClause lat with ('Grouped bys) db params from
deriving instance Show (HavingClause lat with grp db params from)
deriving instance Eq (HavingClause lat with grp db params from)
deriving instance Ord (HavingClause lat with grp db params from)

-- | Render a `HavingClause`.
instance RenderSQL (HavingClause lat with grp db params from) where
  renderSQL = \case
    NoHaving -> ""
    Having [] -> ""
    Having conditions ->
      " HAVING" <+> commaSeparated (renderSQL <$> conditions)

-- | A `CommonTableExpression` is an auxiliary statement in a `with` clause.
data CommonTableExpression statement
  (db :: SchemasType)
  (params :: [NullType])
  (with0 :: FromType)
  (with1 :: FromType) where
  CommonTableExpression
    :: Aliased (statement with db params) (cte ::: common)
    -- ^ aliased statement
    -> CommonTableExpression statement db params with (cte ::: common ': with)
instance
  ( KnownSymbol cte
  , with1 ~ (cte ::: common ': with)
  ) => Aliasable cte
    (statement with db params common)
    (CommonTableExpression statement db params with with1) where
      statement `as` cte = CommonTableExpression (statement `as` cte)
instance
  ( KnownSymbol cte
  , with1 ~ (cte ::: common ': with)
  ) => Aliasable cte
    (statement with db params common)
    (Path (CommonTableExpression statement db params) with with1) where
      statement `as` cte = qsingle (statement `as` cte)

instance (forall c s p r. RenderSQL (statement c s p r)) => RenderSQL
  (CommonTableExpression statement db params with0 with1) where
    renderSQL (CommonTableExpression (statement `As` cte)) =
      renderSQL cte <+> "AS" <+> parenthesized (renderSQL statement)

-- | `with` provides a way to write auxiliary statements for use in a larger query.
-- These statements, referred to as `CommonTableExpression`s, can be thought of as
-- defining temporary tables that exist just for one query.
class With statement where
  with
    :: Path (CommonTableExpression statement db params) with0 with1
    -- ^ common table expressions
    -> statement with1 db params row
    -- ^ larger query
    -> statement with0 db params row
instance With (Query lat) where
  with Done query = query
  with ctes query = UnsafeQuery $
    "WITH" <+> commaSeparated (qtoList renderSQL ctes) <+> renderSQL query

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
  :: Aliased (Query lat (recursive ': with) db params) recursive
  -- ^ recursive query
  -> Query lat (recursive ': with) db params row
  -- ^ larger query
  -> Query lat with db params row
withRecursive (recursive `As` cte) query = UnsafeQuery $
  "WITH RECURSIVE" <+> renderSQL cte
    <+> "AS" <+> parenthesized (renderSQL recursive)
    <+> renderSQL query

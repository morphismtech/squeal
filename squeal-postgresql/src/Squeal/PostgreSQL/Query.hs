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
  , StandaloneDeriving
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , RankNTypes
  , UndecidableInstances
  #-}

module Squeal.PostgreSQL.Query
  ( -- * Queries
    Query (UnsafeQuery, renderQuery)
    -- ** Select
  , select
  , selectDistinct
  , selectStar
  , selectDistinctStar
  , selectDotStar
  , selectDistinctDotStar
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
  , With (with)
  , CommonTableExpression (..)
  , renderCommonTableExpression
  , renderCommonTableExpressions
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
  , renderTableExpression
  , from
  , where_
  , groupBy
  , having
  , orderBy
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
  , By (By1, By2)
  , renderBy
  , GroupByClause (NoGroups, Group)
  , renderGroupByClause
  , HavingClause (NoHaving, Having)
  , renderHavingClause
    -- * Sorting
  , SortExpression (..)
  , renderSortExpression
    -- * Subquery Expressions
  , in_
  , rowIn
  , eqAll
  , rowEqAll
  , eqAny
  , rowEqAny
  , neqAll
  , rowNeqAll
  , neqAny
  , rowNeqAny
  , allLt
  , rowLtAll
  , ltAny
  , rowLtAny
  , lteAll
  , rowLteAll
  , lteAny
  , rowLteAny
  , gtAll
  , rowGtAll
  , gtAny
  , rowGtAny
  , gteAll
  , rowGteAll
  , gteAny
  , rowGteAny
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
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
  query :: Query (DBof (Public Schema)) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = selectStar (from (table #tab))
in printSQL query
:}
SELECT * FROM "tab" AS "tab"

restricted query:

>>> :{
let
  query :: Query (DBof (Public Schema)) '[] '["sum" ::: 'NotNull 'PGint4, "col1" ::: 'NotNull 'PGint4]
  query =
    select ((#col1 + #col2) `as` #sum :* #col1)
      ( from (table #tab)
        & where_ (#col1 .> #col2)
        & where_ (#col2 .> 0) )
in printSQL query
:}
SELECT ("col1" + "col2") AS "sum", "col1" AS "col1" FROM "tab" AS "tab" WHERE (("col1" > "col2") AND ("col2" > 0))

subquery:

>>> :{
let
  query :: Query (DBof (Public Schema)) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = selectStar (from (subquery (selectStar (from (table #tab)) `as` #sub)))
in printSQL query
:}
SELECT * FROM (SELECT * FROM "tab" AS "tab") AS "sub"

limits and offsets:

>>> :{
let
  query :: Query (DBof (Public Schema)) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = selectStar (from (table #tab) & limit 100 & offset 2 & limit 50 & offset 2)
in printSQL query
:}
SELECT * FROM "tab" AS "tab" LIMIT 50 OFFSET 4

parameterized query:

>>> :{
let
  query :: Query (DBof (Public Schema)) '[ 'NotNull 'PGint4] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = selectStar (from (table #tab) & where_ (#col1 .> param @1))
in printSQL query
:}
SELECT * FROM "tab" AS "tab" WHERE ("col1" > ($1 :: int4))

aggregation query:

>>> :{
let
  query :: Query (DBof (Public Schema)) '[] '["sum" ::: 'NotNull 'PGint4, "col1" ::: 'NotNull 'PGint4 ]
  query =
    select (sum_ #col2 `as` #sum :* #col1)
    ( from (table (#tab `as` #table1))
      & groupBy #col1
      & having (#col1 + sum_ #col2 .> 1) )
in printSQL query
:}
SELECT sum("col2") AS "sum", "col1" AS "col1" FROM "tab" AS "table1" GROUP BY "col1" HAVING (("col1" + sum("col2")) > 1)

sorted query:

>>> :{
let
  query :: Query (DBof (Public Schema)) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = selectStar (from (table #tab) & orderBy [#col1 & Asc])
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
  query :: Query (DBof (Public OrdersSchema))
    '[]
    '[ "order_price" ::: 'NotNull 'PGfloat4
     , "customer_name" ::: 'NotNull 'PGtext
     , "shipper_name" ::: 'NotNull 'PGtext
     ]
  query = select
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
  query :: Query (DBof (Public Schema)) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = selectDotStar #t1 (from (table (#tab `as` #t1) & crossJoin (table (#tab `as` #t2))))
in printSQL query
:}
SELECT "t1".* FROM "tab" AS "t1" CROSS JOIN "tab" AS "t2"

value queries:

>>> :{
let
  query :: Query db '[] '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
  query = values (1 `as` #foo :* true `as` #bar) [2 `as` #foo :* false `as` #bar]
in printSQL query
:}
SELECT * FROM (VALUES (1, TRUE), (2, FALSE)) AS t ("foo", "bar")

set operations:

>>> :{
let
  query :: Query (DBof (Public Schema)) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = selectStar (from (table #tab)) `unionAll` selectStar (from (table #tab))
in printSQL query
:}
(SELECT * FROM "tab" AS "tab") UNION ALL (SELECT * FROM "tab" AS "tab")

with queries:

>>> :{
let
  query :: Query (DBof (Public Schema)) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  query = with (
    selectStar (from (table #tab)) `as` #cte1 :>>
    selectStar (from (common #cte1)) `as` #cte2
    ) (selectStar (from (common #cte2)))
in printSQL query
:}
WITH "cte1" AS (SELECT * FROM "tab" AS "tab"), "cte2" AS (SELECT * FROM "cte1" AS "cte1") SELECT * FROM "cte2" AS "cte2"
-}
newtype Query
  (db :: DBType)
  (params :: [NullityType])
  (columns :: RowType)
    = UnsafeQuery { renderQuery :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Query db params columns) where renderSQL = renderQuery

-- | The results of two queries can be combined using the set operation
-- `union`. Duplicate rows are eliminated.
union
  :: Query db params columns
  -> Query db params columns
  -> Query db params columns
q1 `union` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "UNION"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `unionAll`, the disjoint union. Duplicate rows are retained.
unionAll
  :: Query db params columns
  -> Query db params columns
  -> Query db params columns
q1 `unionAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "UNION" <+> "ALL"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `intersect`, the intersection. Duplicate rows are eliminated.
intersect
  :: Query db params columns
  -> Query db params columns
  -> Query db params columns
q1 `intersect` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "INTERSECT"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `intersectAll`, the intersection. Duplicate rows are retained.
intersectAll
  :: Query db params columns
  -> Query db params columns
  -> Query db params columns
q1 `intersectAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "INTERSECT" <+> "ALL"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `except`, the set difference. Duplicate rows are eliminated.
except
  :: Query db params columns
  -> Query db params columns
  -> Query db params columns
q1 `except` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "EXCEPT"
  <+> parenthesized (renderQuery q2)

-- | The results of two queries can be combined using the set operation
-- `exceptAll`, the set difference. Duplicate rows are retained.
exceptAll
  :: Query db params columns
  -> Query db params columns
  -> Query db params columns
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
  :: (SListI columns, columns ~ (col ': cols))
  => NP (Aliased (Expression db params grp from)) columns
  -- ^ select list
  -> TableExpression db params grp from
  -- ^ intermediate virtual table
  -> Query db params columns
select list rels = UnsafeQuery $
  "SELECT"
  <+> renderCommaSeparated (renderAliasedAs renderExpression) list
  <+> renderTableExpression rels

-- | After the select list has been processed, the result table can
-- be subject to the elimination of duplicate rows using `selectDistinct`.
selectDistinct
  :: (SListI columns, columns ~ (col ': cols))
  => NP (Aliased (Expression db params 'Ungrouped from)) columns
  -- ^ select list
  -> TableExpression db params 'Ungrouped from
  -- ^ intermediate virtual table
  -> Query db params columns
selectDistinct list rels = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderCommaSeparated (renderAliasedAs renderExpression) list
  <+> renderTableExpression rels

-- | The simplest kind of query is `selectStar` which emits all columns
-- that the table expression produces.
selectStar
  :: HasUnique table from columns
  => TableExpression db params 'Ungrouped from
  -- ^ intermediate virtual table
  -> Query db params columns
selectStar rels = UnsafeQuery $ "SELECT" <+> "*" <+> renderTableExpression rels

-- | A `selectDistinctStar` emits all columns that the table expression
-- produces and eliminates duplicate rows.
selectDistinctStar
  :: HasUnique table from columns
  => TableExpression db params 'Ungrouped from
  -- ^ intermediate virtual table
  -> Query db params columns
selectDistinctStar rels = UnsafeQuery $
  "SELECT DISTINCT" <+> "*" <+> renderTableExpression rels

-- | When working with multiple tables, it can also be useful to ask
-- for all the columns of a particular table, using `selectDotStar`.
selectDotStar
  :: Has table from columns
  => Alias table
  -- ^ particular virtual subtable
  -> TableExpression db params 'Ungrouped from
  -- ^ intermediate virtual table
  -> Query db params columns
selectDotStar rel tab = UnsafeQuery $
  "SELECT" <+> renderAlias rel <> ".*" <+> renderTableExpression tab

-- | A `selectDistinctDotStar` asks for all the columns of a particular table,
-- and eliminates duplicate rows.
selectDistinctDotStar
  :: Has table from columns
  => Alias table
  -- ^ particular virtual table
  -> TableExpression db params 'Ungrouped from
  -- ^ intermediate virtual table
  -> Query db params columns
selectDistinctDotStar rel tab = UnsafeQuery $
  "SELECT DISTINCT" <+> renderAlias rel <> ".*"
  <+> renderTableExpression tab

-- | `values` computes a row value or set of row values
-- specified by value expressions. It is most commonly used
-- to generate a “constant table” within a larger command,
-- but it can be used on its own.
--
-- >>> type Row = '["a" ::: 'NotNull 'PGint4, "b" ::: 'NotNull 'PGtext]
-- >>> let query = values (1 `as` #a :* "one" `as` #b) [] :: Query db '[] Row
-- >>> printSQL query
-- SELECT * FROM (VALUES (1, E'one')) AS t ("a", "b")
values
  :: SListI cols
  => NP (Aliased (Expression db params 'Ungrouped '[] )) cols
  -> [NP (Aliased (Expression db params 'Ungrouped '[] )) cols]
  -- ^ When more than one row is specified, all the rows must
  -- must have the same number of elements
  -> Query db params cols
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
      :: Aliased (Expression db params 'Ungrouped '[] ) ty -> ByteString
    renderAliasPart (_ `As` name) = renderAlias name
    renderValuePart (value `As` _) = renderExpression value

-- | `values_` computes a row value or set of row values
-- specified by value expressions.
values_
  :: SListI cols
  => NP (Aliased (Expression db params 'Ungrouped '[] )) cols
  -- ^ one row of values
  -> Query db params cols
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
  (db :: DBType)
  (params :: [NullityType])
  (grp :: Grouping)
  (from :: FromType)
    = TableExpression
    { fromClause :: FromClause db params from
    -- ^ A table reference that can be a table name, or a derived table such
    -- as a subquery, a @JOIN@ construct, or complex combinations of these.
    , whereClause :: [Condition db params 'Ungrouped from]
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
    , havingClause :: HavingClause db params grp from
    -- ^ If a table has been grouped using `groupBy`, but only certain groups
    -- are of interest, the `havingClause` can be used, much like a
    -- `whereClause`, to eliminate groups from the result. Expressions in the
    -- `havingClause` can refer both to grouped expressions and to ungrouped
    -- expressions (which necessarily involve an aggregate function).
    , orderByClause :: [SortExpression db params grp from]
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
  :: TableExpression db params grp from
  -> ByteString
renderTableExpression
  (TableExpression frm' whs' grps' hvs' srts' lims' offs') = mconcat
    [ "FROM ", renderFromClause frm'
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
  :: FromClause db params from -- ^ table reference
  -> TableExpression db params 'Ungrouped from
from rels = TableExpression rels [] NoGroups NoHaving [] [] []

-- | A `where_` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `whereClause`.
where_
  :: Condition db params 'Ungrouped from -- ^ filtering condition
  -> TableExpression db params grp from
  -> TableExpression db params grp from
where_ wh rels = rels {whereClause = wh : whereClause rels}

-- | A `groupBy` is a transformation of `TableExpression`s which switches
-- its `Grouping` from `Ungrouped` to `Grouped`. Use @group Nil@ to perform
-- a "grand total" aggregation query.
groupBy
  :: SListI bys
  => NP (By from) bys -- ^ grouped columns
  -> TableExpression db params 'Ungrouped from
  -> TableExpression db params ('Grouped bys) from
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
  :: Condition db params ('Grouped bys) from -- ^ having condition
  -> TableExpression db params ('Grouped bys) from
  -> TableExpression db params ('Grouped bys) from
having hv rels = rels
  { havingClause = case havingClause rels of Having hvs -> Having (hv:hvs) }

-- | An `orderBy` is an endomorphism of `TableExpression`s which appends an
-- ordering to the right of the `orderByClause`.
orderBy
  :: [SortExpression db params grp from] -- ^ sort expressions
  -> TableExpression db params grp from
  -> TableExpression db params grp from
orderBy srts rels = rels {orderByClause = orderByClause rels ++ srts}

-- | A `limit` is an endomorphism of `TableExpression`s which adds to the
-- `limitClause`.
limit
  :: Word64 -- ^ limit parameter
  -> TableExpression db params grp from
  -> TableExpression db params grp from
limit lim rels = rels {limitClause = lim : limitClause rels}

-- | An `offset` is an endomorphism of `TableExpression`s which adds to the
-- `offsetClause`.
offset
  :: Word64 -- ^ offset parameter
  -> TableExpression db params grp from
  -> TableExpression db params grp from
offset off rels = rels {offsetClause = off : offsetClause rels}

{-----------------------------------------
JSON stuff
-----------------------------------------}

unsafeSetOfFunction
  :: ByteString
  -> Expression db params 'Ungrouped '[]  ty
  -> Query db params row
unsafeSetOfFunction fun expr = UnsafeQuery $
  "SELECT * FROM " <> fun <> "(" <> renderExpression expr <> ")"

-- | Expands the outermost JSON object into a set of key/value pairs.
jsonEach
  :: Expression db params 'Ungrouped '[]  (nullity 'PGjson) -- ^ json object
  -> Query db params
      '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGjson]
jsonEach = unsafeSetOfFunction "json_each"

-- | Expands the outermost binary JSON object into a set of key/value pairs.
jsonbEach
  :: Expression db params 'Ungrouped '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> Query db params
      '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGjsonb]
jsonbEach = unsafeSetOfFunction "jsonb_each"

-- | Expands the outermost JSON object into a set of key/value pairs.
jsonEachAsText
  :: Expression db params 'Ungrouped '[]  (nullity 'PGjson) -- ^ json object
  -> Query db params
      '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGtext]
jsonEachAsText = unsafeSetOfFunction "json_each_text"

-- | Expands the outermost binary JSON object into a set of key/value pairs.
jsonbEachAsText
  :: Expression db params 'Ungrouped '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> Query db params
    '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGtext]
jsonbEachAsText = unsafeSetOfFunction "jsonb_each_text"

-- | Returns set of keys in the outermost JSON object.
jsonObjectKeys
  :: Expression db params 'Ungrouped '[]  (nullity 'PGjson) -- ^ json object
  -> Query db params '["json_object_keys" ::: 'NotNull 'PGtext]
jsonObjectKeys = unsafeSetOfFunction "json_object_keys"

-- | Returns set of keys in the outermost JSON object.
jsonbObjectKeys
  :: Expression db params 'Ungrouped '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> Query db params '["jsonb_object_keys" ::: 'NotNull 'PGtext]
jsonbObjectKeys = unsafeSetOfFunction "jsonb_object_keys"

unsafePopulateFunction
  :: ByteString
  -> TypeExpression schemas (nullity ('PGcomposite row))
  -> Expression (commons :=> schemas) params 'Ungrouped '[]  ty
  -> Query (commons :=> schemas) params row
unsafePopulateFunction fun ty expr = UnsafeQuery $
  "SELECT * FROM " <> fun <> "("
    <> "null::" <> renderTypeExpression ty <> ", "
    <> renderExpression expr <> ")"

-- | Expands the JSON expression to a row whose columns match the record
-- type defined by the given table.
jsonPopulateRecord
  :: TypeExpression schemas (nullity ('PGcomposite row)) -- ^ row type
  -> Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity 'PGjson) -- ^ json object
  -> Query (commons :=> schemas) params row
jsonPopulateRecord = unsafePopulateFunction "json_populate_record"

-- | Expands the binary JSON expression to a row whose columns match the record
-- type defined by the given table.
jsonbPopulateRecord
  :: TypeExpression schemas (nullity ('PGcomposite row)) -- ^ row type
  -> Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> Query (commons :=> schemas) params row
jsonbPopulateRecord = unsafePopulateFunction "jsonb_populate_record"

-- | Expands the outermost array of objects in the given JSON expression to a
-- set of rows whose columns match the record type defined by the given table.
jsonPopulateRecordSet
  :: TypeExpression schemas (nullity ('PGcomposite row)) -- ^ row type
  -> Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity 'PGjson) -- ^ json array
  -> Query (commons :=> schemas) params row
jsonPopulateRecordSet = unsafePopulateFunction "json_populate_record_set"

-- | Expands the outermost array of objects in the given binary JSON expression
-- to a set of rows whose columns match the record type defined by the given
-- table.
jsonbPopulateRecordSet
  :: TypeExpression schemas (nullity ('PGcomposite row)) -- ^ row type
  -> Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity 'PGjsonb) -- ^ jsonb array
  -> Query (commons :=> schemas) params row
jsonbPopulateRecordSet = unsafePopulateFunction "jsonb_populate_record_set"

unsafeRecordFunction
  :: (SListI record, json `In` PGJsonType)
  => ByteString
  -> Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity json)
  -> NP (Aliased (TypeExpression schemas)) record
  -> Query (commons :=> schemas) params record
unsafeRecordFunction fun expr types = UnsafeQuery $
  "SELECT * FROM " <> fun <> "("
    <> renderExpression expr <> ")"
    <+> "AS" <+> "x" <> parenthesized (renderCommaSeparated renderTy types)
    where
      renderTy :: Aliased (TypeExpression db) ty -> ByteString
      renderTy (ty `As` alias) =
        renderAlias alias <+> renderTypeExpression ty

-- | Builds an arbitrary record from a JSON object.
jsonToRecord
  :: SListI record
  => Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity 'PGjson) -- ^ json object
  -> NP (Aliased (TypeExpression schemas)) record -- ^ record types
  -> Query (commons :=> schemas) params record
jsonToRecord = unsafeRecordFunction "json_to_record"

-- | Builds an arbitrary record from a binary JSON object.
jsonbToRecord
  :: SListI record
  => Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity 'PGjsonb) -- ^ jsonb object
  -> NP (Aliased (TypeExpression schemas)) record -- ^ record types
  -> Query (commons :=> schemas) params record
jsonbToRecord = unsafeRecordFunction "jsonb_to_record"

-- | Builds an arbitrary set of records from a JSON array of objects.
jsonToRecordSet
  :: SListI record
  => Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity 'PGjson) -- ^ json array
  -> NP (Aliased (TypeExpression schemas)) record -- ^ record types
  -> Query (commons :=> schemas) params record
jsonToRecordSet = unsafeRecordFunction "json_to_record_set"

-- | Builds an arbitrary set of records from a binary JSON array of objects.
jsonbToRecordSet
  :: SListI record
  => Expression (commons :=> schemas) params 'Ungrouped '[]  (nullity 'PGjsonb) -- ^ jsonb array
  -> NP (Aliased (TypeExpression schemas)) record -- ^ record types
  -> Query (commons :=> schemas) params record
jsonbToRecordSet = unsafeRecordFunction "jsonb_to_record_set"

{-----------------------------------------
FROM clauses
-----------------------------------------}

{- |
A `FromClause` can be a table name, or a derived table such
as a subquery, a @JOIN@ construct, or complex combinations of these.
-}
newtype FromClause db params from
  = UnsafeFromClause { renderFromClause :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | A real `table` is a table from the database.
table
  :: (Has sch schemas schema, Has tab schema ('Table table))
  => Aliased (QualifiedAlias sch) (alias ::: tab)
  -> FromClause (commons :=> schemas) params '[alias ::: TableToRow table]
table (tab `As` alias) = UnsafeFromClause $
  renderQualifiedAlias tab <+> "AS" <+> renderAlias alias

-- | `subquery` derives a table from a `Query`.
subquery
  :: Aliased (Query db params) query
  -> FromClause db params '[query]
subquery = UnsafeFromClause . renderAliasedAs (parenthesized . renderQuery)

-- | `view` derives a table from a `View`.
view
  :: (Has sch schemas schema, Has vw schema ('View view))
  => Aliased (QualifiedAlias sch) (alias ::: vw)
  -> FromClause (commons :=> schemas) params '[alias ::: view]
view (vw `As` alias) = UnsafeFromClause $
  renderQualifiedAlias vw <+> "AS" <+> renderAlias alias

common
  :: Has cte commons common
  => Aliased Alias (alias ::: cte)
  -> FromClause (commons :=> schemas) params '[alias ::: common]
common (cte `As` alias) = UnsafeFromClause $
  renderAlias cte <+> "AS" <+> renderAlias alias

{- | @left & crossJoin right@. For every possible combination of rows from
    @left@ and @right@ (i.e., a Cartesian product), the joined table will contain
    a row consisting of all columns in @left@ followed by all columns in @right@.
    If the tables have @n@ and @m@ rows respectively, the joined table will
    have @n * m@ rows.
-}
crossJoin
  :: FromClause db params right
  -- ^ right
  -> FromClause db params left
  -- ^ left
  -> FromClause db params (Join left right)
crossJoin right left = UnsafeFromClause $
  renderFromClause left <+> "CROSS JOIN" <+> renderFromClause right

{- | @left & innerJoin right on@. The joined table is filtered by
the @on@ condition.
-}
innerJoin
  :: FromClause db params right
  -- ^ right
  -> Condition db params 'Ungrouped (Join left right)
  -- ^ @on@ condition
  -> FromClause db params left
  -- ^ left
  -> FromClause db params (Join left right)
innerJoin right on left = UnsafeFromClause $
  renderFromClause left <+> "INNER JOIN" <+> renderFromClause right
  <+> "ON" <+> renderExpression on

{- | @left & leftOuterJoin right on@. First, an inner join is performed.
    Then, for each row in @left@ that does not satisfy the @on@ condition with
    any row in @right@, a joined row is added with null values in columns of @right@.
    Thus, the joined table always has at least one row for each row in @left@.
-}
leftOuterJoin
  :: FromClause db params right
  -- ^ right
  -> Condition db params 'Ungrouped (Join left right)
  -- ^ @on@ condition
  -> FromClause db params left
  -- ^ left
  -> FromClause db params (Join left (NullifyFrom right))
leftOuterJoin right on left = UnsafeFromClause $
  renderFromClause left <+> "LEFT OUTER JOIN" <+> renderFromClause right
  <+> "ON" <+> renderExpression on

{- | @left & rightOuterJoin right on@. First, an inner join is performed.
    Then, for each row in @right@ that does not satisfy the @on@ condition with
    any row in @left@, a joined row is added with null values in columns of @left@.
    This is the converse of a left join: the result table will always
    have a row for each row in @right@.
-}
rightOuterJoin
  :: FromClause db params right
  -- ^ right
  -> Condition db params 'Ungrouped (Join left right)
  -- ^ @on@ condition
  -> FromClause db params left
  -- ^ left
  -> FromClause db params (Join (NullifyFrom left) right)
rightOuterJoin right on left = UnsafeFromClause $
  renderFromClause left <+> "RIGHT OUTER JOIN" <+> renderFromClause right
  <+> "ON" <+> renderExpression on

{- | @left & fullOuterJoin right on@. First, an inner join is performed.
    Then, for each row in @left@ that does not satisfy the @on@ condition with
    any row in @right@, a joined row is added with null values in columns of @right@.
    Also, for each row of @right@ that does not satisfy the join condition
    with any row in @left@, a joined row with null values in the columns of @left@
    is added.
-}
fullOuterJoin
  :: FromClause db params right
  -- ^ right
  -> Condition db params 'Ungrouped (Join left right)
  -- ^ @on@ condition
  -> FromClause db params left
  -- ^ left
  -> FromClause db params
      (Join (NullifyFrom left) (NullifyFrom right))
fullOuterJoin right on left = UnsafeFromClause $
  renderFromClause left <+> "FULL OUTER JOIN" <+> renderFromClause right
  <+> "ON" <+> renderExpression on

{-----------------------------------------
Grouping
-----------------------------------------}

-- | `By`s are used in `group` to reference a list of columns which are then
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

instance (HasUnique rel rels cols, Has col cols ty, by ~ '(rel, col))
  => IsLabel col (By rels by) where fromLabel = By1 fromLabel
instance (HasUnique rel rels cols, Has col cols ty, bys ~ '[ '(rel, col)])
  => IsLabel col (NP (By rels) bys) where fromLabel = By1 fromLabel :* Nil
instance (Has rel rels cols, Has col cols ty, by ~ '(rel, col))
  => IsQualified rel col (By rels by) where (!) = By2
instance (Has rel rels cols, Has col cols ty, bys ~ '[ '(rel, col)])
  => IsQualified rel col (NP (By rels) bys) where
    rel ! col = By2 rel col :* Nil

-- | Renders a `By`.
renderBy :: By from by -> ByteString
renderBy = \case
  By1 column -> renderAlias column
  By2 rel column -> renderAlias rel <> "." <> renderAlias column

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
renderGroupByClause :: GroupByClause grp from -> ByteString
renderGroupByClause = \case
  NoGroups -> ""
  Group Nil -> ""
  Group bys -> " GROUP BY" <+> renderCommaSeparated renderBy bys

-- | A `HavingClause` is used to eliminate groups that are not of interest.
-- An `Ungrouped` `TableExpression` may only use `NoHaving` while a `Grouped`
-- `TableExpression` must use `Having` whose conditions are combined with
-- `.&&`.
data HavingClause db params grp from where
  NoHaving :: HavingClause db params 'Ungrouped from
  Having
    :: [Condition db params ('Grouped bys) from]
    -> HavingClause db params ('Grouped bys) from
deriving instance Show (HavingClause db params grp from)
deriving instance Eq (HavingClause db params grp from)
deriving instance Ord (HavingClause db params grp from)

-- | Render a `HavingClause`.
renderHavingClause :: HavingClause db params grp from -> ByteString
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
data SortExpression db params grp from where
    Asc
      :: Expression db params grp from ('NotNull ty)
      -> SortExpression db params grp from
    Desc
      :: Expression db params grp from ('NotNull ty)
      -> SortExpression db params grp from
    AscNullsFirst
      :: Expression db params grp from  ('Null ty)
      -> SortExpression db params grp from
    AscNullsLast
      :: Expression db params grp from  ('Null ty)
      -> SortExpression db params grp from
    DescNullsFirst
      :: Expression db params grp from  ('Null ty)
      -> SortExpression db params grp from
    DescNullsLast
      :: Expression db params grp from  ('Null ty)
      -> SortExpression db params grp from
deriving instance Show (SortExpression db params grp from)

-- | Render a `SortExpression`.
renderSortExpression :: SortExpression db params grp from -> ByteString
renderSortExpression = \case
  Asc expression -> renderExpression expression <+> "ASC"
  Desc expression -> renderExpression expression <+> "DESC"
  AscNullsFirst expression -> renderExpression expression
    <+> "ASC NULLS FIRST"
  DescNullsFirst expression -> renderExpression expression
    <+> "DESC NULLS FIRST"
  AscNullsLast expression -> renderExpression expression <+> "ASC NULLS LAST"
  DescNullsLast expression -> renderExpression expression <+> "DESC NULLS LAST"

unsafeSubqueryExpression
  :: ByteString
  -> Expression db params grp from ty
  -> Query db params '[alias ::: ty]
  -> Expression db params grp from (nullity 'PGbool)
unsafeSubqueryExpression op x q = UnsafeExpression $
  renderExpression x <+> op <+> parenthesized (renderQuery q)

unsafeRowSubqueryExpression
  :: SListI row
  => ByteString
  -> NP (Aliased (Expression db params grp from)) row
  -> Query db params row
  -> Expression db params grp from (nullity 'PGbool)
unsafeRowSubqueryExpression op xs q = UnsafeExpression $
  renderExpression (row xs) <+> op <+> parenthesized (renderQuery q)

-- | The right-hand side is a sub`Query`, which must return exactly one column.
-- The left-hand expression is evaluated and compared to each row of the
-- sub`Query` result. The result of `in_` is `true` if any equal subquery row is found.
-- The result is `false` if no equal row is found
-- (including the case where the subquery returns no rows).
--
-- >>> printSQL $ true `in_` values_ (true `as` #foo)
-- TRUE IN (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
in_
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
in_ = unsafeSubqueryExpression "IN"

{- | The left-hand side of this form of `rowIn` is a row constructor.
The right-hand side is a sub`Query`,
which must return exactly as many columns as
there are expressions in the left-hand row.
The left-hand expressions are evaluated and compared row-wise to each row
of the subquery result. The result of `rowIn`
is `true` if any equal subquery row is found.
The result is `false` if no equal row is found
(including the case where the subquery returns no rows).

>>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
>>> printSQL $ myRow `rowIn` values_ myRow
ROW(1, FALSE) IN (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
-}
rowIn
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowIn = unsafeRowSubqueryExpression "IN"

-- | >>> printSQL $ true `eqAll` values_ (true `as` #foo)
-- TRUE = ALL (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
eqAll
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
eqAll = unsafeSubqueryExpression "= ALL"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowEqAll` values_ myRow
-- ROW(1, FALSE) = ALL (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowEqAll
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowEqAll = unsafeRowSubqueryExpression "= ALL"

-- | >>> printSQL $ true `eqAny` values_ (true `as` #foo)
-- TRUE = ANY (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
eqAny
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
eqAny = unsafeSubqueryExpression "= ANY"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowEqAny` values_ myRow
-- ROW(1, FALSE) = ANY (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowEqAny
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowEqAny = unsafeRowSubqueryExpression "= ANY"

-- | >>> printSQL $ true `neqAll` values_ (true `as` #foo)
-- TRUE <> ALL (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
neqAll
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
neqAll = unsafeSubqueryExpression "<> ALL"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowNeqAll` values_ myRow
-- ROW(1, FALSE) <> ALL (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowNeqAll
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowNeqAll = unsafeRowSubqueryExpression "<> ALL"

-- | >>> printSQL $ true `neqAny` values_ (true `as` #foo)
-- TRUE <> ANY (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
neqAny
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
neqAny = unsafeSubqueryExpression "<> ANY"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowNeqAny` values_ myRow
-- ROW(1, FALSE) <> ANY (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowNeqAny
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowNeqAny = unsafeRowSubqueryExpression "<> ANY"

-- | >>> printSQL $ true `allLt` values_ (true `as` #foo)
-- TRUE ALL < (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
allLt
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
allLt = unsafeSubqueryExpression "ALL <"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowLtAll` values_ myRow
-- ROW(1, FALSE) ALL < (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowLtAll
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowLtAll = unsafeRowSubqueryExpression "ALL <"

-- | >>> printSQL $ true `ltAny` values_ (true `as` #foo)
-- TRUE ANY < (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
ltAny
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
ltAny = unsafeSubqueryExpression "ANY <"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowLtAll` values_ myRow
-- ROW(1, FALSE) ALL < (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowLtAny
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowLtAny = unsafeRowSubqueryExpression "ANY <"

-- | >>> printSQL $ true `lteAll` values_ (true `as` #foo)
-- TRUE <= ALL (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
lteAll
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
lteAll = unsafeSubqueryExpression "<= ALL"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowLteAll` values_ myRow
-- ROW(1, FALSE) <= ALL (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowLteAll
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowLteAll = unsafeRowSubqueryExpression "<= ALL"

-- | >>> printSQL $ true `lteAny` values_ (true `as` #foo)
-- TRUE <= ANY (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
lteAny
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
lteAny = unsafeSubqueryExpression "<= ANY"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowLteAny` values_ myRow
-- ROW(1, FALSE) <= ANY (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowLteAny
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowLteAny = unsafeRowSubqueryExpression "<= ANY"

-- | >>> printSQL $ true `gtAll` values_ (true `as` #foo)
-- TRUE > ALL (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
gtAll
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
gtAll = unsafeSubqueryExpression "> ALL"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowGtAll` values_ myRow
-- ROW(1, FALSE) > ALL (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowGtAll
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowGtAll = unsafeRowSubqueryExpression "> ALL"

-- | >>> printSQL $ true `gtAny` values_ (true `as` #foo)
-- TRUE > ANY (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
gtAny
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
gtAny = unsafeSubqueryExpression "> ANY"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowGtAny` values_ myRow
-- ROW(1, FALSE) > ANY (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowGtAny
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowGtAny = unsafeRowSubqueryExpression "> ANY"

-- | >>> printSQL $ true `gteAll` values_ (true `as` #foo)
-- TRUE >= ALL (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
gteAll
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
gteAll = unsafeSubqueryExpression ">= ALL"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowGteAll` values_ myRow
-- ROW(1, FALSE) >= ALL (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowGteAll
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowGteAll = unsafeRowSubqueryExpression ">= ALL"

-- | >>> printSQL $ true `gteAny` values_ (true `as` #foo)
-- TRUE >= ANY (SELECT * FROM (VALUES (TRUE)) AS t ("foo"))
gteAny
  :: Expression db params grp from ty -- ^ expression
  -> Query db params '[alias ::: ty] -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
gteAny = unsafeSubqueryExpression ">= ANY"

-- | >>> let myRow = 1 `as` #foo :* false `as` #bar :: NP (Aliased (Expression db params grp from)) '["foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGbool]
-- >>> printSQL $ myRow `rowGteAny` values_ myRow
-- ROW(1, FALSE) >= ANY (SELECT * FROM (VALUES (1, FALSE)) AS t ("foo", "bar"))
rowGteAny
  :: SListI row
  => NP (Aliased (Expression db params grp from)) row -- ^ row constructor
  -> Query db params row -- ^ subquery
  -> Expression db params grp from (nullity 'PGbool)
rowGteAny = unsafeRowSubqueryExpression ">= ANY"

-- | A `CommonTableExpression` is an auxiliary statement in a `with` clause.
data CommonTableExpression statement
  (params :: [NullityType])
  (db0 :: DBType)
  (db1 :: DBType) where
  CommonTableExpression
    :: Aliased (statement (commons :=> schemas) params) (cte ::: common)
    -> CommonTableExpression statement params
      (commons :=> schemas) ((cte ::: common ': commons) :=> schemas)
instance
  ( KnownSymbol cte
  , db0 ~ (commons :=> schemas)
  , db1 ~ ((cte ::: common ': commons) :=> schemas)
  ) => Aliasable cte
    (statement db0 params common)
    (CommonTableExpression statement params db0 db1) where
      statement `as` cte = CommonTableExpression (statement `as` cte)
instance
  ( KnownSymbol cte
  , db0 ~ (commons :=> schemas)
  , db1 ~ ((cte ::: common ': commons) :=> schemas)
  ) => Aliasable cte
    (statement db0 params common)
    (AlignedList (CommonTableExpression statement params) db0 db1) where
      statement `as` cte = single (statement `as` cte)

-- | render a `CommonTableExpression`.
renderCommonTableExpression
  :: (forall db ps row. statement db ps row -> ByteString) -- ^ render statement
  -> CommonTableExpression statement params db0 db1 -> ByteString
renderCommonTableExpression renderStatement
  (CommonTableExpression (statement `As` alias)) =
    renderAlias alias <+> "AS" <+> parenthesized (renderStatement statement)

-- | render a non-empty `AlignedList` of `CommonTableExpression`s.
renderCommonTableExpressions
  :: (forall db ps row. statement db ps row -> ByteString) -- ^ render statement
  -> CommonTableExpression statement params db0 db1
  -> AlignedList (CommonTableExpression statement params) db1 db2
  -> ByteString
renderCommonTableExpressions renderStatement cte ctes =
  renderCommonTableExpression renderStatement cte <> case ctes of
    Done           -> ""
    cte' :>> ctes' -> "," <+>
      renderCommonTableExpressions renderStatement cte' ctes'

-- | `with` provides a way to write auxiliary statements for use in a larger query.
-- These statements, referred to as `CommonTableExpression`s, can be thought of as
-- defining temporary tables that exist just for one query.
class With statement where
  with
    :: AlignedList (CommonTableExpression statement params) db0 db1
    -- ^ common table expressions
    -> statement db1 params row
    -- ^ larger query
    -> statement db0 params row
instance With Query where
  with Done query = query
  with (cte :>> ctes) query = UnsafeQuery $
    "WITH" <+> renderCommonTableExpressions renderQuery cte ctes
      <+> renderQuery query

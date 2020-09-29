{-|
Module: Squeal.PostgreSQL.Query
Description: structured query language
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

structured query language
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
    Query (..)
  , Query_
    -- ** Set Operations
  , union
  , unionAll
  , intersect
  , intersectAll
  , except
  , exceptAll
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Kind (Type)

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

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

* @lat :: FromType@ - scope for `Squeal.PostgreSQL.Query.From.Join.JoinLateral` and subquery expressions,
* @with :: FromType@ - scope for all `Squeal.PostgreSQL.Query.From.common` table expressions,
* @db :: SchemasType@ - scope for all `Squeal.PostgreSQL.Query.From.table`s and `Squeal.PostgreSQL.Query.From.view`s,
* @params :: [NullType]@ - scope for all `Squeal.Expression.Parameter.parameter`s,
* @row :: RowType@ - return type of the `Query`.

Let's see some `Query` examples.

simple query:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  qry :: Query lat with (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = select Star (from (table #tab))
in printSQL qry
:}
SELECT * FROM "tab" AS "tab"

restricted query:

>>> :{
let
  qry :: Query '[] with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry =
    select_ ((#col1 + #col2) `as` #col1 :* #col1 `as` #col2)
      ( from (table #tab)
        & where_ (#col1 .> #col2)
        & where_ (#col2 .> 0) )
in printSQL qry
:}
SELECT ("col1" + "col2") AS "col1", "col1" AS "col2" FROM "tab" AS "tab" WHERE (("col1" > "col2") AND ("col2" > (0 :: int4)))

subquery:

>>> :{
let
  qry :: Query lat with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = select Star (from (subquery (select Star (from (table #tab)) `as` #sub)))
in printSQL qry
:}
SELECT * FROM (SELECT * FROM "tab" AS "tab") AS "sub"

limits and offsets:

>>> :{
let
  qry :: Query lat with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = select Star (from (table #tab) & limit 100 & offset 2 & limit 50 & offset 2)
in printSQL qry
:}
SELECT * FROM "tab" AS "tab" LIMIT 50 OFFSET 4

parameterized query:

>>> :{
let
  qry :: Query '[] with (Public Schema) '[ 'NotNull 'PGint4] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = select Star (from (table #tab) & where_ (#col1 .> param @1))
in printSQL qry
:}
SELECT * FROM "tab" AS "tab" WHERE ("col1" > ($1 :: int4))

aggregation query:

>>> :{
let
  qry :: Query '[] with (Public Schema) params '["col1" ::: 'NotNull 'PGint8, "col2" ::: 'NotNull 'PGint4]
  qry =
    select_ ((fromNull 0 (sum_ (All #col2))) `as` #col1 :* #col1 `as` #col2)
    ( from (table (#tab `as` #table1))
      & groupBy #col1
      & having (sum_ (Distinct #col2) .> 1) )
in printSQL qry
:}
SELECT COALESCE(sum(ALL "col2"), (0 :: int8)) AS "col1", "col1" AS "col2" FROM "tab" AS "table1" GROUP BY "col1" HAVING (sum(DISTINCT "col2") > (1 :: int8))

sorted query:

>>> :{
let
  qry :: Query '[] with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = select Star (from (table #tab) & orderBy [#col1 & Asc])
in printSQL qry
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
  ,"fk_customers" ::: ForeignKey '["customer_id"] "public" "customers" '["id"]
  ,"fk_shippers" ::: ForeignKey '["shipper_id"] "public" "shippers" '["id"] ]
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
type OrderRow =
  '[ "price" ::: 'NotNull 'PGfloat4
   , "customerName" ::: 'NotNull 'PGtext
   , "shipperName" ::: 'NotNull 'PGtext
   ]
:}

>>> :{
let
  qry :: Query lat with (Public OrdersSchema) params OrderRow
  qry = select_
    ( #o ! #price `as` #price :*
      #c ! #name `as` #customerName :*
      #s ! #name `as` #shipperName )
    ( from (table (#orders `as` #o)
      & innerJoin (table (#customers `as` #c))
        (#o ! #customer_id .== #c ! #id)
      & innerJoin (table (#shippers `as` #s))
        (#o ! #shipper_id .== #s ! #id)) )
in printSQL qry
:}
SELECT "o"."price" AS "price", "c"."name" AS "customerName", "s"."name" AS "shipperName" FROM "orders" AS "o" INNER JOIN "customers" AS "c" ON ("o"."customer_id" = "c"."id") INNER JOIN "shippers" AS "s" ON ("o"."shipper_id" = "s"."id")

self-join:

>>> :{
let
  qry :: Query lat with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = select
    (#t1 & DotStar)
    (from (table (#tab `as` #t1) & crossJoin (table (#tab `as` #t2))))
in printSQL qry
:}
SELECT "t1".* FROM "tab" AS "t1" CROSS JOIN "tab" AS "t2"

value queries:

>>> :{
let
  qry :: Query lat with db params '["col1" ::: 'NotNull 'PGtext, "col2" ::: 'NotNull 'PGbool]
  qry = values
    ("true" `as` #col1 :* true `as` #col2)
    ["false" `as` #col1 :* false `as` #col2]
in printSQL qry
:}
SELECT * FROM (VALUES ((E'true' :: text), TRUE), ((E'false' :: text), FALSE)) AS t ("col1", "col2")

set operations:

>>> :{
let
  qry :: Query lat with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = select Star (from (table #tab)) `unionAll` select Star (from (table #tab))
in printSQL qry
:}
(SELECT * FROM "tab" AS "tab") UNION ALL (SELECT * FROM "tab" AS "tab")

with query:

>>> :{
let
  qry :: Query lat with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = with (
    select Star (from (table #tab)) `as` #cte1 :>>
    select Star (from (common #cte1)) `as` #cte2
    ) (select Star (from (common #cte2)))
in printSQL qry
:}
WITH "cte1" AS (SELECT * FROM "tab" AS "tab"), "cte2" AS (SELECT * FROM "cte1" AS "cte1") SELECT * FROM "cte2" AS "cte2"

window functions:

>>> :{
let
  qry :: Query '[] with (Public Schema) db '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint8]
  qry = select
    (#col1 & Also (rank `as` #col2 `Over` (partitionBy #col1 & orderBy [#col2 & Asc])))
    (from (table #tab))
in printSQL qry
:}
SELECT "col1" AS "col1", rank() OVER (PARTITION BY "col1" ORDER BY "col2" ASC) AS "col2" FROM "tab" AS "tab"

correlated subqueries:

>>> :{
let
  qry :: Query '[] with (Public Schema) params '["col1" ::: 'NotNull 'PGint4]
  qry =
    select #col1 (from (table (#tab `as` #t1))
    & where_ (exists (
      select Star (from (table (#tab `as` #t2))
      & where_ (#t2 ! #col2 .== #t1 ! #col1)))))
in printSQL qry
:}
SELECT "col1" AS "col1" FROM "tab" AS "t1" WHERE EXISTS (SELECT * FROM "tab" AS "t2" WHERE ("t2"."col2" = "t1"."col1"))
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
The `Query_` type is parameterized by a @db@ `SchemasType`,
against which the query is type-checked, an input @params@ Haskell `Type`,
and an ouput row Haskell `Type`.

A `Query_` can be run
using `Squeal.PostgreSQL.Session.runQueryParams`, or if @params = ()@
using `Squeal.PostgreSQL.Session.runQuery`.

Generally, @params@ will be a Haskell tuple or record whose entries
may be referenced using positional
`Squeal.PostgreSQL.Expression.Parameter.parameter`s and @row@ will be a
Haskell record, whose entries will be targeted using overloaded labels.

`Query_` is a type family which resolves into a `Query`,
so don't be fooled by the input params and output row Haskell `Type`s,
which are converted into appropriate
Postgres @[@`NullType`@]@ params and `RowType` rows.
Use `Squeal.PostgreSQL.Session.Statement.query` to
fix actual Haskell input params and output rows.

>>> :set -XDeriveAnyClass -XDerivingStrategies
>>> type Columns = '["col1" ::: 'NoDef :=> 'Null 'PGint8, "col2" ::: 'Def :=> 'NotNull 'PGtext]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
data Row = Row { col1 :: Maybe Int64, col2 :: String }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
:}

>>> :{
let
  qry :: Query_ (Public Schema) (Int64, Bool) Row
  qry = select Star (from (table #tab) & where_ (#col1 .> param @1 .&& notNull (param @2)))
  stmt :: Statement (Public Schema) (Int64, Bool) Row
  stmt = query qry
:}

>>> :type qry
qry
  :: Query
       '[]
       '[]
       '["public" ::: '["tab" ::: 'Table ('[] :=> Columns)]]
       '[ 'NotNull 'PGint8, 'NotNull 'PGbool]
       '["col1" ::: 'Null 'PGint8, "col2" ::: 'NotNull 'PGtext]
>>> :type stmt
stmt
  :: Statement
       '["public" ::: '["tab" ::: 'Table ('[] :=> Columns)]]
       (Int64, Bool)
       Row
-}
type family Query_
  (db :: SchemasType)
  (params :: Type)
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

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


* @with :: FromType@ - scope for all `Squeal.PostgreSQL.Query.From.common` table expressions,
* @db :: SchemasType@ - scope for all `Squeal.PostgreSQL.Query.From.table`s and `Squeal.PostgreSQL.Query.From.view`s,
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
using `Squeal.PostgreSQL.Session.runQueryParams`, or if @parameters = ()@
using `Squeal.PostgreSQL.Session.runQuery`.

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
SELECT ("col1" + "col2") AS "col1", "col1" AS "col2" FROM "tab" AS "tab" WHERE (("col1" > "col2") AND ("col2" > (0 :: int4)))

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
SELECT COALESCE(sum(ALL "col2"), (0 :: int8)) AS "col1", "col1" AS "col2" FROM "tab" AS "table1" GROUP BY "col1" HAVING (sum(DISTINCT "col2") > (1 :: int8))

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
SELECT * FROM (VALUES ((E'true' :: text), TRUE), ((E'false' :: text), FALSE)) AS t ("col1", "col2")

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

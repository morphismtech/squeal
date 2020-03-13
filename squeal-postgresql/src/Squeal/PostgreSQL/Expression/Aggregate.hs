{-|
Module: Squeal.PostgreSQL.Expression.Aggregate
Description: Aggregate functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Aggregate functions
-}

{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PatternSynonyms
  , PolyKinds
  , StandaloneDeriving
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Aggregate
  ( -- * Aggregate
    Aggregate (..)
  , AggregateArg (..)
  , pattern All
  , pattern Alls
  , allNotNull
  , pattern Distinct
  , pattern Distincts
  , distinctNotNull
  , FilterWhere (..)
    -- * Aggregate Types
  , PGSum
  , PGAvg
  ) where

import Data.ByteString (ByteString)
import GHC.TypeLits

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Null
import Squeal.PostgreSQL.Expression.Sort
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- |
`Aggregate` functions compute a single result from a set of input values.
`Aggregate` functions can be used as `Grouped` `Expression`s as well
as `Squeal.PostgreSQL.Expression.Window.WindowFunction`s.
-}
class Aggregate arg expr | expr -> arg where

  -- | A special aggregation that does not require an input
  --
  -- >>> :{
  -- let
  --   expression :: Expression ('Grouped bys) '[] with db params from ('NotNull 'PGint8)
  --   expression = countStar
  -- in printSQL expression
  -- :}
  -- count(*)
  countStar :: expr lat with db params from ('NotNull 'PGint8)

  -- | >>> :{
  -- let
  --   expression :: Expression ('Grouped bys) '[] with db params '[tab ::: '["col" ::: null ty]] ('NotNull 'PGint8)
  --   expression = count (All #col)
  -- in printSQL expression
  -- :}
  -- count(ALL "col")
  count
    :: arg '[ty] lat with db params from
    -> expr lat with db params from ('NotNull 'PGint8)

  -- | >>> :{
  -- let
  --   expression :: Expression ('Grouped bys) '[] with db params '[tab ::: '["col" ::: 'Null 'PGnumeric]] ('Null 'PGnumeric)
  --   expression = sum_ (Distinct #col & filterWhere (#col .< 100))
  -- in printSQL expression
  -- :}
  -- sum(DISTINCT "col") FILTER (WHERE ("col" < (100.0 :: numeric)))
  sum_
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null (PGSum ty))

  -- | input values, including nulls, concatenated into an array
  -- >>> :{
  -- let
  --   expression :: Expression ('Grouped bys) '[] with db params '[tab ::: '["col" ::: 'Null 'PGnumeric]] ('Null ('PGvararray ('Null 'PGnumeric)))
  --   expression = arrayAgg (All #col & orderBy [AscNullsFirst #col] & filterWhere (#col .< 100))
  -- in printSQL expression
  -- :}
  -- array_agg(ALL "col" ORDER BY "col" ASC NULLS FIRST) FILTER (WHERE ("col" < (100.0 :: numeric)))
  arrayAgg
    :: arg '[ty] lat with db params from
    -> expr lat with db params from ('Null ('PGvararray ty))

  -- | aggregates values as a JSON array
  jsonAgg
    :: arg '[ty] lat with db params from
    -> expr lat with db params from ('Null 'PGjson)

  -- | aggregates values as a JSON array
  jsonbAgg
    :: arg '[ty] lat with db params from
    -> expr lat with db params from ('Null 'PGjsonb)

  {- |
  the bitwise AND of all non-null input values, or null if none

  >>> :{
  let
    expression :: Expression ('Grouped bys) '[] with db params '[tab ::: '["col" ::: null 'PGint4]] ('Null 'PGint4)
    expression = bitAnd (Distinct #col)
  in printSQL expression
  :}
  bit_and(DISTINCT "col")
  -}
  bitAnd
    :: int `In` PGIntegral
    => arg '[null int] lat with db params from
    -> expr lat with db params from ('Null int)

  {- |
  the bitwise OR of all non-null input values, or null if none

  >>> :{
  let
    expression :: Expression ('Grouped bys) '[] with db params '[tab ::: '["col" ::: null 'PGint4]] ('Null 'PGint4)
    expression = bitOr (All #col)
  in printSQL expression
  :}
  bit_or(ALL "col")
  -}
  bitOr
    :: int `In` PGIntegral
    => arg '[null int] lat with db params from
    -> expr lat with db params from ('Null int)

  {- |
  true if all input values are true, otherwise false

  >>> :{
  let
    winFun :: WindowFunction  'Ungrouped '[] with db params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    winFun = boolAnd (Window #col)
  in printSQL winFun
  :}
  bool_and("col")
  -}
  boolAnd
    :: arg '[null 'PGbool] lat with db params from
    -> expr lat with db params from ('Null 'PGbool)

  {- |
  true if at least one input value is true, otherwise false

  >>> :{
  let
    expression :: Expression ('Grouped bys) '[] with db params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    expression = boolOr (All #col)
  in printSQL expression
  :}
  bool_or(ALL "col")
  -}
  boolOr
    :: arg '[null 'PGbool] lat with db params from
    -> expr lat with db params from ('Null 'PGbool)

  {- |
  equivalent to `boolAnd`

  >>> :{
  let
    expression :: Expression ('Grouped bys) '[] with db params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    expression = every (Distinct #col)
  in printSQL expression
  :}
  every(DISTINCT "col")
  -}
  every
    :: arg '[null 'PGbool] lat with db params from
    -> expr lat with db params from ('Null 'PGbool)

  {- |maximum value of expression across all input values-}
  max_
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null ty)

  -- | minimum value of expression across all input values
  min_
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null ty)

  -- | the average (arithmetic mean) of all input values
  avg
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null (PGAvg ty))

  {- | correlation coefficient

  >>> :{
  let
    expression :: Expression ('Grouped g) '[] c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = corr (Alls (#y *: #x))
  in printSQL expression
  :}
  corr(ALL "y", "x")
  -}
  corr
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  {- | population covariance

  >>> :{
  let
    expression :: Expression ('Grouped g) '[] c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = covarPop (Alls (#y *: #x))
  in printSQL expression
  :}
  covar_pop(ALL "y", "x")
  -}
  covarPop
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  {- | sample covariance

  >>> :{
  let
    winFun :: WindowFunction  'Ungrouped '[] c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    winFun = covarSamp (Windows (#y *: #x))
  in printSQL winFun
  :}
  covar_samp("y", "x")
  -}
  covarSamp
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  {- | average of the independent variable (sum(X)/N)

  >>> :{
  let
    expression :: Expression ('Grouped g) '[] c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = regrAvgX (Alls (#y *: #x))
  in printSQL expression
  :}
  regr_avgx(ALL "y", "x")
  -}
  regrAvgX
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  {- | average of the dependent variable (sum(Y)/N)

  >>> :{
  let
    winFun :: WindowFunction  'Ungrouped '[] c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    winFun = regrAvgY (Windows (#y *: #x))
  in printSQL winFun
  :}
  regr_avgy("y", "x")
  -}
  regrAvgY
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  {- | number of input rows in which both expressions are nonnull

  >>> :{
  let
    winFun :: WindowFunction  'Ungrouped '[] c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGint8)
    winFun = regrCount (Windows (#y *: #x))
  in printSQL winFun
  :}
  regr_count("y", "x")
  -}
  regrCount
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGint8)

  {- | y-intercept of the least-squares-fit linear equation determined by the (X, Y) pairs
  >>> :{
  let
    expression :: Expression ('Grouped g) '[] c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = regrIntercept (Alls (#y *: #x))
  in printSQL expression
  :}
  regr_intercept(ALL "y", "x")
  -}
  regrIntercept
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  -- | @regr_r2(Y, X)@, square of the correlation coefficient
  regrR2
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  -- | @regr_slope(Y, X)@, slope of the least-squares-fit linear equation
  -- determined by the (X, Y) pairs
  regrSlope
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  -- | @regr_sxx(Y, X)@, sum(X^2) - sum(X)^2/N
  -- (“sum of squares” of the independent variable)
  regrSxx
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  -- | @regr_sxy(Y, X)@, sum(X*Y) - sum(X) * sum(Y)/N
  -- (“sum of products” of independent times dependent variable)
  regrSxy
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  -- | @regr_syy(Y, X)@, sum(Y^2) - sum(Y)^2/N
  -- (“sum of squares” of the dependent variable)
  regrSyy
    :: arg '[null 'PGfloat8, null 'PGfloat8] lat with db params from
    -> expr lat with db params from ('Null 'PGfloat8)

  -- | historical alias for `stddevSamp`
  stddev
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null (PGAvg ty))

  -- | population standard deviation of the input values
  stddevPop
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null (PGAvg ty))

  -- | sample standard deviation of the input values
  stddevSamp
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null (PGAvg ty))

  -- | historical alias for `varSamp`
  variance
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null (PGAvg ty))

  -- | population variance of the input values
  -- (square of the population standard deviation)
  varPop
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null (PGAvg ty))

  -- | sample variance of the input values
  -- (square of the sample standard deviation)
  varSamp
    :: arg '[null ty] lat with db params from
    -> expr lat with db params from ('Null (PGAvg ty))

{- |
`AggregateArg`s are used for the input of `Aggregate` `Expression`s.
-}
data AggregateArg
  (xs :: [NullType])
  (lat :: FromType)
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (from :: FromType)
  = AggregateAll
  { aggregateArgs :: NP (Expression 'Ungrouped lat with db params from) xs
  , aggregateOrder :: [SortExpression 'Ungrouped lat with db params from]
  , aggregateFilter :: [Condition 'Ungrouped lat with db params from] }
  | AggregateDistinct
  { aggregateArgs :: NP (Expression 'Ungrouped lat with db params from) xs
  , aggregateOrder :: [SortExpression 'Ungrouped lat with db params from]
  , aggregateFilter :: [Condition 'Ungrouped lat with db params from] }

instance SOP.SListI xs => RenderSQL (AggregateArg xs lat with db params from) where
  renderSQL = \case
    AggregateAll args sorts filters ->
      parenthesized
      ("ALL" <+> renderCommaSeparated renderSQL args<> renderSQL sorts)
      <> renderFilters filters
    AggregateDistinct args sorts filters ->
      parenthesized
      ("DISTINCT" <+> renderCommaSeparated renderSQL args <> renderSQL sorts)
      <> renderFilters filters
    where
      renderFilter wh = "FILTER" <+> parenthesized ("WHERE" <+> wh)
      renderFilters = \case
        [] -> ""
        wh:whs -> " " <> renderFilter (renderSQL (foldr (.&&) wh whs))

instance OrderBy (AggregateArg xs) 'Ungrouped where
  orderBy sorts1 = \case
    AggregateAll xs sorts0 whs -> AggregateAll xs (sorts0 ++ sorts1) whs
    AggregateDistinct xs sorts0 whs -> AggregateDistinct xs (sorts0 ++ sorts1) whs

-- | `All` invokes the aggregate on a single
-- argument once for each input row.
pattern All
  :: Expression 'Ungrouped lat with db params from x
  -> AggregateArg '[x] lat with db params from
pattern All x = Alls (x :* Nil)

-- | `All` invokes the aggregate on multiple
-- arguments once for each input row.
pattern Alls
  :: NP (Expression 'Ungrouped lat with db params from) xs
  -> AggregateArg xs lat with db params from
pattern Alls xs = AggregateAll xs [] []

-- | `allNotNull` invokes the aggregate on a single
-- argument once for each input row where the argument
-- is not null
allNotNull
  :: Expression 'Ungrouped lat with db params from ('Null x)
  -> AggregateArg '[ 'NotNull x] lat with db params from
allNotNull x = All (unsafeNotNull x) & filterWhere (isNotNull x)

{- |
`Distinct` invokes the aggregate once on a single
argument for each distinct value of the expression
(or distinct set of values, for multiple expressions) found in the input.
-}
pattern Distinct
  :: Expression 'Ungrouped lat with db params from x
  -> AggregateArg '[x] lat with db params from
pattern Distinct x = Distincts (x :* Nil)

{- |
`Distinct` invokes the aggregate once on multiple
arguments for each distinct value of the expression
(or distinct set of values, for multiple expressions) found in the input.
-}
pattern Distincts
  :: NP (Expression 'Ungrouped lat with db params from) xs
  -> AggregateArg xs lat with db params from
pattern Distincts xs = AggregateDistinct xs [] []

{- |
`Distinct` invokes the aggregate once on multiple
arguments for each distinct and not null value of the expression
(or distinct set of values, for multiple expressions) found in the input.
-}
distinctNotNull
  :: Expression 'Ungrouped lat with db params from ('Null x)
  -> AggregateArg '[ 'NotNull x] lat with db params from
distinctNotNull x = Distinct (unsafeNotNull x) & filterWhere (isNotNull x)

-- | Permits filtering
-- `Squeal.PostgreSQL.Expression.Window.WindowArg`s and `AggregateArg`s
class FilterWhere arg grp | arg -> grp where
  {- |
  If `filterWhere` is specified, then only the input rows for which
  the `Condition` evaluates to true are fed to the aggregate function;
  other rows are discarded.
  -}
  filterWhere
    :: Condition grp lat with db params from
    -> arg xs lat with db params from
    -> arg xs lat with db params from
instance FilterWhere AggregateArg 'Ungrouped where
  filterWhere wh = \case
    AggregateAll xs sorts whs -> AggregateAll xs sorts (wh : whs)
    AggregateDistinct xs sorts whs -> AggregateDistinct xs sorts (wh : whs)

instance Aggregate AggregateArg (Expression ('Grouped bys)) where
  countStar = UnsafeExpression "count(*)"
  count = unsafeAggregate "count"
  sum_ = unsafeAggregate "sum"
  arrayAgg = unsafeAggregate "array_agg"
  jsonAgg = unsafeAggregate "json_agg"
  jsonbAgg = unsafeAggregate "jsonb_agg"
  bitAnd = unsafeAggregate "bit_and"
  bitOr = unsafeAggregate "bit_or"
  boolAnd = unsafeAggregate "bool_and"
  boolOr = unsafeAggregate "bool_or"
  every = unsafeAggregate "every"
  max_ = unsafeAggregate "max"
  min_ = unsafeAggregate "min"
  avg = unsafeAggregate "avg"
  corr = unsafeAggregate "corr"
  covarPop = unsafeAggregate "covar_pop"
  covarSamp = unsafeAggregate "covar_samp"
  regrAvgX = unsafeAggregate "regr_avgx"
  regrAvgY = unsafeAggregate "regr_avgy"
  regrCount = unsafeAggregate "regr_count"
  regrIntercept = unsafeAggregate "regr_intercept"
  regrR2 = unsafeAggregate "regr_r2"
  regrSlope = unsafeAggregate "regr_slope"
  regrSxx = unsafeAggregate "regr_sxx"
  regrSxy = unsafeAggregate "regr_sxy"
  regrSyy = unsafeAggregate "regr_syy"
  stddev = unsafeAggregate "stddev"
  stddevPop = unsafeAggregate "stddev_pop"
  stddevSamp = unsafeAggregate "stddev_samp"
  variance = unsafeAggregate "variance"
  varPop = unsafeAggregate "var_pop"
  varSamp = unsafeAggregate "var_samp"

-- | escape hatch to define aggregate functions
unsafeAggregate
  :: SOP.SListI xs
  => ByteString -- ^ function
  -> AggregateArg xs lat with db params from
  -> Expression ('Grouped bys) lat with db params from y
unsafeAggregate fun xs = UnsafeExpression $ fun <> renderSQL xs

-- | A type family that calculates `PGSum` `PGType` of
-- a given argument `PGType`.
type family PGSum ty where
  PGSum 'PGint2 = 'PGint8
  PGSum 'PGint4 = 'PGint8
  PGSum 'PGint8 = 'PGnumeric
  PGSum 'PGfloat4 = 'PGfloat4
  PGSum 'PGfloat8 = 'PGfloat8
  PGSum 'PGnumeric = 'PGnumeric
  PGSum 'PGinterval = 'PGinterval
  PGSum 'PGmoney = 'PGmoney
  PGSum pg = TypeError
    ( 'Text "Squeal type error: Cannot sum with argument type "
      ':<>: 'ShowType pg )

-- | A type family that calculates `PGAvg` type of a `PGType`.
type family PGAvg ty where
  PGAvg 'PGint2 = 'PGnumeric
  PGAvg 'PGint4 = 'PGnumeric
  PGAvg 'PGint8 = 'PGnumeric
  PGAvg 'PGnumeric = 'PGnumeric
  PGAvg 'PGfloat4 = 'PGfloat8
  PGAvg 'PGfloat8 = 'PGfloat8
  PGAvg 'PGinterval = 'PGinterval
  PGAvg pg = TypeError
    ('Text "Squeal type error: No average for " ':<>: 'ShowType pg)

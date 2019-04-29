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
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Aggregate
  ( Aggregate (..)
  , Distinction (..)
  , PGAvg
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Kind
import GHC.TypeLits

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- |
`Aggregate` functions compute a single result from a set of input values.
`Aggregate` functions can be used as `GroupedBy` `Expression`s as well
as `Squeal.PostgreSQL.Expression.Window.WindowFunction`s.
-}
class Aggregate expr1 exprN aggr
  | aggr -> expr1, aggr -> exprN where

  -- | A special aggregation that does not require an input
  --
  -- >>> :{
  -- let
  --   expression :: Expression '[] commons ('Grouped bys) schemas params from ('NotNull 'PGint8)
  --   expression = countStar
  -- in printSQL expression
  -- :}
  -- count(*)
  countStar :: aggr ('NotNull 'PGint8)

  -- | >>> :{
  -- let
  --   expression :: Expression '[] commons ('Grouped bys) schemas params '[tab ::: '["col" ::: null ty]] ('NotNull 'PGint8)
  --   expression = count (All #col)
  -- in printSQL expression
  -- :}
  -- count(ALL "col")
  count
    :: expr1 ty
    -- ^ what to count
    -> aggr ('NotNull 'PGint8)

  -- | >>> :{
  -- let
  --   expression :: Expression '[] commons ('Grouped bys) schemas params '[tab ::: '["col" ::: 'Null 'PGnumeric]] ('Null 'PGnumeric)
  --   expression = sum_ (Distinct #col)
  -- in printSQL expression
  -- :}
  -- sum(DISTINCT "col")
  sum_
    :: ty `In` PGNum
    => expr1 (null ty)
    -> aggr ('Null ty)

  -- | input values, including nulls, concatenated into an array
  arrayAgg
    :: expr1 ty
    -> aggr ('Null ('PGvararray ty))

  -- | aggregates values as a JSON array
  jsonAgg
    :: expr1 ty
    -> aggr ('Null 'PGjson)

  -- | aggregates values as a JSON array
  jsonbAgg
    :: expr1 ty
    -> aggr ('Null 'PGjsonb)

  {- |
  the bitwise AND of all non-null input values, or null if none

  >>> :{
  let
    expression :: Expression '[] commons ('Grouped bys) schemas params '[tab ::: '["col" ::: null 'PGint4]] ('Null 'PGint4)
    expression = bitAnd (Distinct #col)
  in printSQL expression
  :}
  bit_and(DISTINCT "col")
  -}
  bitAnd
    :: int `In` PGIntegral
    => expr1 (null int)
    -- ^ what to aggregate
    -> aggr ('Null int)

  {- |
  the bitwise OR of all non-null input values, or null if none

  >>> :{
  let
    expression :: Expression '[] commons ('Grouped bys) schemas params '[tab ::: '["col" ::: null 'PGint4]] ('Null 'PGint4)
    expression = bitOr (All #col)
  in printSQL expression
  :}
  bit_or(ALL "col")
  -}
  bitOr
    :: int `In` PGIntegral
    => expr1 (null int)
    -- ^ what to aggregate
    -> aggr ('Null int)

  {- |
  true if all input values are true, otherwise false

  >>> :{
  let
    winFun :: WindowFunction '[] commons 'Ungrouped schemas params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    winFun = boolAnd #col
  in printSQL winFun
  :}
  bool_and("col")
  -}
  boolAnd
    :: expr1 (null 'PGbool)
    -- ^ what to aggregate
    -> aggr ('Null 'PGbool)

  {- |
  true if at least one input value is true, otherwise false

  >>> :{
  let
    expression :: Expression '[] commons ('Grouped bys) schemas params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    expression = boolOr (All #col)
  in printSQL expression
  :}
  bool_or(ALL "col")
  -}
  boolOr
    :: expr1 (null 'PGbool)
    -- ^ what to aggregate
    -> aggr ('Null 'PGbool)

  {- |
  equivalent to `boolAnd`

  >>> :{
  let
    expression :: Expression '[] commons ('Grouped bys) schemas params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    expression = every (Distinct #col)
  in printSQL expression
  :}
  every(DISTINCT "col")
  -}
  every
    :: expr1 (null 'PGbool)
    -- ^ what to aggregate
    -> aggr ('Null 'PGbool)

  {- |maximum value of expression across all input values-}
  max_
    :: expr1 (null ty)
    -- ^ what to maximize
    -> aggr ('Null ty)

  -- | minimum value of expression across all input values
  min_
    :: expr1 (null ty)
    -- ^ what to minimize
    -> aggr ('Null ty)

  -- | the average (arithmetic mean) of all input values
  avg
    :: expr1 (null ty)
    -- ^ what to average
    -> aggr ('Null (PGAvg ty))

  {- | correlation coefficient

  >>> :{
  let
    expression :: Expression '[] c ('Grouped g) s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = corr (All (#y *: #x))
  in printSQL expression
  :}
  corr(ALL "y", "x")
  -}
  corr
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | population covariance

  >>> :{
  let
    expression :: Expression '[] c ('Grouped g) s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = covarPop (All (#y *: #x))
  in printSQL expression
  :}
  covar_pop(ALL "y", "x")
  -}
  covarPop
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | sample covariance

  >>> :{
  let
    winFun :: WindowFunction '[] c 'Ungrouped s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    winFun = covarSamp (#y *: #x)
  in printSQL winFun
  :}
  covar_samp("y", "x")
  -}
  covarSamp
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | average of the independent variable (sum(X)/N)

  >>> :{
  let
    expression :: Expression '[] c ('Grouped g) s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = regrAvgX (All (#y *: #x))
  in printSQL expression
  :}
  regr_avgx(ALL "y", "x")
  -}
  regrAvgX
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | average of the dependent variable (sum(Y)/N)

  >>> :{
  let
    winFun :: WindowFunction '[] c 'Ungrouped s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    winFun = regrAvgY (#y *: #x)
  in printSQL winFun
  :}
  regr_avgy("y", "x")
  -}
  regrAvgY
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | number of input rows in which both expressions are nonnull

  >>> :{
  let
    winFun :: WindowFunction '[] c 'Ungrouped s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGint8)
    winFun = regrCount (#y *: #x)
  in printSQL winFun
  :}
  regr_count("y", "x")
  -}
  regrCount
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGint8)

  {- | y-intercept of the least-squares-fit linear equation determined by the (X, Y) pairs
  >>> :{
  let
    expression :: Expression '[] c ('Grouped g) s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = regrIntercept (All (#y *: #x))
  in printSQL expression
  :}
  regr_intercept(ALL "y", "x")
  -}
  regrIntercept
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_r2(Y, X)@, square of the correlation coefficient
  regrR2
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_slope(Y, X)@, slope of the least-squares-fit linear equation
  -- determined by the (X, Y) pairs
  regrSlope
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_sxx(Y, X)@, sum(X^2) - sum(X)^2/N
  -- (“sum of squares” of the independent variable)
  regrSxx
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_sxy(Y, X)@, sum(X*Y) - sum(X) * sum(Y)/N
  -- (“sum of products” of independent times dependent variable)
  regrSxy
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_syy(Y, X)@, sum(Y^2) - sum(Y)^2/N
  -- (“sum of squares” of the dependent variable)
  regrSyy
    :: exprN '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | historical alias for `stddevSamp`
  stddev
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | population standard deviation of the input values
  stddevPop
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | sample standard deviation of the input values
  stddevSamp
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | historical alias for `varSamp`
  variance
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | population variance of the input values
  -- (square of the population standard deviation)
  varPop
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | sample variance of the input values
  -- (square of the sample standard deviation)
  varSamp
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

{- |
`Distinction`s are used for the input of `Aggregate` `Expression`s.
`All` invokes the aggregate once for each input row.
`Distinct` invokes the aggregate once for each distinct value of the expression
(or distinct set of values, for multiple expressions) found in the input
-}
data Distinction (expr :: kind -> Type) (ty :: kind)
  = All (expr ty)
  | Distinct (expr ty)
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData (Distinction (Expression outer commons grp schemas params from) ty)
instance RenderSQL (Distinction (Expression outer commons grp schemas params from) ty) where
  renderSQL = \case
    All x -> "ALL" <+> renderSQL x
    Distinct x -> "DISTINCT" <+> renderSQL x
instance SOP.SListI tys => RenderSQL
  (Distinction (NP (Expression outer commons grp schemas params from)) tys) where
    renderSQL = \case
      All xs -> "ALL" <+> renderCommaSeparated renderSQL xs
      Distinct xs -> "DISTINCT" <+> renderCommaSeparated renderSQL xs

instance Aggregate
  (Distinction (Expression outer commons 'Ungrouped schemas params from))
  (Distinction (NP (Expression outer commons 'Ungrouped schemas params from)))
  (Expression outer commons ('Grouped bys) schemas params from) where
    countStar = UnsafeExpression "count(*)"
    count = unsafeAggregate1 "count"
    sum_ = unsafeAggregate1 "sum"
    arrayAgg = unsafeAggregate1 "array_agg"
    jsonAgg = unsafeAggregate1 "json_agg"
    jsonbAgg = unsafeAggregate1 "jsonb_agg"
    bitAnd = unsafeAggregate1 "bit_and"
    bitOr = unsafeAggregate1 "bit_or"
    boolAnd = unsafeAggregate1 "bool_and"
    boolOr = unsafeAggregate1 "bool_or"
    every = unsafeAggregate1 "every"
    max_ = unsafeAggregate1 "max"
    min_ = unsafeAggregate1 "min"
    avg = unsafeAggregate1 "avg"
    corr = unsafeAggregateN "corr"
    covarPop = unsafeAggregateN "covar_pop"
    covarSamp = unsafeAggregateN "covar_samp"
    regrAvgX = unsafeAggregateN "regr_avgx"
    regrAvgY = unsafeAggregateN "regr_avgy"
    regrCount = unsafeAggregateN "regr_count"
    regrIntercept = unsafeAggregateN "regr_intercept"
    regrR2 = unsafeAggregateN "regr_r2"
    regrSlope = unsafeAggregateN "regr_slope"
    regrSxx = unsafeAggregateN "regr_sxx"
    regrSxy = unsafeAggregateN "regr_sxy"
    regrSyy = unsafeAggregateN "regr_syy"
    stddev = unsafeAggregate1 "stddev"
    stddevPop = unsafeAggregate1 "stddev_pop"
    stddevSamp = unsafeAggregate1 "stddev_samp"
    variance = unsafeAggregate1 "variance"
    varPop = unsafeAggregate1 "var_pop"
    varSamp = unsafeAggregate1 "var_samp"

-- | escape hatch to define aggregate functions
unsafeAggregate1
  :: ByteString -- ^ aggregate function
  -> Distinction (Expression outer commons 'Ungrouped schemas params from) x
  -> Expression outer commons ('Grouped bys) schemas params from y
unsafeAggregate1 fun x = UnsafeExpression $ fun <> parenthesized (renderSQL x)

unsafeAggregateN
  :: SOP.SListI xs
  => ByteString -- ^ function
  -> Distinction (NP (Expression outer commons 'Ungrouped schemas params from)) xs
  -> Expression outer commons ('Grouped bys) schemas params from y
unsafeAggregateN fun xs = UnsafeExpression $ fun <> parenthesized (renderSQL xs)

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

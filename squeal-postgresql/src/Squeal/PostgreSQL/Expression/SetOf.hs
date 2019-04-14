{-|
Module: Squeal.PostgreSQL.Expression.SetOf
Description: Set returning functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Set returning functions
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleContexts
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.SetOf
  ( generateSeries
  , generateSeriesStep
  , generateSeriesTimestamp
  , SetOfFunction
  , unsafeSetOfFunction
  , SetOfFunctionN
  , unsafeSetOfFunctionN
  ) where

import GHC.TypeLits

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

{- |
A @RankNType@ for set returning functions with 1 argument.
-}
type SetOfFunction fun ty setof
  =  forall outer commons schemas params
  .  Expression outer commons 'Ungrouped schemas params '[] ty
     -- ^ input
  -> FromClause outer commons schemas params '[fun ::: setof]
     -- ^ output

-- | Escape hatch for a set returning function with 1 argument.
unsafeSetOfFunction
  :: forall fun ty setof. KnownSymbol fun
  => SetOfFunction fun ty setof -- ^ set returning function
unsafeSetOfFunction x = UnsafeFromClause $
  renderSymbol @fun <> parenthesized (renderSQL x)

{- |
A @RankNType@ for set returning functions with multiple argument.
-}
type SetOfFunctionN fun tys setof
  =  forall outer commons schemas params
  .  NP (Expression outer commons 'Ungrouped schemas params '[]) tys
     -- ^ inputs
  -> FromClause outer commons schemas params '[fun ::: setof]
     -- ^ output

-- | Escape hatch for a set returning function with multiple argument.
unsafeSetOfFunctionN
  :: forall fun tys setof. (SOP.SListI tys, KnownSymbol fun)
  => SetOfFunctionN fun tys setof -- ^ set returning function
unsafeSetOfFunctionN xs = UnsafeFromClause $
  renderSymbol @fun <> parenthesized (renderCommaSeparated renderSQL xs)

{- | @generateSeries (start *: stop)@

Generate a series of values, from @start@ to @stop@ with a step size of one
-}
generateSeries
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetOfFunctionN "generate_series" '[ null ty, null ty]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeries = unsafeSetOfFunctionN

{- | @generateSeries (start :* stop *: step)@

Generate a series of values, from @start@ to @stop@ with a step size of @step@
-}
generateSeriesStep
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetOfFunctionN "generate_series" '[null ty, null ty, null ty]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeriesStep = unsafeSetOfFunctionN

{- | @generateSeries (start :* stop *: step)@

Generate a series of values, from @start@ to @stop@ with a step size of @step@
-}
generateSeriesTimestamp
  :: ty `In` '[ 'PGtimestamp, 'PGtimestamptz]
  => SetOfFunctionN "generate_series" '[null ty, null ty, null 'PGinterval]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeriesTimestamp = unsafeSetOfFunctionN

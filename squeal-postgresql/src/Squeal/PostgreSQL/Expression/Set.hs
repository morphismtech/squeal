{-|
Module: Squeal.PostgreSQL.Expression.Set
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

module Squeal.PostgreSQL.Expression.Set
  ( generateSeries
  , generateSeriesStep
  , generateSeriesTimestamp
  , SetFunction
  , unsafeSetFunction
  , SetFunctionDB
  , setFunction
  , SetFunctionN
  , unsafeSetFunctionN
  , SetFunctionNDB
  , setFunctionN
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
type SetFunction fun ty row
  =  forall outer commons schemas params
  .  Expression outer commons 'Ungrouped schemas params '[] ty
     -- ^ input
  -> FromClause outer commons schemas params '[fun ::: row]
     -- ^ output

type SetFunctionDB fun schemas ty row
  =  forall outer commons params
  .  Expression outer commons 'Ungrouped schemas params '[] ty
     -- ^ input
  -> FromClause outer commons schemas params '[fun ::: row]
     -- ^ output

-- | Escape hatch for a set returning function with 1 argument.
unsafeSetFunction
  :: forall fun ty row. KnownSymbol fun
  => SetFunction fun ty row -- ^ set returning function
unsafeSetFunction x = UnsafeFromClause $
  renderSymbol @fun <> parenthesized (renderSQL x)

setFunction
  :: ( Has sch schemas schema
     , Has fun schema ('Function ('[ty] :=> 'ReturnsTable row)) )
  => QualifiedAlias sch fun
  -> SetFunctionDB fun schemas ty row
setFunction _ = unsafeSetFunction

{- |
A @RankNType@ for set returning functions with multiple argument.
-}
type SetFunctionN fun tys row
  =  forall outer commons schemas params
  .  NP (Expression outer commons 'Ungrouped schemas params '[]) tys
     -- ^ inputs
  -> FromClause outer commons schemas params '[fun ::: row]
     -- ^ output

-- | Escape hatch for a set returning function with multiple argument.
unsafeSetFunctionN
  :: forall fun tys row. (SOP.SListI tys, KnownSymbol fun)
  => SetFunctionN fun tys row -- ^ set returning function
unsafeSetFunctionN xs = UnsafeFromClause $
  renderSymbol @fun <> parenthesized (renderCommaSeparated renderSQL xs)

type SetFunctionNDB fun schemas tys row
  =  forall outer commons params
  .  NP (Expression outer commons 'Ungrouped schemas params '[]) tys
     -- ^ inputs
  -> FromClause outer commons schemas params '[fun ::: row]
     -- ^ output

setFunctionN
  :: ( Has sch schemas schema
     , Has fun schema ('Function (tys :=> 'ReturnsTable row))
     , SOP.SListI tys )
  => QualifiedAlias sch fun
  -> SetFunctionNDB fun schemas tys row
setFunctionN _ = unsafeSetFunctionN

{- | @generateSeries (start *: stop)@

Generate a series of values, from @start@ to @stop@ with a step size of one
-}
generateSeries
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetFunctionN "generate_series" '[ null ty, null ty]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeries = unsafeSetFunctionN

{- | @generateSeries (start :* stop *: step)@

Generate a series of values, from @start@ to @stop@ with a step size of @step@
-}
generateSeriesStep
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetFunctionN "generate_series" '[null ty, null ty, null ty]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeriesStep = unsafeSetFunctionN

{- | @generateSeries (start :* stop *: step)@

Generate a series of values, from @start@ to @stop@ with a step size of @step@
-}
generateSeriesTimestamp
  :: ty `In` '[ 'PGtimestamp, 'PGtimestamptz]
  => SetFunctionN "generate_series" '[null ty, null ty, null 'PGinterval]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeriesTimestamp = unsafeSetFunctionN

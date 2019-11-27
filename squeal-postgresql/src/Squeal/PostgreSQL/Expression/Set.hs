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
  =  forall outer commons db params
  .  Expression outer commons 'Ungrouped db params '[] ty
     -- ^ input
  -> FromClause outer commons db params '[fun ::: row]
     -- ^ output

-- | Like `SetFunction` but depends on the schemas of the database
type SetFunctionDB fun db ty row
  =  forall outer commons params
  .  Expression outer commons 'Ungrouped db params '[] ty
     -- ^ input
  -> FromClause outer commons db params '[fun ::: row]
     -- ^ output

-- | Escape hatch for a set returning function of a single variable
unsafeSetFunction
  :: forall fun ty row. KnownSymbol fun
  => SetFunction fun ty row -- ^ set returning function
unsafeSetFunction x = UnsafeFromClause $
  renderSymbol @fun <> parenthesized (renderSQL x)

-- | Call a user defined set returning function of a single variable
setFunction
  :: ( Has sch db schema
     , Has fun schema ('Function ('[ty] :=> 'ReturnsTable row)) )
  => QualifiedAlias sch fun -- ^ function alias
  -> SetFunctionDB fun db ty row
setFunction _ = unsafeSetFunction

{- |
A @RankNType@ for set returning functions with multiple argument.
-}
type SetFunctionN fun tys row
  =  forall outer commons db params
  .  NP (Expression outer commons 'Ungrouped db params '[]) tys
     -- ^ inputs
  -> FromClause outer commons db params '[fun ::: row]
     -- ^ output

-- | Escape hatch for a multivariable set returning function
unsafeSetFunctionN
  :: forall fun tys row. (SOP.SListI tys, KnownSymbol fun)
  => SetFunctionN fun tys row -- ^ set returning function
unsafeSetFunctionN xs = UnsafeFromClause $
  renderSymbol @fun <> parenthesized (renderCommaSeparated renderSQL xs)

-- | Like `SetFunctionN` but depends on the schemas of the database
type SetFunctionNDB fun db tys row
  =  forall outer commons params
  .  NP (Expression outer commons 'Ungrouped db params '[]) tys
     -- ^ inputs
  -> FromClause outer commons db params '[fun ::: row]
     -- ^ output

-- | Call a user defined multivariable set returning function
setFunctionN
  :: ( Has sch db schema
     , Has fun schema ('Function (tys :=> 'ReturnsTable row))
     , SOP.SListI tys )
  => QualifiedAlias sch fun -- ^ function alias
  -> SetFunctionNDB fun db tys row
setFunctionN _ = unsafeSetFunctionN

{- | @generateSeries (start *: stop)@

Generate a series of values,
from @start@ to @stop@ with a step size of one
-}
generateSeries
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetFunctionN "generate_series" '[ null ty, null ty]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeries = unsafeSetFunctionN

{- | @generateSeries (start :* stop *: step)@

Generate a series of values,
from @start@ to @stop@ with a step size of @step@
-}
generateSeriesStep
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetFunctionN "generate_series" '[null ty, null ty, null ty]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeriesStep = unsafeSetFunctionN

{- | @generateSeries (start :* stop *: step)@

Generate a series of values,
from @start@ to @stop@ with a step size of @step@
-}
generateSeriesTimestamp
  :: ty `In` '[ 'PGtimestamp, 'PGtimestamptz]
  => SetFunctionN "generate_series" '[null ty, null ty, null 'PGinterval]
    '["generate_series" ::: null ty] -- ^ set returning function
generateSeriesTimestamp = unsafeSetFunctionN

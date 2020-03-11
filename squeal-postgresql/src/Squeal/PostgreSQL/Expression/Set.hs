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
  ( -- * Set Function
    generateSeries
  , generateSeriesStep
  , generateSeriesTimestamp
  , SetFunction
  , unsafeSetFunction
  , setFunction
  , SetFunctionN
  , unsafeSetFunctionN
  , setFunctionN
  ) where

import Data.ByteString
import GHC.TypeLits

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- |
A @RankNType@ for set returning functions with 1 argument.
-}
type SetFunction ty set
  =  forall lat with db params
  .  Expression lat with 'Ungrouped db params '[] ty
  -- ^ input
  -> FromClause lat with db params '[set]
  -- ^ output

-- | Escape hatch for a set returning function of a single variable
unsafeSetFunction
  :: forall fun ty row. KnownSymbol fun
  => ByteString
  -> SetFunction ty (fun ::: row) -- ^ set returning function
unsafeSetFunction fun x = UnsafeFromClause $
  fun <> parenthesized (renderSQL x)

{- | Call a user defined set returning function of a single variable

>>> type Fn = '[ 'Null 'PGbool] :=> 'ReturnsTable '["ret" ::: 'NotNull 'PGnumeric]
>>> type Schema = '["fn" ::: 'Function Fn]
>>> :{
let
  fn :: SetOf (Public Schema) ('Null 'PGbool) ("fn" ::: '["ret" ::: 'NotNull 'PGnumeric])
  fn = setFunction #fn
in
  printSQL (fn true)
:}
"fn"(TRUE)
-}
setFunction
  :: ( Has sch db schema
     , Has fun schema ('Function ('[ty] :=> 'ReturnsTable row)) )
  => QualifiedAlias sch fun -- ^ function alias
  -> SetOf db ty (fun ::: row)
setFunction fun = unsafeSetFunction (renderSQL fun)

{- |
A @RankNType@ for set returning functions with multiple argument.
-}
type SetFunctionN tys set
  =  forall lat with db params
  .  NP (Expression lat with 'Ungrouped db params '[]) tys
     -- ^ inputs
  -> FromClause lat with db params '[set]
     -- ^ output

{- | Escape hatch for a multivariable set returning function-}
unsafeSetFunctionN
  :: forall fun tys row. (SOP.SListI tys, KnownSymbol fun)
  => ByteString
  -> SetFunctionN tys (fun ::: row) -- ^ set returning function
unsafeSetFunctionN fun xs = UnsafeFromClause $
  fun <> parenthesized (renderCommaSeparated renderSQL xs)

-- | Like `SetFunctionN` but depends on the schemas of the database
type SetFunctionNDB db tys set
  =  forall lat with params
  .  NP (Expression lat with 'Ungrouped db params '[]) tys
     -- ^ inputs
  -> FromClause lat with db params '[set]
     -- ^ output

{- | Call a user defined multivariable set returning function

>>> type Fn = '[ 'Null 'PGbool, 'Null 'PGtext] :=> 'ReturnsTable '["ret" ::: 'NotNull 'PGnumeric]
>>> type Schema = '["fn" ::: 'Function Fn]
>>> :{
let
  fn :: SetOfN (Public Schema)
    '[ 'Null 'PGbool, 'Null 'PGtext]
    ("fn" ::: '["ret" ::: 'NotNull 'PGnumeric])
  fn = setFunctionN #fn
in
  printSQL (fn (true *: "hi"))
:}
"fn"(TRUE, E'hi')
-}
setFunctionN
  :: ( Has sch db schema
     , Has fun schema ('Function (tys :=> 'ReturnsTable row))
     , SOP.SListI tys )
  => QualifiedAlias sch fun -- ^ function alias
  -> SetFunctionNDB db tys (fun ::: row)
setFunctionN fun = unsafeSetFunctionN (renderSQL fun)

{- | @generateSeries (start :* stop)@

Generate a series of values,
from @start@ to @stop@ with a step size of one

>>> renderSQL (generateSeries @'PGint4 (1 *: 10))
"generate_series(1, 10)"
-}
generateSeries
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetFunctionN '[ null ty, null ty]
    ("generate_series" ::: '["generate_series" ::: null ty])
    -- ^ set returning function
generateSeries = unsafeSetFunctionN "generate_series"

{- | @generateSeriesStep (start :* stop *: step)@

Generate a series of values,
from @start@ to @stop@ with a step size of @step@

>>> renderSQL (generateSeriesStep @'PGint8 (2 :* 100 *: 2))
"generate_series(2, 100, 2)"
-}
generateSeriesStep
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetFunctionN '[null ty, null ty, null ty]
    ("generate_series" ::: '["generate_series" ::: null ty])
    -- ^ set returning function
generateSeriesStep = unsafeSetFunctionN "generate_series"

{- | @generateSeriesTimestamp (start :* stop *: step)@

Generate a series of timestamps,
from @start@ to @stop@ with a step size of @step@

>>> :{
let
  start = now
  stop = now !+ interval_ 10 Years
  step = interval_ 1 Months
in renderSQL (generateSeriesTimestamp (start :* stop *: step))
:}
"generate_series(now(), (now() + (INTERVAL '10.0 years')), (INTERVAL '1.0 months'))"
-}
generateSeriesTimestamp
  :: ty `In` '[ 'PGtimestamp, 'PGtimestamptz]
  => SetFunctionN '[null ty, null ty, null 'PGinterval]
    ("generate_series"  ::: '["generate_series" ::: null ty])
    -- ^ set returning function
generateSeriesTimestamp = unsafeSetFunctionN "generate_series"

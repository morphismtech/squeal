{-|
Module: Squeal.PostgreSQL.Query.From.Set
Description: set returning functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

set returning functions
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

module Squeal.PostgreSQL.Query.From.Set
  ( -- * Set Functions
    type (-|->)
  , type (--|->)
  , SetFun
  , SetFunN
  , generateSeries
  , generateSeriesStep
  , generateSeriesTimestamp
  , unsafeSetFunction
  , setFunction
  , unsafeSetFunctionN
  , setFunctionN
  ) where

import Data.ByteString (ByteString)
import Generics.SOP hiding (from)
import GHC.TypeLits

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Query.From
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.Schema

{- |
A @RankNType@ for set returning functions with 1 argument.
-}
type (-|->) arg set = forall db. SetFun db arg set

{- |
A @RankNType@ for set returning functions with multiple argument.
-}
type (--|->) arg set = forall db. SetFunN db arg set
     -- ^ output

{- |
Like `-|->` but depends on the schemas of the database
-}
type SetFun db arg row
  =  forall lat with params
  .  Expression 'Ungrouped lat with db params '[] arg
     -- ^ input
  -> FromClause lat with db params '[row]
     -- ^ output

{- |
Like `--|->` but depends on the schemas of the database
-}
type SetFunN db args set
  =  forall lat with params
  .  NP (Expression 'Ungrouped lat with db params '[]) args
     -- ^ input
  -> FromClause lat with db params '[set]
     -- ^ output

-- $setup
-- >>> import Squeal.PostgreSQL

-- | Escape hatch for a set returning function of a single variable
unsafeSetFunction
  :: forall fun ty row. KnownSymbol fun
  => ByteString
  -> ty -|-> (fun ::: row) -- ^ set returning function
unsafeSetFunction fun x = UnsafeFromClause $
  fun <> parenthesized (renderSQL x)

{- | Call a user defined set returning function of a single variable

>>> type Fn = '[ 'Null 'PGbool] :=> 'ReturnsTable '["ret" ::: 'NotNull 'PGnumeric]
>>> type Schema = '["fn" ::: 'Function Fn]
>>> :{
let
  fn :: SetFun (Public Schema) ('Null 'PGbool) ("fn" ::: '["ret" ::: 'NotNull 'PGnumeric])
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
  -> SetFun db ty (fun ::: row)
setFunction fun = unsafeSetFunction (renderSQL fun)

{- | Escape hatch for a multivariable set returning function-}
unsafeSetFunctionN
  :: forall fun tys row. (SOP.SListI tys, KnownSymbol fun)
  => ByteString
  -> tys --|-> (fun ::: row) -- ^ set returning function
unsafeSetFunctionN fun xs = UnsafeFromClause $
  fun <> parenthesized (renderCommaSeparated renderSQL xs)

{- | Call a user defined multivariable set returning function

>>> type Fn = '[ 'Null 'PGbool, 'Null 'PGtext] :=> 'ReturnsTable '["ret" ::: 'NotNull 'PGnumeric]
>>> type Schema = '["fn" ::: 'Function Fn]
>>> :{
let
  fn :: SetFunN (Public Schema)
    '[ 'Null 'PGbool, 'Null 'PGtext]
    ("fn" ::: '["ret" ::: 'NotNull 'PGnumeric])
  fn = setFunctionN #fn
in
  printSQL (fn (true *: "hi"))
:}
"fn"(TRUE, (E'hi' :: text))
-}
setFunctionN
  :: ( Has sch db schema
     , Has fun schema ('Function (tys :=> 'ReturnsTable row))
     , SOP.SListI tys )
  => QualifiedAlias sch fun -- ^ function alias
  -> SetFunN db tys (fun ::: row)
setFunctionN fun = unsafeSetFunctionN (renderSQL fun)

{- | @generateSeries (start :* stop)@

Generate a series of values,
from @start@ to @stop@ with a step size of one

>>> printSQL (generateSeries @'PGint4 (1 *: 10))
generate_series((1 :: int4), (10 :: int4))
-}
generateSeries
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => '[ null ty, null ty] --|->
    ("generate_series" ::: '["generate_series" ::: null ty])
    -- ^ set returning function
generateSeries = unsafeSetFunctionN "generate_series"

{- | @generateSeriesStep (start :* stop *: step)@

Generate a series of values,
from @start@ to @stop@ with a step size of @step@

>>> printSQL (generateSeriesStep @'PGint8 (2 :* 100 *: 2))
generate_series((2 :: int8), (100 :: int8), (2 :: int8))
-}
generateSeriesStep
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => '[null ty, null ty, null ty] --|->
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
in printSQL (generateSeriesTimestamp (start :* stop *: step))
:}
generate_series(now(), (now() + (INTERVAL '10.000 years')), (INTERVAL '1.000 months'))
-}
generateSeriesTimestamp
  :: ty `In` '[ 'PGtimestamp, 'PGtimestamptz]
  => '[null ty, null ty, null 'PGinterval] --|->
    ("generate_series"  ::: '["generate_series" ::: null ty])
    -- ^ set returning function
generateSeriesTimestamp = unsafeSetFunctionN "generate_series"

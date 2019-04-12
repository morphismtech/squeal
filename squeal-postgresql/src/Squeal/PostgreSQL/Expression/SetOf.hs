{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleContexts
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.SetOf
  ( SetOfFunction
  , unsafeSetOfFunction
  , SetOfFunctionN
  , unsafeSetOfFunctionN
  , generateSeries
  , generateSeriesStep
  , generateSeriesTimestamp
  ) where

import Data.ByteString (ByteString)

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

type SetOfFunction ty setof
  =  forall outer commons schemas params
  .  Expression outer commons 'Ungrouped schemas params '[] ty
  -> FromClause outer commons schemas params setof

unsafeSetOfFunction :: ByteString -> SetOfFunction ty setof
unsafeSetOfFunction fun x = UnsafeFromClause $
  fun <> parenthesized (renderSQL x)

type SetOfFunctionN tys setof
  =  forall outer commons schemas params
  .  NP (Expression outer commons 'Ungrouped schemas params '[]) tys
  -> FromClause outer commons schemas params setof

unsafeSetOfFunctionN
  :: SOP.SListI tys
  => ByteString
  -> SetOfFunctionN tys setof
unsafeSetOfFunctionN fun xs = UnsafeFromClause $
  fun <> parenthesized (renderCommaSeparated renderSQL xs)

generateSeries
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetOfFunctionN '[ null ty, null ty]
    '["generate_series" ::: '["generate_series" ::: '[null ty]]]
generateSeries = unsafeSetOfFunctionN "generate_series"

generateSeriesStep
  :: ty `In` '[ 'PGint4, 'PGint8, 'PGnumeric]
  => SetOfFunctionN '[null ty, null ty, null ty]
    '["generate_series" ::: '["generate_series" ::: '[null ty]]]
generateSeriesStep = unsafeSetOfFunctionN "generate_series"

generateSeriesTimestamp
  :: ty `In` '[ 'PGtimestamp, 'PGtimestamptz]
  => SetOfFunctionN '[null ty, null ty, null 'PGinterval]
  '["generate_series" ::: '["generate_series" ::: '[null ty]]]
generateSeriesTimestamp = unsafeSetOfFunctionN "generate_series"

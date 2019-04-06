{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Math
  ( atan2_
  , quot_
  , rem_
  , trunc
  , round_
  , ceiling_
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Schema

-- | >>> :{
-- let
--   expression :: Expr (null 'PGfloat4)
--   expression = atan2_ (pi *: 2)
-- in printSQL expression
-- :}
-- atan2(pi(), 2)
atan2_
  :: float `In` PGFloating
  => FunctionN '[ null float, null float] (null float)
atan2_ = unsafeFunctionN "atan2"


-- | integer division, truncates the result
--
-- >>> :{
-- let
--   expression :: Expression outer commons grp schemas params from (null 'PGint2)
--   expression = 5 `quot_` 2
-- in printSQL expression
-- :}
-- (5 / 2)
quot_
  :: int `In` PGIntegral
  => Operator (null int) (null int) (null int)
quot_ = unsafeBinaryOp "/"

-- | remainder upon integer division
--
-- >>> :{
-- let
--   expression :: Expression outer commons grp schemas params from (null 'PGint2)
--   expression = 5 `rem_` 2
-- in printSQL expression
-- :}
-- (5 % 2)
rem_
  :: int `In` PGIntegral
  => Operator (null int) (null int) (null int)
rem_ = unsafeBinaryOp "%"

-- | >>> :{
-- let
--   expression :: Expression outer commons grp schemas params from (null 'PGfloat4)
--   expression = trunc pi
-- in printSQL expression
-- :}
-- trunc(pi())
trunc :: frac `In` PGFloating => null frac :--> null frac
trunc = unsafeFunction "trunc"

-- | >>> :{
-- let
--   expression :: Expression outer commons grp schemas params from (null 'PGfloat4)
--   expression = round_ pi
-- in printSQL expression
-- :}
-- round(pi())
round_ :: frac `In` PGFloating => null frac :--> null frac
round_ = unsafeFunction "round"

-- | >>> :{
-- let
--   expression :: Expression outer commons grp schemas params from (null 'PGfloat4)
--   expression = ceiling_ pi
-- in printSQL expression
-- :}
-- ceiling(pi())
ceiling_ :: frac `In` PGFloating => null frac :--> null frac
ceiling_ = unsafeFunction "ceiling"

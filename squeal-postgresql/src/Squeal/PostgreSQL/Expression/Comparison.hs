{-|
Module: Squeal.PostgreSQL.Expression.Comparison
Description: comparison functions and operators
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

comparison functions and operators
-}

{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , TypeInType
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Comparison
  ( -- * Comparison Operators
    (.==)
  , (./=)
  , (.>=)
  , (.<)
  , (.<=)
  , (.>)
    -- * Comparison Functions
  , greatest
  , least
    -- * Between
  , BetweenExpr
  , between
  , notBetween
  , betweenSymmetric
  , notBetweenSymmetric
    -- * Null Comparison
  , isDistinctFrom
  , isNotDistinctFrom
  , isTrue
  , isNotTrue
  , isFalse
  , isNotFalse
  , isUnknown
  , isNotUnknown
  ) where

import Data.ByteString

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | Comparison operations like `.==`, `./=`, `.>`, `.>=`, `.<` and `.<=`
-- will produce @NULL@s if one of their arguments is @NULL@.
--
-- >>> printSQL $ true .== null_
-- (TRUE = NULL)
(.==) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.==) = unsafeBinaryOp "="
infix 4 .==

-- | >>> printSQL $ true ./= null_
-- (TRUE <> NULL)
(./=) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(./=) = unsafeBinaryOp "<>"
infix 4 ./=

-- | >>> printSQL $ true .>= null_
-- (TRUE >= NULL)
(.>=) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.>=) = unsafeBinaryOp ">="
infix 4 .>=

-- | >>> printSQL $ true .< null_
-- (TRUE < NULL)
(.<) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.<) = unsafeBinaryOp "<"
infix 4 .<

-- | >>> printSQL $ true .<= null_
-- (TRUE <= NULL)
(.<=) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.<=) = unsafeBinaryOp "<="
infix 4 .<=

-- | >>> printSQL $ true .> null_
-- (TRUE > NULL)
(.>) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.>) = unsafeBinaryOp ">"
infix 4 .>

-- | >>> let expr = greatest [param @1] currentTimestamp
-- >>> printSQL expr
-- GREATEST(($1 :: timestamp with time zone), CURRENT_TIMESTAMP)
greatest :: FunctionVar ty ty ty
greatest = unsafeFunctionVar "GREATEST"

-- | >>> printSQL $ least [null_] currentTimestamp
-- LEAST(NULL, CURRENT_TIMESTAMP)
least :: FunctionVar ty ty ty
least = unsafeFunctionVar "LEAST"

{- |
A @RankNType@ for comparison expressions like `between`.
-}
type BetweenExpr
  =  forall grp lat with db params from ty
  .  Expression grp lat with db params from ty
  -> ( Expression grp lat with db params from ty
     , Expression grp lat with db params from ty ) -- ^ bounds
  -> Condition grp lat with db params from

unsafeBetweenExpr :: ByteString -> BetweenExpr
unsafeBetweenExpr fun a (x,y) = UnsafeExpression $
  renderSQL a <+> fun <+> renderSQL x <+> "AND" <+> renderSQL y

{- | >>> printSQL $ true `between` (null_, false)
TRUE BETWEEN NULL AND FALSE
-}
between :: BetweenExpr
between = unsafeBetweenExpr "BETWEEN"

{- | >>> printSQL $ true `notBetween` (null_, false)
TRUE NOT BETWEEN NULL AND FALSE
-}
notBetween :: BetweenExpr
notBetween = unsafeBetweenExpr "NOT BETWEEN"

{- | between, after sorting the comparison values

>>> printSQL $ true `betweenSymmetric` (null_, false)
TRUE BETWEEN SYMMETRIC NULL AND FALSE
-}
betweenSymmetric :: BetweenExpr
betweenSymmetric = unsafeBetweenExpr "BETWEEN SYMMETRIC"

{- | not between, after sorting the comparison values

>>> printSQL $ true `notBetweenSymmetric` (null_, false)
TRUE NOT BETWEEN SYMMETRIC NULL AND FALSE
-}
notBetweenSymmetric :: BetweenExpr
notBetweenSymmetric = unsafeBetweenExpr "NOT BETWEEN SYMMETRIC"

{- | not equal, treating null like an ordinary value

>>> printSQL $ true `isDistinctFrom` null_
(TRUE IS DISTINCT FROM NULL)
-}
isDistinctFrom :: Operator (null0 ty) (null1 ty) (null 'PGbool)
isDistinctFrom = unsafeBinaryOp "IS DISTINCT FROM"

{- | equal, treating null like an ordinary value

>>> printSQL $ true `isNotDistinctFrom` null_
(TRUE IS NOT DISTINCT FROM NULL)
-}
isNotDistinctFrom :: Operator (null0 ty) (null1 ty) (null 'PGbool)
isNotDistinctFrom = unsafeBinaryOp "IS NOT DISTINCT FROM"

{- | is true

>>> printSQL $ true & isTrue
(TRUE IS TRUE)
-}
isTrue :: null0 'PGbool --> null1 'PGbool
isTrue = unsafeRightOp "IS TRUE"

{- | is false or unknown

>>> printSQL $ true & isNotTrue
(TRUE IS NOT TRUE)
-}
isNotTrue :: null0 'PGbool --> null1 'PGbool
isNotTrue = unsafeRightOp "IS NOT TRUE"

{- | is false

>>> printSQL $ true & isFalse
(TRUE IS FALSE)
-}
isFalse :: null0 'PGbool --> null1 'PGbool
isFalse = unsafeRightOp "IS FALSE"

{- | is true or unknown

>>> printSQL $ true & isNotFalse
(TRUE IS NOT FALSE)
-}
isNotFalse :: null0 'PGbool --> null1 'PGbool
isNotFalse = unsafeRightOp "IS NOT FALSE"

{- | is unknown

>>> printSQL $ true & isUnknown
(TRUE IS UNKNOWN)
-}
isUnknown :: null0 'PGbool --> null1 'PGbool
isUnknown = unsafeRightOp "IS UNKNOWN"

{- | is true or false

>>> printSQL $ true & isNotUnknown
(TRUE IS NOT UNKNOWN)
-}
isNotUnknown :: null0 'PGbool --> null1 'PGbool
isNotUnknown = unsafeRightOp "IS NOT UNKNOWN"

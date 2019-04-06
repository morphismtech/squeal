{-# LANGUAGE
    OverloadedStrings
  , TypeInType
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Comparison
  ( (.==)
  , (./=)
  , (.>=)
  , (.<)
  , (.<=)
  , (.>)
  , greatest
  , least
  , between
  , notBetween
  , betweenSymmetric
  , notBetweenSymmetric
  , isDistinctFrom
  , isNotDistinctFrom
  , isTrue
  , isNotTrue
  , isFalse
  , isNotFalse
  , isUnknown
  , isNotUnknown
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

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

-- | >>> let expr = greatest [param @1] currentTimestamp :: Expression outer commons grp schemas '[ 'NotNull 'PGtimestamptz] from ('NotNull 'PGtimestamptz)
-- >>> printSQL expr
-- GREATEST(($1 :: timestamp with time zone), CURRENT_TIMESTAMP)
greatest :: FunctionVar ty ty ty
greatest = unsafeFunctionVar "GREATEST"

-- | >>> printSQL $ least [null_] currentTimestamp
-- LEAST(NULL, CURRENT_TIMESTAMP)
least :: FunctionVar ty ty ty
least = unsafeFunctionVar "LEAST"

{- | >>> printSQL $ true `between` (null_, false)
TRUE BETWEEN NULL AND FALSE
-}
between
  :: Expression outer commons grp schemas params from (null ty)
  -> ( Expression outer commons grp schemas params from (null ty)
     , Expression outer commons grp schemas params from (null ty) ) -- ^ bounds
  -> Condition outer commons grp schemas params from
between a (x,y) = UnsafeExpression $ renderSQL a <+> "BETWEEN"
  <+> renderSQL x <+> "AND" <+> renderSQL y

{- | >>> printSQL $ true `notBetween` (null_, false)
TRUE NOT BETWEEN NULL AND FALSE
-}
notBetween
  :: Expression outer commons grp schemas params from (null ty)
  -> ( Expression outer commons grp schemas params from (null ty)
     , Expression outer commons grp schemas params from (null ty) ) -- ^ bounds
  -> Condition outer commons grp schemas params from
notBetween a (x,y) = UnsafeExpression $ renderSQL a <+> "NOT BETWEEN"
  <+> renderSQL x <+> "AND" <+> renderSQL y

{- | between, after sorting the comparison values

>>> printSQL $ true `betweenSymmetric` (null_, false)
TRUE BETWEEN SYMMETRIC NULL AND FALSE
-}
betweenSymmetric
  :: Expression outer commons grp schemas params from (null ty)
  -> ( Expression outer commons grp schemas params from (null ty)
     , Expression outer commons grp schemas params from (null ty) ) -- ^ bounds
  -> Condition outer commons grp schemas params from
betweenSymmetric a (x,y) = UnsafeExpression $ renderSQL a
  <+> "BETWEEN SYMMETRIC" <+> renderSQL x <+> "AND" <+> renderSQL y

{- | not between, after sorting the comparison values

>>> printSQL $ true `notBetweenSymmetric` (null_, false)
TRUE NOT BETWEEN SYMMETRIC NULL AND FALSE
-}
notBetweenSymmetric
  :: Expression outer commons grp schemas params from (null ty)
  -> ( Expression outer commons grp schemas params from (null ty)
     , Expression outer commons grp schemas params from (null ty) ) -- ^ bounds
  -> Condition outer commons grp schemas params from
notBetweenSymmetric a (x,y) = UnsafeExpression $ renderSQL a
  <+> "NOT BETWEEN SYMMETRIC" <+> renderSQL x <+> "AND" <+> renderSQL y

{- | not equal, treating null like an ordinary value

>>> printSQL $ true `isDistinctFrom` null_
(TRUE IS DISTINCT FROM NULL)
-}
isDistinctFrom :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
isDistinctFrom = unsafeBinaryOp "IS DISTINCT FROM"

{- | equal, treating null like an ordinary value

>>> printSQL $ true `isNotDistinctFrom` null_
(TRUE IS NOT DISTINCT FROM NULL)
-}
isNotDistinctFrom :: Operator (null0 ty) (null1 ty) ('NotNull 'PGbool)
isNotDistinctFrom = unsafeBinaryOp "IS NOT DISTINCT FROM"

{- | is true

>>> printSQL $ true & isTrue
(TRUE IS TRUE)
-}
isTrue :: null0 'PGbool :--> null1 'PGbool
isTrue = unsafeUnaryOpR "IS TRUE"

{- | is false or unknown

>>> printSQL $ true & isNotTrue
(TRUE IS NOT TRUE)
-}
isNotTrue :: null0 'PGbool :--> null1 'PGbool
isNotTrue = unsafeUnaryOpR "IS NOT TRUE"

{- | is false

>>> printSQL $ true & isFalse
(TRUE IS FALSE)
-}
isFalse :: null0 'PGbool :--> null1 'PGbool
isFalse = unsafeUnaryOpR "IS FALSE"

{- | is true or unknown

>>> printSQL $ true & isNotFalse
(TRUE IS NOT FALSE)
-}
isNotFalse :: null0 'PGbool :--> null1 'PGbool
isNotFalse = unsafeUnaryOpR "IS NOT FALSE"

{- | is unknown

>>> printSQL $ true & isUnknown
(TRUE IS UNKNOWN)
-}
isUnknown :: null0 'PGbool :--> null1 'PGbool
isUnknown = unsafeUnaryOpR "IS UNKNOWN"

{- | is true or false

>>> printSQL $ true & isNotUnknown
(TRUE IS NOT UNKNOWN)
-}
isNotUnknown :: null0 'PGbool :--> null1 'PGbool
isNotUnknown = unsafeUnaryOpR "IS NOT UNKNOWN"

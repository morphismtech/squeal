{-|
Module: Squeal.PostgreSQL.Expression.Range
Description: Range types and functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Range types and functions
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DeriveFoldable
  , DerivingStrategies
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , PatternSynonyms
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Range
  ( -- * Range
    Range (..)
  , (<=..<=), (<..<), (<=..<), (<..<=)
  , moreThan, atLeast, lessThan, atMost
  , singleton, whole
  , Bound (..)
    -- * Range Function
    -- ** Range Construction
  , range
    -- ** Range Operator
  , (.<@)
  , (@>.)
  , (<<@)
  , (@>>)
  , (&<)
  , (&>)
  , (-|-)
  , (@+)
  , (@*)
  , (@-)
    -- ** Range Function
  , lowerBound
  , upperBound
  , isEmpty
  , lowerInc
  , lowerInf
  , upperInc
  , upperInf
  , rangeMerge
  ) where

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type hiding (bool)
import Squeal.PostgreSQL.Type.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL (tstzrange, numrange, int4range, now, printSQL)

-- | Construct a `range`
--
-- >>> printSQL $ range tstzrange (atLeast now)
-- tstzrange(now(), NULL, '[)')
-- >>> printSQL $ range numrange (0 <=..< 2*pi)
-- numrange((0.0 :: numeric), ((2.0 :: numeric) * pi()), '[)')
-- >>> printSQL $ range int4range Empty
-- ('empty' :: int4range)
range
  :: TypeExpression db (null ('PGrange ty))
  -- ^ range type
  -> Range (Expression grp lat with db params from ('NotNull ty))
  -- ^ range of values
  -> Expression grp lat with db params from (null ('PGrange ty))
range ty = \case
  Empty -> UnsafeExpression $ parenthesized
    (emp <+> "::" <+> renderSQL ty)
  NonEmpty l u -> UnsafeExpression $ renderSQL ty <> parenthesized
    (commaSeparated (args l u))
  where
    emp = singleQuote <> "empty" <> singleQuote
    args l u = [arg l, arg u, singleQuote <> bra l <> ket u <> singleQuote]
    singleQuote = "\'"
    arg = \case
      Infinite -> "NULL"; Closed x -> renderSQL x; Open x -> renderSQL x
    bra = \case Infinite -> "("; Closed _ -> "["; Open _ -> "("
    ket = \case Infinite -> ")"; Closed _ -> "]"; Open _ -> ")"

-- | The type of `Bound` for a `Range`.
data Bound x
  = Infinite -- ^ unbounded
  | Closed x -- ^ inclusive
  | Open x -- ^ exclusive
  deriving
    ( Eq, Ord, Show, Read, GHC.Generic
    , Functor, Foldable, Traversable )

-- | A `Range` datatype that comprises connected subsets of
-- the real line.
data Range x = Empty | NonEmpty (Bound x) (Bound x)
  deriving
    ( Eq, Ord, Show, Read, GHC.Generic
    , Functor, Foldable, Traversable )
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
-- | `PGrange` @(@`PG` @hask)@
instance IsPG hask => IsPG (Range hask) where
  type PG (Range hask) = 'PGrange (PG hask)

-- | Finite `Range` constructor
(<=..<=), (<..<), (<=..<), (<..<=) :: x -> x -> Range x
infix 4 <=..<=, <..<, <=..<, <..<=
x <=..<= y = NonEmpty (Closed x) (Closed y)
x <..< y = NonEmpty (Open x) (Open y)
x <=..< y = NonEmpty (Closed x) (Open y)
x <..<= y = NonEmpty (Open x) (Closed y)

-- | Half-infinite `Range` constructor
moreThan, atLeast, lessThan, atMost :: x -> Range x
moreThan x = NonEmpty (Open x) Infinite
atLeast x = NonEmpty (Closed x) Infinite
lessThan x = NonEmpty Infinite (Open x)
atMost x = NonEmpty Infinite (Closed x)

-- | A point on the line
singleton :: x -> Range x
singleton x = x <=..<= x

-- | The `whole` line
whole :: Range x
whole = NonEmpty Infinite Infinite

-- | range is contained by
(.<@) :: Operator ('NotNull ty) (null ('PGrange ty)) ('Null 'PGbool)
(.<@) = unsafeBinaryOp "<@"

-- | contains range
(@>.) :: Operator (null ('PGrange ty)) ('NotNull ty) ('Null 'PGbool)
(@>.) = unsafeBinaryOp "<@"

-- | strictly left of,
-- return false when an empty range is involved
(<<@) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(<<@) = unsafeBinaryOp "<<"

-- | strictly right of,
-- return false when an empty range is involved
(@>>) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(@>>) = unsafeBinaryOp ">>"

-- | does not extend to the right of,
-- return false when an empty range is involved
(&<) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(&<) = unsafeBinaryOp "&<"

-- | does not extend to the left of,
-- return false when an empty range is involved
(&>) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(&>) = unsafeBinaryOp "&>"

-- | is adjacent to, return false when an empty range is involved
(-|-) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(-|-) = unsafeBinaryOp "-|-"

-- | union, will fail if the resulting range would
-- need to contain two disjoint sub-ranges
(@+) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(@+) = unsafeBinaryOp "+"

-- | intersection
(@*) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(@*) = unsafeBinaryOp "*"

-- | difference, will fail if the resulting range would
-- need to contain two disjoint sub-ranges
(@-) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(@-) = unsafeBinaryOp "-"

-- | lower bound of range
lowerBound :: null ('PGrange ty) --> 'Null ty
lowerBound = unsafeFunction "lower"

-- | upper bound of range
upperBound :: null ('PGrange ty) --> 'Null ty
upperBound = unsafeFunction "upper"

-- | is the range empty?
isEmpty :: null ('PGrange ty) --> 'Null 'PGbool
isEmpty = unsafeFunction "isempty"

-- | is the lower bound inclusive?
lowerInc :: null ('PGrange ty) --> 'Null 'PGbool
lowerInc = unsafeFunction "lower_inc"

-- | is the lower bound infinite?
lowerInf :: null ('PGrange ty) --> 'Null 'PGbool
lowerInf = unsafeFunction "lower_inf"

-- | is the upper bound inclusive?
upperInc :: null ('PGrange ty) --> 'Null 'PGbool
upperInc = unsafeFunction "upper_inc"

-- | is the upper bound infinite?
upperInf :: null ('PGrange ty) --> 'Null 'PGbool
upperInf = unsafeFunction "upper_inf"

-- | the smallest range which includes both of the given ranges
rangeMerge ::
  '[null ('PGrange ty), null ('PGrange ty)]
  ---> null ('PGrange ty)
rangeMerge = unsafeFunctionN "range_merge"

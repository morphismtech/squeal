{-|
Module: Squeal.PostgreSQL.Expression.Range
Description: range types and functions
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
  ( range
  , Range (..)
  , (<=..<=), (<..<), (<=..<), (<..<=)
  , moreThan, atLeast, lessThan, atMost
  , singleton, whole
  , Bound (..)
  , closed, open
  , (<@.)
  , (@>.)
  , (&&.)
  , (<<.)
  , (>>.)
  , (&<.)
  , (&>.)
  , (=|=.)
  , (+.)
  , (*.)
  , (-.)
  , lowerBound
  , upperBound
  , isEmpty
  , lowerInc
  , lowerInf
  , upperInc
  , upperInf
  , rangeMerge
  ) where

import Data.Bool

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type hiding (bool)
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | Construct a `range`
--
-- >>> printSQL $ range tstzrange (atLeast now)
-- tstzrange(now(), NULL, '[)')
-- >>> printSQL $ range numrange (0 <=..< 2*pi)
-- numrange(0, (2 * pi()), '[)')
-- >>> printSQL $ range int4range Empty
-- (empty :: int4range)
range
  :: TypeExpression schemas (null ('PGrange ty))
  -> Range (Expression outer commons grp schemas params from ('NotNull ty))
  -> Expression outer commons grp schemas params from (null ('PGrange ty))
range ty = \case
  Empty -> UnsafeExpression $ parenthesized
    ("empty" <+> "::" <+> renderSQL ty)
  NonEmpty l u -> UnsafeExpression $ renderSQL ty <> parenthesized
    (commaSeparated [renderArg l, renderArg u, renderClopen l u])
    where
      renderArg (Bound _ x) = maybe "NULL" renderSQL x
      renderClopen (Bound l' _) (Bound u' _) =
        "\'" <> bool "(" "[" l' <> bool ")" "]" u' <> "\'"

data Bound x = Bound
  { closedBound :: Bool
  , getBound :: Maybe x
  } deriving
    ( Eq, Ord, Show, Read, GHC.Generic
    , Functor, Foldable, Traversable )
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
closed, open :: Maybe x -> Bound x
closed = Bound True
open = Bound False

data Range x
  = Empty
  | NonEmpty (Bound x) (Bound x)
  deriving
    ( Eq, Ord, Show, Read, GHC.Generic
    , Functor, Foldable, Traversable )
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
type instance PG (Range x) = 'PGrange (PG x)

(<=..<=), (<..<), (<=..<), (<..<=) :: x -> x -> Range x
infix 4 <=..<=, <..<, <=..<, <..<=
x <=..<= y = NonEmpty (closed (Just x)) (closed (Just y))
x <..< y = NonEmpty (open (Just x)) (open (Just y))
x <=..< y = NonEmpty (closed (Just x)) (open (Just y))
x <..<= y = NonEmpty (open (Just x)) (closed (Just y))

moreThan, atLeast, lessThan, atMost :: x -> Range x
moreThan x = NonEmpty (open (Just x)) (open Nothing)
atLeast x = NonEmpty (closed (Just x)) (open Nothing)
lessThan x = NonEmpty (open Nothing) (open (Just x))
atMost x = NonEmpty (open Nothing) (closed (Just x))

singleton :: x -> Range x
singleton x = x <=..<= x

whole :: Range x
whole = NonEmpty (open Nothing) (open Nothing)

(<@.) :: Operator ('NotNull ty) (null ('PGrange ty)) ('Null 'PGbool)
(<@.) = unsafeBinaryOp "<@"

(@>.) :: Operator (null ('PGrange ty)) ('NotNull ty) ('Null 'PGbool)
(@>.) = unsafeBinaryOp "<@"

(&&.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(&&.) = unsafeBinaryOp "&&"

(<<.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(<<.) = unsafeBinaryOp "<<"

(>>.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(>>.) = unsafeBinaryOp ">>"

(&<.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(&<.) = unsafeBinaryOp "&<"

(&>.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(&>.) = unsafeBinaryOp "&>"

(=|=.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(=|=.) = unsafeBinaryOp "&>"

(+.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(+.) = unsafeBinaryOp "+"

(*.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(*.) = unsafeBinaryOp "*"

(-.) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(-.) = unsafeBinaryOp "-"

lowerBound :: null ('PGrange ty) :--> null ty
lowerBound = unsafeFunction "lower"

upperBound :: null ('PGrange ty) :--> null ty
upperBound = unsafeFunction "upper"

isEmpty :: null ('PGrange ty) :--> 'Null 'PGbool
isEmpty = unsafeFunction "isempty"

lowerInc :: null ('PGrange ty) :--> 'Null 'PGbool
lowerInc = unsafeFunction "lower_inc"

lowerInf :: null ('PGrange ty) :--> 'Null 'PGbool
lowerInf = unsafeFunction "lower_inf"

upperInc :: null ('PGrange ty) :--> 'Null 'PGbool
upperInc = unsafeFunction "upper_inc"

upperInf :: null ('PGrange ty) :--> 'Null 'PGbool
upperInf = unsafeFunction "upper_inf"

rangeMerge :: FunctionN
  '[ null ('PGrange ty), null ('PGrange ty)] (null ('PGrange ty))
rangeMerge = unsafeFunctionN "range_merge"

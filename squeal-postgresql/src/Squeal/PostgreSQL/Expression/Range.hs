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
  , greaterThan, atLeast, lessThan, atMost
  , singleton, whole
  , Bound (..)
  , closed, open
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
range
  :: TypeExpression schemas (null ('PGrange ty))
  -> Range (Expression outer commons grp schemas params from ('NotNull ty))
  -> Expression outer commons grp schemas params from (null ('PGrange ty))
range ty = \case
  Empty -> UnsafeExpression "empty"
  NonEmpty l u -> UnsafeExpression $ renderSQL ty <> parenthesized
    (commaSeparated [renderArg l, renderArg u, renderClopen l u])
    where
      renderArg (Bound _ x) = maybe "NULL" renderSQL x
      clopenLeft = bool "(" "["
      clopenRight = bool ")" "]"
      renderClopen (Bound l' _) (Bound u' _) =
        "\'" <> clopenLeft l' <> clopenRight u' <> "\'"

data Bound x = Bound
  { closedBound :: Bool
  , getBound :: Maybe x
  } deriving
    ( Eq, Ord, Show, Read, GHC.Generic
    , Functor, Foldable, Traversable )
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
closed, open :: Maybe x -> Bound x
closed x = Bound True x
open x = Bound False x

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

greaterThan, atLeast, lessThan, atMost :: x -> Range x
greaterThan x = NonEmpty (open (Just x)) (open Nothing)
atLeast x = NonEmpty (closed (Just x)) (open Nothing)
lessThan x = NonEmpty (open Nothing) (open (Just x))
atMost x = NonEmpty (open Nothing) (closed (Just x))

singleton :: x -> Range x
singleton x = x <=..<= x

whole :: Range x
whole = NonEmpty (open Nothing) (open Nothing)

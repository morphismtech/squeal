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
  , halfGT, halfGTE, halfLT, halfLTE
  , point
  , Bound (..)
  , closed, open
  ) where

import Data.Bool

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | >>> printSQL $ range (halfGTE now)
-- [now(), )
-- >>> printSQL $ range (0 <..<= (pi & astype numeric))
-- (0, (pi() :: numeric)]
range
  :: Range (Expression outer commons grp schemas params from ('NotNull ty))
  -> Expression outer commons grp schemas params from (null ('PGrange ty))
range = UnsafeExpression . renderSQL

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

instance RenderSQL x => RenderSQL (Range x) where
  renderSQL = \case
    Empty -> "empty"
    NonEmpty l u -> commaSeparated [renderLower l, renderUpper u]
      where
        renderLower (Bound isclosed x) =
          bool "(" "[" isclosed <> maybe "" renderSQL x
        renderUpper (Bound isclosed x) =
          maybe "" renderSQL x <> bool ")" "]" isclosed

(<=..<=), (<..<), (<=..<), (<..<=) :: x -> x -> Range x
x <=..<= y = NonEmpty (closed (Just x)) (closed (Just y))
x <..< y = NonEmpty (open (Just x)) (open (Just y))
x <=..< y = NonEmpty (closed (Just x)) (open (Just y))
x <..<= y = NonEmpty (open (Just x)) (closed (Just y))

halfGT, halfGTE, halfLT, halfLTE :: x -> Range x
halfGT x = NonEmpty (open (Just x)) (open Nothing)
halfGTE x = NonEmpty (closed (Just x)) (open Nothing)
halfLT x = NonEmpty (open Nothing) (open (Just x))
halfLTE x = NonEmpty (open Nothing) (closed (Just x))

point :: x -> Range x
point x = x <=..<= x

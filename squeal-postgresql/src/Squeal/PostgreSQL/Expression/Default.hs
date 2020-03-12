{-|
Module: Squeal.PostgreSQL.Expression.Default
Description: Default
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Optional expressions that may be `Default` or not.
-}

{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , PatternSynonyms
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Default
  ( -- * Default
    Optional (..)
  , mapOptional
  , pattern NotDefault
  ) where

import Data.Kind
import Generics.SOP

import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- | `Optional` is either `Default` or a value, parameterized by an appropriate
-- `Optionality`.
data Optional (expr :: k -> Type) (ty :: (Optionality, k)) where
  -- | Use the `Default` value for a column.
  Default :: Optional expr ('Def :=> ty)
  -- | `Set` a value for a column.
  Set :: expr ty -> Optional expr (def :=> ty)

instance (forall x. RenderSQL (expr x)) => RenderSQL (Optional expr ty) where
  renderSQL = \case
    Default -> "DEFAULT"
    Set x -> renderSQL x

-- | Map a function over an `Optional` expression.
mapOptional
  :: (expr x -> expr y)
  -> Optional expr (def :=> x)
  -> Optional expr (def :=> y)
mapOptional f = \case
  Default -> Default
  Set x -> Set (f x)

-- | `NotDefault` pattern analagous to `Just`.
pattern NotDefault :: ty -> Optional I ('Def :=> ty)
pattern NotDefault x = Set (I x)

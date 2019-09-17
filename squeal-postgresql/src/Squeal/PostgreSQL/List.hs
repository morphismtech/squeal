{-|
Module: Squeal.PostgreSQL.List
Description: List related types and functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Haskell singly-linked lists are very powerful. This module
provides functionality for type-level lists, heterogeneous
lists and type aligned lists.
-}

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.List
  ( SOP.NP (..)
  , (*:)
  , Path (..)
  , CFunctor (..)
  , CFoldable (..)
  , CFree (..)
  , Join
  , disjoin
  , Additional (..)
  , Elem
  , In
  , Length
  ) where

import Control.Category.Free
import Data.Function ((&))
import Data.Kind
import Data.Type.Bool
import GHC.TypeLits

import Generics.SOP as SOP

-- import Squeal.PostgreSQL.Render

-- | `Join` is simply promoted `++` and is used in @JOIN@s in
-- `Squeal.PostgreSQL.Query.FromClause`s.
type family Join xs ys where
  Join '[] ys = ys
  Join (x ': xs) ys = x ': Join xs ys

-- | `disjoin` is a utility function for splitting an `NP` list into pieces.
disjoin
 :: forall xs ys expr. SListI xs
 => NP expr (Join xs ys)
 -> (NP expr xs, NP expr ys)
disjoin = case sList @xs of
  SNil -> \ys -> (Nil, ys)
  SCons -> \(x :* xsys) ->
    case disjoin xsys of (xs,ys) -> (x :* xs, ys)

-- | The `Additional` class is for appending
-- type-level list parameterized constructors such as `NP`,
-- `Squeal.PostgreSQL.Query.Selection`, and `Squeal.PostgreSQL.Query.FromClause`.
class Additional expr where
  also :: expr ys -> expr xs -> expr (Join xs ys)
instance Additional (NP expr) where
  also ys = \case
    Nil -> ys
    x :* xs -> x :* (xs & also ys)
{-
-- | An `AlignedList` is a type-aligned list or free category.
data AlignedList p x0 x1 where
  Done :: AlignedList p x x
  (:>>) :: p x0 x1 -> AlignedList p x1 x2 -> AlignedList p x0 x2
infixr 7 :>>
instance Category (AlignedList p) where
  id = Done
  (.) list = \case
    Done -> list
    step :>> steps -> step :>> (steps >>> list)
instance (forall t0 t1. RenderSQL (p t0 t1))
  => RenderSQL (AlignedList p x0 x1) where
    renderSQL = \case
      Done -> ""
      step :>> Done -> renderSQL step
      step :>> steps -> renderSQL step <> ", " <> renderSQL steps

-- | `extractList` turns an `AlignedList` into a standard list.
extractList :: (forall a0 a1. p a0 a1 -> b) -> AlignedList p x0 x1 -> [b]
extractList f = \case
  Done -> []
  step :>> steps -> (f step):extractList f steps

-- | `mapAligned` applies a function to each element of an `AlignedList`.
mapAligned
  :: (forall z0 z1. p z0 z1 -> q z0 z1)
  -> AlignedList p x0 x1
  -> AlignedList q x0 x1
mapAligned f = \case
  Done -> Done
  x :>> xs -> f x :>> mapAligned f xs

-- | A `single` step.
single :: p x0 x1 -> AlignedList p x0 x1
single step = step :>> Done
-}
-- | A useful operator for ending an `NP` list of length at least 2 without `Nil`.
(*:) :: f x -> f y -> NP f '[x,y]
x *: y = x :* y :* Nil
infixl 9 *:

-- | @Elem@ is a promoted `Data.List.elem`.
type family Elem x xs where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

-- | @In x xs@ is a constraint that proves that @x@ is in @xs@.
type family In x xs :: Constraint where
  In x xs = If (Elem x xs) (() :: Constraint)
    (TypeError ('ShowType x ':<>: 'Text " is not in " ':<>: 'ShowType xs))

{- | Calculate the `Length` of a type level list

>>> :kind! Length '[Char,String,Bool,Double]
Length '[Char,String,Bool,Double] :: Nat
= 4
-}
type family Length (xs :: [k]) :: Nat where
  Length (x : xs) = 1 + Length xs
  Length '[] = 0

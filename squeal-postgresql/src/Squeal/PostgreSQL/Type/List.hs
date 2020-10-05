{-|
Module: Squeal.PostgreSQL.Type.List
Description: list related types and functions
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

module Squeal.PostgreSQL.Type.List
  ( -- * Heterogeneous List
    SOP.NP (..)
  , (*:)
  , one
  , Join
  , disjoin
  , Additional (..)
    -- * Path
  , Path (..)
    -- * Type Level List
  , Elem
  , In
  , Length
  , Sublist
  ) where

import Control.Category.Free
import Data.Function ((&))
import Data.Kind
import Data.Type.Bool
import GHC.TypeLits

import Generics.SOP as SOP

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

-- | A useful operator for ending an `NP` list of length
-- at least 2 without `Nil`
(*:) :: f x -> f y -> NP f '[x,y]
x *: y = x :* y :* Nil
infixl 8 *:

-- | A list of length `one`.
one :: f x -> NP f '[x]
one f = f :* Nil

-- | @Elem@ is a promoted `Data.List.elem`.
type family Elem x xs where
  Elem x '[] = 'False
  Elem x (x ': _) = 'True
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
  Length '[] = 0
  Length (_ : xs) = 1 + Length xs

{- | `Sublist` checks that one type level list is a sublist of another,
with the same ordering.

>>> :kind! Sublist '[1,2,3] '[4,5,6]
Sublist '[1,2,3] '[4,5,6] :: Bool
= 'False
>>> :kind! Sublist '[1,2,3] '[1,2,3,4]
Sublist '[1,2,3] '[1,2,3,4] :: Bool
= 'True
>>> :kind! Sublist '[1,2,3] '[0,1,0,2,0,3]
Sublist '[1,2,3] '[0,1,0,2,0,3] :: Bool
= 'True
>>> :kind! Sublist '[1,2,3] '[3,2,1]
Sublist '[1,2,3] '[3,2,1] :: Bool
= 'False
-}
type family Sublist (xs :: [k]) (ys :: [k]) :: Bool where
  Sublist '[] ys = 'True
  Sublist (x ': xs) '[] = 'False
  Sublist (x ': xs) (x ': ys) = Sublist xs ys
  Sublist (x ': xs) (y ': ys) = Sublist (x ': xs) ys

{-|
Module: Squeal.PostgreSQL.Expression.Parameter
Description: out-of-line parameters
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

out-of-line parameters
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Parameter
  ( -- * Parameter
    HasParameter (parameter)
  , param
  ) where

import GHC.TypeLits

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- | A `HasParameter` constraint is used to indicate a value that is
supplied externally to a SQL statement.
`Squeal.PostgreSQL.Session.manipulateParams`,
`Squeal.PostgreSQL.Session.queryParams` and
`Squeal.PostgreSQL.Session.traversePrepared` support specifying data values
separately from the SQL command string, in which case `param`s are used to
refer to the out-of-line data values.
-}
class KnownNat n => HasParameter
  (n :: Nat)
  (params :: [NullType])
  (ty :: NullType)
  | n params -> ty where
    -- | `parameter` takes a `Nat` using type application and a `TypeExpression`.
    --
    -- >>> printSQL (parameter @1 int4)
    -- ($1 :: int4)
    parameter
      :: TypeExpression db ty
      -> Expression grp lat with db params from ty
    parameter ty = UnsafeExpression $ parenthesized $
      "$" <> renderNat @n <+> "::"
        <+> renderSQL ty
instance {-# OVERLAPPING #-} params ~ (x ': xs) => HasParameter 1 params x
instance {-# OVERLAPPABLE #-}
  (KnownNat n, HasParameter (n-1) xs x, params ~ (y ': xs))
  => HasParameter n params x

-- | `param` takes a `Nat` using type application and for basic types,
-- infers a `TypeExpression`.
--
-- >>> printSQL (param @1 @('Null 'PGint4))
-- ($1 :: int4)
param
  :: forall n ty lat with db params from grp
   . (NullTyped db ty, HasParameter n params ty)
  => Expression grp lat with db params from ty -- ^ param
param = parameter @n (nulltype @db)

{-|
Module: Squeal.PostgreSQL.Expression.Parameter
Description: Parameters
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Parameters, out-of-line data values
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , KindSignatures
  , MultiParamTypeClasses
  , OverloadedStrings
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
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- | A `HasParameter` constraint is used to indicate a value that is
supplied externally to a SQL statement.
`Squeal.PostgreSQL.PQ.manipulateParams`,
`Squeal.PostgreSQL.PQ.queryParams` and
`Squeal.PostgreSQL.PQ.traversePrepared` support specifying data values
separately from the SQL command string, in which case `param`s are used to
refer to the out-of-line data values.
-}
class KnownNat n => HasParameter
  (n :: Nat)
  (params :: [NullityType])
  (ty :: NullityType)
  | n params -> ty where
    -- | `parameter` takes a `Nat` using type application and a `TypeExpression`.
    --
    -- >>> let expr = parameter @1 int4 :: Expression outer '[] grp db '[ 'Null 'PGint4] from ('Null 'PGint4)
    -- >>> printSQL expr
    -- ($1 :: int4)
    parameter
      :: TypeExpression db ty
      -> Expression outer commons grp db params from ty
    parameter ty = UnsafeExpression $ parenthesized $
      "$" <> renderNat @n <+> "::"
        <+> renderSQL ty
instance {-# OVERLAPPING #-} HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

-- | `param` takes a `Nat` using type application and for basic types,
-- infers a `TypeExpression`.
--
-- >>> let expr = param @1 :: Expression outer commons grp db '[ 'Null 'PGint4] from ('Null 'PGint4)
-- >>> printSQL expr
-- ($1 :: int4)
param
  :: forall n ty outer commons db params from grp
   . (NullityTyped db ty, HasParameter n params ty)
  => Expression outer commons grp db params from ty -- ^ param
param = parameter @n (nullitytype @db)

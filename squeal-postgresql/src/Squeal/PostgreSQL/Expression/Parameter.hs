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
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Parameter
  ( -- * Parameter
    HasParameter (parameter)
  , param
  ) where

import Data.Kind (Constraint)
import GHC.Exts (Any)
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
class KnownNat ix => HasParameter
  (ix :: Nat)
  (params :: [NullType])
  (ty :: NullType)
  | ix params -> ty where
    -- | `parameter` takes a `Nat` using type application and a `TypeExpression`.
    --
    -- >>> printSQL (parameter @1 int4)
    -- ($1 :: int4)
    parameter
      :: TypeExpression db ty
      -> Expression grp lat with db params from ty
    parameter ty = UnsafeExpression $ parenthesized $
      "$" <> renderNat @ix <+> "::"
        <+> renderSQL ty

-- we could do the check for 0 in @HasParameter'@, but this way forces checking 'ix' before delegating,
-- which has the nice effect of ambiguous 'ix' errors mentioning 'HasParameter' instead of @HasParameter'@
instance {-# OVERLAPS #-} (TypeError ('Text "Tried to get the param at index 0, but params are 1-indexed"), x ~ Any) => HasParameter 0 params x
instance {-# OVERLAPS #-} (KnownNat ix, HasParameter' ix params ix params x) => HasParameter ix params x

-- | @HasParameter'@ is an implementation detail of 'HasParameter' allowing us to
-- include the full parameter list in our errors. Generally speaking it shouldn't leak to users
-- of the library
class KnownNat ix => HasParameter'
  (originalIx :: Nat)
  (allParams :: [NullType])
  (ix :: Nat)
  (params :: [NullType])
  (ty :: NullType)
  | ix params -> ty where
instance {-# OVERLAPS #-}
  ( params ~ (y ': xs)
  , y ~ x -- having a separate 'y' type variable is required for 'ParamTypeMismatchError'
  , ParamOutOfBoundsError originalIx allParams params
  , ParamTypeMismatchError originalIx allParams x y
  ) => HasParameter' originalIx allParams 1 params x
instance {-# OVERLAPS #-}
  ( KnownNat ix
  , HasParameter' originalIx allParams (ix-1) xs x
  , params ~ (y ': xs)
  , ParamOutOfBoundsError originalIx allParams params
  )
  => HasParameter' originalIx allParams ix params x

-- | @ParamOutOfBoundsError@ reports a nicer error with more context when we try to do an out-of-bounds lookup successfully do a lookup but
-- find a different field than we expected, or when we find ourself out of bounds
type family ParamOutOfBoundsError (originalIx :: Nat) (allParams :: [NullType]) (params :: [NullType]) :: Constraint where
  ParamOutOfBoundsError originalIx allParams '[] = TypeError
    ('Text "Index " ':<>: 'ShowType originalIx ':<>: 'Text " is out of bounds in 1-indexed parameter list:" ':$$: 'ShowType allParams)
  ParamOutOfBoundsError _ _ _ = ()

-- | @ParamTypeMismatchError@ reports a nicer error with more context when we successfully do a lookup but
-- find a different field than we expected, or when we find ourself out of bounds
type family ParamTypeMismatchError  (originalIx :: Nat) (allParams :: [NullType]) (found :: NullType) (expected :: NullType) :: Constraint where
  ParamTypeMismatchError _ _ found found = ()
  ParamTypeMismatchError originalIx allParams found expected = TypeError
    (     'Text "Type mismatch when looking up param at index " ':<>: 'ShowType originalIx
    ':$$: 'Text "in 1-indexed parameter list:"
    ':$$: 'Text "  " ':<>: 'ShowType allParams
    ':$$: 'Text ""
    ':$$: 'Text "Expected: " ':<>: 'ShowType expected
    ':$$: 'Text "But found: " ':<>: 'ShowType found
    ':$$: 'Text ""
    )

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

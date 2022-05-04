{-|
Module: Squeal.PostgreSQL.Expression.Null
Description: null expressions and handlers
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

null expressions and handlers
-}

{-# LANGUAGE
    DataKinds
  , KindSignatures
  , OverloadedStrings
  , RankNTypes
  , TypeFamilies
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Null
  ( -- * Null
    null_
  , just_
  , unsafeNotNull
  , monoNotNull
  , coalesce
  , fromNull
  , isNull
  , isNotNull
  , matchNull
  , nullIf
  , CombineNullity
    -- deprecated
  , notNull
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | analagous to `Nothing`
--
-- >>> printSQL null_
-- NULL
null_ :: Expr ('Null ty)
null_ = UnsafeExpression "NULL"

-- | analagous to `Just`
--
-- >>> printSQL $ just_ true
-- TRUE
just_ :: 'NotNull ty --> 'Null ty
just_ = UnsafeExpression . renderSQL

-- | Deprecated, use `just_` instead.
{-# DEPRECATED notNull "use just_ instead" #-}
notNull :: 'NotNull ty --> 'Null ty
notNull = UnsafeExpression . renderSQL

-- | Analagous to `Data.Maybe.fromJust` inverse to `notNull`,
-- useful when you know an `Expression` is `NotNull`,
-- because, for instance, you've filtered out @NULL@
-- values in a column.
unsafeNotNull :: 'Null ty --> 'NotNull ty
unsafeNotNull = UnsafeExpression . renderSQL

-- | Some expressions are null polymorphic which may raise
-- inference issues. Use `monoNotNull` to fix their
-- nullity as `NotNull`.
monoNotNull
  :: (forall null. Expression grp lat with db params from (null ty))
  -- ^ null polymorphic
  -> Expression grp lat with db params from ('NotNull ty)
monoNotNull x = x

-- | return the leftmost value which is not NULL
--
-- >>> printSQL $ coalesce [null_, true] false
-- COALESCE(NULL, TRUE, FALSE)
coalesce :: FunctionVar ('Null ty) (null ty) (null ty)
coalesce nullxs notNullx = UnsafeExpression $
  "COALESCE" <> parenthesized (commaSeparated
    ((renderSQL <$> nullxs) <> [renderSQL notNullx]))

-- | analagous to `Data.Maybe.fromMaybe` using @COALESCE@
--
-- >>> printSQL $ fromNull true null_
-- COALESCE(NULL, TRUE)
fromNull
  :: Expression grp lat with db params from ('NotNull ty)
  -- ^ what to convert @NULL@ to
  -> Expression grp lat with db params from ('Null ty)
  -> Expression grp lat with db params from ('NotNull ty)
fromNull notNullx nullx = coalesce [nullx] notNullx

-- | >>> printSQL $ null_ & isNull
-- NULL IS NULL
isNull :: 'Null ty --> null 'PGbool
isNull x = UnsafeExpression $ renderSQL x <+> "IS NULL"

-- | >>> printSQL $ null_ & isNotNull
-- NULL IS NOT NULL
isNotNull :: 'Null ty --> null 'PGbool
isNotNull x = UnsafeExpression $ renderSQL x <+> "IS NOT NULL"

-- | analagous to `maybe` using @IS NULL@
--
-- >>> printSQL $ matchNull true not_ null_
-- CASE WHEN NULL IS NULL THEN TRUE ELSE (NOT NULL) END
matchNull
  :: Expression grp lat with db params from (nullty)
  -- ^ what to convert @NULL@ to
  -> ( Expression grp lat with db params from ('NotNull ty)
       -> Expression grp lat with db params from (nullty) )
  -- ^ function to perform when @NULL@ is absent
  -> Expression grp lat with db params from ('Null ty)
  -> Expression grp lat with db params from (nullty)
matchNull y f x = ifThenElse (isNull x) y
  (f (UnsafeExpression (renderSQL x)))

{-| right inverse to `fromNull`, if its arguments are equal then
`nullIf` gives @NULL@.

>>> :set -XTypeApplications
>>> printSQL (nullIf (false *: param @1))
NULLIF(FALSE, ($1 :: bool))
-}
nullIf :: '[ 'NotNull ty, 'NotNull ty] ---> 'Null ty
nullIf = unsafeFunctionN "NULLIF"

{-| Make the return type of the type family `NotNull` if both arguments are,
   or `Null` otherwise.
-}
type family CombineNullity
      (lhs :: PGType -> NullType) (rhs :: PGType -> NullType) :: PGType -> NullType where
  CombineNullity 'NotNull 'NotNull = 'NotNull
  CombineNullity _ _ = 'Null

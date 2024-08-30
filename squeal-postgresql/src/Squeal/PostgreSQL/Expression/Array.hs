{-|
Module: Squeal.PostgreSQL.Expression.Array
Description: array functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

array functions
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Array
  ( -- * Array Functions
    array
  , array0
  , array1
  , array2
  , cardinality
  , index
  , index1
  , index2
  , unnest
  , arrAny
  , arrAll
  ) where

import Data.String
import Data.Word (Word64)
import GHC.TypeNats

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Query.From.Set
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | Construct an array.
--
-- >>> printSQL $ array [null_, false, true]
-- ARRAY[NULL, FALSE, TRUE]
array
  :: [Expression grp lat with db params from ty]
  -- ^ array elements
  -> Expression grp lat with db params from (null ('PGvararray ty))
array xs = UnsafeExpression $ "ARRAY" <>
  bracketed (commaSeparated (renderSQL <$> xs))

-- | Safely construct an empty array.
--
-- >>> printSQL $ array0 text
-- (ARRAY[] :: text[])
array0
  :: TypeExpression db ty
  -> Expression grp lat with db params from (null ('PGvararray ty))
array0 ty = array [] & astype (vararray ty)

{- | Construct a fixed length array.

>>> printSQL $ array1 (null_ :* false *: true)
ARRAY[NULL, FALSE, TRUE]

>>> :type array1 (null_ :* false *: true)
array1 (null_ :* false *: true)
  :: Expression
       grp
       lat
       with
       db
       params
       from
       (null ('PGfixarray '[3] ('Null 'PGbool)))
-}
array1
  :: (n ~ Length tys, SOP.All ((~) ty) tys)
  => NP (Expression grp lat with db params from) tys
    -- ^ array elements
  -> Expression grp lat with db params from (null ('PGfixarray '[n] ty))
array1 xs = UnsafeExpression $ "ARRAY" <>
  bracketed (renderCommaSeparated renderSQL xs)

{- | Construct a fixed size matrix.

>>> printSQL $ array2 ((null_ :* false *: true) *: (false :* null_ *: true))
ARRAY[[NULL, FALSE, TRUE], [FALSE, NULL, TRUE]]

>>> :type array2 ((null_ :* false *: true) *: (false :* null_ *: true))
array2 ((null_ :* false *: true) *: (false :* null_ *: true))
  :: Expression
       grp
       lat
       with
       db
       params
       from
       (null ('PGfixarray '[2, 3] ('Null 'PGbool)))
-}
array2
  ::  ( SOP.All ((~) tys) tyss
      , SOP.All SOP.SListI tyss
      , Length tyss ~ n1
      , SOP.All ((~) ty) tys
      , Length tys ~ n2 )
  => NP (NP (Expression grp lat with db params from)) tyss
  -- ^ matrix elements
  -> Expression grp lat with db params from (null ('PGfixarray '[n1,n2] ty))
array2 xss = UnsafeExpression $ "ARRAY" <>
  bracketed (renderCommaSeparatedConstraint @SOP.SListI (bracketed . renderCommaSeparated renderSQL) xss)

-- | >>> printSQL $ cardinality (array [null_, false, true])
-- cardinality(ARRAY[NULL, FALSE, TRUE])
cardinality :: null ('PGvararray ty) --> null 'PGint8
cardinality = unsafeFunction "cardinality"

-- | >>> printSQL $ array [null_, false, true] & index 2
-- (ARRAY[NULL, FALSE, TRUE])[2]
index
  :: Word64 -- ^ index
  -> null ('PGvararray ty) --> NullifyType ty
index i arr = UnsafeExpression $
  parenthesized (renderSQL arr) <> "[" <> fromString (show i) <> "]"

-- | Typesafe indexing of fixed length arrays.
--
-- >>> printSQL $ array1 (true *: false) & index1 @1
-- (ARRAY[TRUE, FALSE])[1]
index1
  :: forall i n ty
   . (1 <= i, i <= n, KnownNat i)
  => 'NotNull ('PGfixarray '[n] ty) --> ty
  -- ^ vector index
index1 arr = UnsafeExpression $
  parenthesized (renderSQL arr)
  <> "[" <> fromString (show (natVal (SOP.Proxy @i))) <> "]"

-- | Typesafe indexing of fixed size matrices.
--
-- >>> printSQL $ array2 ((true *: false) *: (false *: true)) & index2 @1 @2
-- (ARRAY[[TRUE, FALSE], [FALSE, TRUE]])[1][2]
index2
  :: forall i j m n ty
   . ( 1 <= i, i <= m, KnownNat i
     , 1 <= j, j <= n, KnownNat j
     )
  => 'NotNull ('PGfixarray '[m,n] ty) --> ty
  -- ^ matrix index
index2 arr = UnsafeExpression $
  parenthesized (renderSQL arr)
  <> "[" <> fromString (show (natVal (SOP.Proxy @i))) <> "]"
  <> "[" <> fromString (show (natVal (SOP.Proxy @j))) <> "]"

-- | Expand an array to a set of rows
--
-- >>> printSQL $ unnest (array [null_, false, true])
-- unnest(ARRAY[NULL, FALSE, TRUE])
unnest :: null ('PGvararray ty) -|-> ("unnest" ::: '["unnest" ::: ty])
unnest = unsafeSetFunction "unnest"

{- |
The right-hand side is a parenthesized expression,
which must yield an array value. The left-hand expression
is evaluated and compared to each element of the array using
the given `Operator`, which must yield a Boolean result.
The result of `arrAll` is `true` if all comparisons yield true
(including the case where the array has zero elements).
The result is `false` if any false result is found.

If the array expression yields a null array,
the result of `arrAll` will be null. If the left-hand expression yields null,
the result of `arrAll` is ordinarily null
(though a non-strict comparison `Operator`
could possibly yield a different result).
Also, if the right-hand array contains any null
elements and no false comparison result is obtained,
the result of `arrAll` will be null, not true
(again, assuming a strict comparison `Operator`).
This is in accordance with SQL's normal rules for Boolean
combinations of null values.

>>> printSQL $ arrAll true (.==) (array [true, false, null_])
(TRUE = ALL (ARRAY[TRUE, FALSE, NULL]))
>>> printSQL $ arrAll "hi" like (array ["bi","hi"])
((E'hi' :: text) LIKE ALL (ARRAY[(E'bi' :: text), (E'hi' :: text)]))
-}
arrAll
  :: Expression grp lat with db params from ty1 -- ^ expression
  -> Operator ty1 ty2 ('Null 'PGbool) -- ^ operator
  -> Expression grp lat with db params from (null ('PGvararray ty2)) -- ^ array
  -> Condition grp lat with db params from
arrAll x (?) xs = x ? (UnsafeExpression $ "ALL" <+> parenthesized (renderSQL xs))

{- |
The right-hand side is a parenthesized expression, which must yield an array
value. The left-hand expression is evaluated and compared to each element of
the array using the given `Operator`, which must yield a Boolean result. The
result of `arrAny` is `true` if any true result is obtained. The result is
`false` if no true result is found (including the case where the array
has zero elements).

If the array expression yields a null array, the result of `arrAny` will
be null. If the left-hand expression yields null, the result of `arrAny` is
ordinarily null (though a non-strict comparison `Operator` could possibly
yield a different result). Also, if the right-hand array contains any
null elements and no true comparison result is obtained, the result of
`arrAny` will be null, not false
(again, assuming a strict comparison `Operator`).
This is in accordance with SQL's normal rules for
Boolean combinations of null values.

>>> printSQL $ arrAny true (.==) (array [true, false, null_])
(TRUE = ANY (ARRAY[TRUE, FALSE, NULL]))
>>> printSQL $ arrAny "hi" like (array ["bi","hi"])
((E'hi' :: text) LIKE ANY (ARRAY[(E'bi' :: text), (E'hi' :: text)]))
-}
arrAny
  :: Expression grp lat with db params from ty1 -- ^ expression
  -> Operator ty1 ty2 ('Null 'PGbool) -- ^ operator
  -> Expression grp lat with db params from (null ('PGvararray ty2)) -- ^ array
  -> Condition grp lat with db params from
arrAny x (?) xs = x ? (UnsafeExpression $ "ANY" <+> parenthesized (renderSQL xs))

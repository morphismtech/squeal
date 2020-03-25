{-|
Module: Squeal.PostgreSQL.Expression.Array
Description: Array functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Array functions
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
  , unnest
  ) where

import Data.String
import Data.Word (Word64)

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Query.From.Set
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | Construct an array.
-- >>> printSQL $ array [null_, false, true]
-- ARRAY[NULL, FALSE, TRUE]
array
  :: [Expression grp lat with db params from ty]
  -- ^ array elements
  -> Expression grp lat with db params from (null ('PGvararray ty))
array xs = UnsafeExpression $ "ARRAY" <>
  bracketed (commaSeparated (renderSQL <$> xs))

-- | Safely construct an empty array.
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
index n expr = UnsafeExpression $
  parenthesized (renderSQL expr) <> "[" <> fromString (show n) <> "]"

-- | Expand an array to a set of rows
unnest :: null ('PGvararray ty) -|-> ("unnest" ::: '["unnest" ::: ty])
unnest = unsafeSetFunction "unnest"

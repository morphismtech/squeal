{-|
Module: Squeal.PostgreSQL.Expression.Collection
Description: Array and composite functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Array and composite functions
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

module Squeal.PostgreSQL.Expression.Collection
  ( array
  , array1
  , array2
  , cardinality
  , index
  , unnest
  , row
  , field
  ) where

import Data.String
import Data.Word (Word64)

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Set
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | >>> printSQL $ array [null_, false, true]
-- ARRAY[NULL, FALSE, TRUE]
array
  :: [Expression outer commons grp schemas params from ty]
  -- ^ array elements
  -> Expression outer commons grp schemas params from (null ('PGvararray ty))
array xs = UnsafeExpression $ "ARRAY" <>
  bracketed (commaSeparated (renderSQL <$> xs))

{- | construct a 1-dimensional fixed length array

>>> printSQL $ array1 (null_ :* false *: true)
ARRAY[NULL, FALSE, TRUE]

>>> :type array1 (null_ :* false *: true)
array1 (null_ :* false *: true)
  :: Expression
       outer
       commons
       grp
       schemas
       params
       from
       (null ('PGfixarray '[3] ('Null 'PGbool)))
-}
array1
  :: (n ~ Length tys, SOP.All ((~) ty) tys)
  => NP (Expression outer commons grp schemas params from) tys
  -> Expression outer commons grp schemas params from (null ('PGfixarray '[n] ty))
array1 xs = UnsafeExpression $ "ARRAY" <>
  bracketed (renderCommaSeparated renderSQL xs)

{- | construct a 2-dimensional fixed length array

>>> printSQL $ array2 ((null_ :* false *: true) *: (false :* null_ *: true))
ARRAY[[NULL, FALSE, TRUE], [FALSE, NULL, TRUE]]

>>> :type array2 ((null_ :* false *: true) *: (false :* null_ *: true))
array2 ((null_ :* false *: true) *: (false :* null_ *: true))
  :: Expression
       outer
       commons
       grp
       schemas
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
  => NP (NP (Expression outer commons grp schemas params from)) tyss
  -> Expression outer commons grp schemas params from (null ('PGfixarray '[n1,n2] ty))
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
unnest :: SetFunction "unnest" (null ('PGvararray ty)) '["unnest" ::: ty]
unnest = unsafeSetFunction

-- | A row constructor is an expression that builds a row value
-- (also called a composite value) using values for its member fields.
--
-- >>> :{
-- type Complex = 'PGcomposite
--   '[ "real"      ::: 'NotNull 'PGfloat8
--    , "imaginary" ::: 'NotNull 'PGfloat8 ]
-- :}
--
-- >>> let i = row (0 `as` #real :* 1 `as` #imaginary) :: Expression outer commons grp schemas params from ('NotNull Complex)
-- >>> printSQL i
-- ROW(0, 1)
row
  :: SOP.SListI row
  => NP (Aliased (Expression outer commons grp schemas params from)) row
  -- ^ zero or more expressions for the row field values
  -> Expression outer commons grp schemas params from (null ('PGcomposite row))
row exprs = UnsafeExpression $ "ROW" <> parenthesized
  (renderCommaSeparated (\ (expr `As` _) -> renderSQL expr) exprs)

-- | >>> :{
-- type Complex = 'PGcomposite
--   '[ "real"      ::: 'NotNull 'PGfloat8
--    , "imaginary" ::: 'NotNull 'PGfloat8 ]
-- type Schema = '["complex" ::: 'Typedef Complex]
-- :}
--
-- >>> let i = row (0 `as` #real :* 1 `as` #imaginary) :: Expression outer '[] grp (Public Schema) from params ('NotNull Complex)
-- >>> printSQL $ i & field #complex #imaginary
-- (ROW(0, 1)::"complex")."imaginary"
field
  :: ( Has sch schemas schema
     , Has tydef schema ('Typedef ('PGcomposite row))
     , Has field row ty)
  => QualifiedAlias sch tydef -- ^ row type
  -> Alias field -- ^ field name
  -> Expression outer commons grp schemas params from ('NotNull ('PGcomposite row))
  -> Expression outer commons grp schemas params from ty
field td fld expr = UnsafeExpression $
  parenthesized (renderSQL expr <> "::" <> renderSQL td)
    <> "." <> renderSQL fld

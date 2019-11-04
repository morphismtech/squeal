{-|
Module: Squeal.PostgreSQL.Expression.Range
Description: range types and functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Range types and functions
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DeriveFoldable
  , DerivingStrategies
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , PatternSynonyms
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Range
  ( range
  , Range (..)
  , (<=..<=), (<..<), (<=..<), (<..<=)
  , moreThan, atLeast, lessThan, atMost
  , singleton, whole
  , Bound (..)
  , (.<@)
  , (@>.)
  , (<<@)
  , (@>>)
  , (&<)
  , (&>)
  , (-|-)
  , (@+)
  , (@*)
  , (@-)
  , lowerBound
  , upperBound
  , isEmpty
  , lowerInc
  , lowerInf
  , upperInc
  , upperInf
  , rangeMerge
  ) where

import BinaryParser
import ByteString.StrictBuilder
import Data.Bits
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import qualified PostgreSQL.Binary.Decoding as Decoding

import Squeal.PostgreSQL.Binary
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type hiding (bool)
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | Construct a `range`
--
-- >>> printSQL $ range tstzrange (atLeast now)
-- tstzrange(now(), NULL, '[)')
-- >>> printSQL $ range numrange (0 <=..< 2*pi)
-- numrange(0, (2 * pi()), '[)')
-- >>> printSQL $ range int4range Empty
-- ('empty' :: int4range)
range
  :: TypeExpression schemas (null ('PGrange ty))
  -> Range (Expression outer commons grp schemas params from ('NotNull ty))
  -> Expression outer commons grp schemas params from (null ('PGrange ty))
range ty = \case
  Empty -> UnsafeExpression $ parenthesized
    (emp <+> "::" <+> renderSQL ty)
  NonEmpty l u -> UnsafeExpression $ renderSQL ty <> parenthesized
    (commaSeparated (args l u))
  where
    emp = singleQuote <> "empty" <> singleQuote
    args l u = [arg l, arg u, singleQuote <> bra l <> ket u <> singleQuote]
    singleQuote = "\'"
    arg = \case
      Infinite -> "NULL"; Closed x -> renderSQL x; Open x -> renderSQL x
    bra = \case Infinite -> "("; Closed _ -> "["; Open _ -> "("
    ket = \case Infinite -> ")"; Closed _ -> "]"; Open _ -> ")"

data Bound x = Infinite | Closed x | Open x
  deriving
    ( Eq, Ord, Show, Read, GHC.Generic
    , Functor, Foldable, Traversable )

data Range x = Empty | NonEmpty (Bound x) (Bound x)
  deriving
    ( Eq, Ord, Show, Read, GHC.Generic
    , Functor, Foldable, Traversable )
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
type instance PG (Range x) = 'PGrange (PG x)
instance ToParam x pg => ToParam (Range x) ('PGrange pg) where
  toParam rng = K $
    word8 (setFlags rng 0) <>
      case rng of
        Empty -> mempty
        NonEmpty lower upper -> putBound lower <> putBound upper
    where
      putBound = \case
        Infinite -> mempty
        Closed value -> putValue (unK (toParam @x @pg value))
        Open value -> putValue (unK (toParam @x @pg value))
      putValue value = int32BE (fromIntegral (builderLength value)) <> value
      setFlags = \case
        Empty -> (`setBit` 0)
        NonEmpty lower upper ->
          setLowerFlags lower . setUpperFlags upper
      setLowerFlags = \case
        Infinite -> (`setBit` 3)
        Closed _ -> (`setBit` 1)
        Open _ -> id
      setUpperFlags = \case
        Infinite -> (`setBit` 4)
        Closed _ -> (`setBit` 2)
        Open _ -> id
instance FromValue pg y => FromValue ('PGrange pg) (Range y) where
  fromValue = do
    flag <- byte
    if testBit flag 0 then return Empty else do
      lower <-
        if testBit flag 3
          then return Infinite
          else do
            len <- sized 4 Decoding.int
            l <- sized len (fromValue @pg)
            return $ if testBit flag 1 then Closed l else Open l
      upper <-
        if testBit flag 4
          then return Infinite
          else do
            len <- sized 4 Decoding.int
            l <- sized len (fromValue @pg)
            return $ if testBit flag 2 then Closed l else Open l
      return $ NonEmpty lower upper

(<=..<=), (<..<), (<=..<), (<..<=) :: x -> x -> Range x
infix 4 <=..<=, <..<, <=..<, <..<=
x <=..<= y = NonEmpty (Closed x) (Closed y)
x <..< y = NonEmpty (Open x) (Open y)
x <=..< y = NonEmpty (Closed x) (Open y)
x <..<= y = NonEmpty (Open x) (Closed y)

moreThan, atLeast, lessThan, atMost :: x -> Range x
moreThan x = NonEmpty (Open x) Infinite
atLeast x = NonEmpty (Closed x) Infinite
lessThan x = NonEmpty Infinite (Open x)
atMost x = NonEmpty Infinite (Closed x)

singleton :: x -> Range x
singleton x = x <=..<= x

whole :: Range x
whole = NonEmpty Infinite Infinite

(.<@) :: Operator ('NotNull ty) (null ('PGrange ty)) ('Null 'PGbool)
(.<@) = unsafeBinaryOp "<@"

(@>.) :: Operator (null ('PGrange ty)) ('NotNull ty) ('Null 'PGbool)
(@>.) = unsafeBinaryOp "<@"

(<<@) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(<<@) = unsafeBinaryOp "<<"

(@>>) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(@>>) = unsafeBinaryOp ">>"

(&<) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(&<) = unsafeBinaryOp "&<"

(&>) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(&>) = unsafeBinaryOp "&>"

(-|-) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) ('Null 'PGbool)
(-|-) = unsafeBinaryOp "-|-"

(@+) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(@+) = unsafeBinaryOp "+"

(@*) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(@*) = unsafeBinaryOp "*"

(@-) :: Operator (null ('PGrange ty)) (null ('PGrange ty)) (null ('PGrange ty))
(@-) = unsafeBinaryOp "-"

lowerBound :: null ('PGrange ty) --> 'Null ty
lowerBound = unsafeFunction "lower"

upperBound :: null ('PGrange ty) --> 'Null ty
upperBound = unsafeFunction "upper"

isEmpty :: null ('PGrange ty) --> 'Null 'PGbool
isEmpty = unsafeFunction "isempty"

lowerInc :: null ('PGrange ty) --> 'Null 'PGbool
lowerInc = unsafeFunction "lower_inc"

lowerInf :: null ('PGrange ty) --> 'Null 'PGbool
lowerInf = unsafeFunction "lower_inf"

upperInc :: null ('PGrange ty) --> 'Null 'PGbool
upperInc = unsafeFunction "upper_inc"

upperInf :: null ('PGrange ty) --> 'Null 'PGbool
upperInf = unsafeFunction "upper_inf"

rangeMerge :: '[null ('PGrange ty), null ('PGrange ty)] ---> null ('PGrange ty)
rangeMerge = unsafeFunctionN "range_merge"

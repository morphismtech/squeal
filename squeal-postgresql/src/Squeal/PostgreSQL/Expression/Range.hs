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

import Control.Applicative
import BinaryParser
import ByteString.StrictBuilder
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

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
-- https://github.com/postgres/postgres/blob/8255c7a5eeba8f1a38b7a431c04909bde4f5e67d/src/backend/utils/adt/rangetypes.c
instance ToParam x pg => ToParam (Range x) ('PGrange pg) where
  toParam = \case
    Empty -> K (bytes "\'empty\'")
    NonEmpty l u -> K $ mconcat
      [bytes (bra l), arg l, utf8Char ',', arg u, bytes (ket u)]
    where
      arg = \case
        Infinite -> ""
        Closed x -> unK (toParam @x @pg x)
        Open x -> unK (toParam @x @pg x)
      bra = \case Infinite -> "("; Closed _ -> "["; Open _ -> "("
      ket = \case Infinite -> ")"; Closed _ -> "]"; Open _ -> ")"
instance FromValue pg y => FromValue ('PGrange pg) (Range y) where
  fromValue =
    Empty <$ unitOfBytes "\'empty\'" <|> do
      boundL <- boundLOpen <|> boundLClosed <|> boundLInfinite
      unitOfBytes ","
      boundR <- boundROpen <|> boundRClosed <|> boundRInfinite
      return $ NonEmpty boundL boundR
    where
      boundLClosed = unitOfBytes "[" *> (Closed <$> fromValue @pg @y)
      boundLOpen = unitOfBytes "(" *> (Open <$> fromValue @pg @y)
      boundLInfinite = Infinite <$ unitOfBytes "("
      boundRClosed = (Closed <$> fromValue @pg @y) <* unitOfBytes "]"
      boundROpen = (Open <$> fromValue @pg @y) <* unitOfBytes ")"
      boundRInfinite = Infinite <$ unitOfBytes ")"

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

lowerBound :: null ('PGrange ty) :--> 'Null ty
lowerBound = unsafeFunction "lower"

upperBound :: null ('PGrange ty) :--> 'Null ty
upperBound = unsafeFunction "upper"

isEmpty :: null ('PGrange ty) :--> 'Null 'PGbool
isEmpty = unsafeFunction "isempty"

lowerInc :: null ('PGrange ty) :--> 'Null 'PGbool
lowerInc = unsafeFunction "lower_inc"

lowerInf :: null ('PGrange ty) :--> 'Null 'PGbool
lowerInf = unsafeFunction "lower_inf"

upperInc :: null ('PGrange ty) :--> 'Null 'PGbool
upperInc = unsafeFunction "upper_inc"

upperInf :: null ('PGrange ty) :--> 'Null 'PGbool
upperInf = unsafeFunction "upper_inf"

rangeMerge :: FunctionN
  '[ null ('PGrange ty), null ('PGrange ty)] (null ('PGrange ty))
rangeMerge = unsafeFunctionN "range_merge"

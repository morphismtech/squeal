{-|
Module: Squeal.PostgreSQL.Expression.InPG
Description: InPG expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

InPG expressions
-}

{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , MultiWayIf
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Inline
  ( -- * InPG
    InPG (..)
  , Inline (..)
  , InlineField (..)
  ) where

import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (doubleDec, floatDec)
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.String
import Data.Text (Text)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.LocalTime (LocalTime(LocalTime), TimeOfDay(TimeOfDay), TimeZone)
import Data.UUID.Types (UUID, toASCIIBytes)
import Data.Vector (Vector, toList)
import GHC.TypeLits

import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Array
import Squeal.PostgreSQL.Expression.Composite
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Null
import Squeal.PostgreSQL.Expression.Range
import Squeal.PostgreSQL.Expression.Time
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

{- |
The `InPG` class allows embedding a Haskell value directly
as an `Expression` using `inPG`.

>>> printSQL (inPG 'a')
(E'a' :: char(1))

>>> printSQL (inPG (1 :: Double))
(1.0 :: float8)

>>> printSQL (inPG (Json ([1, 2] :: [Double])))
('[1.0,2.0]' :: json)

>>> printSQL (inPG (Enumerated GT))
'GT'
-}
class InPG x where inPG :: x -> Expr (null (PG x))
instance InPG Bool where
  inPG = \case
    True -> true
    False -> false
instance JSON.ToJSON x => InPG (Json x) where
  inPG = inferredtype . UnsafeExpression
    . singleQuotedUtf8 . toStrict . JSON.encode . getJson
instance JSON.ToJSON x => InPG (Jsonb x) where
  inPG = inferredtype . UnsafeExpression
    . singleQuotedUtf8 . toStrict . JSON.encode . getJsonb
instance InPG Char where
  inPG chr = inferredtype . UnsafeExpression $
    "E\'" <> fromString (escape chr) <> "\'"
instance InPG String where inPG = inferredtype . fromString
instance InPG Int16 where inPG = inferredtype . fromIntegral
instance InPG Int32 where inPG = inferredtype . fromIntegral
instance InPG Int64 where
  inPG x = inferredtype $
    if x == minBound
      -- For some reason Postgres throws an error with (-9223372036854775808::int8)
      -- even though its a valid lowest value for int8
      then UnsafeExpression "-9223372036854775807-1"
      else fromIntegral x
instance InPG Float where
  inPG x = inferredtype . UnsafeExpression $
    if (isNaN x || isInfinite x)
    then singleQuotedUtf8 (decimal x)
    else decimal x
    where
      decimal = toStrict . toLazyByteString . floatDec
instance InPG Double where
  inPG x = inferredtype . UnsafeExpression $
    if (isNaN x || isInfinite x)
    then singleQuotedUtf8 (decimal x)
    else decimal x
    where
      decimal = toStrict . toLazyByteString . doubleDec
instance InPG Scientific where
  inPG
    = inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    . scientificBuilder
instance InPG Text where inPG = inferredtype . fromString . Text.unpack
instance InPG Lazy.Text where inPG = inferredtype . fromString . Lazy.Text.unpack
instance (KnownNat n, 1 <= n) => InPG (VarChar n) where
  inPG str = inferredtype . UnsafeExpression $
      "E\'" <> fromString (escape =<< (Text.unpack . getVarChar) str) <> "\'"
instance (KnownNat n, 1 <= n) => InPG (FixChar n) where
  inPG str = inferredtype . UnsafeExpression $
      "E\'" <> fromString (escape =<< (Text.unpack . getFixChar) str) <> "\'"
instance InPG DiffTime where
  inPG dt =
    let
      picosecs = diffTimeToPicoseconds dt
      (secs,leftover) = picosecs `quotRem` 1000000000000
      microsecs = leftover `quot` 1000000
    in
      inferredtype $
        interval_ (fromIntegral secs) Seconds
        +! interval_ (fromIntegral microsecs) Microseconds
instance InPG Day where
  inPG day =
    let (y,m,d) = toGregorian day
    in inferredtype $ makeDate (fromInteger y :* fromIntegral m *: fromIntegral d)
instance InPG UTCTime where
  inPG
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%z"
instance InPG (TimeOfDay, TimeZone) where
  inPG (hms, tz)
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    $ formatTime defaultTimeLocale "%H:%M:%S" hms
      <> formatTime defaultTimeLocale "%z" tz
instance InPG TimeOfDay where
  inPG (TimeOfDay hr mn sc) = inferredtype $ makeTime
    (fromIntegral hr :* fromIntegral mn *: fromRational (toRational sc))
instance InPG LocalTime where
  inPG (LocalTime day t) =
    let
      (y,m,d) = toGregorian day
      TimeOfDay hr mn sc = t
    in inferredtype $ makeTimestamp
      ( fromInteger y :* fromIntegral m :* fromIntegral d
        :* fromIntegral hr :* fromIntegral mn *: fromRational (toRational sc) )
instance InPG (Range Int32) where
  inPG = range int4range . fmap inPG
instance InPG (Range Int64) where
  inPG = range int8range . fmap inPG
instance InPG (Range Scientific) where
  inPG = range numrange . fmap inPG
instance InPG (Range LocalTime) where
  inPG = range tsrange . fmap inPG
instance InPG (Range UTCTime) where
  inPG = range tstzrange . fmap inPG
instance InPG (Range Day) where
  inPG = range daterange . fmap inPG
instance InPG UUID where
  inPG
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . toASCIIBytes
instance InPG Money where
  inPG moolah = inferredtype . UnsafeExpression $
    fromString (show dollars)
    <> "." <> fromString (show pennies)
    where
      (dollars,pennies) = cents moolah `divMod` 100
instance Inline x ('NotNull (PG x))
  => InPG (VarArray 'NotNull [x]) where
    inPG (VarArray xs) = array (inline <$> xs)
instance Inline x ('NotNull (PG x))
  => InPG (VarArray 'NotNull (Vector x)) where
    inPG (VarArray xs) = array (inline <$> toList xs)
instance Inline (Maybe x) ('Null (PG x))
  => InPG (VarArray 'Null [Maybe x]) where
    inPG (VarArray xs) = array (inline <$> xs)
instance Inline (Maybe x) ('Null (PG x))
  => InPG (VarArray 'Null (Vector (Maybe x))) where
    inPG (VarArray xs) = array (inline <$> toList xs)
instance InPG Oid where
  inPG (Oid o) = inferredtype . UnsafeExpression . fromString $ show o
instance
  ( SOP.IsEnumType x
  , SOP.HasDatatypeInfo x
  ) => InPG (Enumerated x) where
    inPG =
      let
        gshowConstructor
          :: NP SOP.ConstructorInfo xss
          -> SOP.SOP SOP.I xss
          -> String
        gshowConstructor Nil _ = ""
        gshowConstructor (constructor :* _) (SOP.SOP (SOP.Z _)) =
          SOP.constructorName constructor
        gshowConstructor (_ :* constructors) (SOP.SOP (SOP.S xs)) =
          gshowConstructor constructors (SOP.SOP xs)
      in
        UnsafeExpression
        . singleQuotedUtf8
        . fromString
        . gshowConstructor
            (SOP.constructorInfo (SOP.datatypeInfo (SOP.Proxy @x)))
        . SOP.from
        . getEnumerated
instance
  ( SOP.IsRecord x xs
  , SOP.AllZip InlineField xs (RowPG x)
  ) => InPG (Composite x) where
    inPG
      = row
      . SOP.htrans (SOP.Proxy @InlineField) inlineField
      . SOP.toRecord
      . getComposite

class Inline x ty where inline :: x -> Expr ty
instance (InPG x, pg ~ PG x) => Inline x ('NotNull pg) where inline = inPG
instance (InPG x, pg ~ PG x) => Inline (Maybe x) ('Null pg) where
  inline = maybe null_ inPG

class InlineField
  (field :: (Symbol, Type))
  (fieldpg :: (Symbol, NullType)) where
    inlineField
      :: SOP.P field
      -> Aliased (Expression lat with grp db params from) fieldpg
instance (KnownSymbol alias, Inline x ty)
  => InlineField (alias ::: x) (alias ::: ty) where
    inlineField (SOP.P x) = inline x `as` Alias @alias

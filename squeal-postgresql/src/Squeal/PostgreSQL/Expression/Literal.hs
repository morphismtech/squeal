{-|
Module: Squeal.PostgreSQL.Expression.Literal
Description: Literal expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Literal expressions
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

module Squeal.PostgreSQL.Expression.Literal
  ( -- * Literal
    Literal (..)
  ) where

import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
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
The `Literal` class allows embedding a Haskell value directly
as an `Expression` using `literal`.

>>> printSQL (literal 'a')
(E'a' :: char(1))

>>> printSQL (literal (1 :: Double))
(1.0 :: float8)

>>> printSQL (literal (Json ([1, 2] :: [Double])))
('[1.0,2.0]' :: json)

>>> printSQL (literal (Enumerated GT))
'GT'
-}
class Literal hask where literal :: hask -> Expr (NullPG hask)
instance (Literal hask, NullPG hask ~ 'NotNull (PG hask))
  => Literal (Maybe hask) where
  literal = maybe null_ (notNull . literal)
instance Literal Bool where
  literal = \case
    True -> true
    False -> false
instance JSON.ToJSON hask => Literal (Json hask) where
  literal = inferredtype . UnsafeExpression
    . singleQuotedUtf8 . toStrict . JSON.encode . getJson
instance JSON.ToJSON hask => Literal (Jsonb hask) where
  literal = inferredtype . UnsafeExpression
    . singleQuotedUtf8 . toStrict . JSON.encode . getJsonb
instance Literal Char where
  literal chr = inferredtype . UnsafeExpression $
    "E\'" <> fromString (escape chr) <> "\'"
instance Literal String where literal = inferredtype . fromString
instance Literal Int16 where literal = inferredtype . fromIntegral
instance Literal Int32 where literal = inferredtype . fromIntegral
instance Literal Int64 where
  literal x = inferredtype $
    if x == minBound
      -- For some reason Postgres throws an error with (-9223372036854775808::int8)
      -- even though its a valid lowest value for int8
      then UnsafeExpression "-9223372036854775807-1"
      else fromIntegral x
instance Literal Float where
  literal x = inferredtype $
    if | isNaN x -> UnsafeExpression $ singleQuotedUtf8 "NaN"
       | isInfinite x && x > 0 -> UnsafeExpression $ singleQuotedUtf8 "Infinity"
       | isInfinite x && x < 0 -> UnsafeExpression $ singleQuotedUtf8 "-Infinity"
       | otherwise -> fromRational $ toRational x
instance Literal Double where
  literal x = inferredtype $
    if | isNaN x -> UnsafeExpression $ singleQuotedUtf8 "NaN"
       | isInfinite x && x > 0 -> UnsafeExpression $ singleQuotedUtf8 "Infinity"
       | isInfinite x && x < 0 -> UnsafeExpression $ singleQuotedUtf8 "-Infinity"
       | otherwise -> fromRational $ toRational x
instance Literal Scientific where
  literal
    = inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    . scientificBuilder
instance Literal Text where literal = inferredtype . fromString . Text.unpack
instance Literal Lazy.Text where literal = inferredtype . fromString . Lazy.Text.unpack
instance (KnownNat n, 1 <= n) => Literal (VarChar n) where
  literal str = inferredtype . UnsafeExpression $
      "E\'" <> fromString (escape =<< (Text.unpack . getVarChar) str) <> "\'"
instance (KnownNat n, 1 <= n) => Literal (FixChar n) where
  literal str = inferredtype . UnsafeExpression $
      "E\'" <> fromString (escape =<< (Text.unpack . getFixChar) str) <> "\'"
instance Literal DiffTime where
  literal dt =
    let
      picosecs = diffTimeToPicoseconds dt
      (secs,leftover) = picosecs `quotRem` 1000000000000
      microsecs = leftover `quot` 1000000
    in
      inferredtype $
        interval_ (fromIntegral secs) Seconds
        +! interval_ (fromIntegral microsecs) Microseconds
instance Literal Day where
  literal day =
    let (y,m,d) = toGregorian day
    in inferredtype $ makeDate (fromInteger y :* fromIntegral m *: fromIntegral d)
instance Literal UTCTime where
  literal
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%z"
instance Literal (TimeOfDay, TimeZone) where
  literal (hms, tz)
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    $ formatTime defaultTimeLocale "%H:%M:%S" hms
      <> formatTime defaultTimeLocale "%z" tz
instance Literal TimeOfDay where
  literal (TimeOfDay hr mn sc) = inferredtype $ makeTime
    (fromIntegral hr :* fromIntegral mn *: fromRational (toRational sc))
instance Literal LocalTime where
  literal (LocalTime day t) =
    let
      (y,m,d) = toGregorian day
      TimeOfDay hr mn sc = t
    in inferredtype $ makeTimestamp
      ( fromInteger y :* fromIntegral m :* fromIntegral d
        :* fromIntegral hr :* fromIntegral mn *: fromRational (toRational sc) )
instance Literal (Range Int32) where
  literal = range int4range . fmap literal
instance Literal (Range Int64) where
  literal = range int8range . fmap literal
instance Literal (Range Scientific) where
  literal = range numrange . fmap literal
instance Literal (Range LocalTime) where
  literal = range tsrange . fmap literal
instance Literal (Range UTCTime) where
  literal = range tstzrange . fmap literal
instance Literal (Range Day) where
  literal = range daterange . fmap literal
instance Literal UUID where
  literal
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . toASCIIBytes
instance Literal Money where
  literal moolah = inferredtype . UnsafeExpression $
    fromString (show dollars)
    <> "." <> fromString (show pennies)
    where
      (dollars,pennies) = cents moolah `divMod` 100
instance Literal ty => Literal (VarArray [ty]) where
  literal (VarArray xs) = array (literal <$> xs)
instance Literal ty => Literal (VarArray (Vector ty)) where
  literal (VarArray xs) = array (literal <$> toList xs)
instance Literal Oid where
  literal (Oid o) = inferredtype . UnsafeExpression . fromString $ show o
instance
  ( SOP.IsEnumType x
  , SOP.HasDatatypeInfo x
  ) => Literal (Enumerated x) where
    literal =
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
  , SOP.AllZip LiteralField xs (RowPG x)
  ) => Literal (Composite x) where
    literal
      = row
      . SOP.htrans (SOP.Proxy @LiteralField) literalField
      . SOP.toRecord
      . getComposite

class LiteralField
  (field :: (Symbol, Type))
  (fieldpg :: (Symbol, NullType)) where
    literalField
      :: SOP.P field
      -> Aliased (Expression lat with grp db params from) fieldpg
instance (KnownSymbol alias, Literal x, ty ~ NullPG x)
  => LiteralField (alias ::: x) (alias ::: ty) where
    literalField (SOP.P x) = literal x `as` Alias @alias

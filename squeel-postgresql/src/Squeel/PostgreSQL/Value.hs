{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , FunctionalDependencies
  , KindSignatures
  , MultiParamTypeClasses
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
#-}

module Squeel.PostgreSQL.Value where

import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import Data.Int (Int16,Int32,Int64)
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Functor
import PostgreSQL.Binary.Decoder (Decoder)
import PostgreSQL.Binary.Encoder (Encoder)
import Data.Proxy (Proxy)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import Data.UUID (UUID)

import qualified Data.ByteString.Lazy as Lazy (ByteString, toStrict)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified PostgreSQL.Binary.Encoder as Encoder

import Squeel.PostgreSQL.Type

decodeValue :: FromValue pg x => Proxy pg -> ByteString -> Either Text x
decodeValue = Decoder.run . fromValue

class FromValue (pg :: PGType) x where
  fromValue :: Proxy pg -> Decoder x
instance FromValue 'PGInt2 Int16 where
  fromValue _ = Decoder.int
instance FromValue 'PGInt4 Int32 where
  fromValue _ = Decoder.int
instance FromValue 'PGInt8 Int64 where
  fromValue _ = Decoder.int
instance FromValue 'PGFloat4 Float where
  fromValue _ = Decoder.float4
instance FromValue 'PGFloat8 Double where
  fromValue _ = Decoder.float8
instance FromValue 'PGBool Bool where
  fromValue _ = Decoder.bool
instance FromValue 'PGBytea ByteString where
  fromValue _ = Decoder.bytea_strict
instance FromValue 'PGBytea Lazy.ByteString where
  fromValue _ = Decoder.bytea_lazy
instance FromValue 'PGText Text where
  fromValue _ = Decoder.text_strict
instance FromValue 'PGText Lazy.Text where
  fromValue _ = Decoder.text_lazy
instance FromValue ('PGChar 1) Char where
  fromValue _ = Decoder.char
instance FromValue 'PGNumeric Scientific where
  fromValue _ = Decoder.numeric
instance FromValue 'PGUuid UUID where
  fromValue _ = Decoder.uuid
instance FromJSON x => FromValue 'PGJson x where
  fromValue _ = Decoder.json_bytes (left Text.pack . eitherDecodeStrict)
instance FromJSON x => FromValue 'PGJsonb x where
  fromValue _ = Decoder.jsonb_bytes (left Text.pack . eitherDecodeStrict)
instance FromValue 'PGDate Day where
  fromValue _ = Decoder.date
instance FromValue 'PGTime TimeOfDay where
  fromValue _ = Decoder.time_int
instance FromValue 'PGTimeTZ (TimeOfDay, TimeZone) where
  fromValue _ = Decoder.timetz_int
instance FromValue 'PGTimestamp LocalTime where
  fromValue _ = Decoder.timestamp_int
instance FromValue 'PGTimestampTZ UTCTime where
  fromValue _ = Decoder.timestamptz_int
instance FromValue 'PGInterval DiffTime where
  fromValue _ = Decoder.interval_int

class ToValue x (pg :: PGType) where
  toValue :: Proxy pg -> Encoder x
instance ToValue Int16 'PGInt2 where
  toValue _ = Encoder.int2_int16
instance ToValue Int32 'PGInt4 where
  toValue _ = Encoder.int4_int32
instance ToValue Int64 'PGInt8 where
  toValue _ = Encoder.int8_int64
instance ToValue Float 'PGFloat4 where
  toValue _ = Encoder.float4
instance ToValue Double 'PGFloat8 where
  toValue _ = Encoder.float8
instance ToValue Bool 'PGBool where
  toValue _ = Encoder.bool
instance ToValue ByteString 'PGBytea where
  toValue _ = Encoder.bytea_strict
instance ToValue Lazy.ByteString 'PGBytea where
  toValue _ = Encoder.bytea_lazy
instance ToValue Text 'PGText where
  toValue _ = Encoder.text_strict
instance ToValue Lazy.Text 'PGText where
  toValue _ = Encoder.text_lazy
instance ToValue Char ('PGChar 1) where
  toValue _ = Encoder.char
instance ToValue Scientific 'PGNumeric where
  toValue _ = Encoder.numeric
instance ToValue UUID 'PGUuid where
  toValue _ = Encoder.uuid
instance ToJSON x => ToValue x 'PGJson where
  toValue _ = Encoder.json_bytes . Lazy.toStrict . encode
instance ToJSON x => ToValue x 'PGJsonb where
  toValue _ = Encoder.jsonb_bytes . Lazy.toStrict . encode
instance ToValue Day 'PGDate where
  toValue _ = Encoder.date
instance ToValue TimeOfDay 'PGTime where
  toValue _ = Encoder.time_int
instance ToValue (TimeOfDay, TimeZone) 'PGTimeTZ where
  toValue _ = Encoder.timetz_int
instance ToValue LocalTime 'PGTimestamp where
  toValue _ = Encoder.timestamp_int
instance ToValue UTCTime 'PGTimestampTZ where
  toValue _ = Encoder.timestamptz_int
instance ToValue DiffTime 'PGInterval where
  toValue _ = Encoder.interval_int

class ToValues xs pgs where
  toValues :: Proxy pgs -> Rec Identity xs -> [ByteString]
instance ToValues '[] '[] where toValues _ _ = []
instance (ToValue x pg, ToValues xs pgs)
  => ToValues (x ': xs) (pg ': pgs) where
    toValues (_ :: Proxy (pg ': pgs)) (Identity x :& xs) =
      Encoder.run (toValue (Proxy @pg)) x : toValues (Proxy @pgs) xs

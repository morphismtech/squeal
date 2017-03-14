{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , KindSignatures
  , MultiParamTypeClasses
#-}

module Squeel.PostgreSQL.Value where

import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString, toStrict)
import Data.Int (Int16,Int32,Int64)
import PostgreSQL.Binary.Decoder (Decoder)
import PostgreSQL.Binary.Encoder (Encoder)
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified PostgreSQL.Binary.Encoder as Encoder
import Data.Proxy (Proxy)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Lazy as Lazy (Text)
import Data.UUID (UUID)
import Data.Word (Word16, Word32, Word64)

import Squeel.PostgreSQL.Type

decodeValue :: FromValue pg x => Proxy pg -> ByteString -> Either Text x
decodeValue = Decoder.run . fromValue

class FromValue (pg :: PGType) x where
  fromValue :: Proxy pg -> Decoder x
instance (Integral x, Bits x) => FromValue ('PGType "int") x where
  fromValue _ = Decoder.int
instance FromValue ('PGType "float4") Float where
  fromValue _ = Decoder.float4
instance FromValue ('PGType "float8") Double where
  fromValue _ = Decoder.float8
instance FromValue ('PGType "bool") Bool where
  fromValue _ = Decoder.bool
instance FromValue ('PGType "bytea") ByteString where
  fromValue _ = Decoder.bytea_strict
instance FromValue ('PGType "bytea") Lazy.ByteString where
  fromValue _ = Decoder.bytea_lazy
instance FromValue ('PGType "text") Text where
  fromValue _ = Decoder.text_strict
instance FromValue ('PGType "text") Lazy.Text where
  fromValue _ = Decoder.text_lazy
instance FromValue ('PGType "char") Char where
  fromValue _ = Decoder.char
instance FromValue ('PGType "numeric") Scientific where
  fromValue _ = Decoder.numeric
instance FromValue ('PGType "uuid") UUID where
  fromValue _ = Decoder.uuid
instance FromJSON x => FromValue ('PGType "json") x where
  fromValue _ = Decoder.json_bytes (left Text.pack . eitherDecodeStrict)
instance FromJSON x => FromValue ('PGType "jsonb") x where
  fromValue _ = Decoder.jsonb_bytes (left Text.pack . eitherDecodeStrict)
instance FromValue ('PGType "date") Day where
  fromValue _ = Decoder.date
instance FromValue ('PGType "time") TimeOfDay where
  fromValue _ = Decoder.time_int
instance FromValue ('PGType "timetz") (TimeOfDay, TimeZone) where
  fromValue _ = Decoder.timetz_int
instance FromValue ('PGType "timestamp") LocalTime where
  fromValue _ = Decoder.timestamp_int
instance FromValue ('PGType "timestamptz") UTCTime where
  fromValue _ = Decoder.timestamptz_int
instance FromValue ('PGType "interval") DiffTime where
  fromValue _ = Decoder.interval_int

class ToValue (pg :: PGType) x where
  toValue :: Proxy pg -> Encoder x
instance ToValue ('PGType "int2") Int16 where
  toValue _ = Encoder.int2_int16
instance ToValue ('PGType "int4") Int32 where
  toValue _ = Encoder.int4_int32
instance ToValue ('PGType "int8") Int64 where
  toValue _ = Encoder.int8_int64
instance ToValue ('PGType "word2") Word16 where
  toValue _ = Encoder.int2_word16
instance ToValue ('PGType "word4") Word32 where
  toValue _ = Encoder.int4_word32
instance ToValue ('PGType "word8") Word64 where
  toValue _ = Encoder.int8_word64
instance ToValue ('PGType "float4") Float where
  toValue _ = Encoder.float4
instance ToValue ('PGType "float8") Double where
  toValue _ = Encoder.float8
instance ToValue ('PGType "bool") Bool where
  toValue _ = Encoder.bool
instance ToValue ('PGType "bytea") ByteString where
  toValue _ = Encoder.bytea_strict
instance ToValue ('PGType "bytea") Lazy.ByteString where
  toValue _ = Encoder.bytea_lazy
instance ToValue ('PGType "text") Text where
  toValue _ = Encoder.text_strict
instance ToValue ('PGType "text") Lazy.Text where
  toValue _ = Encoder.text_lazy
instance ToValue ('PGType "char") Char where
  toValue _ = Encoder.char
instance ToValue ('PGType "numeric") Scientific where
  toValue _ = Encoder.numeric
instance ToValue ('PGType "uuid") UUID where
  toValue _ = Encoder.uuid
instance ToJSON x => ToValue ('PGType "json") x where
  toValue _ = Encoder.json_bytes . Lazy.toStrict . encode
instance ToJSON x => ToValue ('PGType "jsonb") x where
  toValue _ = Encoder.jsonb_bytes . Lazy.toStrict . encode
instance ToValue ('PGType "date") Day where
  toValue _ = Encoder.date
instance ToValue ('PGType "time") TimeOfDay where
  toValue _ = Encoder.time_int
instance ToValue ('PGType "timetz") (TimeOfDay, TimeZone) where
  toValue _ = Encoder.timetz_int
instance ToValue ('PGType "timestamp") LocalTime where
  toValue _ = Encoder.timestamp_int
instance ToValue ('PGType "timestamptz") UTCTime where
  toValue _ = Encoder.timestamptz_int
instance ToValue ('PGType "interval") DiffTime where
  toValue _ = Encoder.interval_int

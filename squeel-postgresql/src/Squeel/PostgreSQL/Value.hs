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
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as Lazy (ByteString, toStrict)
import Data.Int (Int16,Int32,Int64)
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Functor
import PostgreSQL.Binary.Decoder (Decoder)
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

class ToValue x (pg :: PGType) where
  toValue :: Proxy pg -> x -> Builder
instance ToValue Int16 ('PGType "int2") where
  toValue _ = Encoder.int2_int16
instance ToValue Int32 ('PGType "int4") where
  toValue _ = Encoder.int4_int32
instance ToValue Int64 ('PGType "int8") where
  toValue _ = Encoder.int8_int64
instance ToValue Word16 ('PGType "word2") where
  toValue _ = Encoder.int2_word16
instance ToValue Word32 ('PGType "word4") where
  toValue _ = Encoder.int4_word32
instance ToValue Word64 ('PGType "word8") where
  toValue _ = Encoder.int8_word64
instance ToValue Float ('PGType "float4") where
  toValue _ = Encoder.float4
instance ToValue Double ('PGType "float8") where
  toValue _ = Encoder.float8
instance ToValue Bool ('PGType "bool") where
  toValue _ = Encoder.bool
instance ToValue ByteString ('PGType "bytea") where
  toValue _ = Encoder.bytea_strict
instance ToValue Lazy.ByteString ('PGType "bytea") where
  toValue _ = Encoder.bytea_lazy
instance ToValue Text ('PGType "text") where
  toValue _ = Encoder.text_strict
instance ToValue Lazy.Text ('PGType "text") where
  toValue _ = Encoder.text_lazy
instance ToValue Char ('PGType "char") where
  toValue _ = Encoder.char
instance ToValue Scientific ('PGType "numeric") where
  toValue _ = Encoder.numeric
instance ToValue UUID ('PGType "uuid") where
  toValue _ = Encoder.uuid
instance ToJSON x => ToValue x ('PGType "json") where
  toValue _ = Encoder.json_bytes . Lazy.toStrict . encode
instance ToJSON x => ToValue x ('PGType "jsonb") where
  toValue _ = Encoder.jsonb_bytes . Lazy.toStrict . encode
instance ToValue Day ('PGType "date") where
  toValue _ = Encoder.date
instance ToValue TimeOfDay ('PGType "time") where
  toValue _ = Encoder.time_int
instance ToValue (TimeOfDay, TimeZone) ('PGType "timetz") where
  toValue _ = Encoder.timetz_int
instance ToValue LocalTime ('PGType "timestamp") where
  toValue _ = Encoder.timestamp_int
instance ToValue UTCTime ('PGType "timestamptz") where
  toValue _ = Encoder.timestamptz_int
instance ToValue DiffTime ('PGType "interval") where
  toValue _ = Encoder.interval_int

class ToValues xs pgs where
  toValues :: Proxy pgs -> Rec Identity xs -> [ByteString]
instance ToValues '[] '[] where toValues _ _ = []
instance (ToValue x pg, ToValues xs pgs)
  => ToValues (x ': xs) (pg ': pgs) where
    toValues (_ :: Proxy (pg ': pgs)) (Identity x :& xs) =
      Encoder.run (toValue (Proxy @pg)) x : toValues (Proxy @pgs) xs

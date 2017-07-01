{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , MagicHash
  , MultiParamTypeClasses
  , PolyKinds
  , ScopedTypeVariables
  , TypeOperators
#-}

module Squeel.PostgreSQL.Binary where

import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import Data.Int (Int16,Int32,Int64)
import Generics.SOP
-- import GHC.Generics
-- import PostgreSQL.Binary.Decoding
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import Data.UUID (UUID)
-- import GHC.Exts

import qualified Data.ByteString.Lazy as Lazy (ByteString, toStrict)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified PostgreSQL.Binary.Decoding as Decoder
import qualified PostgreSQL.Binary.Encoding as Encoder

import Squeel.PostgreSQL.Schema

decodeValue :: HasDecoding pg x => proxy pg -> ByteString -> Either Text x
decodeValue p = Decoder.valueParser $ decoding p

class HasDecoding pg x where
  decoding :: proxy pg -> Decoder.Value x
instance HasDecoding 'PGInt2 Int16 where
  decoding _ = Decoder.int
instance HasDecoding 'PGInt4 Int32 where
  decoding _ = Decoder.int
instance HasDecoding 'PGInt8 Int64 where
  decoding _ = Decoder.int
instance HasDecoding 'PGFloat4 Float where
  decoding _ = Decoder.float4
instance HasDecoding 'PGFloat8 Double where
  decoding _ = Decoder.float8
instance HasDecoding 'PGBool Bool where
  decoding _ = Decoder.bool
instance HasDecoding 'PGBytea ByteString where
  decoding _ = Decoder.bytea_strict
instance HasDecoding 'PGBytea Lazy.ByteString where
  decoding _ = Decoder.bytea_lazy
instance HasDecoding 'PGText Text where
  decoding _ = Decoder.text_strict
instance HasDecoding 'PGText Lazy.Text where
  decoding _ = Decoder.text_lazy
instance HasDecoding ('PGChar 1) Char where
  decoding _ = Decoder.char
instance HasDecoding 'PGNumeric Scientific where
  decoding _ = Decoder.numeric
instance HasDecoding 'PGUuid UUID where
  decoding _ = Decoder.uuid
instance FromJSON x => HasDecoding 'PGJson x where
  decoding _ = Decoder.json_bytes (left Text.pack . eitherDecodeStrict)
instance FromJSON x => HasDecoding 'PGJsonb x where
  decoding _ = Decoder.jsonb_bytes (left Text.pack . eitherDecodeStrict)
instance HasDecoding 'PGDate Day where
  decoding _ = Decoder.date
instance HasDecoding 'PGTime TimeOfDay where
  decoding _ = Decoder.time_int
instance HasDecoding 'PGTimeTZ (TimeOfDay, TimeZone) where
  decoding _ = Decoder.timetz_int
instance HasDecoding 'PGTimestamp LocalTime where
  decoding _ = Decoder.timestamp_int
instance HasDecoding 'PGTimestampTZ UTCTime where
  decoding _ = Decoder.timestamptz_int
instance HasDecoding 'PGInterval DiffTime where
  decoding _ = Decoder.interval_int

decodings
  :: AllZip HasDecoding pgs xs
  => proxy pgs
  -> NP (K ByteString) xs -> Either Text (NP I xs)
decodings _ Nil = return Nil
decodings (_ :: proxy pgs) (K bytestring :* bytestrings) =
  case (hpure Proxy :: NP Proxy pgs) of
    proxy :* proxies -> (:*)
      <$> (I <$> Decoder.valueParser (decoding proxy) bytestring)
      <*> decodings proxies bytestrings

class HasEncoding x pg where
  encoding :: proxy pg -> x -> Encoder.Encoding
instance HasEncoding Int16 'PGInt2 where
  encoding _ = Encoder.int2_int16
instance HasEncoding Int32 'PGInt4 where
  encoding _ = Encoder.int4_int32
instance HasEncoding Int64 'PGInt8 where
  encoding _ = Encoder.int8_int64
instance HasEncoding Float 'PGFloat4 where
  encoding _ = Encoder.float4
instance HasEncoding Double 'PGFloat8 where
  encoding _ = Encoder.float8
instance HasEncoding Bool 'PGBool where
  encoding _ = Encoder.bool
instance HasEncoding ByteString 'PGBytea where
  encoding _ = Encoder.bytea_strict
instance HasEncoding Lazy.ByteString 'PGBytea where
  encoding _ = Encoder.bytea_lazy
instance HasEncoding Text 'PGText where
  encoding _ = Encoder.text_strict
instance HasEncoding Lazy.Text 'PGText where
  encoding _ = Encoder.text_lazy
instance HasEncoding Char ('PGChar 1) where
  encoding _ = Encoder.char_utf8
instance HasEncoding Scientific 'PGNumeric where
  encoding _ = Encoder.numeric
instance HasEncoding UUID 'PGUuid where
  encoding _ = Encoder.uuid
instance ToJSON x => HasEncoding x 'PGJson where
  encoding _ = Encoder.json_bytes . Lazy.toStrict . encode
instance ToJSON x => HasEncoding x 'PGJsonb where
  encoding _ = Encoder.jsonb_bytes . Lazy.toStrict . encode
instance HasEncoding Day 'PGDate where
  encoding _ = Encoder.date
instance HasEncoding TimeOfDay 'PGTime where
  encoding _ = Encoder.time_int
instance HasEncoding (TimeOfDay, TimeZone) 'PGTimeTZ where
  encoding _ = Encoder.timetz_int
instance HasEncoding LocalTime 'PGTimestamp where
  encoding _ = Encoder.timestamp_int
instance HasEncoding UTCTime 'PGTimestampTZ where
  encoding _ = Encoder.timestamptz_int
instance HasEncoding DiffTime 'PGInterval where
  encoding _ = Encoder.interval_int

encodings
  :: AllZip HasEncoding xs pgs
  => proxy pgs
  -> NP I xs
  -> [ByteString]
encodings _ Nil = []
encodings (_ :: proxy pgs) (I x :* xs) =
  case (hpure Proxy :: NP Proxy pgs) of
    proxy :* proxies -> Encoder.encodingBytes (encoding proxy x)
      : encodings proxies xs

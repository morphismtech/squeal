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

import Data.ByteString (ByteString)
import Data.Int (Int16,Int32,Int64)
import Generics.SOP
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import Data.UUID (UUID)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified PostgreSQL.Binary.Decoding as Decoder
import qualified PostgreSQL.Binary.Encoding as Encoder

import Squeel.PostgreSQL.Schema

decodeValue :: HasDecoding pg x => proxy pg -> ByteString -> Either Text x
decodeValue p = Decoder.valueParser $ decoding p

class HasDecoding pg x where
  decoding :: proxy pg -> Decoder.Value x
instance HasDecoding (column ::: 'Required ('NotNull 'PGInt2)) Int16 where
  decoding _ = Decoder.int
instance HasDecoding (column ::: 'Required ('NotNull 'PGInt4)) Int32 where
  decoding _ = Decoder.int
instance HasDecoding (column ::: 'Required ('NotNull 'PGInt8)) Int64 where
  decoding _ = Decoder.int
instance HasDecoding (column ::: 'Required ('NotNull 'PGFloat4)) Float where
  decoding _ = Decoder.float4
instance HasDecoding (column ::: 'Required ('NotNull 'PGFloat8)) Double where
  decoding _ = Decoder.float8
instance HasDecoding (column ::: 'Required ('NotNull 'PGBool)) Bool where
  decoding _ = Decoder.bool
instance HasDecoding (column ::: 'Required ('NotNull 'PGBytea)) ByteString where
  decoding _ = Decoder.bytea_strict
instance HasDecoding (column ::: 'Required ('NotNull 'PGBytea)) Lazy.ByteString where
  decoding _ = Decoder.bytea_lazy
instance HasDecoding (column ::: 'Required ('NotNull 'PGText)) Text where
  decoding _ = Decoder.text_strict
instance HasDecoding (column ::: 'Required ('NotNull 'PGText)) Lazy.Text where
  decoding _ = Decoder.text_lazy
instance HasDecoding (column ::: 'Required ('NotNull ('PGChar 1))) Char where
  decoding _ = Decoder.char
instance HasDecoding (column ::: 'Required ('NotNull 'PGNumeric)) Scientific where
  decoding _ = Decoder.numeric
instance HasDecoding (column ::: 'Required ('NotNull 'PGUuid)) UUID where
  decoding _ = Decoder.uuid
instance HasDecoding (column ::: 'Required ('NotNull 'PGJson)) Aeson.Value where
  decoding _ = Decoder.json_ast
instance HasDecoding (column ::: 'Required ('NotNull 'PGJsonb)) Aeson.Value where
  decoding _ = Decoder.jsonb_ast
instance HasDecoding (column ::: 'Required ('NotNull 'PGDate)) Day where
  decoding _ = Decoder.date
instance HasDecoding (column ::: 'Required ('NotNull 'PGTime)) TimeOfDay where
  decoding _ = Decoder.time_int
instance HasDecoding (column ::: 'Required ('NotNull 'PGTimeTZ)) (TimeOfDay, TimeZone) where
  decoding _ = Decoder.timetz_int
instance HasDecoding (column ::: 'Required ('NotNull 'PGTimestamp)) LocalTime where
  decoding _ = Decoder.timestamp_int
instance HasDecoding (column ::: 'Required ('NotNull 'PGTimestampTZ)) UTCTime where
  decoding _ = Decoder.timestamptz_int
instance HasDecoding (column ::: 'Required ('NotNull 'PGInterval)) DiffTime where
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

class HasEncoding pg x where
  encoding :: proxy pg -> x -> Encoder.Encoding
instance HasEncoding ('Required ('NotNull 'PGInt2)) Int16 where
  encoding _ = Encoder.int2_int16
instance HasEncoding ('Required ('NotNull 'PGInt4)) Int32 where
  encoding _ = Encoder.int4_int32
instance HasEncoding ('Required ('NotNull 'PGInt8)) Int64 where
  encoding _ = Encoder.int8_int64
instance HasEncoding ('Required ('NotNull 'PGFloat4)) Float where
  encoding _ = Encoder.float4
instance HasEncoding ('Required ('NotNull 'PGFloat8)) Double where
  encoding _ = Encoder.float8
instance HasEncoding ('Required ('NotNull 'PGBool)) Bool where
  encoding _ = Encoder.bool
instance HasEncoding ('Required ('NotNull 'PGBytea)) ByteString where
  encoding _ = Encoder.bytea_strict
instance HasEncoding ('Required ('NotNull 'PGBytea)) Lazy.ByteString where
  encoding _ = Encoder.bytea_lazy
instance HasEncoding ('Required ('NotNull 'PGText)) Text where
  encoding _ = Encoder.text_strict
instance HasEncoding ('Required ('NotNull 'PGText)) Lazy.Text where
  encoding _ = Encoder.text_lazy
instance HasEncoding ('Required ('NotNull ('PGChar 1))) Char where
  encoding _ = Encoder.char_utf8
instance HasEncoding ('Required ('NotNull 'PGNumeric)) Scientific where
  encoding _ = Encoder.numeric
instance HasEncoding ('Required ('NotNull 'PGUuid)) UUID where
  encoding _ = Encoder.uuid
instance HasEncoding ('Required ('NotNull 'PGJson)) Aeson.Value where
  encoding _ = Encoder.json_ast
instance HasEncoding ('Required ('NotNull 'PGJsonb)) Aeson.Value where
  encoding _ = Encoder.jsonb_ast
instance HasEncoding ('Required ('NotNull 'PGDate)) Day where
  encoding _ = Encoder.date
instance HasEncoding ('Required ('NotNull 'PGTime)) TimeOfDay where
  encoding _ = Encoder.time_int
instance HasEncoding ('Required ('NotNull 'PGTimeTZ)) (TimeOfDay, TimeZone) where
  encoding _ = Encoder.timetz_int
instance HasEncoding ('Required ('NotNull 'PGTimestamp)) LocalTime where
  encoding _ = Encoder.timestamp_int
instance HasEncoding ('Required ('NotNull 'PGTimestampTZ)) UTCTime where
  encoding _ = Encoder.timestamptz_int
instance HasEncoding ('Required ('NotNull 'PGInterval)) DiffTime where
  encoding _ = Encoder.interval_int

encodings
  :: AllZip HasEncoding pgs xs
  => proxy pgs
  -> NP I xs
  -> [ByteString]
encodings _ Nil = []
encodings (_ :: proxy pgs) (I x :* xs) =
  case (hpure Proxy :: NP Proxy pgs) of
    proxy :* proxies -> Encoder.encodingBytes (encoding proxy x)
      : encodings proxies xs

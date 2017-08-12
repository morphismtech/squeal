{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , KindSignatures
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Squeel.PostgreSQL.Binary2 where

import Data.Aeson hiding (Null)
import Data.Bits
import Data.Int
import Data.Scientific
import Data.Time
import Data.Word
import Data.UUID
import Generics.SOP
import Network.IP.Addr

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict hiding (unpack)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Strict
import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding

import Squeel.PostgreSQL.Schema

class ToParam x (pg :: PGType) where toParam :: x -> K Encoding.Encoding pg
instance ToParam Bool 'PGBool where toParam = K . Encoding.bool
instance ToParam Int16 'PGInt2 where toParam = K . Encoding.int2_int16
instance ToParam Word16 'PGInt2 where toParam = K . Encoding.int2_word16
instance ToParam Int32 'PGInt4 where toParam = K . Encoding.int4_int32
instance ToParam Word32 'PGInt4 where toParam = K . Encoding.int4_word32
instance ToParam Int64 'PGInt8 where toParam = K . Encoding.int8_int64
instance ToParam Word64 'PGInt8 where toParam = K . Encoding.int8_word64
instance ToParam Float 'PGFloat4 where toParam = K . Encoding.float4
instance ToParam Double 'PGFloat8 where toParam = K . Encoding.float8
instance ToParam Scientific 'PGNumeric where toParam = K . Encoding.numeric
instance ToParam UUID 'PGUuid where toParam = K . Encoding.uuid
instance ToParam (NetAddr IP) 'PGInet where toParam = K . Encoding.inet
instance ToParam Char ('PGChar 1) where toParam = K . Encoding.char_utf8
instance ToParam Strict.Text 'PGText where toParam = K . Encoding.text_strict
instance ToParam Lazy.Text 'PGText where toParam = K . Encoding.text_lazy
instance ToParam Strict.ByteString 'PGBytea where
  toParam = K . Encoding.bytea_strict
instance ToParam Lazy.ByteString 'PGBytea where
  toParam = K . Encoding.bytea_lazy
instance ToParam Day 'PGDate where toParam = K . Encoding.date
instance ToParam TimeOfDay 'PGTime where toParam = K . Encoding.time_int
instance ToParam (TimeOfDay, TimeZone) 'PGTimeTZ where
  toParam = K . Encoding.timetz_int
instance ToParam LocalTime 'PGTimestamp where
  toParam = K . Encoding.timestamp_int
instance ToParam UTCTime 'PGTimestampTZ where
  toParam = K . Encoding.timestamptz_int
instance ToParam DiffTime 'PGInterval where toParam = K . Encoding.interval_int
instance ToParam Value 'PGJson where toParam = K . Encoding.json_ast
instance ToParam Value 'PGJsonb where toParam = K . Encoding.jsonb_ast

class ToColumnParam x (ty :: ColumnType) where
  toColumnParam :: x -> K (Maybe Strict.ByteString) ty
instance ToParam x pg => ToColumnParam x ('Required ('NotNull pg)) where
  toColumnParam = K . Just . Encoding.encodingBytes . unK . toParam @x @pg
instance ToParam x pg => ToColumnParam (Maybe x) ('Required ('Null pg)) where
  toColumnParam = K . fmap (Encoding.encodingBytes . unK . toParam @x @pg)
instance ToParam x pg => ToColumnParam (Maybe x) ('Optional (nullity pg)) where
  toColumnParam = K . fmap (Encoding.encodingBytes . unK . toParam @x @pg)

class ToParams x (tys :: [ColumnType]) where
  toParams :: x -> NP (K (Maybe Strict.ByteString)) tys
  default toParams
    :: (IsProductType x xs, AllZip ToColumnParam xs tys)
    => x -> NP (K (Maybe Strict.ByteString)) tys
  toParams
    = htrans (Proxy @ToColumnParam) (toColumnParam . unI)
    . unZ . unSOP . from

class FromValue (pg :: PGType) y where
  fromValue :: proxy pg -> Decoding.Value y
instance FromValue 'PGBool Bool where fromValue _ = Decoding.bool
instance (Integral int, Bits int) => FromValue 'PGInt2 int where
  fromValue _ = Decoding.int
instance (Integral int, Bits int) => FromValue 'PGInt4 int where
  fromValue _ = Decoding.int
instance (Integral int, Bits int) => FromValue 'PGInt8 int where
  fromValue _ = Decoding.int
instance FromValue 'PGFloat4 Float where fromValue _ = Decoding.float4
instance FromValue 'PGFloat8 Double where fromValue _ = Decoding.float8
instance FromValue 'PGNumeric Scientific where fromValue _ = Decoding.numeric
instance FromValue 'PGUuid UUID where fromValue _ = Decoding.uuid
instance FromValue 'PGInet (NetAddr IP) where fromValue _ = Decoding.inet
instance FromValue ('PGChar 1) Char where fromValue _ = Decoding.char
instance FromValue 'PGText Strict.Text where fromValue _ = Decoding.text_strict
instance FromValue 'PGText Lazy.Text where fromValue _ = Decoding.text_lazy
instance FromValue 'PGBytea Strict.ByteString where
  fromValue _ = Decoding.bytea_strict
instance FromValue 'PGBytea Lazy.ByteString where
  fromValue _ = Decoding.bytea_lazy
instance FromValue 'PGDate Day where fromValue _ = Decoding.date
instance FromValue 'PGTime TimeOfDay where fromValue _ = Decoding.time_int
instance FromValue 'PGTimeTZ (TimeOfDay, TimeZone) where
  fromValue _ = Decoding.timetz_int
instance FromValue 'PGTimestamp LocalTime where
  fromValue _ = Decoding.timestamp_int
instance FromValue 'PGTimestampTZ UTCTime where
  fromValue _ = Decoding.timestamptz_int
instance FromValue 'PGInterval DiffTime where
  fromValue _ = Decoding.interval_int
instance FromValue 'PGJson Value where fromValue _ = Decoding.json_ast
instance FromValue 'PGJsonb Value where fromValue _ = Decoding.jsonb_ast

class FromColumnValue (colty :: (Symbol,ColumnType)) y where
  fromColumnValue :: K (Maybe Strict.ByteString) colty -> y
instance FromValue pg y
  => FromColumnValue (column ::: ('Required ('NotNull pg))) y where
    fromColumnValue = \case
      K Nothing -> error "fromColumnValue: saw NULL when expecting NOT NULL"
      K (Just bytes) ->
        let
          errOrValue =
            Decoding.valueParser (fromValue @pg @y Proxy) bytes
          err str = error $ "fromColumnValue: " ++ Strict.unpack str
        in
          either err id errOrValue
instance FromValue pg y
  => FromColumnValue (column ::: ('Required ('Null pg))) (Maybe y) where
    fromColumnValue (K nullOrBytes)
      = either err id
      . Decoding.valueParser (fromValue @pg @y Proxy)
      <$> nullOrBytes
      where
        err str = error $ "fromColumnValue: " ++ Strict.unpack str

class FromRow (columns :: [(Symbol,ColumnType)]) y where
  fromRow :: NP (K (Maybe Strict.ByteString)) columns -> y
  default fromRow
    :: (IsProductType y ys, AllZip FromColumnValue columns ys)
    => NP (K (Maybe Strict.ByteString)) columns -> y
  fromRow
    = to . SOP . Z
    . htrans (Proxy @FromColumnValue) (I . fromColumnValue)

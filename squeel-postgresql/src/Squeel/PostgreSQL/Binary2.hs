{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , ScopedTypeVariables
#-}

module Squeel.PostgreSQL.Binary2 where

import Data.Int
import Data.Scientific
import Data.Time
import Data.Word
import Data.UUID
import Generics.SOP
import Network.IP.Addr

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
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

class ToColumnParam x (ty :: ColumnType) where
  toColumnParam :: x -> K (Maybe Encoding.Encoding) ty
instance ToParam x pg => ToColumnParam x ('Required ('NotNull pg)) where
  toColumnParam = K . Just . unK . (toParam :: x -> K Encoding.Encoding pg)
instance ToParam x pg => ToColumnParam (Maybe x) ('Required ('Null pg)) where
  toColumnParam = K . fmap (unK . (toParam :: x -> K Encoding.Encoding pg))
instance ToParam x pg => ToColumnParam (Maybe x) ('Optional (nullity pg)) where
  toColumnParam = K . fmap (unK . (toParam :: x -> K Encoding.Encoding pg))

class ToParams x (tys :: [ColumnType]) where
  toParams :: x -> NP (K (Maybe Encoding.Encoding)) tys
  default toParams
    :: (IsProductType x xs, AllZip ToColumnParam xs tys)
    => x -> NP (K (Maybe Encoding.Encoding)) tys
  toParams
    = htrans (Proxy :: Proxy ToColumnParam) (toColumnParam . unI)
    . unZ . unSOP . from

class FromValue pg y where
  fromValue :: proxy pg -> Decoding.Value y
class FromRow pgs y where
  fromRow :: NP proxy pgs -> Decoding.Value y
  default fromRow
    :: (IsProductType y ys, AllZip FromValue pgs ys)
    => NP proxy pgs -> Decoding.Value y
  fromRow
    = fmap to . hsequence
    . htrans (Proxy :: Proxy FromValue) fromValue . SOP . Z

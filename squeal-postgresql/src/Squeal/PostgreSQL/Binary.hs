{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , KindSignatures
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Binary where

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
import qualified Generics.SOP.Type.Metadata as Type
import qualified GHC.Generics as GHC
import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding

import Squeal.PostgreSQL.Schema

class ToParam x (pg :: PGType) where toParam :: x -> K Encoding.Encoding pg
instance ToParam Bool 'PGbool where toParam = K . Encoding.bool
instance ToParam Int16 'PGint2 where toParam = K . Encoding.int2_int16
instance ToParam Word16 'PGint2 where toParam = K . Encoding.int2_word16
instance ToParam Int32 'PGint4 where toParam = K . Encoding.int4_int32
instance ToParam Word32 'PGint4 where toParam = K . Encoding.int4_word32
instance ToParam Int64 'PGint8 where toParam = K . Encoding.int8_int64
instance ToParam Word64 'PGint8 where toParam = K . Encoding.int8_word64
instance ToParam Float 'PGfloat4 where toParam = K . Encoding.float4
instance ToParam Double 'PGfloat8 where toParam = K . Encoding.float8
instance ToParam Scientific 'PGnumeric where toParam = K . Encoding.numeric
instance ToParam UUID 'PGuuid where toParam = K . Encoding.uuid
instance ToParam (NetAddr IP) 'PGinet where toParam = K . Encoding.inet
instance ToParam Char ('PGchar 1) where toParam = K . Encoding.char_utf8
instance ToParam Strict.Text 'PGtext where toParam = K . Encoding.text_strict
instance ToParam Lazy.Text 'PGtext where toParam = K . Encoding.text_lazy
instance ToParam Strict.ByteString 'PGbytea where
  toParam = K . Encoding.bytea_strict
instance ToParam Lazy.ByteString 'PGbytea where
  toParam = K . Encoding.bytea_lazy
instance ToParam Day 'PGdate where toParam = K . Encoding.date
instance ToParam TimeOfDay 'PGtime where toParam = K . Encoding.time_int
instance ToParam (TimeOfDay, TimeZone) 'PGtimetz where
  toParam = K . Encoding.timetz_int
instance ToParam LocalTime 'PGtimestamp where
  toParam = K . Encoding.timestamp_int
instance ToParam UTCTime 'PGtimestamptz where
  toParam = K . Encoding.timestamptz_int
instance ToParam DiffTime 'PGinterval where toParam = K . Encoding.interval_int
instance ToParam Value 'PGjson where toParam = K . Encoding.json_ast
instance ToParam Value 'PGjsonb where toParam = K . Encoding.jsonb_ast

class ToColumnParam x (ty :: ColumnType) where
  toColumnParam :: x -> K (Maybe Strict.ByteString) ty
instance ToParam x pg => ToColumnParam x ('Required ('NotNull pg)) where
  toColumnParam = K . Just . Encoding.encodingBytes . unK . toParam @x @pg
instance ToParam x pg => ToColumnParam (Maybe x) ('Required ('Null pg)) where
  toColumnParam = K . fmap (Encoding.encodingBytes . unK . toParam @x @pg)
instance ToParam x pg => ToColumnParam (Maybe x) ('Optional (nullity pg)) where
  toColumnParam = K . fmap (Encoding.encodingBytes . unK . toParam @x @pg)

class SListI tys => ToParams x (tys :: [ColumnType]) where
  toParams :: x -> NP (K (Maybe Strict.ByteString)) tys
instance (SListI tys, IsProductType x xs, AllZip ToColumnParam xs tys)
  => ToParams x tys where
      toParams
        = htrans (Proxy @ToColumnParam) (toColumnParam . unI)
        . unZ . unSOP . from

class FromValue (pg :: PGType) y where
  fromValue :: proxy pg -> Decoding.Value y
instance FromValue 'PGbool Bool where fromValue _ = Decoding.bool
instance (Integral int, Bits int) => FromValue 'PGint2 int where
  fromValue _ = Decoding.int
instance (Integral int, Bits int) => FromValue 'PGint4 int where
  fromValue _ = Decoding.int
instance (Integral int, Bits int) => FromValue 'PGint8 int where
  fromValue _ = Decoding.int
instance FromValue 'PGfloat4 Float where fromValue _ = Decoding.float4
instance FromValue 'PGfloat8 Double where fromValue _ = Decoding.float8
instance FromValue 'PGnumeric Scientific where fromValue _ = Decoding.numeric
instance FromValue 'PGuuid UUID where fromValue _ = Decoding.uuid
instance FromValue 'PGinet (NetAddr IP) where fromValue _ = Decoding.inet
instance FromValue ('PGchar 1) Char where fromValue _ = Decoding.char
instance FromValue 'PGtext Strict.Text where fromValue _ = Decoding.text_strict
instance FromValue 'PGtext Lazy.Text where fromValue _ = Decoding.text_lazy
instance FromValue 'PGbytea Strict.ByteString where
  fromValue _ = Decoding.bytea_strict
instance FromValue 'PGbytea Lazy.ByteString where
  fromValue _ = Decoding.bytea_lazy
instance FromValue 'PGdate Day where fromValue _ = Decoding.date
instance FromValue 'PGtime TimeOfDay where fromValue _ = Decoding.time_int
instance FromValue 'PGtimetz (TimeOfDay, TimeZone) where
  fromValue _ = Decoding.timetz_int
instance FromValue 'PGtimestamp LocalTime where
  fromValue _ = Decoding.timestamp_int
instance FromValue 'PGtimestamptz UTCTime where
  fromValue _ = Decoding.timestamptz_int
instance FromValue 'PGinterval DiffTime where
  fromValue _ = Decoding.interval_int
instance FromValue 'PGjson Value where fromValue _ = Decoding.json_ast
instance FromValue 'PGjsonb Value where fromValue _ = Decoding.jsonb_ast

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

class SListI columns => FromRow (columns :: [(Symbol,ColumnType)]) y where
  fromRow :: NP (K (Maybe Strict.ByteString)) columns -> y
instance
  ( SListI columns, IsProductType y ys
  , AllZip FromColumnValue columns ys
  , HasDatatypeInfo y
  , SameFields (DatatypeInfoOf y) columns
  ) => FromRow columns y where
    fromRow
      = to . SOP . Z . htrans (Proxy @FromColumnValue) (I . fromColumnValue)

class SameField
  (fieldInfo :: Type.FieldInfo) (columnty :: (Symbol,ColumnType)) where
instance field ~ column => SameField ('Type.FieldInfo field) (column ::: ty)

type family SameFields
  (datatypeInfo :: Type.DatatypeInfo) (columns :: [(Symbol,ColumnType)]) where
    SameFields
      ('Type.ADT _module _datatype '[ 'Type.Record _constructor fields])
      columns
        = AllZip SameField fields columns
    SameFields
      ('Type.Newtype _module _datatype ('Type.Record _constructor '[field]))
      '[column]
        = SameField field column

newtype Only x = Only { only :: x }
  deriving (Functor,Foldable,Traversable,Eq,Ord,Read,Show,GHC.Generic)
instance Generic (Only x)
instance HasDatatypeInfo (Only x)

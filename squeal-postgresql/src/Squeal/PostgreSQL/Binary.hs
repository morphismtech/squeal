{-|
Module: Squeal.PostgreSQL.Binary
Description: Binary encoding and decoding
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Binary encoding and decoding between Haskell and PostgreSQL types.
-}

{-# LANGUAGE
    ConstraintKinds
  , DataKinds
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
  , TypeFamilyDependencies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Binary
  ( -- * Encoding
    ToParam (..)
  , ToColumnParam (..)
  , ToParams (..)
    -- * Decoding
  , FromValue (..)
  , FromColumnValue (..)
  , FromRow (..)
    -- * Only
  , Only (..)
  ) where

import BinaryParser
import Data.Aeson hiding (Null)
import Data.Int
import Data.Kind
import Data.Scientific
import Data.Time
import Data.UUID.Types
import Data.Vector (Vector)
import Data.Word
import Generics.SOP
import GHC.TypeLits
import Network.IP.Addr

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict hiding (unpack)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Strict
import qualified Data.Vector as Vector
import qualified GHC.Generics as GHC
import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding

import Squeal.PostgreSQL.Schema

-- | A `ToParam` constraint gives an encoding of a Haskell `Type` into
-- into the binary format of a PostgreSQL `PGType`.
class ToParam (x :: Type) (pg :: PGType) where
  -- | >>> :set -XTypeApplications -XDataKinds
  -- >>> toParam @Bool @'PGbool False
  -- K "\NUL"
  --
  -- >>> toParam @Int16 @'PGint2 0
  -- K "\NUL\NUL"
  --
  -- >>> toParam @Int32 @'PGint4 0
  -- K "\NUL\NUL\NUL\NUL"
  --
  -- >>> :set -XMultiParamTypeClasses
  -- >>> newtype Id = Id { getId :: Int16 } deriving Show
  -- >>> instance ToParam Id 'PGint2 where toParam = toParam . getId
  -- >>> toParam @Id @'PGint2 (Id 1)
  -- K "\NUL\SOH"
  toParam :: x -> K Encoding.Encoding pg
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
instance (HasOid pg, ToParam x pg)
  => ToParam (Vector (Maybe x)) ('PGvararray pg) where
    toParam = K . Encoding.nullableArray_vector
      (oid @pg) (unK . toParam @x @pg)

-- | A `ToColumnParam` constraint lifts the `ToParam` encoding 
-- of a `Type` to a `ColumnType`, encoding `Maybe`s to `Null`s. You should
-- not define instances of `ToColumnParam`, just use the provided instances.
class ToColumnParam (x :: Type) (ty :: NullityType) where
  -- | >>> toColumnParam @Int16 @('NotNull 'PGint2) 0
  -- K (Just "\NUL\NUL")
  --
  -- >>> toColumnParam @(Maybe Int16) @('Null 'PGint2) (Just 0)
  -- K (Just "\NUL\NUL")
  --
  -- >>> toColumnParam @(Maybe Int16) @('Null 'PGint2) Nothing
  -- K Nothing
  toColumnParam :: x -> K (Maybe Strict.ByteString) ty
instance ToParam x pg => ToColumnParam x ('NotNull pg) where
  toColumnParam = K . Just . Encoding.encodingBytes . unK . toParam @x @pg
instance ToParam x pg => ToColumnParam (Maybe x) ('Null pg) where
  toColumnParam = K . fmap (Encoding.encodingBytes . unK . toParam @x @pg)

-- | A `ToParams` constraint generically sequences the encodings of `Type`s
-- of the fields of a tuple or record to a row of `ColumnType`s. You should
-- not define instances of `ToParams`. Instead define `Generic` instances
-- which in turn provide `ToParams` instances.
class SListI tys => ToParams (x :: Type) (tys :: [NullityType]) where
  -- | >>> type Params = '[ 'NotNull 'PGbool, 'Null 'PGint2]
  -- >>> toParams @(Bool, Maybe Int16) @'[ 'NotNull 'PGbool, 'Null 'PGint2] (False, Just 0)
  -- K (Just "\NUL") :* K (Just "\NUL\NUL") :* Nil
  --
  -- >>> :set -XDeriveGeneric
  -- >>> data Tuple = Tuple { p1 :: Bool, p2 :: Maybe Int16} deriving GHC.Generic
  -- >>> instance Generic Tuple
  -- >>> toParams @Tuple @Params (Tuple False (Just 0))
  -- K (Just "\NUL") :* K (Just "\NUL\NUL") :* Nil
  toParams :: x -> NP (K (Maybe Strict.ByteString)) tys
instance (SListI tys, IsProductType x xs, AllZip ToColumnParam xs tys)
  => ToParams x tys where
      toParams
        = htrans (Proxy @ToColumnParam) (toColumnParam . unI)
        . unZ . unSOP . from

-- | A `FromValue` constraint gives a parser from the binary format of
-- a PostgreSQL `PGType` into a Haskell `Type`.
class FromValue (pg :: PGType) (y :: Type) where
  -- | >>> newtype Id = Id { getId :: Int16 } deriving Show
  -- >>> instance FromValue 'PGint2 Id where fromValue = fmap Id . fromValue
  fromValue :: proxy pg -> Decoding.Value y
instance FromValue 'PGbool Bool where fromValue _ = Decoding.bool
instance FromValue 'PGint2 Int16 where fromValue _ = Decoding.int
instance FromValue 'PGint4 Int32 where fromValue _ = Decoding.int
instance FromValue 'PGint8 Int64 where fromValue _ = Decoding.int
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
instance FromValue pg y => FromValue ('PGvararray pg) (Vector (Maybe y)) where
  fromValue _ = Decoding.array
    (Decoding.dimensionArray Vector.replicateM
      (Decoding.nullableValueArray (fromValue (Proxy @pg))))
instance FromValue pg y => FromValue ('PGfixarray n pg) (Vector (Maybe y)) where
  fromValue _ = Decoding.array
    (Decoding.dimensionArray Vector.replicateM
      (Decoding.nullableValueArray (fromValue (Proxy @pg))))
instance
  ( SListI fields
  , IsMaybes ys
  , IsProductType y (Maybes ys)
  , AllZip FromAliasedValue fields ys
  , SameFields (DatatypeInfoOf y) fields
  ) => FromValue ('PGcomposite fields) y where
    fromValue = fmap (to . SOP . Z . maybes) . composite . decoders

class IsMaybes (xs :: [Type]) where
  type family Maybes xs = (mxs :: [Type]) | mxs -> xs
  maybes :: NP Maybe xs -> NP I (Maybes xs)

instance IsMaybes '[] where
  type Maybes '[] = '[]
  maybes Nil = Nil

instance IsMaybes xs => IsMaybes (x ': xs) where
  type Maybes (x ': xs) = Maybe x ': Maybes xs
  maybes (x :* xs) = I x :* maybes xs

class FromAliasedValue (pg :: (Symbol,PGType)) (y :: Type) where
  fromAliasedValue :: proxy pg -> Decoding.Value y
instance FromValue pg y => FromAliasedValue (alias ::: pg) y where
  fromAliasedValue _ = fromValue (Proxy @pg)

decoders
  :: forall pgs ys proxy
   . AllZip FromAliasedValue pgs ys
  => proxy ('PGcomposite pgs)
  -> NP Decoding.Value ys
decoders _ = htrans (Proxy @FromAliasedValue) fromAliasedValue
  (hpure Proxy :: NP Proxy pgs)

composite
  :: SListI ys
  => NP Decoding.Value ys
  -> Decoding.Value (NP Maybe ys)
composite fields = do
-- [for each field]
--  <OID of field's type: sizeof(Oid) bytes>
--  [if value is NULL]
--    <-1: 4 bytes>
--  [else]
--    <length of value: 4 bytes>
--    <value: <length> bytes>
--  [end if]
-- [end for]
-- <number of fields: 4 bytes>
  unitOfSize 4
  let
    each field = do
      unitOfSize 4
      len <- sized 4 Decoding.int
      if len == -1 then return Nothing else Just <$> sized len field
  htraverse' each fields

-- | A `FromColumnValue` constraint lifts the `FromValue` parser
-- to a decoding of a @(Symbol, ColumnType)@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromColumnValue`, just use the provided instances.
class FromColumnValue (colty :: (Symbol,NullityType)) (y :: Type) where
  -- | >>> :set -XTypeOperators -XOverloadedStrings
  -- >>> newtype Id = Id { getId :: Int16 } deriving Show
  -- >>> instance FromValue 'PGint2 Id where fromValue = fmap Id . fromValue
  -- >>> fromColumnValue @("col" ::: 'NotNull 'PGint2) @Id (K (Just "\NUL\SOH"))
  -- Id {getId = 1}
  --
  -- >>> fromColumnValue @("col" ::: 'Null 'PGint2) @(Maybe Id) (K (Just "\NUL\SOH"))
  -- Just (Id {getId = 1})
  fromColumnValue :: K (Maybe Strict.ByteString) colty -> y
instance FromValue pg y
  => FromColumnValue (column ::: ('NotNull pg)) y where
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
  => FromColumnValue (column ::: ('Null pg)) (Maybe y) where
    fromColumnValue (K nullOrBytes)
      = either err id
      . Decoding.valueParser (fromValue @pg @y Proxy)
      <$> nullOrBytes
      where
        err str = error $ "fromColumnValue: " ++ Strict.unpack str

-- | A `FromRow` constraint generically sequences the parsings of the columns
-- of a `RelationType` into the fields of a record `Type` provided they have
-- the same field names. You should not define instances of `FromRow`.
-- Instead define `Generic` and `HasDatatypeInfo` instances which in turn
-- provide `FromRow` instances.
class SListI results => FromRow (results :: RelationType) y where
  -- | >>> :set -XOverloadedStrings
  -- >>> import Data.Text
  -- >>> newtype UserId = UserId { getUserId :: Int16 } deriving Show
  -- >>> instance FromValue 'PGint2 UserId where fromValue = fmap UserId . fromValue
  -- >>> data UserRow = UserRow { userId :: UserId, userName :: Maybe Text } deriving (Show, GHC.Generic)
  -- >>> instance Generic UserRow
  -- >>> instance HasDatatypeInfo UserRow
  -- >>> type User = '["userId" ::: 'NotNull 'PGint2, "userName" ::: 'Null 'PGtext]
  -- >>> fromRow @User @UserRow (K (Just "\NUL\SOH") :* K (Just "bloodninja") :* Nil)
  -- UserRow {userId = UserId {getUserId = 1}, userName = Just "bloodninja"}
  fromRow :: NP (K (Maybe Strict.ByteString)) results -> y
instance
  ( SListI results
  , IsProductType y ys
  , AllZip FromColumnValue results ys
  , SameFields (DatatypeInfoOf y) results
  ) => FromRow results y where
    fromRow
      = to . SOP . Z . htrans (Proxy @FromColumnValue) (I . fromColumnValue)

-- | `Only` is a 1-tuple type, useful for encoding a single parameter with
-- `toParams` or decoding a single value with `fromRow`.
--
-- >>> import Data.Text
-- >>> toParams @(Only (Maybe Text)) @'[ 'Null 'PGtext] (Only (Just "foo"))
-- K (Just "foo") :* Nil
--
-- >>> fromRow @'["fromOnly" ::: 'Null 'PGtext] @(Only (Maybe Text)) (K (Just "bar") :* Nil)
-- Only {fromOnly = Just "bar"}
newtype Only x = Only { fromOnly :: x }
  deriving (Functor,Foldable,Traversable,Eq,Ord,Read,Show,GHC.Generic)
instance Generic (Only x)
instance HasDatatypeInfo (Only x)

{-|
Module: Squeal.PostgreSQL.PQ.Encode
Description: Decoding of result values
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Decoding of result values
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ.Decode
  ( -- * Decode Rows
    DecodeRow (..)
  , decodeRow
  , runDecodeRow
  , genericRow
    -- * Decoding Classes
  , FromValue (..)
  , FromNullValue (..)
  , FromField (..)
  , FromFixArray (..)
  ) where

import BinaryParser
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Int (Int16, Int32, Int64)
import Data.Kind
import Data.Scientific (Scientific)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import Data.Word (Word32)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import GHC.OverloadedLabels
import GHC.TypeLits
import Network.IP.Addr (NetAddr, IP)
import PostgreSQL.Binary.Decoding hiding (Composite)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text
import qualified Data.Vector as Vector
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression.Range
import Squeal.PostgreSQL.Schema
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG

class FromValue pg y where fromValue :: Value y
instance FromValue 'PGbool Bool where fromValue = bool
instance FromValue 'PGint2 Int16 where fromValue = int
instance FromValue 'PGint4 Int32 where fromValue = int
instance FromValue 'PGint8 Int64 where fromValue = int
instance FromValue 'PGoid Word32 where fromValue = int
instance FromValue 'PGfloat4 Float where fromValue = float4
instance FromValue 'PGfloat8 Double where fromValue = float8
instance FromValue 'PGnumeric Scientific where fromValue = numeric
instance FromValue 'PGmoney Money where fromValue = Money <$> int
instance FromValue 'PGuuid UUID where fromValue = uuid
instance FromValue 'PGinet (NetAddr IP) where fromValue = inet
instance FromValue ('PGchar 1) Char where fromValue = char
instance FromValue 'PGtext Strict.Text where fromValue = text_strict
instance FromValue 'PGtext Lazy.Text where fromValue = text_lazy
instance FromValue 'PGtext String where
  fromValue = Strict.Text.unpack <$> text_strict
instance FromValue 'PGbytea Strict.ByteString where
  fromValue = bytea_strict
instance FromValue 'PGbytea Lazy.ByteString where
  fromValue = bytea_lazy
instance FromValue 'PGdate Day where fromValue = date
instance FromValue 'PGtime TimeOfDay where fromValue = time_int
instance FromValue 'PGtimetz (TimeOfDay, TimeZone) where
  fromValue = timetz_int
instance FromValue 'PGtimestamp LocalTime where
  fromValue = timestamp_int
instance FromValue 'PGtimestamptz UTCTime where
  fromValue = timestamptz_int
instance FromValue 'PGinterval DiffTime where
  fromValue = interval_int
instance FromValue 'PGjson Aeson.Value where fromValue = json_ast
instance FromValue 'PGjsonb Aeson.Value where fromValue = jsonb_ast
instance Aeson.FromJSON x => FromValue 'PGjson (Json x) where
  fromValue = Json <$>
    json_bytes (left Strict.Text.pack . Aeson.eitherDecodeStrict)
instance Aeson.FromJSON x => FromValue 'PGjsonb (Jsonb x) where
  fromValue = Jsonb <$>
    jsonb_bytes (left Strict.Text.pack . Aeson.eitherDecodeStrict)
instance FromValue pg y
  => FromValue ('PGvararray ('NotNull pg)) (VarArray (Vector y)) where
    fromValue =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        array $ dimensionArray rep
          (fromFixArray @'[] @('NotNull pg))
instance FromValue pg y
  => FromValue ('PGvararray ('Null pg)) (VarArray (Vector (Maybe y))) where
    fromValue =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        array $ dimensionArray rep
          (fromFixArray @'[] @('Null pg))
instance FromValue pg y
  => FromValue ('PGvararray ('NotNull pg)) (VarArray [y]) where
    fromValue =
      let
        rep n x = VarArray <$> replicateM n x
      in
        array $ dimensionArray rep
          (fromFixArray @'[] @('NotNull pg))
instance FromValue pg y
  => FromValue ('PGvararray ('Null pg)) (VarArray [Maybe y]) where
    fromValue =
      let
        rep n x = VarArray <$> replicateM n x
      in
        array $ dimensionArray rep
          (fromFixArray @'[] @('Null pg))
instance FromFixArray dims ty y
  => FromValue ('PGfixarray dims ty) (FixArray y) where
    fromValue = FixArray <$> array (fromFixArray @dims @ty @y)
instance
  ( SOP.IsEnumType y
  , SOP.HasDatatypeInfo y
  , LabelsPG y ~ labels
  ) => FromValue ('PGenum labels) (Enumerated y) where
    fromValue =
      let
        greadConstructor
          :: SOP.All ((~) '[]) xss
          => NP SOP.ConstructorInfo xss
          -> String
          -> Maybe (SOP.SOP SOP.I xss)
        greadConstructor Nil _ = Nothing
        greadConstructor (constructor :* constructors) name =
          if name == SOP.constructorName constructor
            then Just (SOP.SOP (SOP.Z Nil))
            else SOP.SOP . SOP.S . SOP.unSOP <$>
              greadConstructor constructors name
      in
        fmap Enumerated
        . enum
        $ fmap SOP.to
        . greadConstructor
          (SOP.constructorInfo (SOP.datatypeInfo (SOP.Proxy @y)))
        . Strict.Text.unpack
instance
  ( SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => FromValue ('PGcomposite row) (Composite y) where
    fromValue =
      let
        -- <number of fields: 4 bytes>
        -- [for each field]
        --  <OID of field's type: sizeof(Oid) bytes>
        --  [if value is NULL]
        --    <-1: 4 bytes>
        --  [else]
        --    <length of value: 4 bytes>
        --    <value: <length> bytes>
        --  [end if]
        -- [end for]
        comp = valueParser $ do
          unitOfSize 4
          SOP.hsequence' $ SOP.hpure $ SOP.Comp $ do
            unitOfSize 4
            len <- sized 4 int
            if len == -1
              then return (SOP.K Nothing)
              else SOP.K . Just <$> bytesOfSize len
      in
        fmap Composite (fn (runDecodeRow (genericRow @row) <=< comp))
instance FromValue pg y => FromValue ('PGrange pg) (Range y) where
  fromValue = do
    flag <- byte
    if testBit flag 0 then return Empty else do
      lower <-
        if testBit flag 3
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (fromValue @pg)
            return $ if testBit flag 1 then Closed l else Open l
      upper <-
        if testBit flag 4
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (fromValue @pg)
            return $ if testBit flag 2 then Closed l else Open l
      return $ NonEmpty lower upper

class FromNullValue ty y where
  fromNullValue :: Maybe Strict.ByteString -> Either Strict.Text y
instance FromValue pg y => FromNullValue ('NotNull pg) y where
  fromNullValue = \case
    Nothing -> throwError "fromField: saw NULL when expecting NOT NULL"
    Just bytestring -> valueParser (fromValue @pg) bytestring
instance FromValue pg y => FromNullValue ('Null pg) (Maybe y) where
  fromNullValue = \case
    Nothing -> return Nothing
    Just bytestring -> fmap Just $
      valueParser (fromValue @pg) bytestring

class FromField field y where
  fromField :: Maybe Strict.ByteString -> Either Strict.Text (SOP.P y)
instance (fld0 ~ fld1, FromNullValue ty y)
  => FromField (fld0 ::: ty) (fld1 ::: y) where
    fromField = fmap SOP.P . fromNullValue @ty

-- | A `FromFixArray` constraint gives a decoding to a Haskell `Type`
-- from the binary format of a PostgreSQL fixed-length array.
-- You should not define instances for
-- `FromFixArray`, just use the provided instances.
class FromFixArray (dims :: [Nat]) (ty :: NullType) (y :: Type) where
  fromFixArray :: Array y
instance FromValue pg y => FromFixArray '[] ('NotNull pg) y where
  fromFixArray = valueArray (fromValue @pg @y)
instance FromValue pg y => FromFixArray '[] ('Null pg) (Maybe y) where
  fromFixArray = nullableValueArray (fromValue @pg @y)
instance
  ( SOP.IsProductType product ys
  , Length ys ~ dim
  , SOP.All ((~) y) ys
  , FromFixArray dims ty y )
  => FromFixArray (dim ': dims) ty product where
    fromFixArray =
      let
        rep _ = fmap (SOP.to . SOP.SOP . SOP.Z) . replicateMN
      in
        dimensionArray rep (fromFixArray @dims @ty @y)

replicateMN
  :: forall x xs m. (SOP.All ((~) x) xs, Monad m, SOP.SListI xs)
  => m x -> m (SOP.NP SOP.I xs)
replicateMN mx = SOP.hsequence' $
  SOP.hcpure (SOP.Proxy :: SOP.Proxy ((~) x)) (SOP.Comp (SOP.I <$> mx))

newtype DecodeRow (row :: RowType) (y :: Type) = DecodeRow
  { unDecodeRow :: ReaderT
      (SOP.NP (SOP.K (Maybe Strict.ByteString)) row) (Except Strict.Text) y }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadError Strict.Text )
runDecodeRow
  :: DecodeRow row y
  -> SOP.NP (SOP.K (Maybe Strict.ByteString)) row
  -> Either Strict.Text y
runDecodeRow = fmap runExcept . runReaderT . unDecodeRow
decodeRow
  :: (SOP.NP (SOP.K (Maybe Strict.ByteString)) row -> Either Strict.Text y)
  -> DecodeRow row y
decodeRow dec = DecodeRow . ReaderT $ liftEither . dec
instance {-# OVERLAPPING #-} FromNullValue ty y
  => IsLabel fld (DecodeRow (fld ::: ty ': row) y) where
    fromLabel = decodeRow $ \(SOP.K b SOP.:* _) ->
      fromNullValue @ty b
instance {-# OVERLAPPABLE #-} IsLabel fld (DecodeRow row y)
  => IsLabel fld (DecodeRow (field ': row) y) where
    fromLabel = decodeRow $ \(_ SOP.:* bs) ->
      runDecodeRow (fromLabel @fld) bs
instance {-# OVERLAPPING #-} FromNullValue ty (Maybe y)
  => IsLabel fld (MaybeT (DecodeRow (fld ::: ty ': row)) y) where
    fromLabel = MaybeT . decodeRow $ \(SOP.K b SOP.:* _) ->
      fromNullValue @ty b
instance {-# OVERLAPPABLE #-} IsLabel fld (MaybeT (DecodeRow row) y)
  => IsLabel fld (MaybeT (DecodeRow (field ': row)) y) where
    fromLabel = MaybeT . decodeRow $ \(_ SOP.:* bs) ->
      runDecodeRow (runMaybeT (fromLabel @fld)) bs
genericRow ::
  ( SOP.SListI row
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => DecodeRow row y
genericRow
  = DecodeRow
  . ReaderT
  $ fmap SOP.fromRecord
  . SOP.hsequence'
  . SOP.htrans (SOP.Proxy @FromField) (SOP.Comp . runField)
runField
  :: forall ty y. FromField ty y
  => SOP.K (Maybe Strict.ByteString) ty
  -> Except Strict.Text (SOP.P y)
runField = liftEither . fromField @ty . SOP.unK

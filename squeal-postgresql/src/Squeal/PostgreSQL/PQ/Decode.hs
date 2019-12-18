{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
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
  ( DecodeRow (..)
  , decodeRow
  , runDecodeRow
  , genericRow
  , FromValue (..)
  , FromNullValue (..)
  , FromField (..)
  , FromFixArray (..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Kind
import GHC.OverloadedLabels
import GHC.TypeLits
import PostgreSQL.Binary.Decoding

-- import qualified Data.ByteString.Lazy as Lazy (ByteString)
-- import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.ByteString as Strict (ByteString)
-- import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text as Strict (Text)
-- import qualified Data.Text as Strict.Text
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Schema
import Squeal.PostgreSQL.List

class FromValue pg y where fromValue :: Value y

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
instance {-# OVERLAPPING #-} (fld0 ~ fld1, FromNullValue ty y)
  => IsLabel fld0 (DecodeRow ((fld1 ::: ty) ': row) y) where
    fromLabel = decodeRow $ \(SOP.K b SOP.:* _) -> fromNullValue @ty b
instance {-# OVERLAPPABLE #-} IsLabel fld (DecodeRow row y)
  => IsLabel fld (DecodeRow (field ': row) y) where
    fromLabel = decodeRow $ \(_ SOP.:* bs) -> runDecodeRow (fromLabel @fld) bs
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
runField (SOP.K b) = liftEither $ fromField @ty b

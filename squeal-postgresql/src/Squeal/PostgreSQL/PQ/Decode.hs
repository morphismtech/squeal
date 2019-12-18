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
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Kind
import GHC.OverloadedLabels
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

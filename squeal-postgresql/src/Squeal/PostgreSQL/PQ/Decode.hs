{-# LANGUAGE
    DataKinds
  , DerivingVia
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

module Squeal.PostgreSQL.PQ.Decode where

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

newtype DecodeValue (pg :: PGType) (y :: Type) = DecodeValue
  { runDecodeValue :: Value y }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadError Strict.Text
    )

newtype DecodeNullValue (ty :: NullityType) (y :: Type) = DecodeNullValue
  { runDecodeNullValue :: ReaderT
      (Maybe Strict.ByteString) (Except Strict.Text) y }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadError Strict.Text
    )

newtype DecodeField
  (ty :: (Symbol, NullityType)) (y :: (Symbol, Type)) = DecodeField
    { runDecodeField :: ReaderT
        (Maybe Strict.ByteString) (Except Strict.Text) (SOP.P y) }

newtype DecodeRow (row :: RowType) (y :: Type) = DecodeRow
  { runDecodeRow :: ReaderT
      (SOP.NP (SOP.K (Maybe Strict.ByteString)) row) (Except Strict.Text) y }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadError Strict.Text
    )

class FromValue pg y where fromValue :: DecodeValue pg y
class FromNullValue ty y where fromNullValue :: DecodeNullValue ty y
instance FromValue pg y => FromNullValue ('NotNull pg) y where
  fromNullValue = DecodeNullValue . ReaderT $ \case
    Nothing -> throwError "fromField: saw NULL when expecting NOT NULL"
    Just bytestring -> liftEither $ valueParser
      (runDecodeValue (fromValue @pg)) bytestring
instance FromValue pg y => FromNullValue ('Null pg) (Maybe y) where
  fromNullValue = DecodeNullValue . ReaderT $ \case
    Nothing -> return Nothing
    Just bytestring -> liftEither . fmap Just $ valueParser
      (runDecodeValue (fromValue @pg)) bytestring
class FromField field y where fromField :: DecodeField field y
instance (fld0 ~ fld1, FromNullValue ty y)
  => FromField (fld0 ::: ty) (fld1 ::: y) where
    fromField = DecodeField . fmap SOP.P $
      runDecodeNullValue (fromNullValue @ty)
class FromRow row y where fromRow :: DecodeRow row y
instance
  ( SOP.SListI row
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => FromRow row y where
    fromRow
      = DecodeRow
      . ReaderT
      $ fmap SOP.fromRecord
      . SOP.hsequence'
      . SOP.htrans (SOP.Proxy @FromField) (SOP.Comp . runField)
runField
  :: forall ty y. FromField ty y
  => SOP.K (Maybe Strict.ByteString) ty
  -> Except Strict.Text (SOP.P y)
runField (SOP.K b) = runReaderT (runDecodeField (fromField @ty)) b

instance {-# OVERLAPPING #-} (fld0 ~ fld1, FromNullValue ty y)
  => IsLabel fld0 (DecodeRow ((fld1 ::: ty) ': row) y) where
    fromLabel = DecodeRow . ReaderT $ \(SOP.K b SOP.:* _) ->
      runReaderT (runDecodeNullValue (fromNullValue @ty)) b
instance {-# OVERLAPPABLE #-} IsLabel fld (DecodeRow row y)
  => IsLabel fld (DecodeRow (field ': row) y) where
    fromLabel = DecodeRow . ReaderT $ \(_ SOP.:* bs) ->
      runReaderT (runDecodeRow (fromLabel @fld)) bs

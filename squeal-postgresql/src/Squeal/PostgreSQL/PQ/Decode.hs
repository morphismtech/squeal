{-# LANGUAGE
    DataKinds
  , DerivingVia
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
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
  { getDecodeValue :: Value y }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadError Strict.Text
    )

newtype DecodeNullValue (ty :: NullityType) (y :: Type) = DecodeNullValue
  { runDecodeNullValue :: Maybe Strict.ByteString -> Either Strict.Text y }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError Strict.Text
    ) via ReaderT (Maybe Strict.ByteString) (Either Strict.Text)

newtype DecodeField
  (field :: (Symbol, NullityType)) (y :: (Symbol, Type)) = DecodeField
    { runDecodeField
      :: Maybe Strict.ByteString
      -> (Either Strict.Text SOP.:.: SOP.P) y }

newtype DecodeRow (row :: RowType) (y :: Type) = DecodeRow
  { runDecodeRow
    :: SOP.NP (SOP.K (Maybe Strict.ByteString)) row
    -> Either Strict.Text y }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError Strict.Text
    ) via ReaderT
      (SOP.NP (SOP.K (Maybe Strict.ByteString)) row)
      (Either Strict.Text)

class FromValue pg y where fromValue :: DecodeValue pg y
class FromNullValue ty y where fromNullValue :: DecodeNullValue ty y
class FromField field y where fromField :: DecodeField field y
class FromRow row y where fromRow :: DecodeRow row y

instance {-# OVERLAPPING #-} (fld0 ~ fld1, FromNullValue ty y)
  => IsLabel fld0 (DecodeRow ((fld1 ::: ty) ': row) y) where
    fromLabel = DecodeRow $ \(SOP.K b SOP.:* _) ->
      runDecodeNullValue (fromNullValue @ty) b
instance {-# OVERLAPPABLE #-} IsLabel fld (DecodeRow row y)
  => IsLabel fld (DecodeRow (field ': row) y) where
    fromLabel = DecodeRow $ \(_ SOP.:* bs) ->
      runDecodeRow (fromLabel @fld) bs

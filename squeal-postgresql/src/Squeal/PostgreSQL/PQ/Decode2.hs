{-|
Module: Squeal.PostgreSQL.PQ.Decode2
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

module Squeal.PostgreSQL.PQ.Decode2
  ( -- * Decode Rows
    DecodeRow (..)
  , decodeRow
  , runDecodeRow
  , genericRow
  , devalue
  , value
    -- * Decoding Classes
  , FromValue (..)
  , FromNullValue (..)
  , FromField (..)
  , FromFixArray (..)
  , StateT (..)
  , ExceptT (..)
  ) where

import BinaryParser
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Int (Int16, Int32, Int64)
import Data.Kind
import Data.Scientific (Scientific)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import GHC.OverloadedLabels
import GHC.TypeLits
import Network.IP.Addr (NetAddr, IP)
import PostgreSQL.Binary.Decoding hiding (Composite)
import Unsafe.Coerce

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
import Squeal.PostgreSQL.Expression.Range2
import Squeal.PostgreSQL.Schema
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG2

devalue :: Value x -> StateT Strict.ByteString (Except Strict.Text) x
devalue = unsafeCoerce

value :: StateT Strict.ByteString (Except Strict.Text) x -> Value x
value = unsafeCoerce

-- | A `FromValue` constraint gives a parser from the binary format of
-- a PostgreSQL `PGType` into a Haskell `Type`.
class IsPG hask => FromValue hask where
  {- |
  >>> :set -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving -XDerivingStrategies -XDerivingVia -XUndecidableInstances
  >>> import GHC.Generics as GHC
  >>> :{
  newtype UserId = UserId { getId :: Int64 }
    deriving newtype (IsPG, FromValue)
  :}

  >>> :{
  data Complex = Complex
    { real :: Double
    , imaginary :: Double
    } deriving stock GHC.Generic
      deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
      deriving (IsPG, FromValue) via (Composite Complex)
  :}

  >>> :{
  data Direction = North | South | East | West
    deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, FromValue) via (Enumerated Direction)
  :}

  -}
  fromValue :: StateT Strict.ByteString (Except Strict.Text) hask
instance FromValue Bool where
  fromValue = devalue bool
instance FromValue Int16 where
  fromValue = devalue int
instance FromValue Int32 where
  fromValue = devalue int
instance FromValue Int64 where
  fromValue = devalue int
instance FromValue Oid where
  fromValue = devalue $ Oid <$> int
instance FromValue Float where
  fromValue = devalue float4
instance FromValue Double where
  fromValue = devalue float8
instance FromValue Scientific where
  fromValue = devalue numeric
instance FromValue Money where
  fromValue = devalue $  Money <$> int
instance FromValue UUID where
  fromValue = devalue uuid
instance FromValue (NetAddr IP) where
  fromValue = devalue inet
instance FromValue Char where
  fromValue = devalue char
instance FromValue Strict.Text where
  fromValue = devalue text_strict
instance FromValue Lazy.Text where
  fromValue = devalue text_lazy
instance FromValue String where
  fromValue = devalue $ Strict.Text.unpack <$> text_strict
instance FromValue Strict.ByteString where
  fromValue = devalue bytea_strict
instance FromValue Lazy.ByteString where
  fromValue = devalue bytea_lazy
instance FromValue Day where
  fromValue = devalue date
instance FromValue TimeOfDay where
  fromValue = devalue time_int
instance FromValue (TimeOfDay, TimeZone) where
  fromValue = devalue timetz_int
instance FromValue LocalTime where
  fromValue = devalue timestamp_int
instance FromValue UTCTime where
  fromValue = devalue timestamptz_int
instance FromValue DiffTime where
  fromValue = devalue interval_int
instance FromValue Aeson.Value where
  fromValue = devalue json_ast
instance Aeson.FromJSON x => FromValue (Json x) where
  fromValue = devalue $ Json <$>
    json_bytes (left Strict.Text.pack . Aeson.eitherDecodeStrict)
instance Aeson.FromJSON x => FromValue (Jsonb x) where
  fromValue = devalue $ Jsonb <$>
    jsonb_bytes (left Strict.Text.pack . Aeson.eitherDecodeStrict)
instance FromValue y
  => FromValue (VarArray (Vector y)) where
    fromValue =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('NotNull (PG y)))
instance FromValue y
  => FromValue (VarArray (Vector (Maybe y))) where
    fromValue =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('Null (PG y)))
instance FromValue y
  => FromValue (VarArray [y]) where
    fromValue =
      let
        rep n x = VarArray <$> replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('NotNull (PG y)))
instance FromValue y
  => FromValue (VarArray [Maybe y]) where
    fromValue =
      let
        rep n x = VarArray <$> replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('Null (PG y)))
instance FromFixArray dims ty y => FromValue (FixArray y) where
  fromValue = devalue $ FixArray <$> array (fromFixArray @dims @ty @y)
instance
  ( SOP.IsEnumType y
  , SOP.HasDatatypeInfo y
  , LabelsPG y ~ labels
  ) => FromValue (Enumerated y) where
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
        devalue
        $ fmap Enumerated
        . enum
        $ fmap SOP.to
        . greadConstructor
          (SOP.constructorInfo (SOP.datatypeInfo (SOP.Proxy @y)))
        . Strict.Text.unpack
instance
  ( SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => FromValue (Composite y) where
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
            len :: Int32 <- sized 4 int
            if len == -1
              then return (SOP.K Nothing)
              else SOP.K . Just <$> bytesOfSize (fromIntegral len)
      in
        devalue $
          fmap Composite (fn (runDecodeRow (genericRow @row) <=< comp))
instance FromValue y => FromValue (Range y) where
  fromValue = devalue $ do
    flag <- byte
    if testBit flag 0 then return Empty else do
      lower <-
        if testBit flag 3
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (value fromValue)
            return $ if testBit flag 1 then Closed l else Open l
      upper <-
        if testBit flag 4
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (value fromValue)
            return $ if testBit flag 2 then Closed l else Open l
      return $ NonEmpty lower upper

-- | A `FromNullValue` constraint lifts the `FromValue` parser
-- to a decoding of a @NullityType@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromNullValue`, just use the provided instances.
class FromNullValue (ty :: NullType) (y :: Type) where
  fromNullValue :: Maybe Strict.ByteString -> Either Strict.Text y
instance (FromValue y, pg ~ PG y) => FromNullValue ('NotNull pg) y where
  fromNullValue = \case
    Nothing -> throwError "fromField: saw NULL when expecting NOT NULL"
    Just bytestring -> valueParser (value fromValue) bytestring
instance (FromValue y, pg ~ PG y) => FromNullValue ('Null pg) (Maybe y) where
  fromNullValue = \case
    Nothing -> return Nothing
    Just bytestring -> fmap Just $ valueParser (value fromValue) bytestring

-- | A `FromField` constraint lifts the `FromValue` parser
-- to a decoding of a @(Symbol, NullityType)@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromField`, just use the provided instances.
class FromField (field :: (Symbol, NullType)) (y :: (Symbol, Type)) where
  fromField :: Maybe Strict.ByteString -> Either Strict.Text (SOP.P y)
instance (FromValue y, field ~ (fld ::: 'NotNull (PG y)))
  => FromField field (fld ::: y) where
    fromField = fmap SOP.P . fromNullValue @('NotNull (PG y))
instance (FromValue y, field ~ (fld ::: 'Null (PG y)))
  => FromField field (fld ::: Maybe y) where
    fromField = fmap SOP.P . fromNullValue @('Null (PG y))

-- | A `FromFixArray` constraint gives a decoding to a Haskell `Type`
-- from the binary format of a PostgreSQL fixed-length array.
-- You should not define instances for
-- `FromFixArray`, just use the provided instances.
class FromFixArray (dims :: [Nat]) (ty :: NullType) (y :: Type) where
  fromFixArray :: Array y
instance (FromValue y, pg ~ PG y) => FromFixArray '[] ('NotNull pg) y where
  fromFixArray = valueArray (value fromValue)
instance (FromValue y, pg ~ PG y) => FromFixArray '[] ('Null pg) (Maybe y) where
  fromFixArray = nullableValueArray (value fromValue)
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

{- |
`DecodeRow` describes a decoding of a PostgreSQL `RowType`
into a Haskell `Type`.

`DecodeRow` has an interface given by the classes
`Functor`, `Applicative`, `Alternative`, `Monad`,
`MonadPlus`, `MonadError` `Strict.Text`, and `IsLabel`.

>>> :set -XOverloadedLabels
>>> :{
let
  decode :: DecodeRow
    '[ "fst" ::: 'NotNull 'PGint2, "snd" ::: 'NotNull ('PGchar 1)]
    (Int16, Char)
  decode = (,) <$> #fst <*> #snd
in runDecodeRow decode (SOP.K (Just "\NUL\SOH") :* SOP.K (Just "a") :* Nil)
:}
Right (1,'a')

There is also an `IsLabel` instance for `MaybeT` `DecodeRow`s, useful
for decoding outer joined rows.

>>> :{
let
  decode :: DecodeRow
    '[ "fst" ::: 'Null 'PGint2, "snd" ::: 'Null ('PGchar 1)]
    (Maybe (Int16, Char))
  decode = runMaybeT $ (,) <$> #fst <*> #snd
in runDecodeRow decode (SOP.K (Just "\NUL\SOH") :* SOP.K (Just "a") :* Nil)
:}
Right (Just (1,'a'))

-}
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

-- | Run a `DecodeRow`.
runDecodeRow
  :: DecodeRow row y
  -> SOP.NP (SOP.K (Maybe Strict.ByteString)) row
  -> Either Strict.Text y
runDecodeRow = fmap runExcept . runReaderT . unDecodeRow

-- | Smart constructor for a `DecodeRow`.
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

{- | Row decoder for `SOP.Generic` records.

>>> import qualified GHC.Generics as GHC
>>> import qualified Generics.SOP as SOP
>>> data Two = Two {frst :: Int16, scnd :: String} deriving (Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)
>>> :{
let
  decode :: DecodeRow '[ "frst" ::: 'NotNull 'PGint2, "scnd" ::: 'NotNull 'PGtext] Two
  decode = genericRow
in runDecodeRow decode (SOP.K (Just "\NUL\STX") :* SOP.K (Just "two") :* Nil)
:}
Right (Two {frst = 2, scnd = "two"})
-}
genericRow :: forall row y ys.
  ( SOP.IsRecord y ys
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

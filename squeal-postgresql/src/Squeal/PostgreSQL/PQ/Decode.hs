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
import Squeal.PostgreSQL.Expression.Range
import Squeal.PostgreSQL.Schema
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG

devalue :: Value x -> StateT Strict.ByteString (Except Strict.Text) x
devalue = unsafeCoerce

value :: StateT Strict.ByteString (Except Strict.Text) x -> Value x
value = unsafeCoerce

-- | A `FromValue` constraint gives a parser from the binary format of
-- a PostgreSQL `PGType` into a Haskell `Type`.
class FromValue (pg :: PGType) (y :: Type) where
  -- |
  -- >>> :set -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving -XDerivingStrategies
  -- >>> newtype Id = Id { getId :: Int16 } deriving newtype (FromValue 'PGint2)
  fromValue :: proxy pg -> StateT Strict.ByteString (Except Strict.Text) y
instance FromValue 'PGbool Bool where
  fromValue _ = devalue bool
instance FromValue 'PGint2 Int16 where
  fromValue _ = devalue int
instance FromValue 'PGint4 Int32 where
  fromValue _ = devalue int
instance FromValue 'PGint8 Int64 where
  fromValue _ = devalue int
instance FromValue 'PGoid Oid where
  fromValue _ = devalue $ Oid <$> int
instance FromValue 'PGfloat4 Float where
  fromValue _ = devalue float4
instance FromValue 'PGfloat8 Double where
  fromValue _ = devalue float8
instance FromValue 'PGnumeric Scientific where
  fromValue _ = devalue numeric
instance FromValue 'PGmoney Money where
  fromValue _ = devalue $  Money <$> int
instance FromValue 'PGuuid UUID where
  fromValue _ = devalue uuid
instance FromValue 'PGinet (NetAddr IP) where
  fromValue _ = devalue inet
instance FromValue ('PGchar 1) Char where
  fromValue _ = devalue char
instance FromValue 'PGtext Strict.Text where
  fromValue _ = devalue text_strict
instance FromValue 'PGtext Lazy.Text where
  fromValue _ = devalue text_lazy
instance FromValue 'PGtext String where
  fromValue _ = devalue $ Strict.Text.unpack <$> text_strict
instance FromValue 'PGbytea Strict.ByteString where
  fromValue _ = devalue bytea_strict
instance FromValue 'PGbytea Lazy.ByteString where
  fromValue _ = devalue bytea_lazy
instance FromValue 'PGdate Day where
  fromValue _ = devalue date
instance FromValue 'PGtime TimeOfDay where
  fromValue _ = devalue time_int
instance FromValue 'PGtimetz (TimeOfDay, TimeZone) where
  fromValue _ = devalue timetz_int
instance FromValue 'PGtimestamp LocalTime where
  fromValue _ = devalue timestamp_int
instance FromValue 'PGtimestamptz UTCTime where
  fromValue _ = devalue timestamptz_int
instance FromValue 'PGinterval DiffTime where
  fromValue _ = devalue interval_int
instance FromValue 'PGjson Aeson.Value where
  fromValue _ = devalue json_ast
instance FromValue 'PGjsonb Aeson.Value where
  fromValue _ = devalue jsonb_ast
instance Aeson.FromJSON x => FromValue 'PGjson (Json x) where
  fromValue _ = devalue $ Json <$>
    json_bytes (left Strict.Text.pack . Aeson.eitherDecodeStrict)
instance Aeson.FromJSON x => FromValue 'PGjsonb (Jsonb x) where
  fromValue _ = devalue $ Jsonb <$>
    jsonb_bytes (left Strict.Text.pack . Aeson.eitherDecodeStrict)
instance FromValue pg y
  => FromValue ('PGvararray ('NotNull pg)) (VarArray (Vector y)) where
    fromValue _ =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('NotNull pg))
instance FromValue pg y
  => FromValue ('PGvararray ('Null pg)) (VarArray (Vector (Maybe y))) where
    fromValue _ =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('Null pg))
instance FromValue pg y
  => FromValue ('PGvararray ('NotNull pg)) (VarArray [y]) where
    fromValue _ =
      let
        rep n x = VarArray <$> replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('NotNull pg))
instance FromValue pg y
  => FromValue ('PGvararray ('Null pg)) (VarArray [Maybe y]) where
    fromValue _ =
      let
        rep n x = VarArray <$> replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('Null pg))
instance FromFixArray dims ty y
  => FromValue ('PGfixarray dims ty) (FixArray y) where
    fromValue _ = devalue $ FixArray <$> array (fromFixArray @dims @ty @y)
instance
  ( SOP.IsEnumType y
  , SOP.HasDatatypeInfo y
  , LabelsPG y ~ labels
  ) => FromValue ('PGenum labels) (Enumerated y) where
    fromValue _ =
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
  ) => FromValue ('PGcomposite row) (Composite y) where
    fromValue _ =
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
instance FromValue pg y => FromValue ('PGrange pg) (Range y) where
  fromValue _ = devalue $ do
    flag <- byte
    if testBit flag 0 then return Empty else do
      lower <-
        if testBit flag 3
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (value $ fromValue (SOP.Proxy @pg))
            return $ if testBit flag 1 then Closed l else Open l
      upper <-
        if testBit flag 4
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (value $ fromValue (SOP.Proxy @pg))
            return $ if testBit flag 2 then Closed l else Open l
      return $ NonEmpty lower upper

-- | A `FromNullValue` constraint lifts the `FromValue` parser
-- to a decoding of a @NullityType@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromNullValue`, just use the provided instances.
class FromNullValue (ty :: NullType) (y :: Type) where
  fromNullValue :: Maybe Strict.ByteString -> Either Strict.Text y
instance FromValue pg y => FromNullValue ('NotNull pg) y where
  fromNullValue = \case
    Nothing -> throwError "fromField: saw NULL when expecting NOT NULL"
    Just bytestring -> valueParser
      (value $ fromValue (SOP.Proxy @pg)) bytestring
instance FromValue pg y => FromNullValue ('Null pg) (Maybe y) where
  fromNullValue = \case
    Nothing -> return Nothing
    Just bytestring -> fmap Just $
      valueParser (value $ fromValue (SOP.Proxy @pg)) bytestring

-- | A `FromField` constraint lifts the `FromValue` parser
-- to a decoding of a @(Symbol, NullityType)@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromField`, just use the provided instances.
class FromField (field :: (Symbol, NullType)) (y :: (Symbol, Type)) where
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
  fromFixArray = valueArray
    (value $ fromValue (SOP.Proxy @pg))
instance FromValue pg y => FromFixArray '[] ('Null pg) (Maybe y) where
  fromFixArray = nullableValueArray
    (value $ fromValue (SOP.Proxy @pg))
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

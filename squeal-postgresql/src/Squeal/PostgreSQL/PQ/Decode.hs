{-|
Module: Squeal.PostgreSQL.PQ.Decode
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
  , rowValue
  , devalue
  , value
    -- * Decoding Classes
  , FromPG (..)
  , FromValue (..)
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

rowValue :: SOP.SListI row => DecodeRow row y -> Value y
rowValue decoder =
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
  in fn (runDecodeRow decoder <=< comp)

-- | A `FromPG` constraint gives a parser from the binary format of
-- a PostgreSQL `PGType` into a Haskell `Type`.
class IsPG y => FromPG y where
  {- |
  >>> :set -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving -XDerivingStrategies -XDerivingVia -XUndecidableInstances
  >>> import GHC.Generics as GHC
  >>> :{
  newtype UserId = UserId { getId :: Int64 }
    deriving newtype (IsPG, FromPG)
  :}

  >>> :{
  data Complex = Complex
    { real :: Double
    , imaginary :: Double
    } deriving stock GHC.Generic
      deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
      deriving (IsPG, FromPG) via (Composite Complex)
  :}

  >>> :{
  data Direction = North | South | East | West
    deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, FromPG) via (Enumerated Direction)
  :}

  -}
  fromPG :: StateT Strict.ByteString (Except Strict.Text) y
instance FromPG Bool where
  fromPG = devalue bool
instance FromPG Int16 where
  fromPG = devalue int
instance FromPG Int32 where
  fromPG = devalue int
instance FromPG Int64 where
  fromPG = devalue int
instance FromPG Oid where
  fromPG = devalue $ Oid <$> int
instance FromPG Float where
  fromPG = devalue float4
instance FromPG Double where
  fromPG = devalue float8
instance FromPG Scientific where
  fromPG = devalue numeric
instance FromPG Money where
  fromPG = devalue $  Money <$> int
instance FromPG UUID where
  fromPG = devalue uuid
instance FromPG (NetAddr IP) where
  fromPG = devalue inet
instance FromPG Char where
  fromPG = devalue char
instance FromPG Strict.Text where
  fromPG = devalue text_strict
instance FromPG Lazy.Text where
  fromPG = devalue text_lazy
instance FromPG String where
  fromPG = devalue $ Strict.Text.unpack <$> text_strict
instance FromPG Strict.ByteString where
  fromPG = devalue bytea_strict
instance FromPG Lazy.ByteString where
  fromPG = devalue bytea_lazy
instance FromPG Day where
  fromPG = devalue date
instance FromPG TimeOfDay where
  fromPG = devalue time_int
instance FromPG (TimeOfDay, TimeZone) where
  fromPG = devalue timetz_int
instance FromPG LocalTime where
  fromPG = devalue timestamp_int
instance FromPG UTCTime where
  fromPG = devalue timestamptz_int
instance FromPG DiffTime where
  fromPG = devalue interval_int
instance FromPG Aeson.Value where
  fromPG = devalue json_ast
instance Aeson.FromJSON x => FromPG (Json x) where
  fromPG = devalue $ Json <$>
    json_bytes (left Strict.Text.pack . Aeson.eitherDecodeStrict)
instance Aeson.FromJSON x => FromPG (Jsonb x) where
  fromPG = devalue $ Jsonb <$>
    jsonb_bytes (left Strict.Text.pack . Aeson.eitherDecodeStrict)
instance FromPG y
  => FromPG (VarArray 'NotNull (Vector y)) where
    fromPG =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('NotNull (PG y)))
instance FromPG y
  => FromPG (VarArray 'Null (Vector (Maybe y))) where
    fromPG =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('Null (PG y)))
instance FromPG y
  => FromPG (VarArray 'NotNull [y]) where
    fromPG =
      let
        rep n x = VarArray <$> replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('NotNull (PG y)))
instance FromPG y
  => FromPG (VarArray 'Null [Maybe y]) where
    fromPG =
      let
        rep n x = VarArray <$> replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromFixArray @'[] @('Null (PG y)))
instance FromFixArray dims ty y => FromPG (FixArray y) where
  fromPG = devalue $ FixArray <$> array (fromFixArray @dims @ty @y)
instance
  ( SOP.IsEnumType y
  , SOP.HasDatatypeInfo y
  , LabelsPG y ~ labels
  ) => FromPG (Enumerated y) where
    fromPG =
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
  , RowPG y ~ row
  ) => FromPG (Composite y) where
    fromPG = devalue (fmap Composite (rowValue @row genericRow))
instance FromPG y => FromPG (Range y) where
  fromPG = devalue $ do
    flag <- byte
    if testBit flag 0 then return Empty else do
      lower <-
        if testBit flag 3
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (value fromPG)
            return $ if testBit flag 1 then Closed l else Open l
      upper <-
        if testBit flag 4
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (value fromPG)
            return $ if testBit flag 2 then Closed l else Open l
      return $ NonEmpty lower upper

-- | A `FromValue` constraint lifts the `FromPG` parser
-- to a decoding of a @NullityType@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromValue`, just use the provided instances.
class FromValue (ty :: NullType) (y :: Type) where
  fromValue :: Maybe Strict.ByteString -> Either Strict.Text y
instance (FromPG y, pg ~ PG y) => FromValue ('NotNull pg) y where
  fromValue = \case
    Nothing -> throwError "fromField: saw NULL when expecting NOT NULL"
    Just bytestring -> valueParser (value fromPG) bytestring
instance (FromPG y, pg ~ PG y) => FromValue ('Null pg) (Maybe y) where
  fromValue = \case
    Nothing -> return Nothing
    Just bytestring -> fmap Just $ valueParser (value fromPG) bytestring

-- | A `FromField` constraint lifts the `FromPG` parser
-- to a decoding of a @(Symbol, NullityType)@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromField`, just use the provided instances.
class FromField (field :: (Symbol, NullType)) (y :: (Symbol, Type)) where
  fromField :: Maybe Strict.ByteString -> Either Strict.Text (SOP.P y)
instance (FromValue ty y, fld0 ~ fld1)
  => FromField (fld0 ::: ty) (fld1 ::: y) where
    fromField = fmap SOP.P . fromValue @ty

-- | A `FromFixArray` constraint gives a decoding to a Haskell `Type`
-- from the binary format of a PostgreSQL fixed-length array.
-- You should not define instances for
-- `FromFixArray`, just use the provided instances.
class FromFixArray (dims :: [Nat]) (ty :: NullType) (y :: Type) where
  fromFixArray :: Array y
instance (FromPG y, pg ~ PG y) => FromFixArray '[] ('NotNull pg) y where
  fromFixArray = valueArray (value fromPG)
instance (FromPG y, pg ~ PG y) => FromFixArray '[] ('Null pg) (Maybe y) where
  fromFixArray = nullableValueArray (value fromPG)
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
instance {-# OVERLAPPING #-} FromValue ty y
  => IsLabel fld (DecodeRow (fld ::: ty ': row) y) where
    fromLabel = decodeRow $ \(SOP.K b SOP.:* _) ->
      fromValue @ty b
instance {-# OVERLAPPABLE #-} IsLabel fld (DecodeRow row y)
  => IsLabel fld (DecodeRow (field ': row) y) where
    fromLabel = decodeRow $ \(_ SOP.:* bs) ->
      runDecodeRow (fromLabel @fld) bs
instance {-# OVERLAPPING #-} FromValue ty (Maybe y)
  => IsLabel fld (MaybeT (DecodeRow (fld ::: ty ': row)) y) where
    fromLabel = MaybeT . decodeRow $ \(SOP.K b SOP.:* _) ->
      fromValue @ty b
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

{-|
Module: Squeal.PostgreSQL.PQ.Encode2
Description: Encoding of statement parameters
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Encoding of statement parameters
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ.Encode2
  ( -- * Encode Parameters
    EncodeParams (..)
  , genericParams
  , nilParams
  , (.*)
  , (*.)
  , aParam
  , appendParams
    -- * Encoding Classes
  , ToParam (..)
  , ToNullParam (..)
  , ToField (..)
  , ToFixArray (..)
  ) where

import ByteString.StrictBuilder
import Control.Monad
import Control.Monad.Reader
import Data.Bits
import Data.ByteString as Strict (ByteString)
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.Functor.Contravariant
import Data.Int (Int16, Int32, Int64)
import Data.Kind
import Data.Scientific (Scientific)
import Data.Text as Strict (Text)
import Data.Text.Lazy as Lazy (Text)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Data.Word (Word32)
import Foreign.C.Types (CUInt(CUInt))
import GHC.TypeLits
import Network.IP.Addr (NetAddr, IP)
import PostgreSQL.Binary.Encoding

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.Text as Strict.Text
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression.Range2
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG2
import Squeal.PostgreSQL.PQ.Oid
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL (connectdb, finish)

-- | A `ToParam` constraint gives an encoding of a Haskell `Type` into
-- into the binary format of a PostgreSQL `PGType`.
class IsPG x => ToParam (db :: SchemasType) (x :: Type) where
  -- | >>> :set -XTypeApplications -XDataKinds
  -- >>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb"
  -- >>> runReaderT (toParam @'[] False) conn
  -- "\NUL"
  --
  -- >>> runReaderT (toParam @'[] (0 :: Int16)) conn
  -- "\NUL\NUL"
  --
  -- >>> runReaderT (toParam @'[] (0 :: Int32)) conn
  -- "\NUL\NUL\NUL\NUL"
  --
  -- >>> :set -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving
  -- >>> newtype UserId = UserId { getUserId :: Int64 } deriving newtype (IsPG, ToParam db)
  -- >>> runReaderT (toParam @'[] (UserId 0)) conn
  -- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
  --
  -- >>> finish conn
  toParam :: x -> ReaderT (SOP.K LibPQ.Connection db) IO Encoding
instance ToParam db Bool where toParam = pure . bool
instance ToParam db Int16 where toParam = pure . int2_int16
instance ToParam db Int32 where toParam = pure . int4_int32
instance ToParam db Int64 where toParam = pure . int8_int64
instance ToParam db Oid where toParam = pure . int4_word32 . getOid
instance ToParam db Float where toParam = pure . float4
instance ToParam db Double where toParam = pure . float8
instance ToParam db Scientific where toParam = pure . numeric
instance ToParam db Money where toParam = pure . int8_int64 . cents
instance ToParam db UUID where toParam = pure . uuid
instance ToParam db (NetAddr IP) where toParam = pure . inet
instance ToParam db Char where toParam = pure . char_utf8
instance ToParam db Strict.Text where toParam = pure . text_strict
instance ToParam db Lazy.Text where toParam = pure . text_lazy
instance ToParam db String where
  toParam = pure . text_strict . Strict.Text.pack
instance ToParam db Strict.ByteString where toParam = pure . bytea_strict
instance ToParam db Lazy.ByteString where toParam = pure . bytea_lazy
instance ToParam db Day where toParam = pure . date
instance ToParam db TimeOfDay where toParam = pure . time_int
instance ToParam db (TimeOfDay, TimeZone) where toParam = pure . timetz_int
instance ToParam db LocalTime where toParam = pure . timestamp_int
instance ToParam db UTCTime where toParam = pure . timestamptz_int
instance ToParam db DiffTime where toParam = pure . interval_int
instance ToParam db Aeson.Value where toParam = pure . json_ast
instance Aeson.ToJSON x => ToParam db (Json x) where
  toParam = pure . json_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJson
instance Aeson.ToJSON x => ToParam db (Jsonb x) where
  toParam = pure . jsonb_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJsonb
instance (ToFixArray db '[] ty x, OidOfNull db ty)
  => ToParam db (VarArray [x]) where
    toParam (VarArray arr) = do
      oid <- oidOfNull @db @ty
      let
        dims = [fromIntegral (length arr)]
        nulls = arrayNulls @db @'[] @ty @x
      payload <- dimArray foldM (arrayPayload @db @'[] @ty @x) arr
      return $ encodeArray 1 nulls oid dims payload
instance (ToFixArray db '[] ty x, OidOfNull db ty)
  => ToParam db (VarArray (Vector x)) where
    toParam (VarArray arr) = do
      oid <- oidOfNull @db @ty
      let
        dims = [fromIntegral (length arr)]
        nulls = arrayNulls @db @'[] @ty @x
      payload <- dimArray foldM (arrayPayload @db @'[] @ty @x) arr
      return $ encodeArray 1 nulls oid dims payload
instance (ToFixArray db dims ty x, OidOfNull db ty)
  => ToParam db (FixArray x) where
    toParam (FixArray arr) = do
      oid <- oidOfNull @db @ty
      payload <- arrayPayload @db @dims @ty arr
      let
        dims = arrayDims @db @dims @ty @x
        nulls = arrayNulls @db @dims @ty @x
        ndims = fromIntegral (length dims)
      return $ encodeArray ndims nulls oid dims payload
instance
  ( SOP.IsEnumType x
  , SOP.HasDatatypeInfo x
  , LabelsPG x ~ labels
  ) => ToParam db (Enumerated x) where
    toParam =
      let
        gshowConstructor
          :: NP SOP.ConstructorInfo xss
          -> SOP.SOP SOP.I xss
          -> String
        gshowConstructor Nil _ = ""
        gshowConstructor (constructor :* _) (SOP.SOP (SOP.Z _)) =
          SOP.constructorName constructor
        gshowConstructor (_ :* constructors) (SOP.SOP (SOP.S xs)) =
          gshowConstructor constructors (SOP.SOP xs)
      in
        pure
        . text_strict
        . Strict.Text.pack
        . gshowConstructor
          (SOP.constructorInfo (SOP.datatypeInfo (SOP.Proxy @x)))
        . SOP.from
        . getEnumerated
instance
  ( SOP.SListI fields
  , SOP.IsRecord x xs
  , SOP.AllZip (ToField db) fields xs
  , SOP.All (OidOfField db) fields
  ) => ToParam db (Composite x) where
    toParam (Composite x) = do
      let
        compositeSize
          = int4_int32
          $ fromIntegral
          $ SOP.lengthSList
          $ SOP.Proxy @xs
        each
          :: OidOfField db field
          => SOP.K (Maybe Encoding) field
          -> ReaderT (SOP.K LibPQ.Connection db) IO Encoding
        each (SOP.K field :: SOP.K (Maybe Encoding) field) = do
          oid <- getOid <$> oidOfField @db @field
          return $ int4_word32 oid <> maybe null4 sized field
      fields :: NP (SOP.K (Maybe Encoding)) fields <- hctransverse
        (SOP.Proxy @(ToField db)) (toField @db) (SOP.toRecord x)
      compositePayload <- hcfoldMapM
        (SOP.Proxy @(OidOfField db)) each fields
      return $ compositeSize <> compositePayload
instance ToParam db x => ToParam db (Range x) where
  toParam r = do
    payload <- case r of
      Empty -> return mempty
      NonEmpty lower upper -> (<>) <$> putBound lower <*> putBound upper
    return $ word8 (setFlags r 0) <> payload
    where
      putBound = \case
        Infinite -> return mempty
        Closed value -> sized <$> toParam @db value
        Open value -> sized <$> toParam @db value
      setFlags = \case
        Empty -> (`setBit` 0)
        NonEmpty lower upper ->
          setLowerFlags lower . setUpperFlags upper
      setLowerFlags = \case
        Infinite -> (`setBit` 3)
        Closed _ -> (`setBit` 1)
        Open _ -> id
      setUpperFlags = \case
        Infinite -> (`setBit` 4)
        Closed _ -> (`setBit` 2)
        Open _ -> id

-- | A `ToNullParam` constraint gives an encoding of a Haskell `Type` into
-- into the binary format of a PostgreSQL `NullType`.
-- You should not define instances for `ToNullParam`,
-- just use the provided instances.
class ToNullParam (db :: SchemasType) (ty :: NullType) (x :: Type) where
  toNullParam :: x -> ReaderT (SOP.K LibPQ.Connection db) IO (Maybe Encoding)
instance (ToParam db x, pg ~ PG x) => ToNullParam db ('NotNull pg) x where
  toNullParam = fmap Just . toParam @db
instance (ToParam db x, pg ~ PG x) => ToNullParam db ('Null pg) (Maybe x) where
  toNullParam = maybe (pure Nothing) (fmap Just . toParam @db)

-- | A `ToField` constraint lifts the `ToParam` parser
-- to an encoding of a @(Symbol, Type)@ to a @(Symbol, NullityType)@,
-- encoding `Null`s to `Maybe`s. You should not define instances for
-- `ToField`, just use the provided instances.
class ToField
  (db :: SchemasType)
  (field :: (Symbol, NullType))
  (x :: (Symbol, Type)) where
  toField :: SOP.P x
    -> ReaderT (SOP.K LibPQ.Connection db) IO (SOP.K (Maybe Encoding) field)
instance (fld0 ~ fld1, ToNullParam db ty x)
  => ToField db (fld0 ::: ty) (fld1 ::: x) where
    toField (SOP.P x) = SOP.K <$> toNullParam @db @ty x

-- | A `ToFixArray` constraint gives an encoding of a Haskell `Type`
-- into the binary format of a PostgreSQL fixed-length array.
-- You should not define instances for
-- `ToFixArray`, just use the provided instances.
class ToFixArray
  (db :: SchemasType)
  (dims :: [Nat])
  (ty :: NullType)
  (x :: Type) where
  arrayPayload :: x -> ReaderT (SOP.K LibPQ.Connection db) IO Encoding
  arrayDims :: [Int32]
  arrayNulls :: Bool
instance (ToParam db x, pg ~ PG x)
  => ToFixArray db '[] ('NotNull pg) x where
    arrayPayload = fmap sized . toParam @db @x
    arrayDims = []
    arrayNulls = True
instance (ToParam db x, pg ~ PG x)
  => ToFixArray db '[] ('Null pg) (Maybe x) where
    arrayPayload = maybe (pure null4) (fmap sized . toParam @db @x)
    arrayDims = []
    arrayNulls = False
instance
  ( SOP.IsProductType tuple xs
  , Length xs ~ dim
  , SOP.All ((~) x) xs
  , ToFixArray db dims ty x
  , KnownNat dim )
  => ToFixArray db (dim ': dims) ty tuple where
    arrayPayload
      = dimArray foldlNP (arrayPayload @db @dims @ty @x)
      . SOP.unZ . SOP.unSOP . SOP.from
    arrayDims
      = fromIntegral (natVal (SOP.Proxy @dim))
      : arrayDims @db @dims @ty @x
    arrayNulls = arrayNulls @db @dims @ty @x
foldlNP
  :: (SOP.All ((~) x) xs, Monad m)
  => (z -> x -> m z) -> z -> NP SOP.I xs -> m z
foldlNP f z = \case
  Nil -> pure z
  SOP.I x :* xs -> do
    z' <- f z x
    foldlNP f z' xs

{- |
`EncodeParams` describes an encoding of a Haskell `Type`
into a list of parameter `NullType`s.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb"
>>> :{
let
  encode :: EncodeParams '[]
    '[ 'NotNull 'PGint2, 'NotNull ('PGchar 1), 'NotNull 'PGtext]
    (Int16, (Char, String))
  encode = fst .* fst.snd *. snd.snd
in runReaderT (runEncodeParams encode (1,('a',"foo"))) conn
:}
K (Just "\NUL\SOH") :* K (Just "a") :* K (Just "foo") :* Nil

>>> finish conn
-}
newtype EncodeParams
  (db :: SchemasType)
  (tys :: [NullType])
  (x :: Type) = EncodeParams
  { runEncodeParams :: x
    -> ReaderT (SOP.K LibPQ.Connection db) IO (NP (SOP.K (Maybe Encoding)) tys) }
instance Contravariant (EncodeParams db tys) where
  contramap f (EncodeParams g) = EncodeParams (g . f)

{- | Parameter encoding for `SOP.Generic` tuples and records.

>>> import qualified GHC.Generics as GHC
>>> import qualified Generics.SOP as SOP
>>> data Two = Two Int16 String deriving (GHC.Generic, SOP.Generic)
>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb"
>>> :{
let
  encode :: EncodeParams '[] '[ 'NotNull 'PGint2, 'NotNull 'PGtext] Two
  encode = genericParams
in runReaderT (runEncodeParams encode (Two 2 "two")) conn
:}
K (Just "\NUL\STX") :* K (Just "two") :* Nil

>>> :{
let
  encode :: EncodeParams '[] '[ 'NotNull 'PGint2, 'NotNull 'PGtext] (Int16, String)
  encode = genericParams
in runReaderT (runEncodeParams encode (2, "two")) conn
:}
K (Just "\NUL\STX") :* K (Just "two") :* Nil

>>> finish conn
-}
genericParams :: forall db params x xs.
  ( SOP.IsProductType x xs
  , SOP.AllZip (ToNullParam db) params xs
  ) => EncodeParams db params x
genericParams = EncodeParams
  $ hctransverse (SOP.Proxy @(ToNullParam db)) encodeNullParam
  . SOP.unZ . SOP.unSOP . SOP.from
  where
    encodeNullParam
      :: forall ty y. ToNullParam db ty y
      => SOP.I y -> ReaderT (SOP.K LibPQ.Connection db) IO (SOP.K (Maybe Encoding) ty)
    encodeNullParam = fmap SOP.K . toNullParam @db @ty . SOP.unI

-- | Encode 0 parameters.
nilParams :: EncodeParams db '[] x
nilParams = EncodeParams $ \ _ -> pure Nil

{- | Cons a parameter encoding.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb"
>>> :{
let
  encode :: EncodeParams '[]
    '[ 'Null 'PGint4, 'NotNull 'PGtext]
    (Maybe Int32, String)
  encode = fst .* snd .* nilParams
in runReaderT (runEncodeParams encode (Nothing, "foo")) conn
:}
K Nothing :* K (Just "foo") :* Nil

>>> finish conn
-}
(.*)
  :: forall db x0 ty x tys. (ToNullParam db ty x0)
  => (x -> x0) -- ^ head
  -> EncodeParams db tys x -- ^ tail
  -> EncodeParams db (ty ': tys) x
f .* EncodeParams params = EncodeParams $ \x ->
  (:*) <$> (SOP.K <$> toNullParam @db @ty (f x)) <*> params x
infixr 5 .*

{- | End a parameter encoding.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb"
>>> :{
let
  encode :: EncodeParams '[]
    '[ 'Null 'PGint4, 'NotNull 'PGtext, 'NotNull ('PGchar 1)]
    (Maybe Int32, String, Char)
  encode = (\(x,_,_) -> x) .* (\(_,y,_) -> y) *. (\(_,_,z) -> z)
in runReaderT (runEncodeParams encode (Nothing, "foo", 'z')) conn
:}
K Nothing :* K (Just "foo") :* K (Just "z") :* Nil

>>> finish conn
-}
(*.)
  :: forall db x x0 ty0 x1 ty1
   . (ToNullParam db ty0 x0, ToNullParam db ty1 x1)
  => (x -> x0) -- ^ second to last
  -> (x -> x1) -- ^ last
  -> EncodeParams db '[ty0, ty1] x
f *. g = f .* g .* nilParams
infixl 8 *.

{- | Encode 1 parameter.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb"
>>> :{
let
  encode :: EncodeParams '[] '[ 'NotNull 'PGint4] Int32
  encode = aParam
in runReaderT (runEncodeParams encode 1776) conn
:}
K (Just "\NUL\NUL\ACK\240") :* Nil

>>> finish conn
-}
aParam
  :: forall db x. ToNullParam db (NullPG x) x
  => EncodeParams db '[NullPG x] x
aParam = EncodeParams $
  fmap (\param -> SOP.K param :* Nil) . toNullParam @db @(NullPG x)

{- | Append parameter encodings.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb"
>>> :{
let
  encode :: EncodeParams '[]
    '[ 'NotNull 'PGint4, 'NotNull 'PGint2]
    (Int32, Int16)
  encode = contramap fst aParam `appendParams` contramap snd aParam
in runReaderT (runEncodeParams encode (1776, 2)) conn
:}
K (Just "\NUL\NUL\ACK\240") :* K (Just "\NUL\STX") :* Nil

>>> finish conn
-}
appendParams
  :: EncodeParams db params0 x -- ^ left
  -> EncodeParams db params1 x -- ^ right
  -> EncodeParams db (Join params0 params1) x
appendParams encode0 encode1 = EncodeParams $ \x -> also
  <$> runEncodeParams encode1 x
  <*> runEncodeParams encode0 x

getOid :: LibPQ.Oid -> Word32
getOid (LibPQ.Oid (CUInt oid)) = oid

encodeArray :: Int32 -> Bool -> LibPQ.Oid -> [Int32] -> Encoding -> Encoding
encodeArray ndim nulls oid dimensions payload = mconcat
  [ int4_int32 ndim
  , if nulls then true4 else false4
  , int4_word32 (getOid oid)
  , foldMap (\dimension -> int4_int32 dimension <> true4) dimensions
  , payload ]

dimArray
  :: Functor m
  => (forall b. (b -> a -> m b) -> b -> c -> m b)
  -> (a -> m Encoding) -> c -> m Encoding
dimArray folder elementArray = folder step mempty
  where
    step builder element = (builder <>) <$> elementArray element

null4, true4, false4 :: Encoding
null4 = int4_int32 (-1)
true4 = int4_word32 1
false4 = int4_word32 0

sized :: Encoding -> Encoding
sized bs = int4_int32 (fromIntegral (builderLength bs)) <> bs

hctransverse
  :: (SOP.AllZip c ys xs, Applicative m)
  => SOP.Proxy c
  -> (forall y x. c y x => f x -> m (g y))
  -> NP f xs -> m (NP g ys)
hctransverse c f = \case
  Nil -> pure Nil
  x :* xs -> (:*) <$> f x <*> hctransverse c f xs

hcfoldMapM
  :: (Monoid r, Applicative m, SOP.All c xs)
  => SOP.Proxy c
  -> (forall x. c x => f x -> m r)
  -> NP f xs -> m r
hcfoldMapM c f = \case
  Nil -> pure mempty
  x :* xs -> (<>) <$> f x <*> hcfoldMapM c f xs

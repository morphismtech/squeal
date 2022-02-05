{-|
Module: Squeal.PostgreSQL.Session.Encode
Description: encoding of statement parameters
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

encoding of statement parameters
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
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Session.Encode
  ( -- * Encode Parameters
    EncodeParams (..)
  , GenericParams (..)
  , nilParams
  , (.*)
  , (*.)
  , aParam
  , appendParams
  , rowParam
  , genericRowParams
  , (.#)
  , (#.)
    -- * Encoding Classes
  , ToPG (..)
  , ToParam (..)
  , ToField (..)
  , ToArray (..)
  ) where

import ByteString.StrictBuilder
import Control.Monad
import Control.Monad.Reader
import Data.Bits
import Data.ByteString as Strict (ByteString)
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.Coerce (coerce)
import Data.Functor.Const (Const(Const))
import Data.Functor.Constant (Constant(Constant))
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

import Squeal.PostgreSQL.Expression.Range
import Squeal.PostgreSQL.Session.Oid
import Squeal.PostgreSQL.Type
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.PG
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL (connectdb, finish)

-- | A `ToPG` constraint gives an encoding of a Haskell `Type` into
-- into the binary format of a PostgreSQL `PGType`.
class IsPG x => ToPG (db :: SchemasType) (x :: Type) where
  -- | >>> :set -XTypeApplications -XDataKinds
  -- >>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
  -- >>> runReaderT (toPG @'[] False) conn
  -- "\NUL"
  --
  -- >>> runReaderT (toPG @'[] (0 :: Int16)) conn
  -- "\NUL\NUL"
  --
  -- >>> runReaderT (toPG @'[] (0 :: Int32)) conn
  -- "\NUL\NUL\NUL\NUL"
  --
  -- >>> :set -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving
  -- >>> newtype UserId = UserId { getUserId :: Int64 } deriving newtype (IsPG, ToPG db)
  -- >>> runReaderT (toPG @'[] (UserId 0)) conn
  -- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
  --
  -- >>> finish conn
  toPG :: x -> ReaderT (SOP.K LibPQ.Connection db) IO Encoding
instance ToPG db Bool where toPG = pure . bool
instance ToPG db Int16 where toPG = pure . int2_int16
instance ToPG db Int32 where toPG = pure . int4_int32
instance ToPG db Int64 where toPG = pure . int8_int64
instance ToPG db Oid where toPG = pure . int4_word32 . getOid
instance ToPG db Float where toPG = pure . float4
instance ToPG db Double where toPG = pure . float8
instance ToPG db Scientific where toPG = pure . numeric
instance ToPG db Money where toPG = pure . int8_int64 . cents
instance ToPG db UUID where toPG = pure . uuid
instance ToPG db (NetAddr IP) where toPG = pure . inet
instance ToPG db Char where toPG = pure . char_utf8
instance ToPG db Strict.Text where toPG = pure . text_strict
instance ToPG db Lazy.Text where toPG = pure . text_lazy
instance ToPG db String where
  toPG = pure . text_strict . Strict.Text.pack
instance ToPG db Strict.ByteString where toPG = pure . bytea_strict
instance ToPG db Lazy.ByteString where toPG = pure . bytea_lazy
instance ToPG db (VarChar n) where toPG = pure . text_strict . getVarChar
instance ToPG db (FixChar n) where toPG = pure . text_strict . getFixChar
instance ToPG db x => ToPG db (Const x tag) where toPG = toPG @db @x . coerce
instance ToPG db x => ToPG db (SOP.K x tag) where toPG = toPG @db @x . coerce
instance ToPG db x => ToPG db (Constant x tag) where toPG = toPG @db @x . coerce
instance ToPG db Day where toPG = pure . date
instance ToPG db TimeOfDay where toPG = pure . time_int
instance ToPG db (TimeOfDay, TimeZone) where toPG = pure . timetz_int
instance ToPG db LocalTime where toPG = pure . timestamp_int
instance ToPG db UTCTime where toPG = pure . timestamptz_int
instance ToPG db DiffTime where toPG = pure . interval_int
instance ToPG db Aeson.Value where toPG = pure . json_ast
instance Aeson.ToJSON x => ToPG db (Json x) where
  toPG = pure . json_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJson
instance Aeson.ToJSON x => ToPG db (Jsonb x) where
  toPG = pure . jsonb_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJsonb
instance (NullPG x ~ ty, ToArray db '[] ty x, OidOfNull db ty)
  => ToPG db (VarArray [x]) where
    toPG (VarArray arr) = do
      oid <- oidOfNull @db @ty
      let
        dims = [fromIntegral (length arr)]
        nulls = arrayNulls @db @'[] @ty @x
      payload <- dimArray foldM (arrayPayload @db @'[] @ty @x) arr
      return $ encodeArray 1 nulls oid dims payload
instance (NullPG x ~ ty, ToArray db '[] ty x, OidOfNull db ty)
  => ToPG db (VarArray (Vector x)) where
    toPG (VarArray arr) = do
      oid <- oidOfNull @db @ty
      let
        dims = [fromIntegral (length arr)]
        nulls = arrayNulls @db @'[] @ty @x
      payload <- dimArray foldM (arrayPayload @db @'[] @ty @x) arr
      return $ encodeArray 1 nulls oid dims payload
instance (ToArray db dims ty x, OidOfNull db ty)
  => ToPG db (FixArray x) where
    toPG (FixArray arr) = do
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
  ) => ToPG db (Enumerated x) where
    toPG =
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
  , RowPG x ~ fields
  ) => ToPG db (Composite x) where
    toPG = rowParam @db @fields (contramap getComposite genericRowParams)
instance ToPG db x => ToPG db (Range x) where
  toPG r = do
    payload <- case r of
      Empty -> return mempty
      NonEmpty lower upper -> (<>) <$> putBound lower <*> putBound upper
    return $ word8 (setFlags r 0) <> payload
    where
      putBound = \case
        Infinite -> return mempty
        Closed value -> sized <$> toPG @db value
        Open value -> sized <$> toPG @db value
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

-- | A `ToParam` constraint gives an encoding of a Haskell `Type` into
-- into the binary format of a PostgreSQL `NullType`.
-- You should not define instances for `ToParam`,
-- just use the provided instances.
class ToParam (db :: SchemasType) (ty :: NullType) (x :: Type) where
  toParam :: x -> ReaderT (SOP.K LibPQ.Connection db) IO (Maybe Encoding)
instance (ToPG db x, pg ~ PG x) => ToParam db ('NotNull pg) x where
  toParam = fmap Just . toPG @db
instance (ToPG db x, pg ~ PG x) => ToParam db ('Null pg) (Maybe x) where
  toParam = maybe (pure Nothing) (fmap Just . toPG @db)

-- | A `ToField` constraint lifts the `ToPG` parser
-- to an encoding of a @(Symbol, Type)@ to a @(Symbol, NullityType)@,
-- encoding `Null`s to `Maybe`s. You should not define instances for
-- `ToField`, just use the provided instances.
class ToField
  (db :: SchemasType)
  (field :: (Symbol, NullType))
  (x :: (Symbol, Type)) where
  toField :: SOP.P x
    -> ReaderT (SOP.K LibPQ.Connection db) IO (SOP.K (Maybe Encoding) field)
instance (fld0 ~ fld1, ToParam db ty x)
  => ToField db (fld0 ::: ty) (fld1 ::: x) where
    toField (SOP.P x) = SOP.K <$> toParam @db @ty x

-- | A `ToArray` constraint gives an encoding of a Haskell `Type`
-- into the binary format of a PostgreSQL fixed-length array.
-- You should not define instances for
-- `ToArray`, just use the provided instances.
class ToArray
  (db :: SchemasType)
  (dims :: [Nat])
  (ty :: NullType)
  (x :: Type) where
  arrayPayload :: x -> ReaderT (SOP.K LibPQ.Connection db) IO Encoding
  arrayDims :: [Int32]
  arrayNulls :: Bool
instance (ToPG db x, pg ~ PG x)
  => ToArray db '[] ('NotNull pg) x where
  arrayPayload = fmap sized . toPG @db @x
  arrayDims = []
  arrayNulls = False
instance (ToPG db x, pg ~ PG x)
  => ToArray db '[] ('Null pg) (Maybe x) where
  arrayPayload = maybe (pure null4) (fmap sized . toPG @db @x)
  arrayDims = []
  arrayNulls = True
instance
  ( SOP.IsProductType tuple xs
  , Length xs ~ dim
  , SOP.All ((~) x) xs
  , ToArray db dims ty x
  , KnownNat dim )
  => ToArray db (dim ': dims) ty tuple where
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

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
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
  (tys :: [k])
  (x :: Type) = EncodeParams
  { runEncodeParams :: x
    -> ReaderT (SOP.K LibPQ.Connection db) IO (NP (SOP.K (Maybe Encoding)) tys) }
instance Contravariant (EncodeParams db tys) where
  contramap f (EncodeParams g) = EncodeParams (g . f)

-- | A `GenericParams` constraint to ensure that a Haskell type
-- is a product type,
-- has a `TuplePG`,
-- and all its terms have known Oids,
-- and can be encoded to corresponding Postgres types.
class
  ( SOP.IsProductType x xs
  , params ~ TuplePG x
  , SOP.All (OidOfNull db) params
  , SOP.AllZip (ToParam db) params xs
  ) => GenericParams db params x xs where
  {- | Parameter encoding for `SOP.Generic` tuples and records.

  >>> import qualified GHC.Generics as GHC
  >>> import qualified Generics.SOP as SOP
  >>> data Two = Two Int16 String deriving (GHC.Generic, SOP.Generic)
  >>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
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
  genericParams :: EncodeParams db params x
instance
  ( params ~ TuplePG x
  , SOP.All (OidOfNull db) params
  , SOP.IsProductType x xs
  , SOP.AllZip (ToParam db) params xs
  ) => GenericParams db params x xs where
  genericParams = EncodeParams
    $ hctransverse (SOP.Proxy @(ToParam db)) encodeNullParam
    . SOP.unZ . SOP.unSOP . SOP.from
    where
      encodeNullParam
        :: forall ty y. ToParam db ty y
        => SOP.I y -> ReaderT (SOP.K LibPQ.Connection db) IO (SOP.K (Maybe Encoding) ty)
      encodeNullParam = fmap SOP.K . toParam @db @ty . SOP.unI

-- | Encode 0 parameters.
nilParams :: EncodeParams db '[] x
nilParams = EncodeParams $ \ _ -> pure Nil

{- | Cons a parameter encoding.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
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
  :: forall db x0 ty x tys. (ToParam db ty x0, ty ~ NullPG x0)
  => (x -> x0) -- ^ head
  -> EncodeParams db tys x -- ^ tail
  -> EncodeParams db (ty ': tys) x
f .* EncodeParams params = EncodeParams $ \x ->
  (:*) <$> (SOP.K <$> toParam @db @ty (f x)) <*> params x
infixr 5 .*

{- | End a parameter encoding.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
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
   . ( ToParam db ty0 x0
     , ty0 ~ NullPG x0
     , ToParam db ty1 x1
     , ty1 ~ NullPG x1
     )
  => (x -> x0) -- ^ second to last
  -> (x -> x1) -- ^ last
  -> EncodeParams db '[ty0, ty1] x
f *. g = f .* g .* nilParams
infixl 8 *.

{- | Encode 1 parameter.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
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
  :: forall db x ty. (ToParam db ty x, ty ~ NullPG x)
  => EncodeParams db '[ty] x
aParam = EncodeParams $
  fmap (\param -> SOP.K param :* Nil) . toParam @db @(NullPG x)

{- | Append parameter encodings.

>>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
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

rowParam
  :: forall db row x. (PG x ~ 'PGcomposite row, SOP.All (OidOfField db) row)
  => EncodeParams db row x
  -> x -> ReaderT (SOP.K LibPQ.Connection db) IO Encoding
rowParam enc x = do
  let
    compositeSize
      = int4_int32
      $ fromIntegral
      $ SOP.lengthSList
      $ SOP.Proxy @row
    each
      :: OidOfField db field
      => SOP.K (Maybe Encoding) field
      -> ReaderT (SOP.K LibPQ.Connection db) IO Encoding
    each (SOP.K field :: SOP.K (Maybe Encoding) field) = do
      oid <- getOid <$> oidOfField @db @field
      return $ int4_word32 oid <> maybe null4 sized field
  fields <- runEncodeParams enc x
  compositePayload <- hcfoldMapM
    (SOP.Proxy @(OidOfField db)) each fields
  return $ compositeSize <> compositePayload

(.#)
  :: forall db x0 fld ty x tys. (ToParam db ty x0, ty ~ NullPG x0)
  => Aliased ((->) x) (fld ::: x0) -- ^ head
  -> EncodeParams db tys x -- ^ tail
  -> EncodeParams db (fld ::: ty ': tys) x
(f `As` _) .# EncodeParams params = EncodeParams $ \x ->
  (:*) <$> (SOP.K <$> toParam @db @ty (f x)) <*> params x
infixr 5 .#

(#.)
  :: forall db x x0 fld0 ty0 x1 fld1 ty1
   . ( ToParam db ty0 x0
     , ty0 ~ NullPG x0
     , ToParam db ty1 x1
     , ty1 ~ NullPG x1
     )
  => Aliased ((->) x) (fld0 ::: x0) -- ^ second to last
  -> Aliased ((->) x) (fld1 ::: x1) -- ^ last
  -> EncodeParams db '[fld0 ::: ty0, fld1 ::: ty1] x
f #. g = f .# g .# nilParams
infixl 8 #.

instance (ToParam db ty x, ty ~ NullPG x)
  => IsLabel fld (EncodeParams db '[fld ::: ty] x) where
    fromLabel
      = EncodeParams
      $ fmap (\param -> SOP.K param :* Nil)
      . toParam @db @(NullPG x)

genericRowParams
  ::  forall db row x xs.
      ( SOP.IsRecord x xs
      , SOP.AllZip (ToField db) row xs
      )
  => EncodeParams db row x
genericRowParams
  = EncodeParams
  $ hctransverse (SOP.Proxy @(ToField db)) (toField @db)
  . SOP.toRecord

{-|
Module: Squeal.PostgreSQL.PQ.Encode3
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
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ.Encode3
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
  , ToArray (..)
  ) where

import ByteString.StrictBuilder
import Control.Monad
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
import Squeal.PostgreSQL.Expression.Range
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.PQ.Oid3
import Squeal.PostgreSQL.Schema

-- | A `ToParam` constraint gives an encoding of a Haskell `Type` into
-- into the binary format of a PostgreSQL `PGType`.
class ToParam c pg x where
  -- | >>> :set -XTypeApplications -XDataKinds
  -- >>> toParam @'PGbool False
  -- K "\NUL"
  --
  -- >>> toParam @'PGint2 (0 :: Int16)
  -- K "\NUL\NUL"
  --
  -- >>> toParam @'PGint4 (0 :: Int32)
  -- K "\NUL\NUL\NUL\NUL"
  --
  -- >>> :set -XMultiParamTypeClasses -XGeneralizedNewtypeDeriving
  -- >>> newtype Id = Id { getId :: Int16 } deriving newtype ((forall m. c m => Applicative m) => ToParam c 'PGint2)
  -- >>> toParam @'PGint2 (Id 1)
  -- K "\NUL\SOH"
  toParam :: forall m. c m => x -> m (SOP.K Encoding pg)
instance (forall m. c m => Applicative m) => ToParam c 'PGbool Bool where toParam = pure . SOP.K . bool
instance (forall m. c m => Applicative m) => ToParam c 'PGint2 Int16 where toParam = pure . SOP.K . int2_int16
instance (forall m. c m => Applicative m) => ToParam c 'PGint4 Int32 where toParam = pure . SOP.K . int4_int32
instance (forall m. c m => Applicative m) => ToParam c 'PGint8 Int64 where toParam = pure . SOP.K . int8_int64
instance (forall m. c m => Applicative m) => ToParam c 'PGoid Oid where toParam = pure . SOP.K . int4_word32 . getOid
instance (forall m. c m => Applicative m) => ToParam c 'PGfloat4 Float where toParam = pure . SOP.K . float4
instance (forall m. c m => Applicative m) => ToParam c 'PGfloat8 Double where toParam = pure . SOP.K . float8
instance (forall m. c m => Applicative m) => ToParam c 'PGnumeric Scientific where toParam = pure . SOP.K . numeric
instance (forall m. c m => Applicative m) => ToParam c 'PGmoney Money where toParam = pure . SOP.K . int8_int64 . cents
instance (forall m. c m => Applicative m) => ToParam c 'PGuuid UUID where toParam = pure . SOP.K . uuid
instance (forall m. c m => Applicative m) => ToParam c 'PGinet (NetAddr IP) where toParam = pure . SOP.K . inet
instance (forall m. c m => Applicative m) => ToParam c ('PGchar 1) Char where toParam = pure . SOP.K . char_utf8
instance (forall m. c m => Applicative m) => ToParam c 'PGtext Strict.Text where toParam = pure . SOP.K . text_strict
instance (forall m. c m => Applicative m) => ToParam c 'PGtext Lazy.Text where toParam = pure . SOP.K . text_lazy
instance (forall m. c m => Applicative m) => ToParam c 'PGtext String where
  toParam = pure . SOP.K . text_strict . Strict.Text.pack
instance (forall m. c m => Applicative m) => ToParam c 'PGbytea Strict.ByteString where toParam = pure . SOP.K . bytea_strict
instance (forall m. c m => Applicative m) => ToParam c 'PGbytea Lazy.ByteString where toParam = pure . SOP.K . bytea_lazy
instance (forall m. c m => Applicative m) => ToParam c 'PGdate Day where toParam = pure . SOP.K . date
instance (forall m. c m => Applicative m) => ToParam c 'PGtime TimeOfDay where toParam = pure . SOP.K . time_int
instance (forall m. c m => Applicative m) => ToParam c 'PGtimetz (TimeOfDay, TimeZone) where toParam = pure . SOP.K . timetz_int
instance (forall m. c m => Applicative m) => ToParam c 'PGtimestamp LocalTime where toParam = pure . SOP.K . timestamp_int
instance (forall m. c m => Applicative m) => ToParam c 'PGtimestamptz UTCTime where toParam = pure . SOP.K . timestamptz_int
instance (forall m. c m => Applicative m) => ToParam c 'PGinterval DiffTime where toParam = pure . SOP.K . interval_int
instance (forall m. c m => Applicative m) => ToParam c 'PGjson Aeson.Value where toParam = pure . SOP.K . json_ast
instance (forall m. c m => Applicative m) => ToParam c 'PGjsonb Aeson.Value where toParam = pure . SOP.K . jsonb_ast
instance (Aeson.ToJSON x, forall m. c m => Applicative m) => ToParam c 'PGjson (Json x) where
  toParam = pure . SOP.K . json_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJson
instance (Aeson.ToJSON x, forall m. c m => Applicative m) => ToParam c 'PGjsonb (Jsonb x) where
  toParam = pure . SOP.K . jsonb_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJsonb
instance (ToArray c '[] ty x, OidOfNull c ty, Foldable list, forall m. c m => Monad m)
  => ToParam c ('PGvararray ty) (VarArray (list x)) where
    toParam (VarArray arr) = do
      oid <- oidOfNull @c @ty
      let
        dims = [fromIntegral (length arr)]
        nulls = arrayNulls @c @'[] @ty @x
      payload <- dimArray foldM (arrayPayload @c @'[] @ty @x) arr
      return . SOP.K $ encodeArray 1 nulls oid dims payload
instance (ToArray c dims ty x, OidOfNull c ty, forall m. c m => Monad m)
  => ToParam c ('PGfixarray dims ty) (FixArray x) where
    toParam (FixArray arr) = do
      oid <- oidOfNull @c @ty
      payload <- arrayPayload @c @dims @ty arr
      let
        dims = arrayDims @c @dims @ty @x
        nulls = arrayNulls @c @dims @ty @x
        ndims = fromIntegral (length dims)
      return . SOP.K $ encodeArray ndims nulls oid dims payload
instance
  ( SOP.IsEnumType x
  , SOP.HasDatatypeInfo x
  , LabelsPG x ~ labels
  , forall m. c m => Monad m
  ) => ToParam c ('PGenum labels) (Enumerated x) where
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
        . SOP.K
        . text_strict
        . Strict.Text.pack
        . gshowConstructor
          (SOP.constructorInfo (SOP.datatypeInfo (SOP.Proxy @x)))
        . SOP.from
        . getEnumerated
instance
  ( SOP.SListI fields
  , SOP.IsRecord x xs
  , SOP.AllZip (ToField c) fields xs
  , SOP.All (OidOfField c) fields
  , forall m. c m => Monad m
  ) => ToParam c ('PGcomposite fields) (Composite x) where
    toParam (Composite x) = do
      let
        compositeSize
          = int4_int32
          $ fromIntegral
          $ SOP.lengthSList
          $ SOP.Proxy @xs
        each
          :: (OidOfField c field, c m)
          => SOP.K (Maybe Encoding) field
          -> m Encoding
        each (SOP.K field :: SOP.K (Maybe Encoding) field) = do
          oid <- getOid <$> oidOfField @c @field
          return $ int4_word32 oid <> maybe null4 sized field
      fields :: NP (SOP.K (Maybe Encoding)) fields <- hctransverse
        (SOP.Proxy @(ToField c)) (toField @c) (SOP.toRecord x)
      compositePayload <- hcfoldMapM
        (SOP.Proxy @(OidOfField c)) each fields
      return . SOP.K $ compositeSize <> compositePayload
instance (ToParam c pg x, forall m. c m => Monad m)
  => ToParam c ('PGrange pg) (Range x) where
  toParam r = do
    payload <- case r of
      Empty -> return mempty
      NonEmpty lower upper -> (<>) <$> putBound lower <*> putBound upper
    return . SOP.K $ word8 (setFlags r 0) <> payload
    where
      putBound = \case
        Infinite -> return mempty
        Closed value -> sized . SOP.unK <$> toParam @c @pg value
        Open value -> sized . SOP.unK <$> toParam @c @pg value
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
class ToNullParam c (ty :: NullType) x where
  toNullParam :: forall m. c m => x -> m (Maybe Encoding)
instance (ToParam c pg x, forall m. c m => Functor m)
  => ToNullParam c ('NotNull pg) x where
    toNullParam = fmap (return . SOP.unK) . toParam @c @pg
instance (ToParam c pg x, forall m. c m => Applicative m)
  => ToNullParam c ('Null pg) (Maybe x) where
    toNullParam = maybe (pure Nothing)
      (fmap (Just . SOP.unK) . toParam @c @pg)

-- | A `ToField` constraint lifts the `ToParam` parser
-- to an encoding of a @(Symbol, Type)@ to a @(Symbol, NullityType)@,
-- encoding `Null`s to `Maybe`s. You should not define instances for
-- `ToField`, just use the provided instances.
class ToField c (field :: (Symbol, NullType)) x where
  toField
    :: forall m. c m
    => SOP.P x
    -> m (SOP.K (Maybe Encoding) field)
instance (fld0 ~ fld1, ToNullParam c ty x, forall m. c m => Functor m)
  => ToField c (fld0 ::: ty) (fld1 ::: x) where
    toField (SOP.P x) = SOP.K <$> toNullParam @c @ty x

-- | A `ToArray` constraint gives an encoding of a Haskell `Type`
-- into the binary format of a PostgreSQL fixed-length array.
-- You should not define instances for
-- `ToArray`, just use the provided instances.
class ToArray c (dims :: [Nat]) (ty :: NullType) x where
  arrayPayload :: forall m. c m => x -> m Encoding
  arrayDims :: [Int32]
  arrayNulls :: Bool
instance (forall m. c m => Applicative m, ToParam c pg x)
  => ToArray c '[] ('NotNull pg) x where
    arrayPayload = fmap (sized . SOP.unK) . toParam @c @pg @x
    arrayDims = []
    arrayNulls = True
instance (forall m. c m => Applicative m, ToParam c pg x)
  => ToArray c '[] ('Null pg) (Maybe x) where
    arrayPayload = maybe
      (pure null4) (fmap (sized . SOP.unK) . toParam @c @pg @x)
    arrayDims = []
    arrayNulls = False
instance
  ( SOP.IsProductType tuple xs
  , Length xs ~ dim
  , SOP.All ((~) x) xs
  , ToArray c dims ty x
  , KnownNat dim
  , forall m. c m => Monad m )
  => ToArray c (dim ': dims) ty tuple where
    arrayPayload
      = dimArray foldlNP (arrayPayload @c @dims @ty @x)
      . SOP.unZ . SOP.unSOP . SOP.from
    arrayDims
      = fromIntegral (natVal (SOP.Proxy @dim))
      : arrayDims @c @dims @ty @x
    arrayNulls = arrayNulls @c @dims @ty @x
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

>>> :{
let
  encode :: EncodeParams
    '[ 'NotNull 'PGint2, 'NotNull ('PGchar 1), 'NotNull 'PGtext]
    (Int16, (Char, String))
  encode = fst .* fst.snd *. snd.snd
in runEncodeParams encode (1,('a',"foo"))
:}
K (Just "\NUL\SOH") :* K (Just "a") :* K (Just "foo") :* Nil
-}
newtype EncodeParams c (tys :: [NullType]) (x :: Type) = EncodeParams
  { runEncodeParams
    :: forall m. c m => x -> m (NP (SOP.K (Maybe Encoding)) tys) }
instance Contravariant (EncodeParams m tys) where
  contramap f (EncodeParams g) = EncodeParams (g . f)

{- | Parameter encoding for `SOP.Generic` tuples and records.

>>> import qualified GHC.Generics as GHC
>>> import qualified Generics.SOP as SOP
>>> data Two = Two Int16 String deriving (GHC.Generic, SOP.Generic)
>>> :{
let
  encode :: EncodeParams '[ 'NotNull 'PGint2, 'NotNull 'PGtext] Two
  encode = genericParams
in runEncodeParams encode (Two 2 "two")
:}
K (Just "\NUL\STX") :* K (Just "two") :* Nil

>>> :{
let
  encode :: EncodeParams '[ 'NotNull 'PGint2, 'NotNull 'PGtext] (Int16, String)
  encode = genericParams
in runEncodeParams encode (2, "two")
:}
K (Just "\NUL\STX") :* K (Just "two") :* Nil

-}
genericParams :: forall c x xs tys.
  ( SOP.IsProductType x xs
  , SOP.AllZip (ToNullParam c) tys xs
  , forall m. c m => Applicative m
  ) => EncodeParams c tys x
genericParams = EncodeParams
  $ hctransverse (SOP.Proxy @(ToNullParam c)) encodeNullParam
  . SOP.unZ . SOP.unSOP . SOP.from
  where
    encodeNullParam
      :: forall m ty y. (c m, ToNullParam c ty y)
      => SOP.I y -> m (SOP.K (Maybe Encoding) ty)
    encodeNullParam = fmap SOP.K . toNullParam @c @ty. SOP.unI

      -- fields :: NP (SOP.K (Maybe Encoding)) fields <- hctransverse
      --   (SOP.Proxy @(ToField c)) (toField @c) (SOP.toRecord x)

-- hctransverse
--   :: (SOP.AllZip c ys xs, Applicative m)
--   => SOP.Proxy c
--   -> (forall y x. c y x => f x -> m (g y))
--   -> NP f xs -> m (NP g ys)

-- | Encode 0 parameters.
nilParams :: EncodeParams Monad '[] x
nilParams = EncodeParams $ return (pure Nil)

{- | Cons a parameter encoding.

>>> :{
let
  encode :: EncodeParams
    '[ 'Null 'PGint4, 'NotNull 'PGtext]
    (Maybe Int32, String)
  encode = fst .* snd .* nilParams
in runEncodeParams encode (Nothing, "foo")
:}
K Nothing :* K (Just "foo") :* Nil
-}
(.*)
  :: forall c x0 ty x tys. (ToNullParam c ty x0, forall m. c m => Applicative m)
  => (x -> x0)
  -> EncodeParams c tys x
  -> EncodeParams c (ty ': tys) x
f .* EncodeParams params = EncodeParams $ \x ->
  (:*) <$> (SOP.K <$> toNullParam @c @ty (f x)) <*> params x
infixr 5 .*

{- | End a parameter encoding.

>>> :{
let
  encode :: EncodeParams
    '[ 'Null 'PGint4, 'NotNull 'PGtext, 'NotNull ('PGchar 1)]
    (Maybe Int32, String, Char)
  encode = (\(x,_,_) -> x) .* (\(_,y,_) -> y) *. (\(_,_,z) -> z)
in runEncodeParams encode (Nothing, "foo", 'z')
:}
K Nothing :* K (Just "foo") :* K (Just "z") :* Nil
-}
(*.)
  :: forall x x0 ty0 x1 ty1
   . (ToNullParam Monad ty0 x0, ToNullParam Monad ty1 x1)
  => (x -> x0)
  -> (x -> x1)
  -> EncodeParams Monad '[ty0, ty1] x
f *. g = f .* g .* nilParams
infixl 8 *.

{- | Encode 1 parameter.

>>> :{
let
  encode :: EncodeParams '[ 'NotNull 'PGint4] Int32
  encode = aParam
in runEncodeParams encode 1776
:}
K (Just "\NUL\NUL\ACK\240") :* Nil
-}
aParam
  :: forall c ty x. (ToNullParam c ty x, forall m. c m => Functor m)
  => EncodeParams c '[ty] x
aParam = EncodeParams $
  fmap (\param -> SOP.K param :* Nil) . toNullParam @c @ty

{- | Append parameter encodings.

>>> :{
let
  encode :: EncodeParams
    '[ 'NotNull 'PGint4, 'NotNull 'PGint2]
    (Int32, Int16)
  encode = contramap fst aParam `appendParams` contramap snd aParam
in runEncodeParams encode (1776, 2)
:}
K (Just "\NUL\NUL\ACK\240") :* K (Just "\NUL\STX") :* Nil
-}
appendParams
  :: EncodeParams Monad params0 x
  -> EncodeParams Monad params1 x
  -> EncodeParams Monad (Join params0 params1) x
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
  :: Monad m
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
  :: (Monoid r, Monad m, SOP.All c xs)
  => SOP.Proxy c
  -> (forall x. c x => f x -> m r)
  -> NP f xs -> m r
hcfoldMapM c f = \case
  Nil -> pure mempty
  x :* xs -> (<>) <$> f x <*> hcfoldMapM c f xs

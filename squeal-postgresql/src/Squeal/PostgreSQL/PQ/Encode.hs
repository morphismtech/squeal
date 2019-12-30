{-|
Module: Squeal.PostgreSQL.PQ.Encode
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

module Squeal.PostgreSQL.PQ.Encode
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
import Data.Bits
import Data.ByteString as Strict (ByteString)
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.Function ((&))
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
import Squeal.PostgreSQL.PQ.Oid
import Squeal.PostgreSQL.Schema

-- | A `ToParam` constraint gives an encoding of a Haskell `Type` into
-- into the binary format of a PostgreSQL `PGType`.
class ToParam pg x where
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
  -- >>> newtype Id = Id { getId :: Int16 } deriving newtype (ToParam 'PGint2)
  -- >>> toParam @'PGint2 (Id 1)
  -- K "\NUL\SOH"
  toParam :: x -> SOP.K Encoding pg
instance ToParam 'PGbool Bool where toParam = SOP.K . bool
instance ToParam 'PGint2 Int16 where toParam = SOP.K . int2_int16
instance ToParam 'PGint4 Int32 where toParam = SOP.K . int4_int32
instance ToParam 'PGint8 Int64 where toParam = SOP.K . int8_int64
instance ToParam 'PGoid Oid where toParam = SOP.K . int4_word32 . getOid
instance ToParam 'PGfloat4 Float where toParam = SOP.K . float4
instance ToParam 'PGfloat8 Double where toParam = SOP.K . float8
instance ToParam 'PGnumeric Scientific where toParam = SOP.K . numeric
instance ToParam 'PGmoney Money where toParam = SOP.K . int8_int64 . cents
instance ToParam 'PGuuid UUID where toParam = SOP.K . uuid
instance ToParam 'PGinet (NetAddr IP) where toParam = SOP.K . inet
instance ToParam ('PGchar 1) Char where toParam = SOP.K . char_utf8
instance ToParam 'PGtext Strict.Text where toParam = SOP.K . text_strict
instance ToParam 'PGtext Lazy.Text where toParam = SOP.K . text_lazy
instance ToParam 'PGtext String where
  toParam = SOP.K . text_strict . Strict.Text.pack
instance ToParam 'PGbytea Strict.ByteString where toParam = SOP.K . bytea_strict
instance ToParam 'PGbytea Lazy.ByteString where toParam = SOP.K . bytea_lazy
instance ToParam 'PGdate Day where toParam = SOP.K . date
instance ToParam 'PGtime TimeOfDay where toParam = SOP.K . time_int
instance ToParam 'PGtimetz (TimeOfDay, TimeZone) where toParam = SOP.K . timetz_int
instance ToParam 'PGtimestamp LocalTime where toParam = SOP.K . timestamp_int
instance ToParam 'PGtimestamptz UTCTime where toParam = SOP.K . timestamptz_int
instance ToParam 'PGinterval DiffTime where toParam = SOP.K . interval_int
instance ToParam 'PGjson Aeson.Value where toParam = SOP.K . json_ast
instance ToParam 'PGjsonb Aeson.Value where toParam = SOP.K . jsonb_ast
instance Aeson.ToJSON x => ToParam 'PGjson (Json x) where
  toParam = SOP.K . json_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJson
instance Aeson.ToJSON x => ToParam 'PGjsonb (Jsonb x) where
  toParam = SOP.K . jsonb_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJsonb
instance (ToNullParam ty x, OidOfNull ty)
  => ToParam ('PGvararray ty) (VarArray [x]) where
    toParam
      = SOP.K . array_foldable (getOid (oidOfNull @ty)) (toNullParam @ty)
      . getVarArray
instance (ToParam pg x, OidOf pg)
  => ToParam ('PGvararray ('NotNull pg)) (VarArray (Vector x)) where
    toParam
      = SOP.K . array_vector (getOid (oidOf @pg)) (SOP.unK . toParam @pg)
      . getVarArray
instance (ToParam pg x, OidOf pg)
  => ToParam ('PGvararray ('Null pg)) (VarArray (Vector (Maybe x))) where
    toParam
      = SOP.K . nullableArray_vector
        (getOid (oidOf @pg)) (SOP.unK . toParam @pg)
      . getVarArray
instance (ToFixArray dims ty x, OidOfNull ty)
  => ToParam ('PGfixarray dims ty) (FixArray x) where
    toParam
      = SOP.K . array (getOid (oidOfNull @ty))
      . toFixArray @dims @ty
      . getFixArray
instance
  ( SOP.IsEnumType x
  , SOP.HasDatatypeInfo x
  , LabelsPG x ~ labels
  ) => ToParam ('PGenum labels) (Enumerated x) where
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
        SOP.K
        . text_strict
        . Strict.Text.pack
        . gshowConstructor
          (SOP.constructorInfo (SOP.datatypeInfo (SOP.Proxy @x)))
        . SOP.from
        . getEnumerated
instance
  ( SOP.SListI fields
  , SOP.IsRecord x xs
  , SOP.AllZip ToField fields xs
  , SOP.All OidOfField fields
  ) => ToParam ('PGcomposite fields) (Composite x) where
    toParam =
      let
        toFields = trans_NP_flip (SOP.Proxy @ToField) toField
        composite
          :: NP (SOP.K (Maybe Encoding)) fields
          -> Encoding
        composite fields =
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
          int32BE (fromIntegral (SOP.lengthSList (SOP.Proxy @xs))) <>
            let
              each
                :: OidOfField field
                => SOP.K (Maybe Encoding) field
                -> Encoding
              each (SOP.K field :: SOP.K (Maybe Encoding) field) =
                word32BE (getOid (oidOfField @field))
                <> case field of
                  Nothing -> int64BE (-1)
                  Just value ->
                    int32BE (fromIntegral (builderLength value))
                    <> value
            in
              SOP.hcfoldMap (SOP.Proxy @OidOfField) each fields
      in
        SOP.K . composite . toFields . SOP.toRecord . getComposite
instance ToParam pg x => ToParam ('PGrange pg) (Range x) where
  toParam rng = SOP.K $
    word8 (setFlags rng 0) <>
      case rng of
        Empty -> mempty
        NonEmpty lower upper -> putBound lower <> putBound upper
    where
      putBound = \case
        Infinite -> mempty
        Closed value -> putValue (SOP.unK $ toParam @pg value)
        Open value -> putValue (SOP.unK $ toParam @pg value)
      putValue value = int32BE (fromIntegral (builderLength value)) <> value
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
class ToNullParam ty x where toNullParam :: x -> Maybe Encoding
instance ToParam pg x => ToNullParam ('NotNull pg) x where
  toNullParam = return . SOP.unK . toParam @pg
instance ToParam pg x => ToNullParam ('Null pg) (Maybe x) where
  toNullParam = fmap (SOP.unK . toParam @pg)

-- | A `ToField` constraint lifts the `ToParam` parser
-- to an encoding of a @(Symbol, Type)@ to a @(Symbol, NullityType)@,
-- encoding `Null`s to `Maybe`s. You should not define instances for
-- `ToField`, just use the provided instances.
class ToField field x where
  toField :: SOP.P x -> SOP.K (Maybe Encoding) field
instance (fld0 ~ fld1, ToNullParam ty x)
  => ToField (fld0 ::: ty) (fld1 ::: x) where
    toField (SOP.P x) = SOP.K $ toNullParam @ty x

-- | A `ToFixArray` constraint gives an encoding of a Haskell `Type`
-- into the binary format of a PostgreSQL fixed-length array.
-- You should not define instances for
-- `ToFixArray`, just use the provided instances.
class ToFixArray dims ty x where toFixArray :: x -> Array
instance ToNullParam ty x => ToFixArray '[] ty x where
  toFixArray = maybe nullArray encodingArray . toNullParam @ty
instance
  ( SOP.IsProductType tuple xs
  , Length xs ~ dim
  , SOP.All ((~) x) xs
  , ToFixArray dims ty x )
  => ToFixArray (dim ': dims) ty tuple where
    toFixArray
      = dimensionArray foldlN (toFixArray @dims @ty @x)
      . SOP.unZ . SOP.unSOP . SOP.from
foldlN
  :: SOP.All ((~) x) xs
  => (z -> x -> z) -> z -> NP SOP.I xs -> z
foldlN f z = \case
  Nil -> z
  SOP.I x :* xs -> let z' = f z x in seq z' $ foldlN f z' xs

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
newtype EncodeParams (tys :: [NullType]) (x :: Type) = EncodeParams
  { runEncodeParams :: x -> NP (SOP.K (Maybe Encoding)) tys }
instance Contravariant (EncodeParams tys) where
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
genericParams ::
  ( SOP.IsProductType x xs
  , SOP.AllZip ToNullParam tys xs
  ) => EncodeParams tys x
genericParams = EncodeParams
  $ trans_NP_flip (SOP.Proxy @ToNullParam) encodeNullParam
  . SOP.unZ . SOP.unSOP . SOP.from
  where
    encodeNullParam
      :: forall ty x. ToNullParam ty x
      => SOP.I x -> SOP.K (Maybe Encoding) ty
    encodeNullParam = SOP.K . toNullParam @ty . SOP.unI

trans_NP_flip
  :: SOP.AllZip c ys xs
  => proxy c
  -> (forall y x . c y x => f x -> g y)
  -> NP f xs -> NP g ys
trans_NP_flip _ _t Nil = Nil
trans_NP_flip p  t (x :* xs) = t x :* trans_NP_flip p t xs

-- | Encode 0 parameters.
nilParams :: EncodeParams '[] x
nilParams = EncodeParams $ return Nil

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
  :: forall x0 ty x tys. ToNullParam ty x0
  => (x -> x0)
  -> EncodeParams tys x
  -> EncodeParams (ty ': tys) x
f .* EncodeParams params = EncodeParams $ \x ->
  SOP.K (toNullParam @ty (f x)) :* params x
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
   . (ToNullParam ty0 x0, ToNullParam ty1 x1)
  => (x -> x0)
  -> (x -> x1)
  -> EncodeParams '[ty0, ty1] x
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
aParam :: forall ty x. ToNullParam ty x => EncodeParams '[ty] x
aParam = EncodeParams $ \x -> SOP.K (toNullParam @ty x) :* Nil

{- | End a parameter encoding.

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
  :: EncodeParams params0 x
  -> EncodeParams params1 x
  -> EncodeParams (Join params0 params1) x
appendParams encode0 encode1 = EncodeParams $ \x ->
  runEncodeParams encode0 x & also (runEncodeParams encode1 x)

getOid :: LibPQ.Oid -> Word32
getOid (LibPQ.Oid (CUInt oid)) = oid

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
  ( EncodeParams (..)
  , genericParams
  , nilParams
  , (.*)
  , (*.)
  , oneParam
  , appendParams
  , deconsParams
  , disjoinParams
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

class ToParam pg x where toParam :: x -> Encoding
instance ToParam 'PGbool Bool where toParam = bool
instance ToParam 'PGint2 Int16 where toParam = int2_int16
instance ToParam 'PGint4 Int32 where toParam = int4_int32
instance ToParam 'PGint8 Int64 where toParam = int8_int64
instance ToParam 'PGoid Oid where toParam = int4_word32 . getOid
instance ToParam 'PGfloat4 Float where toParam = float4
instance ToParam 'PGfloat8 Double where toParam = float8
instance ToParam 'PGnumeric Scientific where toParam = numeric
instance ToParam 'PGmoney Money where toParam = int8_int64 . cents
instance ToParam 'PGuuid UUID where toParam = uuid
instance ToParam 'PGinet (NetAddr IP) where toParam = inet
instance ToParam ('PGchar 1) Char where toParam = char_utf8
instance ToParam 'PGtext Strict.Text where toParam = text_strict
instance ToParam 'PGtext Lazy.Text where toParam = text_lazy
instance ToParam 'PGtext String where
  toParam = text_strict . Strict.Text.pack
instance ToParam 'PGbytea Strict.ByteString where toParam = bytea_strict
instance ToParam 'PGbytea Lazy.ByteString where toParam = bytea_lazy
instance ToParam 'PGdate Day where toParam = date
instance ToParam 'PGtime TimeOfDay where toParam = time_int
instance ToParam 'PGtimetz (TimeOfDay, TimeZone) where toParam = timetz_int
instance ToParam 'PGtimestamp LocalTime where toParam = timestamp_int
instance ToParam 'PGtimestamptz UTCTime where toParam = timestamptz_int
instance ToParam 'PGinterval DiffTime where toParam = interval_int
instance ToParam 'PGjson Aeson.Value where toParam = json_ast
instance ToParam 'PGjsonb Aeson.Value where toParam = jsonb_ast
instance Aeson.ToJSON x => ToParam 'PGjson (Json x) where
  toParam = json_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJson
instance Aeson.ToJSON x => ToParam 'PGjsonb (Jsonb x) where
  toParam = jsonb_bytes
    . Lazy.ByteString.toStrict . Aeson.encode . getJsonb
instance (ToNullParam ty x, OidOfNull ty)
  => ToParam ('PGvararray ty) (VarArray [x]) where
    toParam
      = array_foldable (getOid (oidOfNull @ty)) (toNullParam @ty)
      . getVarArray
instance (ToParam pg x, OidOf pg)
  => ToParam ('PGvararray ('NotNull pg)) (VarArray (Vector x)) where
    toParam
      = array_vector (getOid (oidOf @pg)) (toParam @pg)
      . getVarArray
instance (ToParam pg x, OidOf pg)
  => ToParam ('PGvararray ('Null pg)) (VarArray (Vector (Maybe x))) where
    toParam
      = nullableArray_vector
        (getOid (oidOf @pg)) (toParam @pg)
      . getVarArray
instance (ToFixArray dims ty x, OidOfNull ty)
  => ToParam ('PGfixarray dims ty) (FixArray x) where
    toParam
      = array (getOid (oidOfNull @ty))
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
        text_strict
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
        encodeField
          :: forall field y. ToField field y
          => SOP.P y -> SOP.K (Maybe Encoding) field
        encodeField = SOP.K . toField @field

        encoders
          :: SOP.AllZip ToField fields ys
          => NP SOP.P ys -> NP (SOP.K (Maybe Encoding)) fields
        encoders = SOP.htrans (SOP.Proxy @ToField) encodeField

        composite
          :: SOP.All OidOfField row
          => NP (SOP.K (Maybe Encoding)) row
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
        composite . encoders . SOP.toRecord . getComposite
instance ToParam pg x => ToParam ('PGrange pg) (Range x) where
  toParam rng =
    word8 (setFlags rng 0) <>
      case rng of
        Empty -> mempty
        NonEmpty lower upper -> putBound lower <> putBound upper
    where
      putBound = \case
        Infinite -> mempty
        Closed value -> putValue (toParam @pg value)
        Open value -> putValue (toParam @pg value)
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

class ToNullParam ty x where toNullParam :: x -> Maybe Encoding
instance ToParam pg x => ToNullParam ('NotNull pg) x where
  toNullParam = return . toParam @pg
instance ToParam pg x => ToNullParam ('Null pg) (Maybe x) where
  toNullParam = fmap (toParam @pg)

class ToField field x where toField :: SOP.P x -> Maybe Encoding
instance (fld0 ~ fld1, ToNullParam ty x) => ToField (fld0 ::: ty) (fld1 ::: x) where
  toField (SOP.P x) = toNullParam @ty x

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

newtype EncodeParams (tys :: [NullType]) (x :: Type) = EncodeParams
  { runEncodeParams :: x -> NP (SOP.K (Maybe Encoding)) tys }
instance Contravariant (EncodeParams tys) where
  contramap f (EncodeParams g) = EncodeParams (g . f)
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

nilParams :: EncodeParams '[] x
nilParams = EncodeParams $ return Nil

(.*)
  :: forall x0 ty x tys. ToNullParam ty x0
  => (x -> x0)
  -> EncodeParams tys x
  -> EncodeParams (ty ': tys) x
f .* EncodeParams params = EncodeParams $ \x ->
  SOP.K (toNullParam @ty (f x)) :* params x
infixr 5 .*

(*.)
  :: forall x x0 ty0 x1 ty1
   . (ToNullParam ty0 x0, ToNullParam ty1 x1)
  => (x -> x0)
  -> (x -> x1)
  -> EncodeParams '[ty0, ty1] x
f *. g = f .* g .* nilParams
infixl 9 *.

oneParam :: ToNullParam ty y => (x -> y) -> EncodeParams '[ty] x
oneParam f = f .* nilParams

appendParams
  :: EncodeParams params0 x
  -> EncodeParams params1 x
  -> EncodeParams (Join params0 params1) x
appendParams encode0 encode1 = EncodeParams $ \x ->
  runEncodeParams encode0 x & also (runEncodeParams encode1 x)

deconsParams
  :: forall param params x y z. ToNullParam param y
  => (x -> (y,z))
  -> EncodeParams params z
  -> EncodeParams (param ': params) x
deconsParams u encode = EncodeParams $ \x ->
  let (y,z) = u x
  in SOP.K (toNullParam @param y) :* runEncodeParams encode z

disjoinParams
  :: (x -> (y,z))
  -> EncodeParams params0 y
  -> EncodeParams params1 z
  -> EncodeParams (Join params0 params1) x
disjoinParams u encode0 encode1 = EncodeParams $ \x ->
  let (y,z) = u x
  in runEncodeParams encode0 y & also (runEncodeParams encode1 z)

getOid :: LibPQ.Oid -> Word32
getOid (LibPQ.Oid (CUInt oid)) = oid

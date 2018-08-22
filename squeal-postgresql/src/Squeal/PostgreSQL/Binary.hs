{-|
Module: Squeal.PostgreSQL.Binary
Description: Binary encoding and decoding
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

This module provides binary encoding and decoding between Haskell and PostgreSQL types.

Instances are governed by the `Generic` and `HasDatatypeInfo` typeclasses, so you absolutely
do not need to define your own instances to decode retrieved rows into Haskell values or
to encode Haskell values into statement parameters.

>>> import Data.Int (Int16)
>>> import Data.Text (Text)

>>> data Row = Row { col1 :: Int16, col2 :: Text } deriving (Eq, GHC.Generic)
>>> instance Generic Row
>>> instance HasDatatypeInfo Row

>>> import Control.Monad (void)
>>> import Control.Monad.Base (liftBase)
>>> import Squeal.PostgreSQL

>>> :{
let
  query :: Query '[]
    '[ 'NotNull 'PGint2, 'NotNull 'PGtext]
    '["col1" ::: 'NotNull 'PGint2, "col2" ::: 'NotNull 'PGtext]
  query = values_ (param @1 `as` #col1 :* param @2 `as` #col2)
:}

>>> :{
let
  roundtrip :: IO ()
  roundtrip = void . withConnection "host=localhost port=5432 dbname=exampledb" $ do
    result <- runQueryParams query (2 :: Int16, "hi" :: Text)
    Just row <- firstRow result
    liftBase . print $ row == Row 2 "hi"
:}

>>> roundtrip
True

In addition to being able to encode and decode basic Haskell types like `Int16` and `Text`,
Squeal permits you to encode and decode Haskell types which are equivalent to
Postgres enumerated and composite types.

Enumerated (enum) types are data types that comprise a static, ordered set of values.
They are equivalent to Haskell algebraic data types whose constructors are nullary.
An example of an enum type might be the days of the week,
or a set of status values for a piece of data.

>>> data Schwarma = Beef | Lamb | Chicken deriving (Show, GHC.Generic)
>>> instance Generic Schwarma
>>> instance HasDatatypeInfo Schwarma

A composite type represents the structure of a row or record;
it is essentially just a list of field names and their data types. They are almost
equivalent to Haskell record types. However, because of the potential presence of @NULL@
all the record fields must be `Maybe`s of basic types.

>>> data Person = Person {name :: Maybe Text, age :: Maybe Int32} deriving (Show, GHC.Generic)
>>> instance Generic Person
>>> instance HasDatatypeInfo Person

We can create the equivalent Postgres types directly from their Haskell types.

>>> :{
type Schema =
  '[ "schwarma" ::: 'Typedef (EnumFrom Schwarma)
   , "person" ::: 'Typedef (CompositeFrom Person)
   ]
:}

>>> :{
let
  setup :: Definition '[] Schema
  setup =
    createTypeEnumFrom @Schwarma #schwarma >>>
    createTypeCompositeFrom @Person #person
:}

Then we can perform roundtrip queries;

>>> :{
let
  querySchwarma :: Query Schema
    '[ 'NotNull (EnumFrom Schwarma)]
    '["fromOnly" ::: 'NotNull (EnumFrom Schwarma)]
  querySchwarma = values_ (parameter @1 (typedef #schwarma) `as` #fromOnly)
:}

>>> :{
let
  queryPerson :: Query Schema
    '[ 'NotNull (CompositeFrom Person)]
    '["fromOnly" ::: 'NotNull (CompositeFrom Person)]
  queryPerson = values_ (parameter @1 (typedef #person) `as` #fromOnly)
:}

And finally drop the types.

>>> :{
let
  teardown :: Definition Schema '[]
  teardown = dropType #schwarma >>> dropType #person
:}

Now let's run it.

>>> :{
let
  session = do
    result1 <- runQueryParams querySchwarma (Only Chicken)
    Just (Only schwarma) <- firstRow result1
    liftBase $ print (schwarma :: Schwarma)
    result2 <- runQueryParams queryPerson (Only (Person (Just "Faisal") (Just 24)))
    Just (Only person) <- firstRow result2
    liftBase $ print (person :: Person)
in
  void . withConnection "host=localhost port=5432 dbname=exampledb" $
    define setup
    & pqThen session
    & pqThen (define teardown)
:}
Chicken
Person {name = Just "Faisal", age = Just 24}
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Binary
  ( -- * Encoding
    ToParam (..)
  , ToParams (..)
    -- * Decoding
  , FromValue (..)
  , FromRow (..)
    -- * Only
  , Only (..)
  ) where

import BinaryParser
import ByteString.StrictBuilder
import Control.Monad
import Data.Aeson hiding (Null)
import Data.Int
import Data.Kind
import Data.Monoid hiding (All)
import Data.Scientific
import Data.Time
import Data.UUID.Types
import Data.Vector (Vector)
import Data.Word
import Generics.SOP
import Generics.SOP.Record
import GHC.TypeLits
import Network.IP.Addr

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict hiding (pack, unpack)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Strict hiding (empty)
import qualified Data.Vector as Vector
import qualified GHC.Generics as GHC
import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding

import Squeal.PostgreSQL.Schema

-- | A `ToParam` constraint gives an encoding of a Haskell `Type` into
-- into the binary format of a PostgreSQL `PGType`.
class ToParam (x :: Type) (pg :: PGType) where
  -- | >>> :set -XTypeApplications -XDataKinds
  -- >>> toParam @Bool @'PGbool False
  -- K "\NUL"
  --
  -- >>> toParam @Int16 @'PGint2 0
  -- K "\NUL\NUL"
  --
  -- >>> toParam @Int32 @'PGint4 0
  -- K "\NUL\NUL\NUL\NUL"
  --
  -- >>> :set -XMultiParamTypeClasses
  -- >>> newtype Id = Id { getId :: Int16 } deriving Show
  -- >>> instance ToParam Id 'PGint2 where toParam = toParam . getId
  -- >>> toParam @Id @'PGint2 (Id 1)
  -- K "\NUL\SOH"
  toParam :: x -> K Encoding.Encoding pg
instance ToParam Bool 'PGbool where toParam = K . Encoding.bool
instance ToParam Int16 'PGint2 where toParam = K . Encoding.int2_int16
instance ToParam Word16 'PGint2 where toParam = K . Encoding.int2_word16
instance ToParam Int32 'PGint4 where toParam = K . Encoding.int4_int32
instance ToParam Word32 'PGint4 where toParam = K . Encoding.int4_word32
instance ToParam Int64 'PGint8 where toParam = K . Encoding.int8_int64
instance ToParam Word64 'PGint8 where toParam = K . Encoding.int8_word64
instance ToParam Float 'PGfloat4 where toParam = K . Encoding.float4
instance ToParam Double 'PGfloat8 where toParam = K . Encoding.float8
instance ToParam Scientific 'PGnumeric where toParam = K . Encoding.numeric
instance ToParam UUID 'PGuuid where toParam = K . Encoding.uuid
instance ToParam (NetAddr IP) 'PGinet where toParam = K . Encoding.inet
instance ToParam Char ('PGchar 1) where toParam = K . Encoding.char_utf8
instance ToParam Strict.Text 'PGtext where toParam = K . Encoding.text_strict
instance ToParam Lazy.Text 'PGtext where toParam = K . Encoding.text_lazy
instance ToParam Strict.ByteString 'PGbytea where
  toParam = K . Encoding.bytea_strict
instance ToParam Lazy.ByteString 'PGbytea where
  toParam = K . Encoding.bytea_lazy
instance ToParam Day 'PGdate where toParam = K . Encoding.date
instance ToParam TimeOfDay 'PGtime where toParam = K . Encoding.time_int
instance ToParam (TimeOfDay, TimeZone) 'PGtimetz where
  toParam = K . Encoding.timetz_int
instance ToParam LocalTime 'PGtimestamp where
  toParam = K . Encoding.timestamp_int
instance ToParam UTCTime 'PGtimestamptz where
  toParam = K . Encoding.timestamptz_int
instance ToParam DiffTime 'PGinterval where toParam = K . Encoding.interval_int
instance ToParam Value 'PGjson where toParam = K . Encoding.json_ast
instance ToParam Value 'PGjsonb where toParam = K . Encoding.jsonb_ast
instance ToArray x ('PGvararray ty) => ToParam x ('PGvararray ty) where
  toParam
    = K . Encoding.array (baseOid @x @('PGvararray ty))
    . unK . toArray @x @('PGvararray ty)
instance ToArray x ('PGfixarray n ty) => ToParam x ('PGfixarray n ty) where
  toParam
    = K . Encoding.array (baseOid @x @('PGfixarray n ty))
    . unK . toArray @x @('PGfixarray n ty)
instance
  ( IsEnumType x
  , HasDatatypeInfo x
  , LabelsFrom x ~ labels
  ) => ToParam x ('PGenum labels) where
    toParam =
      let
        gshowConstructor :: NP ConstructorInfo xss -> SOP I xss -> String
        gshowConstructor Nil _ = ""
        gshowConstructor (constructor :* _) (SOP (Z _)) =
          constructorName constructor
        gshowConstructor (_ :* constructors) (SOP (S xs)) =
          gshowConstructor constructors (SOP xs)
      in
        K . Encoding.text_strict
        . Strict.pack
        . gshowConstructor (constructorInfo (datatypeInfo (Proxy @x)))
        . from
instance
  ( SListI fields
  , IsRecord x xs
  , AllZip ToField xs fields
  , All HasAliasedOid fields
  ) => ToParam x ('PGcomposite fields) where
    toParam =
      let

        encoders = htrans (Proxy @ToField) toField

        composite
          :: All HasAliasedOid row
          => NP (K (Maybe Encoding.Encoding)) row
          -> K Encoding.Encoding ('PGcomposite row)
        composite fields = K $
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
          int32BE (fromIntegral (lengthSList (Proxy @xs))) <>
            let
              each
                :: HasAliasedOid field
                => K (Maybe Encoding.Encoding) field
                -> Encoding.Encoding
              each (K field :: K (Maybe Encoding.Encoding) field) =
                word32BE (aliasedOid @field)
                <> case field of
                  Nothing -> int64BE (-1)
                  Just value ->
                    int32BE (fromIntegral (builderLength value))
                    <> value
            in
              hcfoldMap (Proxy @HasAliasedOid) each fields

      in
        composite . encoders . toRecord

class HasAliasedOid (field :: (Symbol, NullityType)) where
  aliasedOid :: Word32
instance HasOid ty => HasAliasedOid (alias ::: nullity ty) where
  aliasedOid = oid @ty

class ToNullityParam (x :: Type) (ty :: NullityType) where
  toNullityParam :: x -> K (Maybe Encoding.Encoding) ty
instance ToParam x pg => ToNullityParam x ('NotNull pg) where
  toNullityParam = K . Just . unK . toParam @x @pg
instance ToParam x pg => ToNullityParam (Maybe x) ('Null pg) where
  toNullityParam = K . fmap (unK . toParam @x @pg)

class ToField (x :: (Symbol, Type)) (field :: (Symbol, NullityType)) where
  toField :: P x -> K (Maybe Encoding.Encoding) field
instance ToNullityParam x ty => ToField (alias ::: x) (alias ::: ty) where
  toField (P x) = K . unK $ toNullityParam @x @ty x

class ToArray (x :: Type) (array :: PGType) where
  toArray :: x -> K Encoding.Array array
  default toArray :: ToParam x array => x -> K Encoding.Array array
  toArray = K . Encoding.encodingArray . unK . toParam @x @array
  baseOid :: Word32
  default baseOid :: HasOid array => Word32
  baseOid = oid @array
instance ToArray Bool 'PGbool
instance ToArray Int16 'PGint2
instance ToArray Word16 'PGint2
instance ToArray Int32 'PGint4
instance ToArray Word32 'PGint4
instance ToArray Int64 'PGint8
instance ToArray Word64 'PGint8
instance ToArray Float 'PGfloat4
instance ToArray Double 'PGfloat8
instance ToArray Scientific 'PGnumeric
instance ToArray UUID 'PGuuid
instance ToArray (NetAddr IP) 'PGinet
instance ToArray Char ('PGchar 1)
instance ToArray Strict.Text 'PGtext
instance ToArray Lazy.Text 'PGtext
instance ToArray Strict.ByteString 'PGbytea
instance ToArray Lazy.ByteString 'PGbytea
instance ToArray Day 'PGdate
instance ToArray TimeOfDay 'PGtime
instance ToArray (TimeOfDay, TimeZone) 'PGtimetz
instance ToArray LocalTime 'PGtimestamp
instance ToArray UTCTime 'PGtimestamptz
instance ToArray DiffTime 'PGinterval
instance ToArray Value 'PGjson
instance ToArray Value 'PGjsonb
instance ToArray x array
  => ToArray (Vector x) ('PGvararray ('NotNull array)) where
    toArray = K . Encoding.dimensionArray Vector.foldl'
      (unK . toArray @x @array)
    baseOid = baseOid @x @array
instance ToArray x array
  => ToArray (Vector (Maybe x)) ('PGvararray ('Null array)) where
    toArray = K . Encoding.dimensionArray Vector.foldl'
      (maybe Encoding.nullArray (unK . toArray @x @array))
    baseOid = baseOid @x @array
instance (HomogeneousProduct n x product, ToArray x array)
  => ToArray product ('PGfixarray n ('NotNull array)) where
    toArray = K . Encoding.dimensionArray foldlN
      (unK . toArray @x @array)
    baseOid = baseOid @x @array
instance (HomogeneousProduct n (Maybe x) product, ToArray x array)
  => ToArray product ('PGfixarray n ('Null array)) where
    toArray = K . Encoding.dimensionArray foldlN
      (maybe Encoding.nullArray (unK . toArray @x @array))
    baseOid = baseOid @x @array

-- | A `ToParams` constraint generically sequences the encodings of `Type`s
-- of the fields of a tuple or record to a row of `ColumnType`s. You should
-- not define instances of `ToParams`. Instead define `Generic` instances
-- which in turn provide `ToParams` instances.
class SListI tys => ToParams (x :: Type) (tys :: [NullityType]) where
  -- | >>> type Params = '[ 'NotNull 'PGbool, 'Null 'PGint2]
  -- >>> toParams @(Bool, Maybe Int16) @'[ 'NotNull 'PGbool, 'Null 'PGint2] (False, Just 0)
  -- K (Just "\NUL") :* K (Just "\NUL\NUL") :* Nil
  --
  -- >>> :set -XDeriveGeneric
  -- >>> data Tuple = Tuple { p1 :: Bool, p2 :: Maybe Int16} deriving GHC.Generic
  -- >>> instance Generic Tuple
  -- >>> toParams @Tuple @Params (Tuple False (Just 0))
  -- K (Just "\NUL") :* K (Just "\NUL\NUL") :* Nil
  toParams :: x -> NP (K (Maybe Encoding.Encoding)) tys
instance (SListI tys, IsProductType x xs, AllZip ToNullityParam xs tys)
  => ToParams x tys where
      toParams
        = htrans (Proxy @ToNullityParam) (toNullityParam . unI)
        . unZ . unSOP . from

-- | A `FromValue` constraint gives a parser from the binary format of
-- a PostgreSQL `PGType` into a Haskell `Type`.
class FromValue (pg :: PGType) (y :: Type) where
  -- | >>> newtype Id = Id { getId :: Int16 } deriving Show
  -- >>> instance FromValue 'PGint2 Id where fromValue = Id <$> fromValue @'PGint2
  fromValue :: Decoding.Value y
instance FromValue 'PGbool Bool where fromValue = Decoding.bool
instance FromValue 'PGint2 Int16 where fromValue = Decoding.int
instance FromValue 'PGint4 Int32 where fromValue = Decoding.int
instance FromValue 'PGint8 Int64 where fromValue = Decoding.int
instance FromValue 'PGfloat4 Float where fromValue = Decoding.float4
instance FromValue 'PGfloat8 Double where fromValue = Decoding.float8
instance FromValue 'PGnumeric Scientific where fromValue = Decoding.numeric
instance FromValue 'PGuuid UUID where fromValue = Decoding.uuid
instance FromValue 'PGinet (NetAddr IP) where fromValue = Decoding.inet
instance FromValue ('PGchar 1) Char where fromValue = Decoding.char
instance FromValue 'PGtext Strict.Text where fromValue = Decoding.text_strict
instance FromValue 'PGtext Lazy.Text where fromValue = Decoding.text_lazy
instance FromValue 'PGbytea Strict.ByteString where
  fromValue = Decoding.bytea_strict
instance FromValue 'PGbytea Lazy.ByteString where
  fromValue = Decoding.bytea_lazy
instance FromValue 'PGdate Day where fromValue = Decoding.date
instance FromValue 'PGtime TimeOfDay where fromValue = Decoding.time_int
instance FromValue 'PGtimetz (TimeOfDay, TimeZone) where
  fromValue = Decoding.timetz_int
instance FromValue 'PGtimestamp LocalTime where
  fromValue = Decoding.timestamp_int
instance FromValue 'PGtimestamptz UTCTime where
  fromValue = Decoding.timestamptz_int
instance FromValue 'PGinterval DiffTime where
  fromValue = Decoding.interval_int
instance FromValue 'PGjson Value where fromValue = Decoding.json_ast
instance FromValue 'PGjsonb Value where fromValue = Decoding.jsonb_ast
instance FromValue pg y
  => FromValue ('PGvararray ('Null pg)) (Vector (Maybe y)) where
    fromValue = Decoding.array
      (Decoding.dimensionArray Vector.replicateM
        (Decoding.nullableValueArray (fromValue @pg)))
instance FromValue pg y
  => FromValue ('PGvararray ('NotNull pg)) (Vector y) where
    fromValue = Decoding.array
      (Decoding.dimensionArray Vector.replicateM
        (Decoding.valueArray (fromValue @pg)))
instance
  ( IsEnumType y
  , HasDatatypeInfo y
  , LabelsFrom y ~ labels
  ) => FromValue ('PGenum labels) y where
    fromValue =
      let
        greadConstructor
          :: All ((~) '[]) xss
          => NP ConstructorInfo xss
          -> String
          -> Maybe (SOP I xss)
        greadConstructor Nil _ = Nothing
        greadConstructor (constructor :* constructors) name =
          if name == constructorName constructor
            then Just (SOP (Z Nil))
            else SOP . S . unSOP <$> greadConstructor constructors name
      in
        Decoding.enum
        $ fmap to
        . greadConstructor (constructorInfo (datatypeInfo (Proxy @y)))
        . Strict.unpack

instance
  ( FromRow fields y
  ) => FromValue ('PGcomposite fields) y where
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
        composite = Decoding.valueParser $ do
          unitOfSize 4
          hsequence' $ hpure $ Comp $ do
            unitOfSize 4
            len <- sized 4 Decoding.int
            if len == -1
              then return (K Nothing)
              else K . Just <$> bytesOfSize len
      in
        Decoding.fn (fromRow @fields <=< composite)

-- | A `FromField` constraint lifts the `FromValue` parser
-- to a decoding of a @(Symbol, NullityType)@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromField`, just use the provided instances.
class FromField (pg :: (Symbol, NullityType)) (y :: (Symbol, Type)) where
  fromField
    :: K (Maybe Strict.ByteString) pg
    -> (Either Strict.Text :.: P) y
instance FromValue pg y
  => FromField (column ::: ('NotNull pg)) (column ::: y) where
    fromField = Comp . \case
      K Nothing -> Left "fromField: saw NULL when expecting NOT NULL"
      K (Just bytestring) -> P <$>
        Decoding.valueParser (fromValue @pg) bytestring
instance FromValue pg y
  => FromField (column ::: 'Null pg) (column ::: Maybe y) where
    fromField = Comp . \case
      K Nothing -> Right $ P Nothing
      K (Just bytestring) -> P . Just <$>
        Decoding.valueParser (fromValue @pg) bytestring

class FromArray (ty :: NullityType) (y :: Type) where
  fromArray :: Decoding.Array y
instance FromArray ('NotNull 'PGbool) Bool where
  fromArray = Decoding.valueArray (fromValue @'PGbool)
instance FromArray ('Null 'PGbool) (Maybe Bool) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGbool)
instance FromArray ('NotNull 'PGint2) Int16 where
  fromArray = Decoding.valueArray (fromValue @'PGint2)
instance FromArray ('Null 'PGint2) (Maybe Int16) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGint2)
instance FromArray ('NotNull 'PGint4) Int32 where
  fromArray = Decoding.valueArray (fromValue @'PGint4)
instance FromArray ('Null 'PGint4) (Maybe Int32) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGint4)
instance FromArray ('NotNull 'PGint8) Int64 where
  fromArray = Decoding.valueArray (fromValue @'PGint8)
instance FromArray ('Null 'PGint8) (Maybe Int64) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGint8)
instance FromArray ('NotNull 'PGfloat4) Float where
  fromArray = Decoding.valueArray (fromValue @'PGfloat4)
instance FromArray ('Null 'PGfloat4) (Maybe Float) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGfloat4)
instance FromArray ('NotNull 'PGfloat8) Double where
  fromArray = Decoding.valueArray (fromValue @'PGfloat8)
instance FromArray ('Null 'PGfloat8) (Maybe Double) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGfloat8)
instance FromArray ('NotNull 'PGnumeric) Scientific where
  fromArray = Decoding.valueArray (fromValue @'PGnumeric)
instance FromArray ('Null 'PGnumeric) (Maybe Scientific) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGnumeric)
instance FromArray ('NotNull 'PGuuid) UUID where
  fromArray = Decoding.valueArray (fromValue @'PGuuid)
instance FromArray ('Null 'PGuuid) (Maybe UUID) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGuuid)
instance FromArray ('NotNull 'PGinet) (NetAddr IP) where
  fromArray = Decoding.valueArray (fromValue @'PGinet)
instance FromArray ('Null 'PGinet) (Maybe (NetAddr IP)) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGinet)
instance FromArray ('NotNull ('PGchar 1)) Char where
  fromArray = Decoding.valueArray (fromValue @('PGchar 1))
instance FromArray ('Null ('PGchar 1)) (Maybe Char) where
  fromArray = Decoding.nullableValueArray (fromValue @('PGchar 1))
instance FromArray ('NotNull 'PGtext) Strict.Text where
  fromArray = Decoding.valueArray (fromValue @'PGtext)
instance FromArray ('Null 'PGtext) (Maybe Strict.Text) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGtext)
instance FromArray ('NotNull 'PGtext) Lazy.Text where
  fromArray = Decoding.valueArray (fromValue @'PGtext)
instance FromArray ('Null 'PGtext) (Maybe Lazy.Text) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGtext)
instance FromArray ('NotNull 'PGbytea) Strict.ByteString where
  fromArray = Decoding.valueArray (fromValue @'PGbytea)
instance FromArray ('Null 'PGbytea) (Maybe Strict.ByteString) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGbytea)
instance FromArray ('NotNull 'PGbytea) Lazy.ByteString where
  fromArray = Decoding.valueArray (fromValue @'PGbytea)
instance FromArray ('Null 'PGbytea) (Maybe Lazy.ByteString) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGbytea)
instance FromArray ('NotNull 'PGdate) Day where
  fromArray = Decoding.valueArray (fromValue @'PGdate)
instance FromArray ('Null 'PGdate) (Maybe Day) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGdate)
instance FromArray ('NotNull 'PGtime) TimeOfDay where
  fromArray = Decoding.valueArray (fromValue @'PGtime)
instance FromArray ('Null 'PGtime) (Maybe TimeOfDay) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGtime)
instance FromArray ('NotNull 'PGtimetz) (TimeOfDay, TimeZone) where
  fromArray = Decoding.valueArray (fromValue @'PGtimetz)
instance FromArray ('Null 'PGtimetz) (Maybe (TimeOfDay, TimeZone)) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGtimetz)
instance FromArray ('NotNull 'PGtimestamp) LocalTime where
  fromArray = Decoding.valueArray (fromValue @'PGtimestamp)
instance FromArray ('Null 'PGtimestamp) (Maybe LocalTime) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGtimestamp)
instance FromArray ('NotNull 'PGtimestamptz) UTCTime where
  fromArray = Decoding.valueArray (fromValue @'PGtimestamptz)
instance FromArray ('Null 'PGtimestamptz) (Maybe UTCTime) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGtimestamptz)
instance FromArray ('NotNull 'PGinterval) DiffTime where
  fromArray = Decoding.valueArray (fromValue @'PGinterval)
instance FromArray ('Null 'PGinterval) (Maybe DiffTime) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGinterval)
instance FromArray ('NotNull 'PGjson) Value where
  fromArray = Decoding.valueArray (fromValue @'PGjson)
instance FromArray ('Null 'PGjson) (Maybe Value) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGjson)
instance FromArray ('NotNull 'PGjsonb) Value where
  fromArray = Decoding.valueArray (fromValue @'PGjsonb)
instance FromArray ('Null 'PGjsonb) (Maybe Value) where
  fromArray = Decoding.nullableValueArray (fromValue @'PGjsonb)
instance FromArray array y
  => FromArray (nullity ('PGvararray array)) (Vector y) where
    fromArray =
      Decoding.dimensionArray Vector.replicateM (fromArray @array @y)
instance (FromArray array y, HomogeneousProduct n y product)
  => FromArray (nullity ('PGfixarray n array)) product where
    fromArray =
      Decoding.dimensionArray (pure replicateMN) (fromArray @array @y)

-- | A `FromRow` constraint generically sequences the parsings of the columns
-- of a `RowType` into the fields of a record `Type` provided they have
-- the same field names. You should not define instances of `FromRow`.
-- Instead define `Generic` and `HasDatatypeInfo` instances which in turn
-- provide `FromRow` instances.
class SListI result => FromRow (result :: RowType) y where
  -- | >>> :set -XOverloadedStrings
  -- >>> import Data.Text
  -- >>> newtype UserId = UserId { getUserId :: Int16 } deriving Show
  -- >>> instance FromValue 'PGint2 UserId where fromValue = UserId <$> fromValue @'PGint2
  -- >>> data UserRow = UserRow { userId :: UserId, userName :: Maybe Text } deriving (Show, GHC.Generic)
  -- >>> instance Generic UserRow
  -- >>> instance HasDatatypeInfo UserRow
  -- >>> type User = '["userId" ::: 'NotNull 'PGint2, "userName" ::: 'Null 'PGtext]
  -- >>> fromRow @User @UserRow (K (Just "\NUL\SOH") :* K (Just "bloodninja") :* Nil)
  -- Right (UserRow {userId = UserId {getUserId = 1}, userName = Just "bloodninja"})
  fromRow :: NP (K (Maybe Strict.ByteString)) result -> Either Strict.Text y
instance
  ( SListI result
  , IsRecord y ys
  , AllZip FromField result ys
  ) => FromRow result y where
    fromRow
      = fmap fromRecord
      . hsequence'
      . htrans (Proxy @FromField) fromField

-- | `Only` is a 1-tuple type, useful for encoding a single parameter with
-- `toParams` or decoding a single value with `fromRow`.
--
-- >>> import Data.Text
-- >>> toParams @(Only (Maybe Text)) @'[ 'Null 'PGtext] (Only (Just "foo"))
-- K (Just "foo") :* Nil
--
-- >>> fromRow @'["fromOnly" ::: 'Null 'PGtext] @(Only (Maybe Text)) (K (Just "bar") :* Nil)
-- Right (Only {fromOnly = Just "bar"})
newtype Only x = Only { fromOnly :: x }
  deriving (Functor,Foldable,Traversable,Eq,Ord,Read,Show,GHC.Generic)
instance Generic (Only x)
instance HasDatatypeInfo (Only x)

class HomogeneousProduct
  (n :: Nat) (x :: Type) (product :: Type)
  | product -> n where
    foldlN :: (b -> x -> b) -> b -> product -> b
    replicateMN :: Monad m => m x -> m product
instance
  ( IsProductType product xs
  , All ((~) x) xs
  , SListI xs
  , Length xs ~ n )
  => HomogeneousProduct n x product where

    foldlN f z =
      let
        foldlNP
          :: All ((~) x') xs'
          => (z -> x' -> z) -> z -> NP I xs' -> z
        foldlNP f' z' = \case
          Nil -> z'
          I x' :* xs' -> let z'' = f' z' x' in seq z'' $ foldlNP f' z'' xs'
      in
        foldlNP f z  . unZ . unSOP . from

    replicateMN mx =
      let
        replicateMNP :: Monad m => m x -> m (NP I xs)
        replicateMNP mx' = hsequence' $
          hcpure (Proxy :: Proxy ((~) x)) (Comp (I <$> mx'))
      in
        to . SOP . Z <$> replicateMNP mx

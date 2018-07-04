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
  query = values_ (param @1 `As` #col1 :* param @2 `As` #col2 :* Nil)
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
  querySchwarma = values_ (parameter @1 #schwarma `As` #fromOnly :* Nil)
:}

>>> :{
let
  queryPerson :: Query Schema
    '[ 'NotNull (CompositeFrom Person)]
    '["fromOnly" ::: 'NotNull (CompositeFrom Person)]
  queryPerson = values_ (parameter @1 #person `As` #fromOnly :* Nil)
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
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Binary
  ( -- * Encoding
    ToParam (..)
  , ToColumnParam (..)
  , ToParams (..)
    -- * Decoding
  , FromValue (..)
  , FromColumnValue (..)
  , FromRow (..)
    -- * Only
  , Only (..)
  ) where

import BinaryParser
import ByteString.StrictBuilder
import Control.Monad.State.Strict
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
import GHC.TypeLits
import Network.IP.Addr

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict hiding (pack, unpack)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Strict
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
instance (HasOid pg, ToParam x pg)
  => ToParam (Vector (Maybe x)) ('PGvararray pg) where
    toParam = K . Encoding.nullableArray_vector
      (oid @pg) (unK . toParam @x @pg)

data FromListN a = FromListN {-# UNPACK #-} !Int [a]

type family Length (xs :: [k]) :: Nat where
  Length (x : xs) = 1 + Length xs
  Length '[] = 0
instance
  ( HasOid pg
  , ToParam x pg
  , Generic c
  , list ~ (x:xs)
  , code ~ Code c
  , code ~ '[list]
  , len ~ Length list
  , KnownNat len
  , All ((~) x) list
  , SListI list
  , SListI code
  ) => ToParam c ('PGfixarray len pg) where
    toParam c = K
      (Encoding.array_vector
       (oid @pg)
       (unK . toParam @x @pg)
       (case execState list (FromListN 0 []) of
          FromListN len xs
            | len == expectLen -> Vector.fromListN len xs
            | otherwise -> error $
              "toParam @'PGfixarray: vector with incorrect length\n\
              \expected " ++ show expectLen ++ " but got " ++ show len))
      where
        expectLen :: Int
        expectLen = fromIntegral (natVal (Proxy :: Proxy len))

        list :: State (FromListN x) ()
        list = hctraverse_
          (Proxy :: Proxy ((~) list))
          (hctraverse_
           (Proxy :: Proxy ((~) x))
           (putElem . unI))
          (unSOP (from c))

        putElem :: x -> State (FromListN x) ()
        putElem x =
          modify' (\(FromListN n xs) -> FromListN (n+1) (x:xs))

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
  , MapMaybes xs
  , IsProductType x (Maybes xs)
  , AllZip ToAliasedParam xs fields
  , FieldNamesFrom x ~ AliasesOf fields
  , All HasAliasedOid fields
  ) => ToParam x ('PGcomposite fields) where
    toParam =
      let

        encoders = htrans (Proxy @ToAliasedParam) toAliasedParam

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
        composite . encoders . unMaybes . unZ . unSOP . from

class HasAliasedOid (field :: (Symbol, PGType)) where aliasedOid :: Word32
instance HasOid ty => HasAliasedOid (alias ::: ty) where aliasedOid = oid @ty

class ToAliasedParam (x :: Type) (field :: (Symbol, PGType)) where
  toAliasedParam :: Maybe x -> K (Maybe Encoding.Encoding) field
instance ToParam x ty => ToAliasedParam x (alias ::: ty) where
  toAliasedParam = \case
    Nothing -> K Nothing
    Just x -> K . Just . unK $ toParam @x @ty x

-- | A `ToColumnParam` constraint lifts the `ToParam` encoding
-- of a `Type` to a `NullityType`, encoding `Maybe`s to `Null`s. You should
-- not define instances of `ToColumnParam`, just use the provided instances.
class ToColumnParam (x :: Type) (ty :: NullityType) where
  -- | >>> toColumnParam @Int16 @('NotNull 'PGint2) 0
  -- K (Just "\NUL\NUL")
  --
  -- >>> toColumnParam @(Maybe Int16) @('Null 'PGint2) (Just 0)
  -- K (Just "\NUL\NUL")
  --
  -- >>> toColumnParam @(Maybe Int16) @('Null 'PGint2) Nothing
  -- K Nothing
  toColumnParam :: x -> K (Maybe Strict.ByteString) ty
instance ToParam x pg => ToColumnParam x ('NotNull pg) where
  toColumnParam = K . Just . Encoding.encodingBytes . unK . toParam @x @pg
instance ToParam x pg => ToColumnParam (Maybe x) ('Null pg) where
  toColumnParam = K . fmap (Encoding.encodingBytes . unK . toParam @x @pg)

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
  toParams :: x -> NP (K (Maybe Strict.ByteString)) tys
instance (SListI tys, IsProductType x xs, AllZip ToColumnParam xs tys)
  => ToParams x tys where
      toParams
        = htrans (Proxy @ToColumnParam) (toColumnParam . unI)
        . unZ . unSOP . from

-- | A `FromValue` constraint gives a parser from the binary format of
-- a PostgreSQL `PGType` into a Haskell `Type`.
class FromValue (pg :: PGType) (y :: Type) where
  -- | >>> newtype Id = Id { getId :: Int16 } deriving Show
  -- >>> instance FromValue 'PGint2 Id where fromValue = fmap Id . fromValue
  fromValue :: proxy pg -> Decoding.Value y
instance FromValue 'PGbool Bool where fromValue _ = Decoding.bool
instance FromValue 'PGint2 Int16 where fromValue _ = Decoding.int
instance FromValue 'PGint4 Int32 where fromValue _ = Decoding.int
instance FromValue 'PGint8 Int64 where fromValue _ = Decoding.int
instance FromValue 'PGfloat4 Float where fromValue _ = Decoding.float4
instance FromValue 'PGfloat8 Double where fromValue _ = Decoding.float8
instance FromValue 'PGnumeric Scientific where fromValue _ = Decoding.numeric
instance FromValue 'PGuuid UUID where fromValue _ = Decoding.uuid
instance FromValue 'PGinet (NetAddr IP) where fromValue _ = Decoding.inet
instance FromValue ('PGchar 1) Char where fromValue _ = Decoding.char
instance FromValue 'PGtext Strict.Text where fromValue _ = Decoding.text_strict
instance FromValue 'PGtext Lazy.Text where fromValue _ = Decoding.text_lazy
instance FromValue 'PGbytea Strict.ByteString where
  fromValue _ = Decoding.bytea_strict
instance FromValue 'PGbytea Lazy.ByteString where
  fromValue _ = Decoding.bytea_lazy
instance FromValue 'PGdate Day where fromValue _ = Decoding.date
instance FromValue 'PGtime TimeOfDay where fromValue _ = Decoding.time_int
instance FromValue 'PGtimetz (TimeOfDay, TimeZone) where
  fromValue _ = Decoding.timetz_int
instance FromValue 'PGtimestamp LocalTime where
  fromValue _ = Decoding.timestamp_int
instance FromValue 'PGtimestamptz UTCTime where
  fromValue _ = Decoding.timestamptz_int
instance FromValue 'PGinterval DiffTime where
  fromValue _ = Decoding.interval_int
instance FromValue 'PGjson Value where fromValue _ = Decoding.json_ast
instance FromValue 'PGjsonb Value where fromValue _ = Decoding.jsonb_ast
instance FromValue pg y => FromValue ('PGvararray pg) (Vector (Maybe y)) where
  fromValue _ = Decoding.array
    (Decoding.dimensionArray Vector.replicateM
      (Decoding.nullableValueArray (fromValue (Proxy @pg))))
instance FromValue pg y => FromValue ('PGfixarray n pg) (Vector (Maybe y)) where
  fromValue _ = Decoding.array
    (Decoding.dimensionArray Vector.replicateM
      (Decoding.nullableValueArray (fromValue (Proxy @pg))))
instance
  ( IsEnumType y
  , HasDatatypeInfo y
  , LabelsFrom y ~ labels
  ) => FromValue ('PGenum labels) y where
    fromValue _ =
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
  ( SListI fields
  , MapMaybes ys
  , IsProductType y (Maybes ys)
  , AllZip FromAliasedValue fields ys
  , FieldNamesFrom y ~ AliasesOf fields
  ) => FromValue ('PGcomposite fields) y where
    fromValue =
      let
        decoders
          :: forall pgs zs proxy
          . AllZip FromAliasedValue pgs zs
          => proxy ('PGcomposite pgs)
          -> NP Decoding.Value zs
        decoders _ = htrans (Proxy @FromAliasedValue) fromAliasedValue
          (hpure Proxy :: NP Proxy pgs)

        composite fields = do
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
          unitOfSize 4
          let
            each field = do
              unitOfSize 4
              len <- sized 4 Decoding.int
              if len == -1 then return Nothing else Just <$> sized len field
          htraverse' each fields

      in fmap (to . SOP . Z . maybes) . composite . decoders

class FromAliasedValue (pg :: (Symbol,PGType)) (y :: Type) where
  fromAliasedValue :: proxy pg -> Decoding.Value y
instance FromValue pg y => FromAliasedValue (alias ::: pg) y where
  fromAliasedValue _ = fromValue (Proxy @pg)

-- | A `FromColumnValue` constraint lifts the `FromValue` parser
-- to a decoding of a @(Symbol, NullityType)@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromColumnValue`, just use the provided instances.
class FromColumnValue (colty :: (Symbol,NullityType)) (y :: Type) where
  -- | >>> :set -XTypeOperators -XOverloadedStrings
  -- >>> newtype Id = Id { getId :: Int16 } deriving Show
  -- >>> instance FromValue 'PGint2 Id where fromValue = fmap Id . fromValue
  -- >>> fromColumnValue @("col" ::: 'NotNull 'PGint2) @Id (K (Just "\NUL\SOH"))
  -- Id {getId = 1}
  --
  -- >>> fromColumnValue @("col" ::: 'Null 'PGint2) @(Maybe Id) (K (Just "\NUL\SOH"))
  -- Just (Id {getId = 1})
  fromColumnValue :: K (Maybe Strict.ByteString) colty -> y
instance FromValue pg y
  => FromColumnValue (column ::: ('NotNull pg)) y where
    fromColumnValue = \case
      K Nothing -> error "fromColumnValue: saw NULL when expecting NOT NULL"
      K (Just bs) ->
        let
          errOrValue =
            Decoding.valueParser (fromValue @pg @y Proxy) bs
          err str = error $ "fromColumnValue: " ++ Strict.unpack str
        in
          either err id errOrValue
instance FromValue pg y
  => FromColumnValue (column ::: ('Null pg)) (Maybe y) where
    fromColumnValue (K nullOrBytes)
      = either err id
      . Decoding.valueParser (fromValue @pg @y Proxy)
      <$> nullOrBytes
      where
        err str = error $ "fromColumnValue: " ++ Strict.unpack str

-- | A `FromRow` constraint generically sequences the parsings of the columns
-- of a `RelationType` into the fields of a record `Type` provided they have
-- the same field names. You should not define instances of `FromRow`.
-- Instead define `Generic` and `HasDatatypeInfo` instances which in turn
-- provide `FromRow` instances.
class SListI results => FromRow (results :: RelationType) y where
  -- | >>> :set -XOverloadedStrings
  -- >>> import Data.Text
  -- >>> newtype UserId = UserId { getUserId :: Int16 } deriving Show
  -- >>> instance FromValue 'PGint2 UserId where fromValue = fmap UserId . fromValue
  -- >>> data UserRow = UserRow { userId :: UserId, userName :: Maybe Text } deriving (Show, GHC.Generic)
  -- >>> instance Generic UserRow
  -- >>> instance HasDatatypeInfo UserRow
  -- >>> type User = '["userId" ::: 'NotNull 'PGint2, "userName" ::: 'Null 'PGtext]
  -- >>> fromRow @User @UserRow (K (Just "\NUL\SOH") :* K (Just "bloodninja") :* Nil)
  -- UserRow {userId = UserId {getUserId = 1}, userName = Just "bloodninja"}
  fromRow :: NP (K (Maybe Strict.ByteString)) results -> y
instance
  ( SListI results
  , IsProductType y ys
  , AllZip FromColumnValue results ys
  , FieldNamesFrom y ~ AliasesOf results
  ) => FromRow results y where
    fromRow
      = to . SOP . Z . htrans (Proxy @FromColumnValue) (I . fromColumnValue)

-- | `Only` is a 1-tuple type, useful for encoding a single parameter with
-- `toParams` or decoding a single value with `fromRow`.
--
-- >>> import Data.Text
-- >>> toParams @(Only (Maybe Text)) @'[ 'Null 'PGtext] (Only (Just "foo"))
-- K (Just "foo") :* Nil
--
-- >>> fromRow @'["fromOnly" ::: 'Null 'PGtext] @(Only (Maybe Text)) (K (Just "bar") :* Nil)
-- Only {fromOnly = Just "bar"}
newtype Only x = Only { fromOnly :: x }
  deriving (Functor,Foldable,Traversable,Eq,Ord,Read,Show,GHC.Generic)
instance Generic (Only x)
instance HasDatatypeInfo (Only x)

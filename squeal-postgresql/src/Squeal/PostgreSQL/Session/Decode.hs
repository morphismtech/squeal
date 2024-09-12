{-|
Module: Squeal.PostgreSQL.Session.Decode
Description: decoding of result values
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

decoding of result values
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , CPP
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
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Session.Decode
  ( -- * Decode Types
    FromPG (..)
  , devalue
  , rowValue
  , enumValue
    -- * Decode Rows
  , DecodeRow (..)
  , decodeRow
  , runDecodeRow
  , GenericRow (..)
  , genericProductRow
  , appendRows
  , consRow
  , ArrayField (..)
    -- * Decoding Classes
  , FromValue (..)
  , FromField (..)
  , FromAliasedValue (..)
  , FromArray (..)
  , StateT (..)
  , ExceptT (..)
  ) where

import BinaryParser
import Control.Applicative
import Control.Arrow
import Control.Monad
#if MIN_VERSION_base(4,13,0)
#else
import Control.Monad.Fail
#endif
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Coerce (coerce)
import Data.Functor.Constant (Constant(Constant))
import Data.Int (Int16, Int32, Int64)
import Data.Kind
import Data.Scientific (Scientific)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, TimeZone, LocalTime, UTCTime, DiffTime)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.LibPQ (Oid(Oid))
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

import Squeal.PostgreSQL.Expression.Range
import Squeal.PostgreSQL.Type
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.PG
import Squeal.PostgreSQL.Type.Schema

-- | Converts a `Value` type from @postgresql-binary@ for use in
-- the `fromPG` method of `FromPG`.
devalue :: Value x -> StateT Strict.ByteString (Except Strict.Text) x
devalue = unsafeCoerce

revalue :: StateT Strict.ByteString (Except Strict.Text) x -> Value x
revalue = unsafeCoerce

{- |
>>> :set -XTypeFamilies
>>> :{
data Complex = Complex
  { real :: Double
  , imaginary :: Double
  }
instance IsPG Complex where
  type PG Complex = 'PGcomposite '[
    "re" ::: 'NotNull 'PGfloat8,
    "im" ::: 'NotNull 'PGfloat8]
instance FromPG Complex where
  fromPG = rowValue $ do
    re <- #re
    im <- #im
    return Complex {real = re, imaginary = im}
:}
-}
rowValue
  :: (PG y ~ 'PGcomposite row, SOP.SListI row)
  => DecodeRow row y -- ^ fields
  -> StateT Strict.ByteString (Except Strict.Text) y
rowValue decoder = devalue $
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
      deriving (IsPG, FromPG) via Composite Complex
  :}

  >>> :{
  data Direction = North | South | East | West
    deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, FromPG) via Enumerated Direction
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
instance KnownNat n => FromPG (VarChar n) where
  fromPG = devalue $ text_strict >>= \t ->
    case varChar t of
      Nothing -> throwError $ Strict.Text.pack $ concat
        [ "Source for VarChar has wrong length"
        , "; expected length "
        , show (natVal (SOP.Proxy @n))
        , ", actual length "
        , show (Strict.Text.length t)
        , "."
        ]
      Just x -> pure x
instance KnownNat n => FromPG (FixChar n) where
  fromPG = devalue $ text_strict >>= \t ->
    case fixChar t of
      Nothing -> throwError $ Strict.Text.pack $ concat
        [ "Source for FixChar has wrong length"
        , "; expected length "
        , show (natVal (SOP.Proxy @n))
        , ", actual length "
        , show (Strict.Text.length t)
        , "."
        ]
      Just x -> pure x
instance FromPG x => FromPG (Const x tag) where
  fromPG = coerce $ fromPG @x
instance FromPG x => FromPG (SOP.K x tag) where
  fromPG = coerce $ fromPG @x
instance FromPG x => FromPG (Constant x tag) where
  fromPG = coerce $ fromPG @x
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
instance (FromArray '[] ty y, ty ~ NullPG y)
  => FromPG (VarArray (Vector y)) where
    fromPG =
      let
        rep n x = VarArray <$> Vector.replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromArray @'[] @(NullPG y))
instance (FromArray '[] ty y, ty ~ NullPG y)
  => FromPG (VarArray [y]) where
    fromPG =
      let
        rep n x = VarArray <$> replicateM n x
      in
        devalue . array $ dimensionArray rep
          (fromArray @'[] @(NullPG y))
instance FromArray dims ty y => FromPG (FixArray y) where
  fromPG = devalue $ FixArray <$> array (fromArray @dims @ty @y)
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
    fromPG = rowValue (Composite <$> genericRow)
instance FromPG y => FromPG (Range y) where
  fromPG = devalue $ do
    flag <- byte
    if testBit flag 0 then return Empty else do
      lower <-
        if testBit flag 3
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (revalue fromPG)
            return $ if testBit flag 1 then Closed l else Open l
      upper <-
        if testBit flag 4
          then return Infinite
          else do
            len <- sized 4 int
            l <- sized len (revalue fromPG)
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
    Just bytestring -> valueParser (revalue fromPG) bytestring
instance (FromPG y, pg ~ PG y) => FromValue ('Null pg) (Maybe y) where
  fromValue = \case
    Nothing -> return Nothing
    Just bytestring -> fmap Just $ valueParser (revalue fromPG) bytestring

-- | A `FromField` constraint lifts the `FromPG` parser
-- to a decoding of a @(Symbol, NullityType)@ to a `Type`,
-- decoding `Null`s to `Maybe`s. You should not define instances for
-- `FromField`, just use the provided instances.
class FromField (field :: (Symbol, NullType)) (y :: (Symbol, Type)) where
  fromField :: Maybe Strict.ByteString -> Either Strict.Text (SOP.P y)
instance (FromValue ty y, fld0 ~ fld1)
  => FromField (fld0 ::: ty) (fld1 ::: y) where
    fromField = fmap SOP.P . fromValue @ty

-- | A `FromArray` constraint gives a decoding to a Haskell `Type`
-- from the binary format of a PostgreSQL fixed-length array.
-- You should not define instances for
-- `FromArray`, just use the provided instances.
class FromArray (dims :: [Nat]) (ty :: NullType) (y :: Type) where
  fromArray :: Array y
instance (FromPG y, pg ~ PG y) => FromArray '[] ('NotNull pg) y where
  fromArray = valueArray (revalue fromPG)
instance (FromPG y, pg ~ PG y) => FromArray '[] ('Null pg) (Maybe y) where
  fromArray = nullableValueArray (revalue fromPG)
instance
  ( SOP.IsProductType product ys
  , Length ys ~ dim
  , SOP.All ((~) y) ys
  , FromArray dims ty y )
  => FromArray (dim ': dims) ty product where
    fromArray =
      let
        rep _ = fmap (SOP.to . SOP.SOP . SOP.Z) . replicateMN
      in
        dimensionArray rep (fromArray @dims @ty @y)

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
instance MonadFail (DecodeRow row) where
  fail = throwError . fromString

-- | Run a `DecodeRow`.
runDecodeRow
  :: DecodeRow row y
  -> SOP.NP (SOP.K (Maybe Strict.ByteString)) row
  -> Either Strict.Text y
runDecodeRow = fmap runExcept . runReaderT . unDecodeRow

{- | Append two row decoders with a combining function.

>>> import GHC.Generics as GHC
>>> :{
data L = L {fst :: Int16, snd :: Char}
  deriving stock (GHC.Generic, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
data R = R {thrd :: Bool, frth :: Bool}
  deriving stock (GHC.Generic, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
type Row = '[
  "fst" ::: 'NotNull 'PGint2,
  "snd" ::: 'NotNull ('PGchar 1),
  "thrd" ::: 'NotNull 'PGbool,
  "frth" ::: 'NotNull 'PGbool]
:}

>>> :{
let
  decode :: DecodeRow Row (L,R)
  decode = appendRows (,) genericRow genericRow
  row4 =
    SOP.K (Just "\NUL\SOH") :*
    SOP.K (Just "a") :*
    SOP.K (Just "\NUL") :*
    SOP.K (Just "\NUL") :* Nil
in runDecodeRow decode row4
:}
Right (L {fst = 1, snd = 'a'},R {thrd = False, frth = False})
-}
appendRows
  :: SOP.SListI left
  => (l -> r -> z) -- ^ combining function
  -> DecodeRow left l -- ^ left decoder
  -> DecodeRow right r -- ^ right decoder
  -> DecodeRow (Join left right) z
appendRows f decL decR = decodeRow $ \row -> case disjoin row of
  (rowL, rowR) -> f <$> runDecodeRow decL rowL <*> runDecodeRow decR rowR

{- | Cons a column and a row decoder with a combining function.

>>> :{
let
  decode :: DecodeRow
    '["fst" ::: 'NotNull 'PGtext, "snd" ::: 'NotNull 'PGint2, "thrd" ::: 'NotNull ('PGchar 1)]
    (String, (Int16, Char))
  decode = consRow (,) #fst (consRow (,) #snd #thrd)
in runDecodeRow decode (SOP.K (Just "hi") :* SOP.K (Just "\NUL\SOH") :* SOP.K (Just "a") :* Nil)
:}
Right ("hi",(1,'a'))
-}
consRow
  :: FromValue head h
  => (h -> t -> z) -- ^ combining function
  -> Alias col -- ^ alias of head
  -> DecodeRow tail t -- ^ tail decoder
  -> DecodeRow (col ::: head ': tail) z
consRow f _ dec = decodeRow $ \case
  (SOP.K h :: SOP.K (Maybe Strict.ByteString) (col ::: head)) :* t
    -> f <$> fromValue @head h <*> runDecodeRow dec t

-- | Smart constructor for a `DecodeRow`.
decodeRow
  :: (SOP.NP (SOP.K (Maybe Strict.ByteString)) row -> Either Strict.Text y)
  -> DecodeRow row y
decodeRow dec = DecodeRow . ReaderT $ liftEither . dec
instance {-# OVERLAPPING #-} (KnownSymbol fld, FromValue ty y)
  => IsLabel fld (DecodeRow (fld ::: ty ': row) y) where
    fromLabel = decodeRow $ \(SOP.K b SOP.:* _) -> do
      let
        flderr = mconcat
          [ "field name: "
          , "\"", fromString (symbolVal (SOP.Proxy @fld)), "\"; "
          ]
      left (flderr <>) $ fromValue @ty b
instance {-# OVERLAPPABLE #-} IsLabel fld (DecodeRow row y)
  => IsLabel fld (DecodeRow (field ': row) y) where
    fromLabel = decodeRow $ \(_ SOP.:* bs) ->
      runDecodeRow (fromLabel @fld) bs
instance {-# OVERLAPPING #-} (KnownSymbol fld, FromValue ty (Maybe y))
  => IsLabel fld (MaybeT (DecodeRow (fld ::: ty ': row)) y) where
    fromLabel = MaybeT . decodeRow $ \(SOP.K b SOP.:* _) -> do
      let
        flderr = mconcat
          [ "field name: "
          , "\"", fromString (symbolVal (SOP.Proxy @fld)), "\"; "
          ]
      left (flderr <>) $ fromValue @ty b
instance {-# OVERLAPPABLE #-} IsLabel fld (MaybeT (DecodeRow row) y)
  => IsLabel fld (MaybeT (DecodeRow (field ': row)) y) where
    fromLabel = MaybeT . decodeRow $ \(_ SOP.:* bs) ->
      runDecodeRow (runMaybeT (fromLabel @fld)) bs

{- | Utility for decoding array fields in a `DecodeRow`,
accessed via overloaded labels.
-}
newtype ArrayField row y = ArrayField
  { runArrayField
      :: StateT Strict.ByteString (Except Strict.Text) y
      -> DecodeRow row [y]
  }
instance {-# OVERLAPPING #-}
  ( KnownSymbol fld
  , PG y ~ ty
  , arr ~ 'NotNull ('PGvararray ('NotNull ty))
  ) => IsLabel fld (ArrayField (fld ::: arr ': row) y) where
    fromLabel = ArrayField $ \yval ->
      decodeRow $ \(SOP.K bytesMaybe SOP.:* _) -> do
        let
          flderr = mconcat
            [ "field name: "
            , "\"", fromString (symbolVal (SOP.Proxy @fld)), "\"; "
            ]
          yarr
            = devalue
            . array
            . dimensionArray replicateM
            . valueArray
            . revalue
            $ yval
        case bytesMaybe of
          Nothing -> Left (flderr <> "encountered unexpected NULL")
          Just bytes -> runExcept (evalStateT yarr bytes)
instance {-# OVERLAPPABLE #-} IsLabel fld (ArrayField row y)
  => IsLabel fld (ArrayField (field ': row) y) where
    fromLabel = ArrayField $ \yval ->
      decodeRow $ \(_ SOP.:* bytess) ->
        runDecodeRow (runArrayField (fromLabel @fld) yval) bytess

-- | A `GenericRow` constraint to ensure that a Haskell type
-- is a record type,
-- has a `RowPG`,
-- and all its fields and can be decoded from corresponding Postgres fields.
class
  ( SOP.IsRecord y ys
  , row ~ RowPG y
  , SOP.AllZip FromField row ys
  ) => GenericRow row y ys where
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
  genericRow :: DecodeRow row y
instance
  ( row ~ RowPG y
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => GenericRow row y ys where
  genericRow
    = DecodeRow
    . ReaderT
    $ fmap SOP.fromRecord
    . SOP.hsequence'
    . SOP.htrans (SOP.Proxy @FromField) runField
    where
      runField
        :: forall ty z. FromField ty z
        => SOP.K (Maybe Strict.ByteString) ty
        -> (Except Strict.Text SOP.:.: SOP.P) z
      runField
        = SOP.Comp
        . liftEither
        . fromField @ty
        . SOP.unK

{- | Assistant class for `genericProductRow`,
this class forgets the name of a field while decoding it.
-}
class FromAliasedValue (field :: (Symbol, NullType)) (y :: Type) where
  fromAliasedValue :: Maybe Strict.ByteString -> Either Strict.Text y
instance FromValue ty y => FromAliasedValue (fld ::: ty) y where
  fromAliasedValue = fromValue @ty

{- | Positionally `DecodeRow`. More general than `genericRow`,
which matches records both positionally and by field name,
`genericProductRow` matches records _or_ tuples purely positionally.

>>> import qualified GHC.Generics as GHC
>>> import qualified Generics.SOP as SOP
>>> :{
let
  decode :: DecodeRow '[ "foo" ::: 'NotNull 'PGint2, "bar" ::: 'NotNull 'PGtext] (Int16, String)
  decode = genericProductRow
in runDecodeRow decode (SOP.K (Just "\NUL\STX") :* SOP.K (Just "two") :* Nil)
:}
Right (2,"two")
-}
genericProductRow
  :: ( SOP.IsProductType y ys
     , SOP.AllZip FromAliasedValue row ys
     )
  => DecodeRow row y
genericProductRow
  = DecodeRow
  . ReaderT
  $ fmap SOP.productTypeTo
  . SOP.hsequence'
  . SOP.htrans (SOP.Proxy @FromAliasedValue) runField
  where
    runField
      :: forall ty z. FromAliasedValue ty z
      => SOP.K (Maybe Strict.ByteString) ty
      -> (Except Strict.Text SOP.:.: SOP.I) z
    runField
      = SOP.Comp
      . fmap SOP.I
      . liftEither
      . fromAliasedValue @ty
      . SOP.unK

{- |
>>> :{
data Dir = North | East | South | West
instance IsPG Dir where
  type PG Dir = 'PGenum '["north", "south", "east", "west"]
instance FromPG Dir where
  fromPG = enumValue $
    label @"north" North :*
    label @"south" South :*
    label @"east" East :*
    label @"west" West
:}
-}
enumValue
  :: (SOP.All KnownSymbol labels, PG y ~ 'PGenum labels)
  => NP (SOP.K y) labels -- ^ labels
  -> StateT Strict.ByteString (Except Strict.Text) y
enumValue = devalue . enum . labels
  where
  labels
    :: SOP.All KnownSymbol labels
    => NP (SOP.K y) labels
    -> Text -> Maybe y
  labels = \case
    Nil -> \_ -> Nothing
    ((y :: SOP.K y label) :* ys) -> \ str ->
      if str == fromString (symbolVal (SOP.Proxy @label))
      then Just (SOP.unK y)
      else labels ys str

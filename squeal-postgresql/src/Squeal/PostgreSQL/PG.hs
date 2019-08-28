{-|
Module: Squeal.PostgreSQL.PG
Description: Embedding of Haskell types into Postgres's type system
Copyright: (c) Eitan Chatav, 2010
Maintainer: eitan@morphism.tech
Stability: experimental

`Squeal.PostgreSQL.PG` provides type families for turning Haskell
`Type`s into corresponding Postgres types.
-}
{-# LANGUAGE
    AllowAmbiguousTypes
  , DeriveAnyClass
  , DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , DerivingStrategies
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.PG
  ( -- * PG embeddings
    PG
  , NullPG
  , TuplePG
  , RowPG
    -- * Storage newtypes
  , Money (..)
  , Json (..)
  , Jsonb (..)
  , Composite (..)
  , Enumerated (..)
  , VarArray (..)
  , FixArray (..)
  , Oid (..)
    -- * Type families
  , LabelsPG
  , DimPG
  , FixPG
  , TupleOf
  , TupleCodeOf
  , RowOf
  , ConstructorsOf
  , ConstructorNameOf
  , ConstructorNamesOf
  ) where

import Data.Aeson (Value)
import Data.Kind (Type)
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.Vector (Vector)
import Data.Word (Word32)
import Data.UUID.Types (UUID)
import GHC.TypeLits
import Network.IP.Addr (NetAddr, IP)

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text as Strict (Text)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP
import qualified Generics.SOP.Type.Metadata as Type

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import Data.Text (Text)

{- | The `PG` type family embeds a subset of Haskell types
as Postgres types. As an open type family, `PG` is extensible.

>>> :kind! PG LocalTime
PG LocalTime :: PGType
= 'PGtimestamp

>>> newtype MyDouble = My Double
>>> :set -XTypeFamilies
>>> type instance PG MyDouble = 'PGfloat8
-}
type family PG (hask :: Type) :: PGType
-- | `PGbool`
type instance PG Bool = 'PGbool
-- | `PGint2`
type instance PG Int16 = 'PGint2
-- | `PGint4`
type instance PG Int32 = 'PGint4
-- | `PGint8`
type instance PG Int64 = 'PGint8
-- | `PGint2`
type instance PG Oid = 'PGoid
-- | `PGnumeric`
type instance PG Scientific = 'PGnumeric
-- | `PGfloat4`
type instance PG Float = 'PGfloat4
-- | `PGfloat8`
type instance PG Double = 'PGfloat8
-- | `PGchar` @1@
type instance PG Char = 'PGchar 1
-- | `PGtext`
type instance PG Strict.Text = 'PGtext
-- | `PGtext`
type instance PG Lazy.Text = 'PGtext
-- | `PGtext`
type instance PG String = 'PGtext
-- | `PGbytea`
type instance PG Strict.ByteString = 'PGbytea
-- | `PGbytea`
type instance PG Lazy.ByteString = 'PGbytea
-- | `PGtimestamp`
type instance PG LocalTime = 'PGtimestamp
-- | `PGtimestamptz`
type instance PG UTCTime = 'PGtimestamptz
-- | `PGdate`
type instance PG Day = 'PGdate
-- | `PGtime`
type instance PG TimeOfDay = 'PGtime
-- | `PGtimetz`
type instance PG (TimeOfDay, TimeZone) = 'PGtimetz
-- | `PGinterval`
type instance PG DiffTime = 'PGinterval
-- | `PGuuid`
type instance PG UUID = 'PGuuid
-- | `PGinet`
type instance PG (NetAddr IP) = 'PGinet
-- | `PGjson`
type instance PG Value = 'PGjson

{-| The `LabelsPG` type family calculates the constructors of a
Haskell enum type.

>>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
>>> instance SOP.Generic Schwarma
>>> instance SOP.HasDatatypeInfo Schwarma
>>> :kind! LabelsPG Schwarma
LabelsPG Schwarma :: [Type.ConstructorName]
= '["Beef", "Lamb", "Chicken"]
-}
type family LabelsPG (hask :: Type) :: [Type.ConstructorName] where
  LabelsPG hask =
    ConstructorNamesOf (ConstructorsOf (SOP.DatatypeInfoOf hask))

{- |
`RowPG` turns a Haskell `Type` into a `RowType`.

`RowPG` may be applied to normal Haskell record types provided they
have `SOP.Generic` and `SOP.HasDatatypeInfo` instances;

>>> data Person = Person { name :: Strict.Text, age :: Int32 } deriving GHC.Generic
>>> instance SOP.Generic Person
>>> instance SOP.HasDatatypeInfo Person
>>> :kind! RowPG Person
RowPG Person :: [(Symbol, NullityType)]
= '["name" ::: 'NotNull 'PGtext, "age" ::: 'NotNull 'PGint4]
-}
type family RowPG (hask :: Type) :: RowType where
  RowPG hask = RowOf (SOP.RecordCodeOf hask)

-- | `RowOf` applies `NullPG` to the fields of a list.
type family RowOf (record :: [(Symbol, Type)]) :: RowType where
  RowOf '[] = '[]
  RowOf (col ::: ty ': record) = col ::: NullPG ty ': RowOf record

{- | `NullPG` turns a Haskell type into a `NullityType`.

>>> :kind! NullPG Double
NullPG Double :: NullityType
= 'NotNull 'PGfloat8
>>> :kind! NullPG (Maybe Double)
NullPG (Maybe Double) :: NullityType
= 'Null 'PGfloat8
-}
type family NullPG (hask :: Type) :: NullityType where
  NullPG (Maybe hask) = 'Null (PG hask)
  NullPG hask = 'NotNull (PG hask)

{- | `TuplePG` turns a Haskell tuple type (including record types) into
the corresponding list of `NullityType`s.

>>> :kind! TuplePG (Double, Maybe Char)
TuplePG (Double, Maybe Char) :: [NullityType]
= '[ 'NotNull 'PGfloat8, 'Null ('PGchar 1)]
-}
type family TuplePG (hask :: Type) :: [NullityType] where
  TuplePG hask = TupleOf (TupleCodeOf hask (SOP.Code hask))

-- | `TupleOf` turns a list of Haskell `Type`s into a list of `NullityType`s.
type family TupleOf (tuple :: [Type]) :: [NullityType] where
  TupleOf '[] = '[]
  TupleOf (hask ': tuple) = NullPG hask ': TupleOf tuple

-- | `TupleCodeOf` takes the `SOP.Code` of a haskell `Type`
-- and if it's a simple product returns it, otherwise giving a `TypeError`.
type family TupleCodeOf (hask :: Type) (code :: [[Type]]) :: [Type] where
  TupleCodeOf hask '[tuple] = tuple
  TupleCodeOf hask '[] =
    TypeError
      (    'Text "The type `" ':<>: 'ShowType hask ':<>: 'Text "' is not a tuple type."
      ':$$: 'Text "It is a void type with no constructors."
      )
  TupleCodeOf hask (_ ': _ ': _) =
    TypeError
      (    'Text "The type `" ':<>: 'ShowType hask ':<>: 'Text "' is not a tuple type."
      ':$$: 'Text "It is a sum type with more than one constructor."
      )

-- | Calculates constructors of a datatype.
type family ConstructorsOf (datatype :: Type.DatatypeInfo)
  :: [Type.ConstructorInfo] where
    ConstructorsOf ('Type.ADT _module _datatype constructors) =
      constructors
    ConstructorsOf ('Type.Newtype _module _datatype constructor) =
      '[constructor]

-- | Calculates the name of a nullary constructor, otherwise
-- generates a type error.
type family ConstructorNameOf (constructor :: Type.ConstructorInfo)
  :: Type.ConstructorName where
    ConstructorNameOf ('Type.Constructor name) = name
    ConstructorNameOf ('Type.Infix name _assoc _fix) = TypeError
      ('Text "ConstructorNameOf error: non-nullary constructor "
        ':<>: 'Text name)
    ConstructorNameOf ('Type.Record name _fields) = TypeError
      ('Text "ConstructorNameOf error: non-nullary constructor "
        ':<>: 'Text name)

-- | Calculate the names of nullary constructors.
type family ConstructorNamesOf (constructors :: [Type.ConstructorInfo])
  :: [Type.ConstructorName] where
    ConstructorNamesOf '[] = '[]
    ConstructorNamesOf (constructor ': constructors) =
      ConstructorNameOf constructor ': ConstructorNamesOf constructors

-- | `DimPG` turns Haskell nested homogeneous tuples into a list of lengths.
type family DimPG (hask :: Type) :: [Nat] where
  DimPG (x,x) = 2 ': DimPG x
  DimPG (x,x,x) = 3 ': DimPG x
  DimPG (x,x,x,x) = 4 ': DimPG x
  DimPG (x,x,x,x,x) = 5 ': DimPG x
  DimPG (x,x,x,x,x,x) = 6 ': DimPG x
  DimPG (x,x,x,x,x,x,x) = 7 ': DimPG x
  DimPG (x,x,x,x,x,x,x,x) = 8 ': DimPG x
  DimPG (x,x,x,x,x,x,x,x,x) = 9 ': DimPG x
  DimPG (x,x,x,x,x,x,x,x,x,x) = 10 ': DimPG x
  DimPG x = '[]

-- | `FixPG` extracts `NullPG` of the base type of nested homogeneous tuples.
type family FixPG (hask :: Type) :: NullityType where
  FixPG (x,x) = FixPG x
  FixPG (x,x,x) = FixPG x
  FixPG (x,x,x,x) = FixPG x
  FixPG (x,x,x,x,x) = FixPG x
  FixPG (x,x,x,x,x,x) = FixPG x
  FixPG (x,x,x,x,x,x,x) = FixPG x
  FixPG (x,x,x,x,x,x,x,x) = FixPG x
  FixPG (x,x,x,x,x,x,x,x,x) = FixPG x
  FixPG (x,x,x,x,x,x,x,x,x,x) = FixPG x
  FixPG (x,x,x,x,x,x,x,x,x,x,x) = FixPG x
  FixPG x = NullPG x

{- | The `Money` newtype stores a monetary value in terms
of the number of cents, i.e. @$2,000.20@ would be expressed as
@Money { cents = 200020 }@.

>>> import Control.Monad (void)
>>> import Control.Monad.IO.Class (liftIO)
>>> import Squeal.PostgreSQL
>>> :{
let
  roundTrip :: Query_ (Public '[]) (Only Money) (Only Money)
  roundTrip = values_ $ parameter @1 money `as` #fromOnly
:}

>>> let input = Only (Money 20020)

>>> :{
withConnection "host=localhost port=5432 dbname=exampledb" $ do
  result <- runQueryParams roundTrip input
  Just output <- firstRow result
  liftIO . print $ input == output
:}
True
-}
newtype Money = Money { cents :: Int64 }
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)
-- | `PGmoney`
type instance PG Money = 'PGmoney

{- | The `Json` newtype is an indication that the Haskell
type it's applied to should be stored as a `PGjson`.
-}
newtype Json hask = Json {getJson :: hask}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)
-- | `PGjson`
type instance PG (Json hask) = 'PGjson

{- | The `Jsonb` newtype is an indication that the Haskell
type it's applied to should be stored as a `PGjsonb`.
-}
newtype Jsonb hask = Jsonb {getJsonb :: hask}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)
-- | `PGjsonb`
type instance PG (Jsonb hask) = 'PGjsonb

{- | The `Composite` newtype is an indication that the Haskell
type it's applied to should be stored as a `PGcomposite`.
-}
newtype Composite record = Composite {getComposite :: record}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)
-- | `PGcomposite` @(@`RowPG` @hask)@
type instance PG (Composite hask) = 'PGcomposite (RowPG hask)

{- | The `Enumerated` newtype is an indication that the Haskell
type it's applied to should be stored as a `PGenum`.
-}
newtype Enumerated enum = Enumerated {getEnumerated :: enum}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)
-- | `PGenum` @(@`LabelsPG` @hask)@
type instance PG (Enumerated hask) = 'PGenum (LabelsPG hask)

{- | The `VarArray` newtype is an indication that the Haskell
type it's applied to should be stored as a `PGvararray`.

>>> :kind! PG (VarArray (Vector Double))
PG (VarArray (Vector Double)) :: PGType
= 'PGvararray ('NotNull 'PGfloat8)
-}
newtype VarArray arr = VarArray {getVarArray :: arr}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)
-- | `PGvararray` @(@`NullPG` @hask)@
type instance PG (VarArray (Vector hask)) = 'PGvararray (NullPG hask)
type instance PG (VarArray [hask]) = 'PGvararray (NullPG hask)

{- | The `FixArray` newtype is an indication that the Haskell
type it's applied to should be stored as a `PGfixarray`.

>>> :kind! PG (FixArray ((Double, Double), (Double, Double)))
PG (FixArray ((Double, Double), (Double, Double))) :: PGType
= 'PGfixarray '[2, 2] ('NotNull 'PGfloat8)
-}
newtype FixArray arr = FixArray {getFixArray :: arr}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)
-- | `PGfixarray` @(@`DimPG` @hask) (@`FixPG` @hask)@
type instance PG (FixArray hask) = 'PGfixarray (DimPG hask) (FixPG hask)

{- | Object identifiers (`Oid`s) are used internally by PostgreSQL
as primary keys for various system tables.
-}
newtype Oid = Oid { getOid :: Word32 }
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)

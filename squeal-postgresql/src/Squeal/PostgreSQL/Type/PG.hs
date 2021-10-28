{-|
Module: Squeal.PostgreSQL.Type.PG
Description: embedding of Haskell types into Postgres type system
Copyright: (c) Eitan Chatav, 2010
Maintainer: eitan@morphism.tech
Stability: experimental

Provides type families for turning Haskell `Type`s
into corresponding Postgres types.
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

module Squeal.PostgreSQL.Type.PG
  ( -- * PG
    IsPG (..)
  , NullPG
  , ColumnPG
  , TuplePG
  , RowPG
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
import Data.Functor.Const (Const)
import Data.Functor.Constant (Constant)
import Data.Kind (Type)
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.Vector (Vector)
import Data.UUID.Types (UUID)
import GHC.TypeLits
import Network.IP.Addr (NetAddr, IP)

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text as Strict (Text)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP
import qualified Generics.SOP.Type.Metadata as Type

import Squeal.PostgreSQL.Expression.Default
import Squeal.PostgreSQL.Type
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import Data.Text (Text)
-- >>> import qualified GHC.Generics as GHC

{- | The `PG` type family embeds a subset of Haskell types
as Postgres types. As an open type family, `PG` is extensible.

>>> :kind! PG LocalTime
PG LocalTime :: PGType
= 'PGtimestamp

The preferred way to generate `PG`s of your own type is through
generalized newtype deriving or via deriving.

>>> newtype UserId = UserId {getUserId :: UUID} deriving newtype IsPG

>>> :kind! PG UserId
PG UserId :: PGType
= 'PGuuid

>>> :{
data Answer = Yes | No
  deriving stock GHC.Generic
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving IsPG via Enumerated Answer
:}

>>> :kind! PG Answer
PG Answer :: PGType
= 'PGenum '["Yes", "No"]

>>> :{
data Complex = Complex {real :: Double, imaginary :: Double}
  deriving stock GHC.Generic
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving IsPG via Composite Complex
:}

>>> :kind! PG Complex
PG Complex :: PGType
= 'PGcomposite
    '["real" ::: 'NotNull 'PGfloat8,
      "imaginary" ::: 'NotNull 'PGfloat8]
-}
class IsPG (hask :: Type) where type PG hask :: PGType
-- | `PGbool`
instance IsPG Bool where type PG Bool = 'PGbool
-- | `PGint2`
instance IsPG Int16 where type PG Int16 = 'PGint2
-- | `PGint4`
instance IsPG Int32 where type PG Int32 = 'PGint4
-- | `PGint8`
instance IsPG Int64 where type PG Int64 = 'PGint8
-- | `PGint2`
instance IsPG LibPQ.Oid where type PG LibPQ.Oid = 'PGoid
-- | `PGnumeric`
instance IsPG Scientific where type PG Scientific = 'PGnumeric
-- | `PGfloat4`
instance IsPG Float where type PG Float = 'PGfloat4
-- | `PGfloat8`
instance IsPG Double where type PG Double = 'PGfloat8
-- | `PGchar` @1@
instance IsPG Char where type PG Char = 'PGchar 1
-- | `PGtext`
instance IsPG Strict.Text where type PG Strict.Text = 'PGtext
-- | `PGtext`
instance IsPG Lazy.Text where type PG Lazy.Text = 'PGtext
-- | `PGtext`
instance IsPG String where type PG String = 'PGtext
-- | `PGbytea`
instance IsPG Strict.ByteString where type PG Strict.ByteString = 'PGbytea
-- | `PGbytea`
instance IsPG Lazy.ByteString where type PG Lazy.ByteString = 'PGbytea
-- | `PGtimestamp`
instance IsPG LocalTime where type PG LocalTime = 'PGtimestamp
-- | `PGtimestamptz`
instance IsPG UTCTime where type PG UTCTime = 'PGtimestamptz
-- | `PGdate`
instance IsPG Day where type PG Day = 'PGdate
-- | `PGtime`
instance IsPG TimeOfDay where type PG TimeOfDay = 'PGtime
-- | `PGtimetz`
instance IsPG (TimeOfDay, TimeZone) where type PG (TimeOfDay, TimeZone) = 'PGtimetz
-- | `PGinterval`
instance IsPG DiffTime where type PG DiffTime = 'PGinterval
-- | `PGuuid`
instance IsPG UUID where type PG UUID = 'PGuuid
-- | `PGinet`
instance IsPG (NetAddr IP) where type PG (NetAddr IP) = 'PGinet
-- | `PGjson`
instance IsPG Value where type PG Value = 'PGjson
-- | `PGvarchar`
instance IsPG (VarChar n) where type PG (VarChar n) = 'PGvarchar n
-- | `PGvarchar`
instance IsPG (FixChar n) where type PG (FixChar n) = 'PGchar n
-- | `PG hask`
instance IsPG hask => IsPG (Const hask tag) where type PG (Const hask tag) = PG hask
-- | `PG hask`
instance IsPG hask => IsPG (SOP.K hask tag) where type PG (SOP.K hask tag) = PG hask
-- | `PG hask`
instance IsPG hask => IsPG (Constant hask tag) where type PG (Constant hask tag) = PG hask

-- | `PGmoney`
instance IsPG Money where type PG Money = 'PGmoney
-- | `PGjson`
instance IsPG (Json hask) where type PG (Json hask) = 'PGjson
-- | `PGjsonb`
instance IsPG (Jsonb hask) where type PG (Jsonb hask) = 'PGjsonb
-- | `PGcomposite` @(@`RowPG` @hask)@
instance IsPG (Composite hask) where
  type PG (Composite hask) = 'PGcomposite (RowPG hask)
-- | `PGenum` @(@`LabelsPG` @hask)@
instance IsPG (Enumerated hask) where
  type PG (Enumerated hask) = 'PGenum (LabelsPG hask)
-- | `PGvararray` @(@`NullPG` @x)@
instance IsPG (VarArray (Vector x)) where
  type PG (VarArray (Vector x)) = 'PGvararray (NullPG x)
-- | `PGvararray` @(@`NullPG` @x)@
instance IsPG (VarArray [x]) where
  type PG (VarArray [x]) = 'PGvararray (NullPG x)
-- | `PGfixarray` @(@`DimPG` @hask) (@`FixPG` @hask)@
instance IsPG (FixArray hask) where
  type PG (FixArray hask) = 'PGfixarray (DimPG hask) (FixPG hask)

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
RowPG Person :: [(Symbol, NullType)]
= '["name" ::: 'NotNull 'PGtext, "age" ::: 'NotNull 'PGint4]
-}
type family RowPG (hask :: Type) :: RowType where
  RowPG hask = RowOf (SOP.RecordCodeOf hask)

-- | `RowOf` applies `NullPG` to the fields of a list.
type family RowOf (record :: [(Symbol, Type)]) :: RowType where
  RowOf (col ::: ty ': record) = col ::: NullPG ty ': RowOf record
  RowOf '[] = '[]

{- | `NullPG` turns a Haskell type into a `NullType`.

>>> :kind! NullPG Double
NullPG Double :: NullType
= 'NotNull 'PGfloat8
>>> :kind! NullPG (Maybe Double)
NullPG (Maybe Double) :: NullType
= 'Null 'PGfloat8
-}
type family NullPG (hask :: Type) :: NullType where
  NullPG (Maybe hask) = 'Null (PG hask)
  NullPG hask = 'NotNull (PG hask)

{- | `ColumnPG` turns a Haskell type into a `ColumnType`.

>>> :kind! ColumnPG Double
ColumnPG Double :: (Optionality, NullType)
= 'NoDef :=> 'NotNull 'PGfloat8
>>> :kind! ColumnPG (Optional SOP.I ('Def :=> Double))
ColumnPG (Optional SOP.I ('Def :=> Double)) :: (Optionality,
                                                NullType)
= 'Def :=> 'NotNull 'PGfloat8
-}
type family ColumnPG (hask :: Type) :: ColumnType where
  ColumnPG (Optional SOP.I (def :=> hask)) = def :=> NullPG hask
  ColumnPG hask = 'NoDef :=> NullPG hask

{- | `TuplePG` turns a Haskell tuple type (including record types) into
the corresponding list of `NullType`s.

>>> :kind! TuplePG (Double, Maybe Char)
TuplePG (Double, Maybe Char) :: [NullType]
= '[ 'NotNull 'PGfloat8, 'Null ('PGchar 1)]
-}
type family TuplePG (hask :: Type) :: [NullType] where
  TuplePG hask = TupleOf (TupleCodeOf hask (SOP.Code hask))

-- | `TupleOf` turns a list of Haskell `Type`s into a list of `NullType`s.
type family TupleOf (tuple :: [Type]) :: [NullType] where
  TupleOf (hask ': tuple) = NullPG hask ': TupleOf tuple
  TupleOf '[] = '[]

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
    ConstructorsOf ('Type.ADT _module _datatype constructors _strictness) =
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

-- | `DimPG` turns Haskell nested homogeneous tuples into a list of lengths,
-- up to a depth of 10 for each dimension.
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

-- | `FixPG` extracts `NullPG` of the base type of nested homogeneous tuples,
-- up to a depth of 10 for each dimension.
type family FixPG (hask :: Type) :: NullType where
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

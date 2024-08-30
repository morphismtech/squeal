{-|
Module: Squeal.PostgreSQL.Type
Description: types
Copyright: (c) Eitan Chatav, 2010
Maintainer: eitan@morphism.tech
Stability: experimental

storage newtypes
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
  , DataKinds
  , PolyKinds
  , TypeOperators
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Type
  ( -- * Storage newtypes
    Money (..)
  , Json (..)
  , Jsonb (..)
  , Composite (..)
  , Enumerated (..)
  , VarArray (..)
  , FixArray (..)
  , VarChar, varChar, getVarChar
  , FixChar, fixChar, getFixChar
  , Only (..)
  ) where

import Data.Proxy
import Data.Int (Int64)
import GHC.TypeLits

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

-- $setup
-- >>> import Squeal.PostgreSQL

{- | The `Money` newtype stores a monetary value in terms
of the number of cents, i.e. @$2,000.20@ would be expressed as
@Money { cents = 200020 }@.

>>> :kind! PG Money
PG Money :: PGType
= 'PGmoney
-}
newtype Money = Money { cents :: Int64 }
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)

{- | The `Json` newtype is an indication that the Haskell
type it's applied to should be stored as a
`Squeal.PostgreSQL.Type.Schema.PGjson`.

>>> :kind! PG (Json [String])
PG (Json [String]) :: PGType
= 'PGjson
-}
newtype Json hask = Json {getJson :: hask}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)

{- | The `Jsonb` newtype is an indication that the Haskell
type it's applied to should be stored as a
`Squeal.PostgreSQL.Type.Schema.PGjsonb`.

>>> :kind! PG (Jsonb [String])
PG (Jsonb [String]) :: PGType
= 'PGjsonb
-}
newtype Jsonb hask = Jsonb {getJsonb :: hask}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)

{- | The `Composite` newtype is an indication that the Haskell
type it's applied to should be stored as a
`Squeal.PostgreSQL.Type.Schema.PGcomposite`.

>>> :{
data Complex = Complex
  { real :: Double
  , imaginary :: Double
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
:}

>>> :kind! PG (Composite Complex)
PG (Composite Complex) :: PGType
= 'PGcomposite
    '["real" ::: 'NotNull 'PGfloat8,
      "imaginary" ::: 'NotNull 'PGfloat8]
-}
newtype Composite record = Composite {getComposite :: record}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)

{- | The `Enumerated` newtype is an indication that the Haskell
type it's applied to should be stored as a
`Squeal.PostgreSQL.Type.Schema.PGenum`.

>>> :kind! PG (Enumerated Ordering)
PG (Enumerated Ordering) :: PGType
= 'PGenum '["LT", "EQ", "GT"]
-}
newtype Enumerated enum = Enumerated {getEnumerated :: enum}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)

{- | The `VarArray` newtype is an indication that the Haskell
type it's applied to should be stored as a
`Squeal.PostgreSQL.Type.Schema.PGvararray`.

>>> import Data.Vector
>>> :kind! PG (VarArray (Vector Double))
PG (VarArray (Vector Double)) :: PGType
= 'PGvararray ('NotNull 'PGfloat8)
-}
newtype VarArray arr
  = VarArray {getVarArray :: arr}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)

{- | The `FixArray` newtype is an indication that the Haskell
type it's applied to should be stored as a
`Squeal.PostgreSQL.Type.Schema.PGfixarray`.

>>> :kind! PG (FixArray ((Double, Double), (Double, Double)))
PG (FixArray ((Double, Double), (Double, Double))) :: PGType
= 'PGfixarray '[2, 2] ('NotNull 'PGfloat8)
-}
newtype FixArray arr = FixArray {getFixArray :: arr}
  deriving stock (Eq, Ord, Show, Read, GHC.Generic)
  deriving anyclass (SOP.HasDatatypeInfo, SOP.Generic)

-- | `Only` is a 1-tuple type, useful for encoding or decoding a singleton
newtype Only x = Only { fromOnly :: x }
  deriving (Functor,Foldable,Traversable,Eq,Ord,Read,Show,GHC.Generic)
instance SOP.Generic (Only x)
instance SOP.HasDatatypeInfo (Only x)

{- | Variable-length text type with limit

>>> :kind! PG (VarChar 4)
PG (VarChar 4) :: PGType
= 'PGvarchar 4
-}
newtype VarChar (n :: Nat) = VarChar Strict.Text
  deriving (Eq,Ord,Read,Show)

-- | Constructor for `VarChar`
varChar :: forall  n . KnownNat n => Strict.Text -> Maybe (VarChar n)
varChar t =
  if Strict.Text.length t <= fromIntegral (natVal @n Proxy)
  then Just $ VarChar t
  else Nothing

-- | Access the `Strict.Text` of a `VarChar`
getVarChar :: VarChar n -> Strict.Text
getVarChar (VarChar t) = t

{- | Fixed-length, blank padded

>>> :kind! PG (FixChar 4)
PG (FixChar 4) :: PGType
= 'PGchar 4
-}
newtype FixChar (n :: Nat) = FixChar Strict.Text
  deriving (Eq,Ord,Read,Show)

-- | Constructor for `FixChar`
fixChar :: forall  n . KnownNat n => Strict.Text -> Maybe (FixChar n)
fixChar t =
  if Strict.Text.length t == fromIntegral (natVal @n Proxy)
  then Just $ FixChar t
  else Nothing

-- | Access the `Strict.Text` of a `FixChar`
getFixChar :: FixChar n -> Strict.Text
getFixChar (FixChar t) = t

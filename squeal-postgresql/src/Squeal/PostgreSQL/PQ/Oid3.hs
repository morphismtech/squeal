{-|
Module: Squeal.PostgreSQL.PQ.Oid3
Description: Object identifiers
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Object identifiers are used internally by PostgreSQL as
primary keys. They are needed to correctly encode
statement parameters.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , FlexibleInstances
  , QuantifiedConstraints
  , MultiParamTypeClasses
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ.Oid3
  ( -- * Oids
    LibPQ.Oid
  , OidOf (..)
  , OidOfArray (..)
  , OidOfNull (..)
  , OidOfField (..)
  ) where

import GHC.TypeLits

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Schema

-- | The `LibPQ.Oid` of a `PGType`
--
-- >>> :set -XTypeApplications
-- >>> oidOf @'PGbool
-- Oid 16
class OidOf c (pg :: PGType) where
  oidOf :: forall m. c m => m LibPQ.Oid
-- | The `LibPQ.Oid` of an array
class OidOfArray c (pg :: PGType) where
  oidOfArray :: forall m. c m => m LibPQ.Oid
instance OidOfArray c pg => OidOf c ('PGvararray (null pg)) where
  oidOf = oidOfArray @c @pg
instance OidOfArray c pg => OidOf c ('PGfixarray dims (null pg)) where
  oidOf = oidOfArray @c @pg
-- | The `LibPQ.Oid` of a `NullType`
class OidOfNull c (ty :: NullType) where
  oidOfNull :: forall m. c m => m LibPQ.Oid
instance OidOf c pg => OidOfNull c (null pg) where
  oidOfNull = oidOf @c @pg
-- | The `LibPQ.Oid` of a field
class OidOfField c (field :: (Symbol, NullType)) where
  oidOfField :: forall m. c m => m LibPQ.Oid
instance OidOfNull c ty => OidOfField c (fld ::: ty) where
  oidOfField = oidOfNull @c @ty

instance (forall m. c m => Applicative m) => OidOf c 'PGbool where oidOf = pure $ LibPQ.Oid 16
instance (forall m. c m => Applicative m) => OidOfArray c 'PGbool where oidOfArray = pure $ LibPQ.Oid 1000
instance (forall m. c m => Applicative m) => OidOf c 'PGint2 where oidOf = pure $ LibPQ.Oid 21
instance (forall m. c m => Applicative m) => OidOfArray c 'PGint2 where oidOfArray = pure $ LibPQ.Oid 1005
instance (forall m. c m => Applicative m) => OidOf c 'PGint4 where oidOf = pure $ LibPQ.Oid 23
instance (forall m. c m => Applicative m) => OidOfArray c 'PGint4 where oidOfArray = pure $ LibPQ.Oid 1007
instance (forall m. c m => Applicative m) => OidOf c 'PGint8 where oidOf = pure $ LibPQ.Oid 20
instance (forall m. c m => Applicative m) => OidOfArray c 'PGint8 where oidOfArray = pure $ LibPQ.Oid 1016
instance (forall m. c m => Applicative m) => OidOf c 'PGnumeric where oidOf = pure $ LibPQ.Oid 1700
instance (forall m. c m => Applicative m) => OidOfArray c 'PGnumeric where oidOfArray = pure $ LibPQ.Oid 1231
instance (forall m. c m => Applicative m) => OidOf c 'PGfloat4 where oidOf = pure $ LibPQ.Oid 700
instance (forall m. c m => Applicative m) => OidOfArray c 'PGfloat4 where oidOfArray = pure $ LibPQ.Oid 1021
instance (forall m. c m => Applicative m) => OidOf c 'PGfloat8 where oidOf = pure $ LibPQ.Oid 701
instance (forall m. c m => Applicative m) => OidOfArray c 'PGfloat8 where oidOfArray = pure $ LibPQ.Oid 1022
instance (forall m. c m => Applicative m) => OidOf c 'PGmoney where oidOf = pure $ LibPQ.Oid 790
instance (forall m. c m => Applicative m) => OidOfArray c 'PGmoney where oidOfArray = pure $ LibPQ.Oid 791
instance (forall m. c m => Applicative m) => OidOf c ('PGchar n) where oidOf = pure $ LibPQ.Oid 18
instance (forall m. c m => Applicative m) => OidOfArray c ('PGchar n) where oidOfArray = pure $ LibPQ.Oid 1002
instance (forall m. c m => Applicative m) => OidOf c ('PGvarchar n) where oidOf = pure $ LibPQ.Oid 1043
instance (forall m. c m => Applicative m) => OidOfArray c ('PGvarchar n) where oidOfArray = pure $ LibPQ.Oid 1015
instance (forall m. c m => Applicative m) => OidOf c 'PGtext where oidOf = pure $ LibPQ.Oid 25
instance (forall m. c m => Applicative m) => OidOfArray c 'PGtext where oidOfArray = pure $ LibPQ.Oid 1009
instance (forall m. c m => Applicative m) => OidOf c 'PGbytea where oidOf = pure $ LibPQ.Oid 17
instance (forall m. c m => Applicative m) => OidOfArray c 'PGbytea where oidOfArray = pure $ LibPQ.Oid 1001
instance (forall m. c m => Applicative m) => OidOf c 'PGtimestamp where oidOf = pure $ LibPQ.Oid 1114
instance (forall m. c m => Applicative m) => OidOfArray c 'PGtimestamp where oidOfArray = pure $ LibPQ.Oid 1115
instance (forall m. c m => Applicative m) => OidOf c 'PGtimestamptz where oidOf = pure $ LibPQ.Oid 1184
instance (forall m. c m => Applicative m) => OidOfArray c 'PGtimestamptz where oidOfArray = pure $ LibPQ.Oid 1185
instance (forall m. c m => Applicative m) => OidOf c 'PGdate where oidOf = pure $ LibPQ.Oid 1082
instance (forall m. c m => Applicative m) => OidOfArray c 'PGdate where oidOfArray = pure $ LibPQ.Oid 1182
instance (forall m. c m => Applicative m) => OidOf c 'PGtime where oidOf = pure $ LibPQ.Oid 1083
instance (forall m. c m => Applicative m) => OidOfArray c 'PGtime where oidOfArray = pure $ LibPQ.Oid 1183
instance (forall m. c m => Applicative m) => OidOf c 'PGtimetz where oidOf = pure $ LibPQ.Oid 1266
instance (forall m. c m => Applicative m) => OidOfArray c 'PGtimetz where oidOfArray = pure $ LibPQ.Oid 1270
instance (forall m. c m => Applicative m) => OidOf c 'PGinterval where oidOf = pure $ LibPQ.Oid 1186
instance (forall m. c m => Applicative m) => OidOfArray c 'PGinterval where oidOfArray = pure $ LibPQ.Oid 1187
instance (forall m. c m => Applicative m) => OidOf c 'PGuuid where oidOf = pure $ LibPQ.Oid 2950
instance (forall m. c m => Applicative m) => OidOfArray c 'PGuuid where oidOfArray = pure $ LibPQ.Oid 2951
instance (forall m. c m => Applicative m) => OidOf c 'PGinet where oidOf = pure $ LibPQ.Oid 869
instance (forall m. c m => Applicative m) => OidOfArray c 'PGinet where oidOfArray = pure $ LibPQ.Oid 1041
instance (forall m. c m => Applicative m) => OidOf c 'PGjson where oidOf = pure $ LibPQ.Oid 114
instance (forall m. c m => Applicative m) => OidOfArray c 'PGjson where oidOfArray = pure $ LibPQ.Oid 199
instance (forall m. c m => Applicative m) => OidOf c 'PGjsonb where oidOf = pure $ LibPQ.Oid 3802
instance (forall m. c m => Applicative m) => OidOfArray c 'PGjsonb where oidOfArray = pure $ LibPQ.Oid 3807
instance (forall m. c m => Applicative m) => OidOf c 'PGtsvector where oidOf = pure $ LibPQ.Oid 3614
instance (forall m. c m => Applicative m) => OidOfArray c 'PGtsvector where oidOfArray = pure $ LibPQ.Oid 3643
instance (forall m. c m => Applicative m) => OidOf c 'PGtsquery where oidOf = pure $ LibPQ.Oid 3615
instance (forall m. c m => Applicative m) => OidOfArray c 'PGtsquery where oidOfArray = pure $ LibPQ.Oid 3645
instance (forall m. c m => Applicative m) => OidOf c 'PGoid where oidOf = pure $ LibPQ.Oid 26
instance (forall m. c m => Applicative m) => OidOfArray c 'PGoid where oidOfArray = pure $ LibPQ.Oid 1028
instance (forall m. c m => Applicative m) => OidOf c ('PGrange 'PGint4) where oidOf = pure $ LibPQ.Oid 3904
instance (forall m. c m => Applicative m) => OidOfArray c ('PGrange 'PGint4) where oidOfArray = pure $ LibPQ.Oid 3905
instance (forall m. c m => Applicative m) => OidOf c ('PGrange 'PGint8) where oidOf = pure $ LibPQ.Oid 3926
instance (forall m. c m => Applicative m) => OidOfArray c ('PGrange 'PGint8) where oidOfArray = pure $ LibPQ.Oid 3927
instance (forall m. c m => Applicative m) => OidOf c ('PGrange 'PGnumeric) where oidOf = pure $ LibPQ.Oid 3906
instance (forall m. c m => Applicative m) => OidOfArray c ('PGrange 'PGnumeric) where oidOfArray = pure $ LibPQ.Oid 3907
instance (forall m. c m => Applicative m) => OidOf c ('PGrange 'PGtimestamp) where oidOf = pure $ LibPQ.Oid 3908
instance (forall m. c m => Applicative m) => OidOfArray c ('PGrange 'PGtimestamp) where oidOfArray = pure $ LibPQ.Oid 3909
instance (forall m. c m => Applicative m) => OidOf c ('PGrange 'PGtimestamptz) where oidOf = pure $ LibPQ.Oid 3910
instance (forall m. c m => Applicative m) => OidOfArray c ('PGrange 'PGtimestamptz) where oidOfArray = pure $ LibPQ.Oid 3911
instance (forall m. c m => Applicative m) => OidOf c ('PGrange 'PGdate) where oidOf = pure $ LibPQ.Oid 3912
instance (forall m. c m => Applicative m) => OidOfArray c ('PGrange 'PGdate) where oidOfArray = pure $ LibPQ.Oid 3913
-- instance {-# OVERLAPPABLE #-} OidOf ('PGrange ty) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGrange ty) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGcomposite row) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGcomposite row) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGenum labels) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGenum labels) where oidOfArray = LibPQ.invalidOid

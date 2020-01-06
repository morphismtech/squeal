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
  , MultiParamTypeClasses
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
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

instance OidOf Applicative 'PGbool where oidOf = pure $ LibPQ.Oid 16
instance OidOfArray Applicative 'PGbool where oidOfArray = pure $ LibPQ.Oid 1000
instance OidOf Applicative 'PGint2 where oidOf = pure $ LibPQ.Oid 21
instance OidOfArray Applicative 'PGint2 where oidOfArray = pure $ LibPQ.Oid 1005
instance OidOf Applicative 'PGint4 where oidOf = pure $ LibPQ.Oid 23
instance OidOfArray Applicative 'PGint4 where oidOfArray = pure $ LibPQ.Oid 1007
instance OidOf Applicative 'PGint8 where oidOf = pure $ LibPQ.Oid 20
instance OidOfArray Applicative 'PGint8 where oidOfArray = pure $ LibPQ.Oid 1016
instance OidOf Applicative 'PGnumeric where oidOf = pure $ LibPQ.Oid 1700
instance OidOfArray Applicative 'PGnumeric where oidOfArray = pure $ LibPQ.Oid 1231
instance OidOf Applicative 'PGfloat4 where oidOf = pure $ LibPQ.Oid 700
instance OidOfArray Applicative 'PGfloat4 where oidOfArray = pure $ LibPQ.Oid 1021
instance OidOf Applicative 'PGfloat8 where oidOf = pure $ LibPQ.Oid 701
instance OidOfArray Applicative 'PGfloat8 where oidOfArray = pure $ LibPQ.Oid 1022
instance OidOf Applicative 'PGmoney where oidOf = pure $ LibPQ.Oid 790
instance OidOfArray Applicative 'PGmoney where oidOfArray = pure $ LibPQ.Oid 791
instance OidOf Applicative ('PGchar n) where oidOf = pure $ LibPQ.Oid 18
instance OidOfArray Applicative ('PGchar n) where oidOfArray = pure $ LibPQ.Oid 1002
instance OidOf Applicative ('PGvarchar n) where oidOf = pure $ LibPQ.Oid 1043
instance OidOfArray Applicative ('PGvarchar n) where oidOfArray = pure $ LibPQ.Oid 1015
instance OidOf Applicative 'PGtext where oidOf = pure $ LibPQ.Oid 25
instance OidOfArray Applicative 'PGtext where oidOfArray = pure $ LibPQ.Oid 1009
instance OidOf Applicative 'PGbytea where oidOf = pure $ LibPQ.Oid 17
instance OidOfArray Applicative 'PGbytea where oidOfArray = pure $ LibPQ.Oid 1001
instance OidOf Applicative 'PGtimestamp where oidOf = pure $ LibPQ.Oid 1114
instance OidOfArray Applicative 'PGtimestamp where oidOfArray = pure $ LibPQ.Oid 1115
instance OidOf Applicative 'PGtimestamptz where oidOf = pure $ LibPQ.Oid 1184
instance OidOfArray Applicative 'PGtimestamptz where oidOfArray = pure $ LibPQ.Oid 1185
instance OidOf Applicative 'PGdate where oidOf = pure $ LibPQ.Oid 1082
instance OidOfArray Applicative 'PGdate where oidOfArray = pure $ LibPQ.Oid 1182
instance OidOf Applicative 'PGtime where oidOf = pure $ LibPQ.Oid 1083
instance OidOfArray Applicative 'PGtime where oidOfArray = pure $ LibPQ.Oid 1183
instance OidOf Applicative 'PGtimetz where oidOf = pure $ LibPQ.Oid 1266
instance OidOfArray Applicative 'PGtimetz where oidOfArray = pure $ LibPQ.Oid 1270
instance OidOf Applicative 'PGinterval where oidOf = pure $ LibPQ.Oid 1186
instance OidOfArray Applicative 'PGinterval where oidOfArray = pure $ LibPQ.Oid 1187
instance OidOf Applicative 'PGuuid where oidOf = pure $ LibPQ.Oid 2950
instance OidOfArray Applicative 'PGuuid where oidOfArray = pure $ LibPQ.Oid 2951
instance OidOf Applicative 'PGinet where oidOf = pure $ LibPQ.Oid 869
instance OidOfArray Applicative 'PGinet where oidOfArray = pure $ LibPQ.Oid 1041
instance OidOf Applicative 'PGjson where oidOf = pure $ LibPQ.Oid 114
instance OidOfArray Applicative 'PGjson where oidOfArray = pure $ LibPQ.Oid 199
instance OidOf Applicative 'PGjsonb where oidOf = pure $ LibPQ.Oid 3802
instance OidOfArray Applicative 'PGjsonb where oidOfArray = pure $ LibPQ.Oid 3807
instance OidOf Applicative 'PGtsvector where oidOf = pure $ LibPQ.Oid 3614
instance OidOfArray Applicative 'PGtsvector where oidOfArray = pure $ LibPQ.Oid 3643
instance OidOf Applicative 'PGtsquery where oidOf = pure $ LibPQ.Oid 3615
instance OidOfArray Applicative 'PGtsquery where oidOfArray = pure $ LibPQ.Oid 3645
instance OidOf Applicative 'PGoid where oidOf = pure $ LibPQ.Oid 26
instance OidOfArray Applicative 'PGoid where oidOfArray = pure $ LibPQ.Oid 1028
instance OidOf Applicative ('PGrange 'PGint4) where oidOf = pure $ LibPQ.Oid 3904
instance OidOfArray Applicative ('PGrange 'PGint4) where oidOfArray = pure $ LibPQ.Oid 3905
instance OidOf Applicative ('PGrange 'PGint8) where oidOf = pure $ LibPQ.Oid 3926
instance OidOfArray Applicative ('PGrange 'PGint8) where oidOfArray = pure $ LibPQ.Oid 3927
instance OidOf Applicative ('PGrange 'PGnumeric) where oidOf = pure $ LibPQ.Oid 3906
instance OidOfArray Applicative ('PGrange 'PGnumeric) where oidOfArray = pure $ LibPQ.Oid 3907
instance OidOf Applicative ('PGrange 'PGtimestamp) where oidOf = pure $ LibPQ.Oid 3908
instance OidOfArray Applicative ('PGrange 'PGtimestamp) where oidOfArray = pure $ LibPQ.Oid 3909
instance OidOf Applicative ('PGrange 'PGtimestamptz) where oidOf = pure $ LibPQ.Oid 3910
instance OidOfArray Applicative ('PGrange 'PGtimestamptz) where oidOfArray = pure $ LibPQ.Oid 3911
instance OidOf Applicative ('PGrange 'PGdate) where oidOf = pure $ LibPQ.Oid 3912
instance OidOfArray Applicative ('PGrange 'PGdate) where oidOfArray = pure $ LibPQ.Oid 3913
-- instance {-# OVERLAPPABLE #-} OidOf ('PGrange ty) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGrange ty) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGcomposite row) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGcomposite row) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGenum labels) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGenum labels) where oidOfArray = LibPQ.invalidOid

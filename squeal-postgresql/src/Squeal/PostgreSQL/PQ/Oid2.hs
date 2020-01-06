{-|
Module: Squeal.PostgreSQL.PQ.Oid2
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
  , DataKinds
  , FlexibleInstances
  , MultiParamTypeClasses
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.PQ.Oid2
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
class OidOf m (pg :: PGType) where
  oidOf :: m LibPQ.Oid
-- | The `LibPQ.Oid` of an array
class OidOfArray m (pg :: PGType) where
  oidOfArray :: m LibPQ.Oid
instance OidOfArray m pg => OidOf m ('PGvararray (null pg)) where
  oidOf = oidOfArray @m @pg
instance OidOfArray m pg => OidOf m ('PGfixarray dims (null pg)) where
  oidOf = oidOfArray @m @pg
-- | The `LibPQ.Oid` of a `NullType`
class OidOfNull m (ty :: NullType) where
  oidOfNull :: m LibPQ.Oid
instance OidOf m pg => OidOfNull m (null pg) where
  oidOfNull = oidOf @m @pg
-- | The `LibPQ.Oid` of a field
class OidOfField m (field :: (Symbol, NullType)) where
  oidOfField :: m LibPQ.Oid
instance OidOfNull m ty => OidOfField m (fld ::: ty) where
  oidOfField = oidOfNull @m @ty

instance Applicative m => OidOf m 'PGbool where oidOf = pure $ LibPQ.Oid 16
instance Applicative m => OidOfArray m 'PGbool where oidOfArray = pure $ LibPQ.Oid 1000
instance Applicative m => OidOf m 'PGint2 where oidOf = pure $ LibPQ.Oid 21
instance Applicative m => OidOfArray m 'PGint2 where oidOfArray = pure $ LibPQ.Oid 1005
instance Applicative m => OidOf m 'PGint4 where oidOf = pure $ LibPQ.Oid 23
instance Applicative m => OidOfArray m 'PGint4 where oidOfArray = pure $ LibPQ.Oid 1007
instance Applicative m => OidOf m 'PGint8 where oidOf = pure $ LibPQ.Oid 20
instance Applicative m => OidOfArray m 'PGint8 where oidOfArray = pure $ LibPQ.Oid 1016
instance Applicative m => OidOf m 'PGnumeric where oidOf = pure $ LibPQ.Oid 1700
instance Applicative m => OidOfArray m 'PGnumeric where oidOfArray = pure $ LibPQ.Oid 1231
instance Applicative m => OidOf m 'PGfloat4 where oidOf = pure $ LibPQ.Oid 700
instance Applicative m => OidOfArray m 'PGfloat4 where oidOfArray = pure $ LibPQ.Oid 1021
instance Applicative m => OidOf m 'PGfloat8 where oidOf = pure $ LibPQ.Oid 701
instance Applicative m => OidOfArray m 'PGfloat8 where oidOfArray = pure $ LibPQ.Oid 1022
instance Applicative m => OidOf m 'PGmoney where oidOf = pure $ LibPQ.Oid 790
instance Applicative m => OidOfArray m 'PGmoney where oidOfArray = pure $ LibPQ.Oid 791
instance Applicative m => OidOf m ('PGchar n) where oidOf = pure $ LibPQ.Oid 18
instance Applicative m => OidOfArray m ('PGchar n) where oidOfArray = pure $ LibPQ.Oid 1002
instance Applicative m => OidOf m ('PGvarchar n) where oidOf = pure $ LibPQ.Oid 1043
instance Applicative m => OidOfArray m ('PGvarchar n) where oidOfArray = pure $ LibPQ.Oid 1015
instance Applicative m => OidOf m 'PGtext where oidOf = pure $ LibPQ.Oid 25
instance Applicative m => OidOfArray m 'PGtext where oidOfArray = pure $ LibPQ.Oid 1009
instance Applicative m => OidOf m 'PGbytea where oidOf = pure $ LibPQ.Oid 17
instance Applicative m => OidOfArray m 'PGbytea where oidOfArray = pure $ LibPQ.Oid 1001
instance Applicative m => OidOf m 'PGtimestamp where oidOf = pure $ LibPQ.Oid 1114
instance Applicative m => OidOfArray m 'PGtimestamp where oidOfArray = pure $ LibPQ.Oid 1115
instance Applicative m => OidOf m 'PGtimestamptz where oidOf = pure $ LibPQ.Oid 1184
instance Applicative m => OidOfArray m 'PGtimestamptz where oidOfArray = pure $ LibPQ.Oid 1185
instance Applicative m => OidOf m 'PGdate where oidOf = pure $ LibPQ.Oid 1082
instance Applicative m => OidOfArray m 'PGdate where oidOfArray = pure $ LibPQ.Oid 1182
instance Applicative m => OidOf m 'PGtime where oidOf = pure $ LibPQ.Oid 1083
instance Applicative m => OidOfArray m 'PGtime where oidOfArray = pure $ LibPQ.Oid 1183
instance Applicative m => OidOf m 'PGtimetz where oidOf = pure $ LibPQ.Oid 1266
instance Applicative m => OidOfArray m 'PGtimetz where oidOfArray = pure $ LibPQ.Oid 1270
instance Applicative m => OidOf m 'PGinterval where oidOf = pure $ LibPQ.Oid 1186
instance Applicative m => OidOfArray m 'PGinterval where oidOfArray = pure $ LibPQ.Oid 1187
instance Applicative m => OidOf m 'PGuuid where oidOf = pure $ LibPQ.Oid 2950
instance Applicative m => OidOfArray m 'PGuuid where oidOfArray = pure $ LibPQ.Oid 2951
instance Applicative m => OidOf m 'PGinet where oidOf = pure $ LibPQ.Oid 869
instance Applicative m => OidOfArray m 'PGinet where oidOfArray = pure $ LibPQ.Oid 1041
instance Applicative m => OidOf m 'PGjson where oidOf = pure $ LibPQ.Oid 114
instance Applicative m => OidOfArray m 'PGjson where oidOfArray = pure $ LibPQ.Oid 199
instance Applicative m => OidOf m 'PGjsonb where oidOf = pure $ LibPQ.Oid 3802
instance Applicative m => OidOfArray m 'PGjsonb where oidOfArray = pure $ LibPQ.Oid 3807
instance Applicative m => OidOf m 'PGtsvector where oidOf = pure $ LibPQ.Oid 3614
instance Applicative m => OidOfArray m 'PGtsvector where oidOfArray = pure $ LibPQ.Oid 3643
instance Applicative m => OidOf m 'PGtsquery where oidOf = pure $ LibPQ.Oid 3615
instance Applicative m => OidOfArray m 'PGtsquery where oidOfArray = pure $ LibPQ.Oid 3645
instance Applicative m => OidOf m 'PGoid where oidOf = pure $ LibPQ.Oid 26
instance Applicative m => OidOfArray m 'PGoid where oidOfArray = pure $ LibPQ.Oid 1028
instance Applicative m => OidOf m ('PGrange 'PGint4) where oidOf = pure $ LibPQ.Oid 3904
instance Applicative m => OidOfArray m ('PGrange 'PGint4) where oidOfArray = pure $ LibPQ.Oid 3905
instance Applicative m => OidOf m ('PGrange 'PGint8) where oidOf = pure $ LibPQ.Oid 3926
instance Applicative m => OidOfArray m ('PGrange 'PGint8) where oidOfArray = pure $ LibPQ.Oid 3927
instance Applicative m => OidOf m ('PGrange 'PGnumeric) where oidOf = pure $ LibPQ.Oid 3906
instance Applicative m => OidOfArray m ('PGrange 'PGnumeric) where oidOfArray = pure $ LibPQ.Oid 3907
instance Applicative m => OidOf m ('PGrange 'PGtimestamp) where oidOf = pure $ LibPQ.Oid 3908
instance Applicative m => OidOfArray m ('PGrange 'PGtimestamp) where oidOfArray = pure $ LibPQ.Oid 3909
instance Applicative m => OidOf m ('PGrange 'PGtimestamptz) where oidOf = pure $ LibPQ.Oid 3910
instance Applicative m => OidOfArray m ('PGrange 'PGtimestamptz) where oidOfArray = pure $ LibPQ.Oid 3911
instance Applicative m => OidOf m ('PGrange 'PGdate) where oidOf = pure $ LibPQ.Oid 3912
instance Applicative m => OidOfArray m ('PGrange 'PGdate) where oidOfArray = pure $ LibPQ.Oid 3913
-- instance {-# OVERLAPPABLE #-} OidOf ('PGrange ty) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGrange ty) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGcomposite row) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGcomposite row) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGenum labels) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGenum labels) where oidOfArray = LibPQ.invalidOid

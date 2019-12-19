{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleInstances
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.PQ.Oid where

import GHC.TypeLits

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Schema

class OidOf (pg :: PGType) where
  oidOf :: LibPQ.Oid
class OidOfArray (pg :: PGType) where
  oidOfArray :: LibPQ.Oid
instance OidOfArray pg => OidOf ('PGvararray (null pg)) where
  oidOf = oidOfArray @pg
instance OidOfArray pg => OidOf ('PGfixarray dims (null pg)) where
  oidOf = oidOfArray @pg
class OidOfNull (ty :: NullType) where
  oidOfNull :: LibPQ.Oid
instance OidOf pg => OidOfNull (null pg) where
  oidOfNull = oidOf @pg
class OidOfField (field :: (Symbol, NullType)) where
  oidOfField :: LibPQ.Oid
instance OidOfNull ty => OidOfField (fld ::: ty) where
  oidOfField = oidOfNull @ty

instance OidOf 'PGbool where oidOf = LibPQ.Oid 16
instance OidOfArray 'PGbool where oidOfArray = LibPQ.Oid 1000
instance OidOf 'PGint2 where oidOf = LibPQ.Oid 21
instance OidOfArray 'PGint2 where oidOfArray = LibPQ.Oid 1005
instance OidOf 'PGint4 where oidOf = LibPQ.Oid 23
instance OidOfArray 'PGint4 where oidOfArray = LibPQ.Oid 1007
instance OidOf 'PGint8 where oidOf = LibPQ.Oid 20
instance OidOfArray 'PGint8 where oidOfArray = LibPQ.Oid 1016
instance OidOf 'PGnumeric where oidOf = LibPQ.Oid 1700
instance OidOfArray 'PGnumeric where oidOfArray = LibPQ.Oid 1231
instance OidOf 'PGfloat4 where oidOf = LibPQ.Oid 700
instance OidOfArray 'PGfloat4 where oidOfArray = LibPQ.Oid 1021
instance OidOf 'PGfloat8 where oidOf = LibPQ.Oid 701
instance OidOfArray 'PGfloat8 where oidOfArray = LibPQ.Oid 1022
instance OidOf 'PGmoney where oidOf = LibPQ.Oid 790
instance OidOfArray 'PGmoney where oidOfArray = LibPQ.Oid 791
instance OidOf ('PGchar n) where oidOf = LibPQ.Oid 18
instance OidOfArray ('PGchar n) where oidOfArray = LibPQ.Oid 1002
instance OidOf ('PGvarchar n) where oidOf = LibPQ.Oid 1043
instance OidOfArray ('PGvarchar n) where oidOfArray = LibPQ.Oid 1015
instance OidOf 'PGtext where oidOf = LibPQ.Oid 25
instance OidOfArray 'PGtext where oidOfArray = LibPQ.Oid 1009
instance OidOf 'PGbytea where oidOf = LibPQ.Oid 17
instance OidOfArray 'PGbytea where oidOfArray = LibPQ.Oid 1001
instance OidOf 'PGtimestamp where oidOf = LibPQ.Oid 1114
instance OidOfArray 'PGtimestamp where oidOfArray = LibPQ.Oid 1115
instance OidOf 'PGtimestamptz where oidOf = LibPQ.Oid 1184
instance OidOfArray 'PGtimestamptz where oidOfArray = LibPQ.Oid 1185
instance OidOf 'PGdate where oidOf = LibPQ.Oid 1082
instance OidOfArray 'PGdate where oidOfArray = LibPQ.Oid 1182
instance OidOf 'PGtime where oidOf = LibPQ.Oid 1083
instance OidOfArray 'PGtime where oidOfArray = LibPQ.Oid 1183
instance OidOf 'PGtimetz where oidOf = LibPQ.Oid 1266
instance OidOfArray 'PGtimetz where oidOfArray = LibPQ.Oid 1270
instance OidOf 'PGinterval where oidOf = LibPQ.Oid 1186
instance OidOfArray 'PGinterval where oidOfArray = LibPQ.Oid 1187
instance OidOf 'PGuuid where oidOf = LibPQ.Oid 2950
instance OidOfArray 'PGuuid where oidOfArray = LibPQ.Oid 2951
instance OidOf 'PGinet where oidOf = LibPQ.Oid 869
instance OidOfArray 'PGinet where oidOfArray = LibPQ.Oid 1041
instance OidOf 'PGjson where oidOf = LibPQ.Oid 114
instance OidOfArray 'PGjson where oidOfArray = LibPQ.Oid 199
instance OidOf 'PGjsonb where oidOf = LibPQ.Oid 3802
instance OidOfArray 'PGjsonb where oidOfArray = LibPQ.Oid 3807
instance OidOf 'PGtsvector where oidOf = LibPQ.Oid 3614
instance OidOfArray 'PGtsvector where oidOfArray = LibPQ.Oid 3643
instance OidOf 'PGtsquery where oidOf = LibPQ.Oid 3615
instance OidOfArray 'PGtsquery where oidOfArray = LibPQ.Oid 3645
instance OidOf 'PGoid where oidOf = LibPQ.Oid 26
instance OidOfArray 'PGoid where oidOfArray = LibPQ.Oid 1028
instance OidOf ('PGrange 'PGint4) where oidOf = LibPQ.Oid 3904
instance OidOfArray ('PGrange 'PGint4) where oidOfArray = LibPQ.Oid 3905
instance OidOf ('PGrange 'PGint8) where oidOf = LibPQ.Oid 3926
instance OidOfArray ('PGrange 'PGint8) where oidOfArray = LibPQ.Oid 3927
instance OidOf ('PGrange 'PGnumeric) where oidOf = LibPQ.Oid 3906
instance OidOfArray ('PGrange 'PGnumeric) where oidOfArray = LibPQ.Oid 3907
instance OidOf ('PGrange 'PGtimestamp) where oidOf = LibPQ.Oid 3908
instance OidOfArray ('PGrange 'PGtimestamp) where oidOfArray = LibPQ.Oid 3909
instance OidOf ('PGrange 'PGtimestamptz) where oidOf = LibPQ.Oid 3910
instance OidOfArray ('PGrange 'PGtimestamptz) where oidOfArray = LibPQ.Oid 3911
instance OidOf ('PGrange 'PGdate) where oidOf = LibPQ.Oid 3912
instance OidOfArray ('PGrange 'PGdate) where oidOfArray = LibPQ.Oid 3913
-- instance {-# OVERLAPPABLE #-} OidOf ('PGrange ty) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGrange ty) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGvararray (null ('PGrange ty) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGcomposite row) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGcomposite row) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGvararray (null ('PGcomposite row) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGenum labels) where oidOf = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOfArray ('PGenum labels) where oidOfArray = LibPQ.invalidOid
-- instance {-# OVERLAPPABLE #-} OidOf ('PGvararray (null ('PGenum labels) where oidOfArray = LibPQ.invalidOid

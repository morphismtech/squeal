{-# LANGUAGE
    DataKinds
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Squeel.PostgreSQL.Type where

import Data.Proxy
import GHC.TypeLits

import qualified Database.PostgreSQL.LibPQ as LibPQ

data PGType
  = PGBool
  | PGInt2
  | PGInt4
  | PGInt8
  | PGNumeric
  | PGFloat4
  | PGFloat8
  | PGSerial2
  | PGSerial4
  | PGSerial8
  | PGMoney
  | PGChar Nat
  | PGVarChar Nat
  | PGText
  | PGBytea
  | PGTimestamp
  | PGTimestampTZ
  | PGDate
  | PGTime
  | PGTimeTZ
  | PGInterval
  | PGUuid
  | PGJson
  | PGJsonb

class ToOid pg where toOid :: Proxy pg -> LibPQ.Oid
class ToOids pgs where toOids :: Proxy pgs -> [LibPQ.Oid]
instance ToOids '[] where toOids _ = []
instance (ToOid pg, ToOids pgs) => ToOids (pg ': pgs) where
  toOids (_ :: Proxy (pg ': pgs)) = toOid (Proxy @pg) : toOids (Proxy @pgs)

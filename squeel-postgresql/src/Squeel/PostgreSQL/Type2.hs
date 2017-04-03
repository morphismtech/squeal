{-# LANGUAGE
    DataKinds
  , GADTs
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
#-}

module Squeel.PostgreSQL.Type2 where

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

data Named k = (:=) Symbol k
type family Unnamed named where Unnamed (name ':= x) = x
type family Name k where Name (name ':= x) = name
type Column = Named PGType
type Table = Named [Column]
type Database = Named [Table]
type Schema = Named [Database]
data Alias (obj :: k -> *) (named :: Named k) where
  As :: KnownSymbol name => obj x -> Alias obj (name ':= x)
  Elem
    :: (KnownSymbol name, (name ':= x) `Elem` list ~ 'True)
    => Alias (obj list) (name ':= x)

type family Elem x xs where
  x `Elem` '[] = 'False
  x `Elem` (x ': xs) = 'True
  x `Elem` (x' ': xs) = x `Elem` xs

class ToOid pg where toOid :: Proxy pg -> LibPQ.Oid
class ToOids pgs where toOids :: Proxy pgs -> [LibPQ.Oid]
instance ToOids '[] where toOids _ = []
instance (ToOid pg, ToOids pgs) => ToOids (pg ': pgs) where
  toOids (_ :: Proxy (pg ': pgs)) = toOid (Proxy @pg) : toOids (Proxy @pgs)

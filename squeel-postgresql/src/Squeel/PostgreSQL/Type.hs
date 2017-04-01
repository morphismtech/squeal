{-# LANGUAGE
    DataKinds
  , GADTs
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
#-}

module Squeel.PostgreSQL.Type where

import Data.Proxy
import qualified Database.PostgreSQL.LibPQ as LibPQ
import GHC.TypeLits

newtype PGType = PGType Symbol
data Named x = (:=) Symbol x
type family Unnamed named where Unnamed (name ':= x) = x
type family Name x where Name (name ':= x) = name
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

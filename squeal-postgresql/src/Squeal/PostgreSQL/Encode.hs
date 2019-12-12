{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleInstances
  , PolyKinds
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.Encode where

import Data.Function ((&))
import Data.Functor.Contravariant
import Database.PostgreSQL.LibPQ
import Generics.SOP
import Generics.SOP.Record
import PostgreSQL.Binary.Encoding

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Schema

newtype Encode f t x = Encode {getEncode :: x -> f t}
instance Contravariant (Encode f t) where
  contramap f (Encode t) = Encode (t . f)

emptyEncode :: Encode (NP f) '[] x
emptyEncode = Encode (const Nil)

oneEncode :: Encode f t x -> Encode (NP f) '[t] x
oneEncode (Encode t) = Encode (one . t)

andEncode
  :: Encode f t x
  -> Encode (NP f) ts x
  -> Encode (NP f) (t ': ts) x
andEncode (Encode t) (Encode ts) =
  Encode $ \x -> t x :* ts x

appendEncode
  :: Encode (NP f) ts x
  -> Encode (NP f) vs x
  -> Encode (NP f) (Join ts vs) x
appendEncode (Encode ts) (Encode vs) =
  Encode $ \x -> ts x & also (vs x)

divideEncode
  :: (x -> (y,z))
  -> Encode f t y
  -> Encode (NP f) ts z
  -> Encode (NP f) (t ': ts) x
divideEncode u (Encode t) (Encode ts) =
  Encode (\x -> let (y,z) = u x in t y :* ts z)

separateEncode
  :: (x -> (y,z))
  -> Encode (NP f) ts y
  -> Encode (NP f) vs z
  -> Encode (NP f) (Join ts vs) x
separateEncode u (Encode ts) (Encode vs) =
  Encode (\x -> let (y,z) = u x in ts y & also (vs z))

class ToPG pg x where
  toPG :: Encode (K Encoding) pg x
class ToNull ty x where
  toNull :: Encode (K (Maybe Encoding)) ty x
instance ToPG pg x => ToNull ('NotNull pg) x where
  toNull = Encode (K . Just . unK . getEncode (toPG @pg))
instance ToPG pg x => ToNull ('Null pg) (Maybe x) where
  toNull = Encode (K . fmap (unK . getEncode (toPG @pg)))
class ToFixArray n ty x where
  toFixArray :: Encode (K Array) '(n,ty) x
class ToField field x where
  toField :: Encode (K (Maybe Encoding)) field (P x)
class ToTuple tuple x where
  toTuple :: Encode (NP (K (Maybe Encoding))) tuple x

class OidOf pg where oidOf :: Oid
instance OidOf pg => OidOf ('NotNull pg) where oidOf = oidOf @pg
instance OidOf pg => OidOf ('Null pg) where oidOf = oidOf @pg
instance OidOf ty => OidOf (field ::: ty) where oidOf = oidOf @ty
class OidOfArray pg where oidOfArray :: Oid

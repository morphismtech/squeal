{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , PolyKinds
#-}

module Squeal.PostgreSQL.PQ.Oid where

import GHC.TypeLits

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeal.PostgreSQL.Schema

class OidOf (pg :: PGType) where oidOf :: LibPQ.Oid
class OidOfArray (pg :: PGType) where oidOfArray :: LibPQ.Oid
class OidOfNull (ty :: NullType) where oidOfNull :: LibPQ.Oid
class OidOfField (field :: (Symbol, NullType)) where oidOfField :: LibPQ.Oid

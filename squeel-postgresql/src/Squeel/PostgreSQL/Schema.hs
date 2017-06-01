{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , MagicHash
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeOperators
#-}

module Squeel.PostgreSQL.Schema
  ( PGType (..)
  , ToOid (..)
  , ToOids (..)
  , PGNum
  , (:::)
  , Alias (Alias)
  , Aliased (As)
  , renderAliased
  , module GHC.OverloadedLabels
  , module GHC.TypeLits
  ) where

import Data.ByteString
import Data.Monoid
import Data.String
import GHC.Exts
import GHC.OverloadedLabels
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

class ToOid pg where toOid :: Proxy# pg -> LibPQ.Oid
class ToOids pgs where toOids :: Proxy# pgs -> [LibPQ.Oid]
instance ToOids '[] where toOids _ = []
instance (ToOid pg, ToOids pgs) => ToOids (pg ': pgs) where
  toOids _ = toOid (proxy# :: Proxy# pg) : toOids (proxy# :: Proxy# pgs)

class PGNum (ty :: PGType) where
instance PGNum 'PGInt2
instance PGNum 'PGInt4
instance PGNum 'PGInt8
instance PGNum 'PGNumeric
instance PGNum 'PGFloat4
instance PGNum 'PGFloat8

type (:::) (alias :: Symbol) (ty :: polykind) = '(alias,ty)

data Alias (alias :: Symbol) = Alias (Proxy# alias)

instance alias1 ~ alias2 => IsLabel alias1 (Alias alias2) where
  fromLabel = Alias

data Aliased expression aliased where
  As
    :: KnownSymbol alias
    => expression ty
    -> Alias alias
    -> Aliased expression (alias ::: ty)

instance (KnownSymbol alias, IsLabel alias (expression ty))
  => IsLabel alias (Aliased expression (alias ::: ty)) where
    fromLabel alias = fromLabel alias `As` fromLabel alias

renderAliased
  :: (forall ty. expression ty -> ByteString)
  -> Aliased expression aliased
  -> ByteString
renderAliased render (expression `As` Alias alias) =
  render expression <> " AS " <> fromString (symbolVal' alias)

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MagicHash
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
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
  , KnownPGType (..)
  , KnownColumns (..)
  , renderAliased
  , NullityType (..)
  , NullifyType
  , NullifyColumns
  , NullifyTable
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

class KnownPGType (ty :: PGType) where
  renderPGType :: Proxy# ty -> ByteString
instance KnownPGType 'PGBool where renderPGType _ = "bool"
instance KnownPGType 'PGInt2 where renderPGType _ = "int2"
instance KnownPGType 'PGInt4 where renderPGType _ = "int4"
instance KnownPGType 'PGInt8 where renderPGType _ = "int8"
instance KnownPGType 'PGNumeric where renderPGType _ = "numeric"
instance KnownPGType 'PGFloat4 where renderPGType _ = "float4"
instance KnownPGType 'PGFloat8 where renderPGType _ = "float8"
instance KnownPGType 'PGSerial2 where renderPGType _ = "serial2"
instance KnownPGType 'PGSerial4 where renderPGType _ = "serial4"
instance KnownPGType 'PGSerial8 where renderPGType _ = "serial8"
instance KnownPGType 'PGMoney where renderPGType _ = "money"
instance KnownNat n => KnownPGType ('PGChar n) where
  renderPGType _ = "char("
    <> fromString (show (natVal' (proxy# :: Proxy# n)))
    <> ")"
instance KnownNat n => KnownPGType ('PGVarChar n) where
  renderPGType _ = "varchar("
    <> fromString (show (natVal' (proxy# :: Proxy# n)))
    <> ")"
instance KnownPGType 'PGText where renderPGType _ = "text"
instance KnownPGType 'PGBytea where renderPGType _ = "bytea"
instance KnownPGType 'PGTimestamp where renderPGType _ = "timestamp"
instance KnownPGType 'PGTimestampTZ where renderPGType _ = "timestampTZ"
instance KnownPGType 'PGDate where renderPGType _ = "date"
instance KnownPGType 'PGTime where renderPGType _ = "time"
instance KnownPGType 'PGTimeTZ where renderPGType _ = "timeTZ"
instance KnownPGType 'PGInterval where renderPGType _ = "interval"
instance KnownPGType 'PGUuid where renderPGType _ = "uuid"
instance KnownPGType 'PGJson where renderPGType _ = "json"
instance KnownPGType 'PGJsonb where renderPGType _ = "jsonb"

class KnownColumns (columns :: [(Symbol,NullityType)]) where
  renderColumns :: Proxy# columns -> ByteString
instance
  ( KnownSymbol column
  , KnownPGType ty
  ) => KnownColumns '[column ::: 'NotNull ty] where
    renderColumns _ =
      fromString (symbolVal' (proxy# :: Proxy# column))
      <> " "
      <> renderPGType (proxy# :: Proxy# ty)
      <> " NOT NULL"
instance
  ( KnownSymbol column
  , KnownPGType ty
  ) => KnownColumns '[column ::: 'Null ty] where
    renderColumns _ =
      fromString (symbolVal' (proxy# :: Proxy# column))
      <> " "
      <> renderPGType (proxy# :: Proxy# ty)
      <> " NULL"
instance
  ( KnownSymbol column
  , KnownPGType ty
  , KnownColumns (columnsHead ': columnsTail)
  ) => KnownColumns ((column ::: 'NotNull ty) ': columnsHead ': columnsTail) where
    renderColumns _ =
      fromString (symbolVal' (proxy# :: Proxy# column))
      <> " "
      <> renderPGType (proxy# :: Proxy# ty)
      <> " NOT NULL, "
      <> renderColumns (proxy# :: Proxy# (columnsHead ': columnsTail))
instance
  ( KnownSymbol column
  , KnownPGType ty
  , KnownColumns (columnsHead ': columnsTail)
  ) => KnownColumns ((column ::: 'Null ty) ': columnsHead ': columnsTail) where
    renderColumns _ =
      fromString (symbolVal' (proxy# :: Proxy# column))
      <> " "
      <> renderPGType (proxy# :: Proxy# ty)
      <> " NULL, "
      <> renderColumns (proxy# :: Proxy# (columnsHead ': columnsTail))

data NullityType = Null PGType | NotNull PGType

type family NullifyType nullty where
  NullifyType ('Null ty) = 'Null ty
  NullifyType ('NotNull ty) = 'Null ty

type family NullifyColumns columns where
  NullifyColumns '[] = '[]
  NullifyColumns ((column ::: ty) ': columns) =
    (column ::: (NullifyType ty)) ': (NullifyColumns columns)

type family NullifyTable table where
  NullifyTable (table ::: columns) = table ::: NullifyColumns columns

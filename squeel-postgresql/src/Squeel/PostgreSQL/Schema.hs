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
  , PGNum (..)
  , PGFractional
  , PGFloating
  , (:::)
  , Alias (Alias)
  , renderAlias
  , Aliased (As)
  , renderAliased
  , NullityType (..)
  , NullifyType
  , NullifyColumns
  , NullifyTables
  , ColumnType (..)
  , Create
  , Drop
  , Alter
  , Rename
  , Join
  , Grouping (..)
  , IsTableColumn (..)
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
--staticTypeInfo
instance ToOid ('Required ('NotNull 'PGBool)) where toOid _ = LibPQ.Oid 16
instance ToOid ('Required ('NotNull 'PGBytea)) where toOid _ = LibPQ.Oid 17
instance ToOid ('Required ('NotNull 'PGInt8)) where toOid _ = LibPQ.Oid 20
instance ToOid ('Required ('NotNull 'PGInt2)) where toOid _ = LibPQ.Oid 21
instance ToOid ('Required ('NotNull 'PGInt4)) where toOid _ = LibPQ.Oid 23
instance ToOid ('Required ('NotNull 'PGText)) where toOid _ = LibPQ.Oid 26
class ToOids pgs where toOids :: Proxy# pgs -> [LibPQ.Oid]
instance ToOids '[] where toOids _ = []
instance (ToOid pg, ToOids pgs) => ToOids (pg ': pgs) where
  toOids _ = toOid (proxy# :: Proxy# pg) : toOids (proxy# :: Proxy# pgs)

class PGNum (ty :: PGType) where
  decimal :: proxy ty -> Bool
instance PGNum 'PGInt2 where decimal _ = False
instance PGNum 'PGInt4 where decimal _ = False
instance PGNum 'PGInt8 where decimal _ = False
instance PGNum 'PGNumeric where decimal _ = True
instance PGNum 'PGFloat4 where decimal _ = True
instance PGNum 'PGFloat8 where decimal _ = True

class PGNum ty => PGFractional (ty :: PGType) where
instance PGFractional 'PGNumeric
instance PGFractional 'PGFloat4
instance PGFractional 'PGFloat8

class PGFractional ty => PGFloating (ty :: PGType) where
instance PGFloating 'PGNumeric
instance PGFloating 'PGFloat4
instance PGFloating 'PGFloat8

type (:::) (alias :: Symbol) (ty :: polykind) = '(alias,ty)

data Alias (alias :: Symbol) = Alias (Proxy# alias)

renderAlias :: KnownSymbol alias => Alias alias -> ByteString
renderAlias (Alias alias) = fromString (symbolVal' alias)

instance alias1 ~ alias2 => IsLabel alias1 (Alias alias2) where
  fromLabel = Alias

data Aliased expression aliased where
  As
    :: KnownSymbol alias
    => expression ty
    -> Alias alias
    -> Aliased expression (alias ::: ty)

renderAliased
  :: (forall ty. expression ty -> ByteString)
  -> Aliased expression aliased
  -> ByteString
renderAliased render (expression `As` Alias alias) =
  render expression <> " AS " <> fromString (symbolVal' alias)

data NullityType = Null PGType | NotNull PGType

data ColumnType = Optional NullityType | Required NullityType

type family NullifyType (ty :: ColumnType) where
  NullifyType (optionality ('Null ty)) = optionality ('Null ty)
  NullifyType (optionality ('NotNull ty)) = optionality ('Null ty)

type family NullifyColumns columns where
  NullifyColumns '[] = '[]
  NullifyColumns ((column ::: ty) ': columns) =
    (column ::: NullifyType ty) ': NullifyColumns columns

type family NullifyTables tables where
  NullifyTables '[] = '[]
  NullifyTables ((column ::: ty) ': columns) =
    (column ::: NullifyColumns ty) ': NullifyTables columns

type family Create alias x xs where
  Create alias x '[] = '[alias ::: x]
  Create alias y (x ': xs) = x ': Create alias y xs

type family Drop alias xs where
  Drop alias ((alias ::: x) ': xs) = xs
  Drop alias (x ': xs) = x ': Drop alias xs

type family Alter alias xs x where
  Alter alias ((alias ::: x0) ': xs) x1 = (alias ::: x1) ': xs
  Alter alias (x0 ': xs) x1 = x0 ': Alter alias xs x1

type family Rename alias0 alias1 xs where
  Rename alias0 alias1 ((alias0 ::: x0) ': xs) = (alias1 ::: x0) ': xs
  Rename alias0 alias1 (x ': xs) = x ': Rename alias0 alias1 xs

type family Join xs ys where
  Join '[] ys = ys
  Join (x ': xs) ys = x ': Join xs ys

data Grouping
  = Ungrouped
  | Grouped [(Symbol,Symbol)]

class IsTableColumn table column expression where
  (&.) :: Alias table -> Alias column -> expression

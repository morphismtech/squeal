{-|
Module: Squeal.PostgreSQL.Expression
Description: Type expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Type expressions.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Type
  ( -- * type casting
    cast
  , astype
  , inferredtype
    -- * type expressions
  , TypeExpression (..)
  , typedef
  , typetable
  , typeview
  , bool
  , int2
  , smallint
  , int4
  , int
  , integer
  , int8
  , bigint
  , numeric
  , float4
  , real
  , float8
  , doublePrecision
  , money
  , text
  , char
  , character
  , varchar
  , characterVarying
  , bytea
  , timestamp
  , timestampWithTimeZone
  , timestamptz
  , date
  , time
  , timeWithTimeZone
  , timetz
  , interval
  , uuid
  , inet
  , json
  , jsonb
  , vararray
  , fixarray
  , tsvector
  , tsquery
  , oid
  , int4range
  , int8range
  , numrange
  , tsrange
  , tstzrange
  , daterange
    -- * column type definitions
  , ColumnTypeExpression (..)
  , nullable
  , notNullable
  , default_
  , serial2
  , smallserial
  , serial4
  , serial
  , serial8
  , bigserial
  , hask
    -- * type inference
  , PGTyped (..)
  , PGNullityTyped (..)
  , FieldTyped (..)
  ) where

import Control.DeepSeq
import Data.ByteString
import Data.String
import GHC.TypeLits

import qualified Data.ByteString as ByteString
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- When a `cast` is applied to an `Expression` of a known type, it
-- represents a run-time type conversion. The cast will succeed only if a
-- suitable type conversion operation has been defined.
--
-- | >>> printSQL $ true & cast int4
-- (TRUE :: int4)
cast
  :: TypeExpression db ty1
  -- ^ type to cast as
  -> Expression outer commons grp db params from ty0
  -- ^ value to convert
  -> Expression outer commons grp db params from ty1
cast ty x = UnsafeExpression $ parenthesized $
  renderSQL x <+> "::" <+> renderSQL ty

-- | A safe version of `cast` which just matches a value with its type.
--
-- >>> printSQL (1 & astype int)
-- (1 :: int)
astype
  :: TypeExpression db ty
  -- ^ type to specify as
  -> Expression outer commons grp db params from ty
  -- ^ value
  -> Expression outer commons grp db params from ty
astype = cast

-- | `inferredtype` will add a type annotation to an `Expression`
-- which can be useful for fixing the storage type of a value.
--
-- >>> printSQL (inferredtype true)
-- (TRUE :: bool)
inferredtype
  :: PGTyped db ty
  => Expression outer common grp db params from ty
  -- ^ value
  -> Expression outer common grp db params from ty
inferredtype = astype pgtype

{-----------------------------------------
type expressions
-----------------------------------------}

-- | `TypeExpression`s are used in `cast`s and
-- `Squeal.PostgreSQL.Definition.createTable` commands.
newtype TypeExpression (db :: SchemasType) (ty :: NullityType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (TypeExpression db ty) where
  renderSQL = renderTypeExpression

-- | The enum or composite type in a `Typedef` can be expressed by its alias.
typedef
  :: (Has sch db schema, Has td schema ('Typedef ty))
  => QualifiedAlias sch td
  -- ^ type alias
  -> TypeExpression db (null ty)
typedef = UnsafeTypeExpression . renderSQL

-- | The composite type corresponding to a `Table` definition can be expressed
-- by its alias.
typetable
  :: (Has sch db schema, Has tab schema ('Table table))
  => QualifiedAlias sch tab
  -- ^ table alias
  -> TypeExpression db (null ('PGcomposite (TableToRow table)))
typetable = UnsafeTypeExpression . renderSQL

-- | The composite type corresponding to a `View` definition can be expressed
-- by its alias.
typeview
  :: (Has sch db schema, Has vw schema ('View view))
  => QualifiedAlias sch vw
  -- ^ view alias
  -> TypeExpression db (null ('PGcomposite view))
typeview = UnsafeTypeExpression . renderSQL

-- | logical Boolean (true/false)
bool :: TypeExpression db (null 'PGbool)
bool = UnsafeTypeExpression "bool"
-- | signed two-byte integer
int2, smallint :: TypeExpression db (null 'PGint2)
int2 = UnsafeTypeExpression "int2"
smallint = UnsafeTypeExpression "smallint"
-- | signed four-byte integer
int4, int, integer :: TypeExpression db (null 'PGint4)
int4 = UnsafeTypeExpression "int4"
int = UnsafeTypeExpression "int"
integer = UnsafeTypeExpression "integer"
-- | signed eight-byte integer
int8, bigint :: TypeExpression db (null 'PGint8)
int8 = UnsafeTypeExpression "int8"
bigint = UnsafeTypeExpression "bigint"
-- | arbitrary precision numeric type
numeric :: TypeExpression db (null 'PGnumeric)
numeric = UnsafeTypeExpression "numeric"
-- | single precision floating-point number (4 bytes)
float4, real :: TypeExpression db (null 'PGfloat4)
float4 = UnsafeTypeExpression "float4"
real = UnsafeTypeExpression "real"
-- | double precision floating-point number (8 bytes)
float8, doublePrecision :: TypeExpression db (null 'PGfloat8)
float8 = UnsafeTypeExpression "float8"
doublePrecision = UnsafeTypeExpression "double precision"
-- | currency amount
money :: TypeExpression schema (null 'PGmoney)
money = UnsafeTypeExpression "money"
-- | variable-length character string
text :: TypeExpression db (null 'PGtext)
text = UnsafeTypeExpression "text"
-- | fixed-length character string
char, character
  :: forall n db null. (KnownNat n, 1 <= n)
  => TypeExpression db (null ('PGchar n))
char = UnsafeTypeExpression $ "char(" <> renderNat @n <> ")"
character = UnsafeTypeExpression $  "character(" <> renderNat @n <> ")"
-- | variable-length character string
varchar, characterVarying
  :: forall n db null. (KnownNat n, 1 <= n)
  => TypeExpression db (null ('PGvarchar n))
varchar = UnsafeTypeExpression $ "varchar(" <> renderNat @n <> ")"
characterVarying = UnsafeTypeExpression $
  "character varying(" <> renderNat @n <> ")"
-- | binary data ("byte array")
bytea :: TypeExpression db (null 'PGbytea)
bytea = UnsafeTypeExpression "bytea"
-- | date and time (no time zone)
timestamp :: TypeExpression db (null 'PGtimestamp)
timestamp = UnsafeTypeExpression "timestamp"
-- | date and time, including time zone
timestampWithTimeZone, timestamptz :: TypeExpression db (null 'PGtimestamptz)
timestampWithTimeZone = UnsafeTypeExpression "timestamp with time zone"
timestamptz = UnsafeTypeExpression "timestamptz"
-- | calendar date (year, month, day)
date :: TypeExpression db (null 'PGdate)
date = UnsafeTypeExpression "date"
-- | time of day (no time zone)
time :: TypeExpression db (null 'PGtime)
time = UnsafeTypeExpression "time"
-- | time of day, including time zone
timeWithTimeZone, timetz :: TypeExpression db (null 'PGtimetz)
timeWithTimeZone = UnsafeTypeExpression "time with time zone"
timetz = UnsafeTypeExpression "timetz"
-- | time span
interval :: TypeExpression db (null 'PGinterval)
interval = UnsafeTypeExpression "interval"
-- | universally unique identifier
uuid :: TypeExpression db (null 'PGuuid)
uuid = UnsafeTypeExpression "uuid"
-- | IPv4 or IPv6 host address
inet :: TypeExpression db (null 'PGinet)
inet = UnsafeTypeExpression "inet"
-- | textual JSON data
json :: TypeExpression db (null 'PGjson)
json = UnsafeTypeExpression "json"
-- | binary JSON data, decomposed
jsonb :: TypeExpression db (null 'PGjsonb)
jsonb = UnsafeTypeExpression "jsonb"
-- | variable length array
vararray
  :: TypeExpression db pg
  -> TypeExpression db (null ('PGvararray pg))
vararray ty = UnsafeTypeExpression $ renderSQL ty <> "[]"
-- | fixed length array
--
-- >>> renderSQL (fixarray @'[2] json)
-- "json[2]"
fixarray
  :: forall dims db null pg. SOP.All KnownNat dims
  => TypeExpression db pg
  -> TypeExpression db (null ('PGfixarray dims pg))
fixarray ty = UnsafeTypeExpression $
  renderSQL ty <> renderDims @dims
  where
    renderDims :: forall ns. SOP.All KnownNat ns => ByteString
    renderDims =
      ("[" <>)
      . (<> "]")
      . ByteString.intercalate "]["
      . SOP.hcollapse
      $ SOP.hcmap (SOP.Proxy @KnownNat)
        (K . fromString . show . natVal)
        (SOP.hpure SOP.Proxy :: SOP.NP SOP.Proxy ns)
-- | text search query
tsvector :: TypeExpression db (null 'PGtsvector)
tsvector = UnsafeTypeExpression "tsvector"
-- | text search document
tsquery :: TypeExpression db (null 'PGtsquery)
tsquery = UnsafeTypeExpression "tsquery"
-- | Object identifiers (OIDs) are used internally by PostgreSQL
-- as primary keys for various system tables.
oid :: TypeExpression db (null 'PGoid)
oid = UnsafeTypeExpression "oid"
-- | Range of integer
int4range :: TypeExpression db (null ('PGrange 'PGint4))
int4range = UnsafeTypeExpression "int4range"
-- | Range of bigint
int8range :: TypeExpression db (null ('PGrange 'PGint8))
int8range = UnsafeTypeExpression "int8range"
-- | Range of numeric
numrange :: TypeExpression db (null ('PGrange 'PGnumeric))
numrange = UnsafeTypeExpression "numrange"
-- | Range of timestamp without time zone
tsrange  :: TypeExpression db (null ('PGrange 'PGtimestamp))
tsrange = UnsafeTypeExpression "tsrange"
-- | Range of timestamp with time zone
tstzrange :: TypeExpression db (null ('PGrange 'PGtimestamptz))
tstzrange = UnsafeTypeExpression "tstzrange"
-- | Range of date
daterange :: TypeExpression db (null ('PGrange 'PGdate))
daterange = UnsafeTypeExpression "daterange"

-- | `pgtype` is a demoted version of a `PGType`
class PGTyped db (ty :: NullityType) where
  pgtype :: TypeExpression db ty
instance PGTyped db (null 'PGbool) where pgtype = bool
instance PGTyped db (null 'PGint2) where pgtype = int2
instance PGTyped db (null 'PGint4) where pgtype = int4
instance PGTyped db (null 'PGint8) where pgtype = int8
instance PGTyped db (null 'PGnumeric) where pgtype = numeric
instance PGTyped db (null 'PGfloat4) where pgtype = float4
instance PGTyped db (null 'PGfloat8) where pgtype = float8
instance PGTyped db (null 'PGmoney) where pgtype = money
instance PGTyped db (null 'PGtext) where pgtype = text
instance (KnownNat n, 1 <= n)
  => PGTyped db (null ('PGchar n)) where pgtype = char @n
instance (KnownNat n, 1 <= n)
  => PGTyped db (null ('PGvarchar n)) where pgtype = varchar @n
instance PGTyped db (null 'PGbytea) where pgtype = bytea
instance PGTyped db (null 'PGtimestamp) where pgtype = timestamp
instance PGTyped db (null 'PGtimestamptz) where pgtype = timestampWithTimeZone
instance PGTyped db (null 'PGdate) where pgtype = date
instance PGTyped db (null 'PGtime) where pgtype = time
instance PGTyped db (null 'PGtimetz) where pgtype = timeWithTimeZone
instance PGTyped db (null 'PGinterval) where pgtype = interval
instance PGTyped db (null 'PGuuid) where pgtype = uuid
instance PGTyped db (null 'PGjson) where pgtype = json
instance PGTyped db (null 'PGjsonb) where pgtype = jsonb
instance PGTyped db ty
  => PGTyped db (null ('PGvararray ty)) where
    pgtype = vararray (pgtype @db @ty)
instance (SOP.All KnownNat dims, PGTyped db ty)
  => PGTyped db (null ('PGfixarray dims ty)) where
    pgtype = fixarray @dims (pgtype @db @ty)
instance PGTyped db (null 'PGtsvector) where pgtype = tsvector
instance PGTyped db (null 'PGtsquery) where pgtype = tsquery
instance PGTyped db (null 'PGoid) where pgtype = oid
instance PGTyped db (null ('PGrange 'PGint4)) where pgtype = int4range
instance PGTyped db (null ('PGrange 'PGint8)) where pgtype = int8range
instance PGTyped db (null ('PGrange 'PGnumeric)) where pgtype = numrange
instance PGTyped db (null ('PGrange 'PGtimestamp)) where pgtype = tsrange
instance PGTyped db (null ('PGrange 'PGtimestamptz)) where pgtype = tstzrange
instance PGTyped db (null ('PGrange 'PGdate)) where pgtype = daterange

-- | Lift `PGTyped` to a field
class FieldTyped db ty where
  fieldtype :: Aliased (TypeExpression db) ty
instance (KnownSymbol alias, PGTyped db ty)
  => FieldTyped db (alias ::: ty) where
    fieldtype = pgtype `As` Alias

-- | `ColumnTypeExpression`s are used in
-- `Squeal.PostgreSQL.Definition.createTable` commands.
newtype ColumnTypeExpression (db :: SchemasType) (ty :: ColumnType)
  = UnsafeColumnTypeExpression { renderColumnTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (ColumnTypeExpression db ty) where
  renderSQL = renderColumnTypeExpression

-- | used in `Squeal.PostgreSQL.Definition.createTable`
-- commands as a column constraint to note that
-- @NULL@ may be present in a column
nullable
  :: TypeExpression db (nullity ty)
  -> ColumnTypeExpression db ('NoDef :=> 'Null ty)
nullable ty = UnsafeColumnTypeExpression $ renderSQL ty <+> "NULL"

-- | used in `Squeal.PostgreSQL.Definition.createTable`
-- commands as a column constraint to ensure
-- @NULL@ is not present in a column
notNullable
  :: TypeExpression db (nullity ty)
  -> ColumnTypeExpression db ('NoDef :=> 'NotNull ty)
notNullable ty = UnsafeColumnTypeExpression $ renderSQL ty <+> "NOT NULL"

-- | used in `Squeal.PostgreSQL.Definition.createTable`
-- commands as a column constraint to give a default
default_
  :: Expression '[] '[] 'Ungrouped db '[] '[] ty
  -> ColumnTypeExpression db ('NoDef :=> ty)
  -> ColumnTypeExpression db ('Def :=> ty)
default_ x ty = UnsafeColumnTypeExpression $
  renderSQL ty <+> "DEFAULT" <+> renderExpression x

-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint2`
serial2, smallserial
  :: ColumnTypeExpression db ('Def :=> 'NotNull 'PGint2)
serial2 = UnsafeColumnTypeExpression "serial2"
smallserial = UnsafeColumnTypeExpression "smallserial"
-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint4`
serial4, serial
  :: ColumnTypeExpression db ('Def :=> 'NotNull 'PGint4)
serial4 = UnsafeColumnTypeExpression "serial4"
serial = UnsafeColumnTypeExpression "serial"
-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint8`
serial8, bigserial
  :: ColumnTypeExpression db ('Def :=> 'NotNull 'PGint8)
serial8 = UnsafeColumnTypeExpression "serial8"
bigserial = UnsafeColumnTypeExpression "bigserial"

-- | Like @PGTyped@ but also accounts for nullity.
class PGNullityTyped db (nullty :: NullityType) where
  pgNullityType :: ColumnTypeExpression db ('NoDef :=> nullty)

instance PGTyped db ('Null ty) => PGNullityTyped db ('Null ty) where
  pgNullityType = nullable (pgtype @_ @('Null ty))

instance PGTyped db ('NotNull ty) => PGNullityTyped db ('NotNull ty) where
  pgNullityType = notNullable (pgtype @_ @('NotNull ty))

-- | Allow you to specify pg column types in relation to haskell types.
-- >>> printSQL $ hask @(Maybe String)
-- text NULL
--
-- >>> printSQL $ hask @Double
-- float8 NOT NULL
hask
  :: forall hask db. PGNullityTyped db (NullPG hask)
  => ColumnTypeExpression db ('NoDef :=> NullPG hask)
hask = pgNullityType

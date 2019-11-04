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
#-}

module Squeal.PostgreSQL.Expression.Type
  ( TypeExpression (..)
  , cast
  , astype
  , inferredtype
  , PGTyped (..)
  , FieldTyped (..)
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
  :: TypeExpression schemas ty1
  -- ^ type to cast as
  -> Expression outer commons grp schemas params from ty0
  -- ^ value to convert
  -> Expression outer commons grp schemas params from ty1
cast ty x = UnsafeExpression $ parenthesized $
  renderSQL x <+> "::" <+> renderSQL ty

-- | A safe version of `cast` which just matches a value with its type.
--
-- >>> printSQL (1 & astype int)
-- (1 :: int)
astype
  :: TypeExpression schemas ty
  -- ^ type to specify as
  -> Expression outer commons grp schemas params from ty
  -- ^ value
  -> Expression outer commons grp schemas params from ty
astype = cast

-- | `inferredtype` will add a type annotation to an `Expression`
-- which can be useful for fixing the storage type of a value.
--
-- >>> printSQL (inferredtype true)
-- (TRUE :: bool)
inferredtype
  :: PGTyped schemas ty
  => Expression outer common grp schemas params from ty
  -> Expression outer common grp schemas params from ty
inferredtype = astype pgtype

{-----------------------------------------
type expressions
-----------------------------------------}

-- | `TypeExpression`s are used in `cast`s and
-- `Squeal.PostgreSQL.Definition.createTable` commands.
newtype TypeExpression (schemas :: SchemasType) (ty :: NullityType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (TypeExpression schemas ty) where
  renderSQL = renderTypeExpression

-- | The enum or composite type in a `Typedef` can be expressed by its alias.
typedef
  :: (Has sch schemas schema, Has td schema ('Typedef ty))
  => QualifiedAlias sch td
  -> TypeExpression schemas (null ty)
typedef = UnsafeTypeExpression . renderSQL

-- | The composite type corresponding to a `Table` definition can be expressed
-- by its alias.
typetable
  :: (Has sch schemas schema, Has tab schema ('Table table))
  => QualifiedAlias sch tab
  -> TypeExpression schemas (null ('PGcomposite (TableToRow table)))
typetable = UnsafeTypeExpression . renderSQL

-- | The composite type corresponding to a `View` definition can be expressed
-- by its alias.
typeview
  :: (Has sch schemas schema, Has vw schema ('View view))
  => QualifiedAlias sch vw
  -> TypeExpression schemas (null ('PGcomposite view))
typeview = UnsafeTypeExpression . renderSQL

-- | logical Boolean (true/false)
bool :: TypeExpression schemas (null 'PGbool)
bool = UnsafeTypeExpression "bool"
-- | signed two-byte integer
int2, smallint :: TypeExpression schemas (null 'PGint2)
int2 = UnsafeTypeExpression "int2"
smallint = UnsafeTypeExpression "smallint"
-- | signed four-byte integer
int4, int, integer :: TypeExpression schemas (null 'PGint4)
int4 = UnsafeTypeExpression "int4"
int = UnsafeTypeExpression "int"
integer = UnsafeTypeExpression "integer"
-- | signed eight-byte integer
int8, bigint :: TypeExpression schemas (null 'PGint8)
int8 = UnsafeTypeExpression "int8"
bigint = UnsafeTypeExpression "bigint"
-- | arbitrary precision numeric type
numeric :: TypeExpression schemas (null 'PGnumeric)
numeric = UnsafeTypeExpression "numeric"
-- | single precision floating-point number (4 bytes)
float4, real :: TypeExpression schemas (null 'PGfloat4)
float4 = UnsafeTypeExpression "float4"
real = UnsafeTypeExpression "real"
-- | double precision floating-point number (8 bytes)
float8, doublePrecision :: TypeExpression schemas (null 'PGfloat8)
float8 = UnsafeTypeExpression "float8"
doublePrecision = UnsafeTypeExpression "double precision"
-- | currency amount
money :: TypeExpression schema (null 'PGmoney)
money = UnsafeTypeExpression "money"
-- | variable-length character string
text :: TypeExpression schemas (null 'PGtext)
text = UnsafeTypeExpression "text"
-- | fixed-length character string
char, character
  :: forall n schemas null. (KnownNat n, 1 <= n)
  => TypeExpression schemas (null ('PGchar n))
char = UnsafeTypeExpression $ "char(" <> renderNat @n <> ")"
character = UnsafeTypeExpression $  "character(" <> renderNat @n <> ")"
-- | variable-length character string
varchar, characterVarying
  :: forall n schemas null. (KnownNat n, 1 <= n)
  => TypeExpression schemas (null ('PGvarchar n))
varchar = UnsafeTypeExpression $ "varchar(" <> renderNat @n <> ")"
characterVarying = UnsafeTypeExpression $
  "character varying(" <> renderNat @n <> ")"
-- | binary data ("byte array")
bytea :: TypeExpression schemas (null 'PGbytea)
bytea = UnsafeTypeExpression "bytea"
-- | date and time (no time zone)
timestamp :: TypeExpression schemas (null 'PGtimestamp)
timestamp = UnsafeTypeExpression "timestamp"
-- | date and time, including time zone
timestampWithTimeZone, timestamptz :: TypeExpression schemas (null 'PGtimestamptz)
timestampWithTimeZone = UnsafeTypeExpression "timestamp with time zone"
timestamptz = UnsafeTypeExpression "timestamptz"
-- | calendar date (year, month, day)
date :: TypeExpression schemas (null 'PGdate)
date = UnsafeTypeExpression "date"
-- | time of day (no time zone)
time :: TypeExpression schemas (null 'PGtime)
time = UnsafeTypeExpression "time"
-- | time of day, including time zone
timeWithTimeZone, timetz :: TypeExpression schemas (null 'PGtimetz)
timeWithTimeZone = UnsafeTypeExpression "time with time zone"
timetz = UnsafeTypeExpression "timetz"
-- | time span
interval :: TypeExpression schemas (null 'PGinterval)
interval = UnsafeTypeExpression "interval"
-- | universally unique identifier
uuid :: TypeExpression schemas (null 'PGuuid)
uuid = UnsafeTypeExpression "uuid"
-- | IPv4 or IPv6 host address
inet :: TypeExpression schemas (null 'PGinet)
inet = UnsafeTypeExpression "inet"
-- | textual JSON data
json :: TypeExpression schemas (null 'PGjson)
json = UnsafeTypeExpression "json"
-- | binary JSON data, decomposed
jsonb :: TypeExpression schemas (null 'PGjsonb)
jsonb = UnsafeTypeExpression "jsonb"
-- | variable length array
vararray
  :: TypeExpression schemas pg
  -> TypeExpression schemas (null ('PGvararray pg))
vararray ty = UnsafeTypeExpression $ renderSQL ty <> "[]"
-- | fixed length array
--
-- >>> renderSQL (fixarray @'[2] json)
-- "json[2]"
fixarray
  :: forall dims schemas null pg. SOP.All KnownNat dims
  => TypeExpression schemas pg
  -> TypeExpression schemas (null ('PGfixarray dims pg))
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
tsvector :: TypeExpression schemas (null 'PGtsvector)
tsvector = UnsafeTypeExpression "tsvector"
-- | text search document
tsquery :: TypeExpression schemas (null 'PGtsquery)
tsquery = UnsafeTypeExpression "tsquery"
-- | Object identifiers (OIDs) are used internally by PostgreSQL
-- as primary keys for various system tables.
oid :: TypeExpression schemas (null 'PGoid)
oid = UnsafeTypeExpression "oid"
-- | Range of integer
int4range :: TypeExpression schemas (null ('PGrange 'PGint4))
int4range = UnsafeTypeExpression "int4range"
-- | Range of bigint
int8range :: TypeExpression schemas (null ('PGrange 'PGint8))
int8range = UnsafeTypeExpression "int8range"
-- | Range of numeric
numrange :: TypeExpression schemas (null ('PGrange 'PGnumeric))
numrange = UnsafeTypeExpression "numrange"
-- | Range of timestamp without time zone
tsrange  :: TypeExpression schemas (null ('PGrange 'PGtimestamp))
tsrange = UnsafeTypeExpression "tsrange"
-- | Range of timestamp with time zone
tstzrange :: TypeExpression schemas (null ('PGrange 'PGtimestamptz))
tstzrange = UnsafeTypeExpression "tstzrange"
-- | Range of date
daterange :: TypeExpression schemas (null ('PGrange 'PGdate))
daterange = UnsafeTypeExpression "daterange"

-- | `pgtype` is a demoted version of a `PGType`
class PGTyped schemas (ty :: NullityType) where
  pgtype :: TypeExpression schemas ty
instance PGTyped schemas (null 'PGbool) where pgtype = bool
instance PGTyped schemas (null 'PGint2) where pgtype = int2
instance PGTyped schemas (null 'PGint4) where pgtype = int4
instance PGTyped schemas (null 'PGint8) where pgtype = int8
instance PGTyped schemas (null 'PGnumeric) where pgtype = numeric
instance PGTyped schemas (null 'PGfloat4) where pgtype = float4
instance PGTyped schemas (null 'PGfloat8) where pgtype = float8
instance PGTyped schemas (null 'PGmoney) where pgtype = money
instance PGTyped schemas (null 'PGtext) where pgtype = text
instance (KnownNat n, 1 <= n)
  => PGTyped schemas (null ('PGchar n)) where pgtype = char @n
instance (KnownNat n, 1 <= n)
  => PGTyped schemas (null ('PGvarchar n)) where pgtype = varchar @n
instance PGTyped schemas (null 'PGbytea) where pgtype = bytea
instance PGTyped schemas (null 'PGtimestamp) where pgtype = timestamp
instance PGTyped schemas (null 'PGtimestamptz) where pgtype = timestampWithTimeZone
instance PGTyped schemas (null 'PGdate) where pgtype = date
instance PGTyped schemas (null 'PGtime) where pgtype = time
instance PGTyped schemas (null 'PGtimetz) where pgtype = timeWithTimeZone
instance PGTyped schemas (null 'PGinterval) where pgtype = interval
instance PGTyped schemas (null 'PGuuid) where pgtype = uuid
instance PGTyped schemas (null 'PGjson) where pgtype = json
instance PGTyped schemas (null 'PGjsonb) where pgtype = jsonb
instance PGTyped schemas ty
  => PGTyped schemas (null ('PGvararray ty)) where
    pgtype = vararray (pgtype @schemas @ty)
instance (SOP.All KnownNat dims, PGTyped schemas ty)
  => PGTyped schemas (null ('PGfixarray dims ty)) where
    pgtype = fixarray @dims (pgtype @schemas @ty)
instance PGTyped schemas (null 'PGtsvector) where pgtype = tsvector
instance PGTyped schemas (null 'PGtsquery) where pgtype = tsquery
instance PGTyped schemas (null 'PGoid) where pgtype = oid

-- | Lift `PGTyped` to a field
class FieldTyped schemas ty where
  fieldtype :: Aliased (TypeExpression schemas) ty
instance (KnownSymbol alias, PGTyped schemas ty)
  => FieldTyped schemas (alias ::: ty) where
    fieldtype = pgtype `As` Alias

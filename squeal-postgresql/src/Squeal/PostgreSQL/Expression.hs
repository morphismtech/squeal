{-|
Module: Squeal.PostgreSQL.Expression
Description: Squeal expressions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal expressions are the atoms used to build statements.
-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MagicHash
  , OverloadedStrings
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
  , RankNTypes
#-}

module Squeal.PostgreSQL.Expression
  ( -- * Expression
    Expression (..)
  , Expr
  , (:-->)
  , unsafeFunction
  , unsafeUnaryOpL
  , unsafeUnaryOpR
  , Operator
  , unsafeBinaryOp
  , FunctionVar
  , unsafeFunctionVar
  , FunctionN
  , unsafeFunctionN
  , Literal (..)
  , HasParameter (parameter)
  , param
  , PGSubset (..)
    -- * Types
  , TypeExpression (..)
  , cast
  , PGTyped (..)
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
  , date
  , time
  , timeWithTimeZone
  , interval
  , uuid
  , inet
  , json
  , jsonb
  , vararray
  , fixarray
  , tsvector
  , tsquery
    -- * Re-export
  , (&)
  , K (..)
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import ByteString.StrictBuilder (builderBytes)
import Data.Function ((&))
import Data.Semigroup hiding (All)
import qualified Data.Aeson as JSON
import Data.Int
import Data.String
import Data.Text (Text)
import Generics.SOP hiding (All, from)
import GHC.OverloadedLabels
import GHC.TypeLits
import Numeric
import Prelude hiding (id, (.))

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Binary
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
column expressions
-----------------------------------------}

{- | `Expression`s are used in a variety of contexts,
such as in the target `Squeal.PostgreSQL.Query.List` of the
`Squeal.PostgreSQL.Query.select` command,
as new column values in `Squeal.PostgreSQL.Manipulation.insertRow` or
`Squeal.PostgreSQL.Manipulation.update`,
or in search `Condition`s in a number of commands.

The expression syntax allows the calculation of
values from primitive expression using arithmetic, logical,
and other operations.

The type parameters of `Expression` are

* @outer :: @ `FromType`, the @from@ clauses of any outer queries in which
  the `Expression` is a correlated subquery expression;
* @commons :: @ `FromType`, the `Squeal.PostgreSQL.Query.CommonTableExpression`s
  that are in scope for the `Expression`;
* @grp :: @ `Grouping`, the `Grouping` of the @from@ clause which may limit
  which columns may be referenced by alias;
* @schemas :: @ `SchemasType`, the schemas of your database that are in
  scope for the `Expression`;
* @from :: @ `FromType`, the @from@ clause which the `Expression` may use
  to reference columns by alias;
* @ty :: @ `NullityType`, the type of the `Expression`.
-}
newtype Expression
  (outer :: FromType)
  (commons :: FromType)
  (grp :: Grouping)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (from :: FromType)
  (ty :: NullityType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Expression outer commons grp schemas params from ty) where
  renderSQL = renderExpression

-- | An `Expr` is a closed `Expression`.
-- It is a F`RankNType` but don't be scared.
-- Think of it as an expression which sees no
-- namespaces, so you can't use parameters
-- or alias references. It can be used as
-- a simple piece of more complex `Expression`s.
type Expr x
  = forall outer commons grp schemas params from
  . Expression outer commons grp schemas params from x
    -- ^ cannot reference aliases

-- | A `RankNType` for binary operators.
type Operator x1 x2 y
  =  forall outer commons grp schemas params from
  .  Expression outer commons grp schemas params from x1
     -- ^ left input
  -> Expression outer commons grp schemas params from x2
     -- ^ right input
  -> Expression outer commons grp schemas params from y
     -- ^ output

-- | A `RankNType` for functions with a single argument.
-- These could be either function calls or unary operators.
-- This is a subtype of the usual Haskell function type `Prelude.->`,
-- indeed a subcategory as it is closed under the usual
-- `Prelude..` and `Prelude.id`.
type (:-->) x y
  =  forall outer commons grp schemas params from
  .  Expression outer commons grp schemas params from x
     -- ^ input
  -> Expression outer commons grp schemas params from y
     -- ^ output

{- | A `RankNType` for functions with a fixed-length list of heterogeneous arguments.
Use the `*:` operator to end your argument lists, like so.

>>> printSQL (unsafeFunctionN "fun" (true :* false :* localTime *: true))
fun(TRUE, FALSE, LOCALTIME, TRUE)
-}
type FunctionN xs y
  =  forall outer commons grp schemas params from
  .  NP (Expression outer commons grp schemas params from) xs
     -- ^ inputs
  -> Expression outer commons grp schemas params from y
     -- ^ output

{- | A `RankNType` for functions with a variable-length list of
homogeneous arguments and at least 1 more argument.
-}
type FunctionVar x0 x1 y
  =  forall outer commons grp schemas params from
  .  [Expression outer commons grp schemas params from x0]
     -- ^ inputs
  -> Expression outer commons grp schemas params from x1
     -- ^ must have at least 1 input
  -> Expression outer commons grp schemas params from y
     -- ^ output

{- | >>> printSQL (unsafeFunctionVar "greatest" [true, null_] false)
greatest(TRUE, NULL, FALSE)
-}
unsafeFunctionVar :: ByteString -> FunctionVar x0 x1 y
unsafeFunctionVar fun xs x = UnsafeExpression $ fun <> parenthesized
  (commaSeparated (renderSQL <$> xs) <> ", " <> renderSQL x)

{- | A `HasParameter` constraint is used to indicate a value that is
supplied externally to a SQL statement.
`Squeal.PostgreSQL.PQ.manipulateParams`,
`Squeal.PostgreSQL.PQ.queryParams` and
`Squeal.PostgreSQL.PQ.traversePrepared` support specifying data values
separately from the SQL command string, in which case `param`s are used to
refer to the out-of-line data values.
-}
class KnownNat n => HasParameter
  (n :: Nat)
  (params :: [NullityType])
  (ty :: NullityType)
  | n params -> ty where
    -- | `parameter` takes a `Nat` using type application and a `TypeExpression`.
    --
    -- >>> let expr = parameter @1 int4 :: Expression outer '[] grp schemas '[ 'Null 'PGint4] from ('Null 'PGint4)
    -- >>> printSQL expr
    -- ($1 :: int4)
    parameter
      :: TypeExpression schemas ty
      -> Expression outer commons grp schemas params from ty
    parameter ty = UnsafeExpression $ parenthesized $
      "$" <> renderNat @n <+> "::"
        <+> renderSQL ty
instance {-# OVERLAPPING #-} HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

-- | `param` takes a `Nat` using type application and for basic types,
-- infers a `TypeExpression`.
--
-- >>> let expr = param @1 :: Expression outer commons grp schemas '[ 'Null 'PGint4] from ('Null 'PGint4)
-- >>> printSQL expr
-- ($1 :: int4)
param
  :: forall n outer commons schemas params from grp ty
   . (PGTyped schemas ty, HasParameter n params ty)
  => Expression outer commons grp schemas params from ty -- ^ param
param = parameter @n (pgtype @schemas)

instance (HasUnique tab (Join outer from) row, Has col row ty)
  => IsLabel col (Expression outer commons 'Ungrouped schemas params from ty) where
    fromLabel = UnsafeExpression $ renderSQL (Alias @col)
instance (HasUnique tab (Join outer from) row, Has col row ty, tys ~ '[ty])
  => IsLabel col (NP (Expression outer commons 'Ungrouped schemas params from) tys) where
    fromLabel = fromLabel @col :* Nil
instance (HasUnique tab (Join outer from) row, Has col row ty, column ~ (col ::: ty))
  => IsLabel col
    (Aliased (Expression outer commons 'Ungrouped schemas params from) column) where
    fromLabel = fromLabel @col `As` Alias
instance (HasUnique tab (Join outer from) row, Has col row ty, columns ~ '[col ::: ty])
  => IsLabel col
    (NP (Aliased (Expression outer commons 'Ungrouped schemas params from)) columns) where
    fromLabel = fromLabel @col :* Nil

instance (Has tab (Join outer from) row, Has col row ty)
  => IsQualified tab col (Expression outer commons 'Ungrouped schemas params from ty) where
    tab ! col = UnsafeExpression $
      renderSQL tab <> "." <> renderSQL col
instance (Has tab (Join outer from) row, Has col row ty, tys ~ '[ty])
  => IsQualified tab col (NP (Expression outer commons 'Ungrouped schemas params from) tys) where
    tab ! col = tab ! col :* Nil
instance (Has tab (Join outer from) row, Has col row ty, column ~ (col ::: ty))
  => IsQualified tab col
    (Aliased (Expression outer commons 'Ungrouped schemas params from) column) where
    tab ! col = tab ! col `As` col
instance (Has tab (Join outer from) row, Has col row ty, columns ~ '[col ::: ty])
  => IsQualified tab col
    (NP (Aliased (Expression outer commons 'Ungrouped schemas params from)) columns) where
    tab ! col = tab ! col :* Nil

instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  ) => IsLabel col
    (Expression outer commons ('Grouped bys) schemas params from ty) where
      fromLabel = UnsafeExpression $ renderSQL (Alias @col)
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , tys ~ '[ty]
  ) => IsLabel col
    (NP (Expression outer commons ('Grouped bys) schemas params from) tys) where
      fromLabel = fromLabel @col :* Nil
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , column ~ (col ::: ty)
  ) => IsLabel col
    (Aliased (Expression outer commons ('Grouped bys) schemas params from) column) where
      fromLabel = fromLabel @col `As` Alias
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , columns ~ '[col ::: ty]
  ) => IsLabel col
    (NP (Aliased (Expression outer commons ('Grouped bys) schemas params from)) columns) where
      fromLabel = fromLabel @col :* Nil

instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  ) => IsQualified tab col
    (Expression outer commons ('Grouped bys) schemas params from ty) where
      tab ! col = UnsafeExpression $
        renderSQL tab <> "." <> renderSQL col
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , tys ~ '[ty]
  ) => IsQualified tab col
    (NP (Expression outer commons ('Grouped bys) schemas params from) tys) where
      tab ! col = tab ! col :* Nil
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , column ~ (col ::: ty)
  ) => IsQualified tab col
    (Aliased (Expression outer commons ('Grouped bys) schemas params from) column) where
      tab ! col = tab ! col `As` col
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , columns ~ '[col ::: ty]
  ) => IsQualified tab col
    (NP (Aliased (Expression outer commons ('Grouped bys) schemas params from)) columns) where
      tab ! col = tab ! col :* Nil

instance (KnownSymbol label, label `In` labels) => IsPGlabel label
  (Expression outer commons grp schemas params from (null ('PGenum labels))) where
  label = UnsafeExpression $ renderSQL (PGlabel @label)

-- | >>> printSQL $ unsafeBinaryOp "OR" true false
-- (TRUE OR FALSE)
unsafeBinaryOp :: ByteString -> Operator ty0 ty1 ty2
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderSQL x <+> op <+> renderSQL y

-- | >>> printSQL $ unsafeUnaryOpL "NOT" true
-- (NOT TRUE)
unsafeUnaryOpL :: ByteString -> x :--> y
unsafeUnaryOpL op x = UnsafeExpression $ parenthesized $ op <+> renderSQL x

-- | >>> printSQL $ true & unsafeUnaryOpR "IS NOT TRUE"
-- (TRUE IS NOT TRUE)
unsafeUnaryOpR :: ByteString -> x :--> y
unsafeUnaryOpR op x = UnsafeExpression $ parenthesized $ renderSQL x <+> op

-- | >>> printSQL $ unsafeFunction "f" true
-- f(TRUE)
unsafeFunction :: ByteString -> x :--> y
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderSQL x)

-- | >>> printSQL $ unsafeFunctionN "f" (currentTime :* localTimestamp :* false *: literal 'a')
-- f(CURRENT_TIME, LOCALTIMESTAMP, FALSE, E'a')
unsafeFunctionN :: SListI xs => ByteString -> FunctionN xs y
unsafeFunctionN fun xs = UnsafeExpression $
  fun <> parenthesized (renderCommaSeparated renderSQL xs)

instance ty `In` PGNum
  => Num (Expression outer commons grp schemas params from (null ty)) where
    (+) = unsafeBinaryOp "+"
    (-) = unsafeBinaryOp "-"
    (*) = unsafeBinaryOp "*"
    abs = unsafeFunction "abs"
    signum = unsafeFunction "sign"
    fromInteger
      = UnsafeExpression
      . fromString
      . show

instance (ty `In` PGNum, ty `In` PGFloating) => Fractional
  (Expression outer commons grp schemas params from (null ty)) where
    (/) = unsafeBinaryOp "/"
    fromRational
      = UnsafeExpression
      . fromString
      . ($ "")
      . showFFloat Nothing
      . fromRat @Double

instance (ty `In` PGNum, ty `In` PGFloating) => Floating
  (Expression outer commons grp schemas params from (null ty)) where
    pi = UnsafeExpression "pi()"
    exp = unsafeFunction "exp"
    log = unsafeFunction "ln"
    sqrt = unsafeFunction "sqrt"
    b ** x = UnsafeExpression $
      "power(" <> renderSQL b <> ", " <> renderSQL x <> ")"
    logBase b y = log y / log b
    sin = unsafeFunction "sin"
    cos = unsafeFunction "cos"
    tan = unsafeFunction "tan"
    asin = unsafeFunction "asin"
    acos = unsafeFunction "acos"
    atan = unsafeFunction "atan"
    sinh x = (exp x - exp (-x)) / 2
    cosh x = (exp x + exp (-x)) / 2
    tanh x = sinh x / cosh x
    asinh x = log (x + sqrt (x*x + 1))
    acosh x = log (x + sqrt (x*x - 1))
    atanh x = log ((1 + x) / (1 - x)) / 2

class PGSubset container where
  (@>) :: Operator (null0 container) (null1 container) ('Null 'PGbool)
  (@>) = unsafeBinaryOp "@>"
  (<@) :: Operator (null0 container) (null1 container) ('Null 'PGbool)
  (<@) = unsafeBinaryOp "<@"
instance PGSubset 'PGjsonb
instance PGSubset 'PGtsquery
instance PGSubset ('PGvararray ty)

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

{-----------------------------------------
text
-----------------------------------------}

instance IsString
  (Expression outer commons grp schemas params from (null 'PGtext)) where
    fromString str = UnsafeExpression $
      "E\'" <> fromString (escape =<< str) <> "\'"
instance IsString
  (Expression outer commons grp schemas params from (null 'PGtsvector)) where
    fromString str = cast tsvector . UnsafeExpression $
      "E\'" <> fromString (escape =<< str) <> "\'"
instance IsString
  (Expression outer commons grp schemas params from (null 'PGtsquery)) where
    fromString str = cast tsquery . UnsafeExpression $
      "E\'" <> fromString (escape =<< str) <> "\'"

instance Semigroup
  (Expression outer commons grp schemas params from (null ('PGvararray ty))) where
    (<>) = unsafeBinaryOp "||"
instance Semigroup
  (Expression outer commons grp schemas params from (null 'PGjsonb)) where
    (<>) = unsafeBinaryOp "||"
instance Semigroup
  (Expression outer commons grp schemas params from (null 'PGtext)) where
    (<>) = unsafeBinaryOp "||"
instance Semigroup
  (Expression outer commons grp schemas params from (null 'PGtsvector)) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression outer commons grp schemas params from (null 'PGtext)) where
    mempty = fromString ""
    mappend = (<>)
instance Monoid
  (Expression outer commons grp schemas params from (null 'PGtsvector)) where
    mempty = fromString ""
    mappend = (<>)

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
timestampWithTimeZone :: TypeExpression schemas (null 'PGtimestamptz)
timestampWithTimeZone = UnsafeTypeExpression "timestamp with time zone"
-- | calendar date (year, month, day)
date :: TypeExpression schemas (null 'PGdate)
date = UnsafeTypeExpression "date"
-- | time of day (no time zone)
time :: TypeExpression schemas (null 'PGtime)
time = UnsafeTypeExpression "time"
-- | time of day, including time zone
timeWithTimeZone :: TypeExpression schemas (null 'PGtimetz)
timeWithTimeZone = UnsafeTypeExpression "time with time zone"
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
      . hcollapse
      $ hcmap (SOP.Proxy @KnownNat)
        (K . fromString . show . natVal)
        (hpure SOP.Proxy :: NP SOP.Proxy ns)
tsvector :: TypeExpression schemas (null 'PGtsvector)
tsvector = UnsafeTypeExpression "tsvector"
tsquery :: TypeExpression schemas (null 'PGtsquery)
tsquery = UnsafeTypeExpression "tsquery"

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

class Literal hask where
  literal :: hask -> Expr (null (PG hask))
instance JSON.ToJSON hask => Literal (Json hask) where
  literal = cast json . UnsafeExpression
    . singleQuotedUtf8 . toStrict . JSON.encode . getJson
instance JSON.ToJSON hask => Literal (Jsonb hask) where
  literal = cast jsonb . UnsafeExpression
    . singleQuotedUtf8 . toStrict . JSON.encode . getJsonb
instance Literal Char where
  literal chr = UnsafeExpression $
    "E\'" <> fromString (escape chr) <> "\'"
instance Literal String where literal = fromString
instance Literal Int16 where literal = fromIntegral
instance Literal Int32 where literal = fromIntegral
instance Literal Int64 where literal = fromIntegral
instance Literal Float where literal = fromRational . toRational
instance Literal Double where literal = fromRational . toRational
instance Literal Text where literal = fromString . Text.unpack
instance Literal Lazy.Text where literal = fromString . Lazy.Text.unpack
instance ToParam (Enumerated enum) (PG (Enumerated enum))
  => Literal (Enumerated enum) where
    literal
      = UnsafeExpression
      . singleQuotedUtf8
      . builderBytes
      . unK
      . toParam @(Enumerated enum) @(PG (Enumerated enum))

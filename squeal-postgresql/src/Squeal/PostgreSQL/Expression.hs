{-|
Module: Squeal.PostgreSQL.Query
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
  , HasParameter (parameter)
  , param
    -- ** Null
  , null_
  , notNull
  , coalesce
  , fromNull
  , isNull
  , isNotNull
  , matchNull
  , nullIf
    -- ** Collections
  , array
  , index
  , row
  , field
    -- ** Functions
  , unsafeBinaryOp
  , unsafeUnaryOp
  , unsafeFunction
  , unsafeVariadicFunction
  , atan2_
  , cast
  , quot_
  , rem_
  , trunc
  , round_
  , ceiling_
  , greatest
  , least
    -- ** Conditions
  , true
  , false
  , not_
  , (.&&)
  , (.||)
  , Condition
  , caseWhenThenElse
  , ifThenElse
  , (.==)
  , (./=)
  , (.>=)
  , (.<)
  , (.<=)
  , (.>)
    -- ** Time
  , currentDate
  , currentTime
  , currentTimestamp
  , localTime
  , localTimestamp
    -- ** Text
  , lower
  , upper
  , charLength
  , like
  , ilike
    -- ** Json
    -- *** Json and Jsonb operators
  , (.->)
  , (.->>)
  , (.#>)
  , (.#>>)
    -- *** Jsonb operators
  , (.@>)
  , (.<@)
  , (.?)
  , (.?|)
  , (.?&)
  , (.-.)
  , (#-.)
    -- *** Functions
  , jsonLit
  , jsonbLit
  , toJson
  , toJsonb
  , arrayToJson
  , rowToJson
  , jsonBuildArray
  , jsonbBuildArray
  , jsonBuildObject
  , jsonbBuildObject
  , jsonObject
  , jsonbObject
  , jsonZipObject
  , jsonbZipObject
  , jsonArrayLength
  , jsonbArrayLength
  , jsonExtractPath
  , jsonbExtractPath
  , jsonExtractPathAsText
  , jsonbExtractPathAsText
  , jsonTypeof
  , jsonbTypeof
  , jsonStripNulls
  , jsonbStripNulls
  , jsonbSet
  , jsonbInsert
  , jsonbPretty
    -- ** Aggregation
  , Aggregate (..)
  , Distinction (..)
  , PGAvg
    -- * Window Functions
  , WindowDefinition (..)
  , partitionBy
  , WindowFunction (..)
  , rank
  , rowNumber
  , denseRank
  , percentRank
  , cumeDist
  , ntile
  , lag
  , lead
  , firstValue
  , lastValue
  , nthValue
    -- * Sorting
  , SortExpression (..)
  , OrderBy (..)
    -- * Types
  , TypeExpression (..)
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
    -- * Re-export
  , (&)
  , NP (..)
  , (:*:) (..)
  , I (..)
  , K (..)
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Function ((&))
import Data.Semigroup hiding (All)
import qualified Data.Aeson as JSON
import Data.Ratio
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.Generics ((:*:) (..))
import GHC.OverloadedLabels
import GHC.TypeLits
import Prelude hiding (id, (.))

import qualified Data.ByteString as ByteString
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

{-----------------------------------------
column expressions
-----------------------------------------}

{- | `Expression`s are used in a variety of contexts,
such as in the target list of the `Squeal.PostgreSQL.Query.select` command,
as new column values in `Squeal.PostgreSQL.Manipulation.insertRow` or
`Squeal.PostgreSQL.Manipulation.update`,
or in search `Condition`s in a number of commands.

The expression syntax allows the calculation of
values from primitive expression using arithmetic, logical,
and other operations.
-}
newtype Expression
  (outer :: FromType)
  (grp :: Grouping)
  (commons :: FromType)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (from :: FromType)
  (ty :: NullityType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (Expression outer grp commons schemas params from ty) where
  renderSQL = renderExpression

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
    -- >>> let expr = parameter @1 int4 :: Expression outer grp '[] schemas '[ 'Null 'PGint4] from ('Null 'PGint4)
    -- >>> printSQL expr
    -- ($1 :: int4)
    parameter
      :: TypeExpression schemas ty
      -> Expression outer grp commons schemas params from ty
    parameter ty = UnsafeExpression $ parenthesized $
      "$" <> renderNat @n <+> "::"
        <+> renderSQL ty
instance {-# OVERLAPPING #-} HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

-- | `param` takes a `Nat` using type application and for basic types,
-- infers a `TypeExpression`.
--
-- >>> let expr = param @1 :: Expression outer grp commons schemas '[ 'Null 'PGint4] from ('Null 'PGint4)
-- >>> printSQL expr
-- ($1 :: int4)
param
  :: forall n outer commons schemas params from grp ty
   . (PGTyped schemas ty, HasParameter n params ty)
  => Expression outer grp commons schemas params from ty -- ^ param
param = parameter @n (pgtype @schemas)

instance (HasUnique tab (Join outer from) row, Has col row ty)
  => IsLabel col (Expression outer 'Ungrouped commons schemas params from ty) where
    fromLabel = UnsafeExpression $ renderSQL (Alias @col)
instance (HasUnique tab (Join outer from) row, Has col row ty, tys ~ '[ty])
  => IsLabel col (NP (Expression outer 'Ungrouped commons schemas params from) tys) where
    fromLabel = fromLabel @col :* Nil
instance (HasUnique tab (Join outer from) row, Has col row ty, column ~ (col ::: ty))
  => IsLabel col
    (Aliased (Expression outer 'Ungrouped commons schemas params from) column) where
    fromLabel = fromLabel @col `As` Alias
instance (HasUnique tab (Join outer from) row, Has col row ty, columns ~ '[col ::: ty])
  => IsLabel col
    (NP (Aliased (Expression outer 'Ungrouped commons schemas params from)) columns) where
    fromLabel = fromLabel @col :* Nil

instance (Has tab (Join outer from) row, Has col row ty)
  => IsQualified tab col (Expression outer 'Ungrouped commons schemas params from ty) where
    tab ! col = UnsafeExpression $
      renderSQL tab <> "." <> renderSQL col
instance (Has tab (Join outer from) row, Has col row ty, tys ~ '[ty])
  => IsQualified tab col (NP (Expression outer 'Ungrouped commons schemas params from) tys) where
    tab ! col = tab ! col :* Nil
instance (Has tab (Join outer from) row, Has col row ty, column ~ (col ::: ty))
  => IsQualified tab col
    (Aliased (Expression outer 'Ungrouped commons schemas params from) column) where
    tab ! col = tab ! col `As` col
instance (Has tab (Join outer from) row, Has col row ty, columns ~ '[col ::: ty])
  => IsQualified tab col
    (NP (Aliased (Expression outer 'Ungrouped commons schemas params from)) columns) where
    tab ! col = tab ! col :* Nil

instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  ) => IsLabel col
    (Expression outer ('Grouped bys) commons schemas params from ty) where
      fromLabel = UnsafeExpression $ renderSQL (Alias @col)
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , tys ~ '[ty]
  ) => IsLabel col
    (NP (Expression outer ('Grouped bys) commons schemas params from) tys) where
      fromLabel = fromLabel @col :* Nil
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , column ~ (col ::: ty)
  ) => IsLabel col
    (Aliased (Expression outer ('Grouped bys) commons schemas params from) column) where
      fromLabel = fromLabel @col `As` Alias
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , columns ~ '[col ::: ty]
  ) => IsLabel col
    (NP (Aliased (Expression outer ('Grouped bys) commons schemas params from)) columns) where
      fromLabel = fromLabel @col :* Nil

instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  ) => IsQualified tab col
    (Expression outer ('Grouped bys) commons schemas params from ty) where
      tab ! col = UnsafeExpression $
        renderSQL tab <> "." <> renderSQL col
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , tys ~ '[ty]
  ) => IsQualified tab col
    (NP (Expression outer ('Grouped bys) commons schemas params from) tys) where
      tab ! col = tab ! col :* Nil
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , column ~ (col ::: ty)
  ) => IsQualified tab col
    (Aliased (Expression outer ('Grouped bys) commons schemas params from) column) where
      tab ! col = tab ! col `As` col
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , columns ~ '[col ::: ty]
  ) => IsQualified tab col
    (NP (Aliased (Expression outer ('Grouped bys) commons schemas params from)) columns) where
      tab ! col = tab ! col :* Nil

-- | analagous to `Nothing`
--
-- >>> printSQL null_
-- NULL
null_ :: Expression outer grp commons schemas params from ('Null ty)
null_ = UnsafeExpression "NULL"

-- | analagous to `Just`
--
-- >>> printSQL $ notNull true
-- TRUE
notNull
  :: Expression outer grp commons schemas params from ('NotNull ty)
  -> Expression outer grp commons schemas params from ('Null ty)
notNull = UnsafeExpression . renderSQL

-- | return the leftmost value which is not NULL
--
-- >>> printSQL $ coalesce [null_, true] false
-- COALESCE(NULL, TRUE, FALSE)
coalesce
  :: [Expression outer grp commons schemas params from ('Null ty)]
  -- ^ @NULL@s may be present
  -> Expression outer grp commons schemas params from ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression outer grp commons schemas params from ('NotNull ty)
coalesce nullxs notNullx = UnsafeExpression $
  "COALESCE" <> parenthesized (commaSeparated
    ((renderSQL <$> nullxs) <> [renderSQL notNullx]))

-- | analagous to `Data.Maybe.fromMaybe` using @COALESCE@
--
-- >>> printSQL $ fromNull true null_
-- COALESCE(NULL, TRUE)
fromNull
  :: Expression outer grp commons schemas params from ('NotNull ty)
  -- ^ what to convert @NULL@ to
  -> Expression outer grp commons schemas params from ('Null ty)
  -> Expression outer grp commons schemas params from ('NotNull ty)
fromNull notNullx nullx = coalesce [nullx] notNullx

-- | >>> printSQL $ null_ & isNull
-- NULL IS NULL
isNull
  :: Expression outer grp commons schemas params from ('Null ty)
  -- ^ possibly @NULL@
  -> Condition outer grp commons schemas params from
isNull x = UnsafeExpression $ renderSQL x <+> "IS NULL"

-- | >>> printSQL $ null_ & isNotNull
-- NULL IS NOT NULL
isNotNull
  :: Expression outer grp commons schemas params from ('Null ty)
  -- ^ possibly @NULL@
  -> Condition outer grp commons schemas params from
isNotNull x = UnsafeExpression $ renderSQL x <+> "IS NOT NULL"

-- | analagous to `maybe` using @IS NULL@
--
-- >>> printSQL $ matchNull true not_ null_
-- CASE WHEN NULL IS NULL THEN TRUE ELSE (NOT NULL) END
matchNull
  :: Expression outer grp commons schemas params from (nullty)
  -- ^ what to convert @NULL@ to
  -> ( Expression outer grp commons schemas params from ('NotNull ty)
       -> Expression outer grp commons schemas params from (nullty) )
  -- ^ function to perform when @NULL@ is absent
  -> Expression outer grp commons schemas params from ('Null ty)
  -> Expression outer grp commons schemas params from (nullty)
matchNull y f x = ifThenElse (isNull x) y
  (f (UnsafeExpression (renderSQL x)))

{-| right inverse to `fromNull`, if its arguments are equal then
`nullIf` gives @NULL@.

>>> :set -XTypeApplications -XDataKinds
>>> let expr = nullIf false (param @1) :: Expression outer grp commons schemas '[ 'NotNull 'PGbool] from ('Null 'PGbool)
>>> printSQL expr
NULL IF (FALSE, ($1 :: bool))
-}
nullIf
  :: Expression outer grp commons schemas params from ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression outer grp commons schemas params from ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression outer grp commons schemas params from ('Null ty)
nullIf x y = UnsafeExpression $ "NULL IF" <+> parenthesized
  (renderSQL x <> ", " <> renderSQL y)

-- | >>> printSQL $ array [null_, false, true]
-- ARRAY[NULL, FALSE, TRUE]
array
  :: [Expression outer grp commons schemas params from ty]
  -- ^ array elements
  -> Expression outer grp commons schemas params from (nullity ('PGvararray ty))
array xs = UnsafeExpression $
  "ARRAY[" <> commaSeparated (renderSQL <$> xs) <> "]"

-- | >>> printSQL $ array [null_, false, true] & index 2
-- (ARRAY[NULL, FALSE, TRUE])[2]
index
  :: Word64 -- ^ index
  -> Expression outer grp commons schemas params from (nullity ('PGvararray ty)) -- ^ array
  -> Expression outer grp commons schemas params from (NullifyType ty)
index n expr = UnsafeExpression $
  parenthesized (renderSQL expr) <> "[" <> fromString (show n) <> "]"

instance (KnownSymbol label, label `In` labels) => IsPGlabel label
  (Expression outer grp commons schemas params from (nullity ('PGenum labels))) where
  label = UnsafeExpression $ renderSQL (PGlabel @label)

-- | A row constructor is an expression that builds a row value
-- (also called a composite value) using values for its member fields.
--
-- >>> :{
-- type Complex = 'PGcomposite
--   '[ "real"      ::: 'NotNull 'PGfloat8
--    , "imaginary" ::: 'NotNull 'PGfloat8 ]
-- :}
--
-- >>> let i = row (0 `as` #real :* 1 `as` #imaginary) :: Expression outer grp commons schemas params from ('NotNull Complex)
-- >>> printSQL i
-- ROW(0, 1)
row
  :: SListI row
  => NP (Aliased (Expression outer grp commons schemas params from)) row
  -- ^ zero or more expressions for the row field values
  -> Expression outer grp commons schemas params from (nullity ('PGcomposite row))
row exprs = UnsafeExpression $ "ROW" <> parenthesized
  (renderCommaSeparated (\ (expr `As` _) -> renderSQL expr) exprs)

-- | >>> :{
-- type Complex = 'PGcomposite
--   '[ "real"      ::: 'NotNull 'PGfloat8
--    , "imaginary" ::: 'NotNull 'PGfloat8 ]
-- type Schema = '["complex" ::: 'Typedef Complex]
-- :}
--
-- >>> let i = row (0 `as` #real :* 1 `as` #imaginary) :: Expression outer grp '[] (Public Schema) from params ('NotNull Complex)
-- >>> printSQL $ i & field #complex #imaginary
-- (ROW(0, 1)::"complex")."imaginary"
field
  :: ( Has sch schemas schema
     , Has tydef schema ('Typedef ('PGcomposite row))
     , Has field row ty)
  => QualifiedAlias sch tydef -- ^ row type
  -> Alias field -- ^ field name
  -> Expression outer grp commons schemas params from ('NotNull ('PGcomposite row))
  -> Expression outer grp commons schemas params from ty
field td fld expr = UnsafeExpression $
  parenthesized (renderSQL expr <> "::" <> renderSQL td)
    <> "." <> renderSQL fld

instance Semigroup
  (Expression outer grp commons schemas params from (nullity ('PGvararray ty))) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression outer grp commons schemas params from (nullity ('PGvararray ty))) where
    mempty = array []
    mappend = (<>)

-- | >>> let expr = greatest currentTimestamp [param @1] :: Expression outer grp commons schemas '[ 'NotNull 'PGtimestamptz] from ('NotNull 'PGtimestamptz)
-- >>> printSQL expr
-- GREATEST(CURRENT_TIMESTAMP, ($1 :: timestamp with time zone))
greatest
  :: Expression outer grp commons schemas params from (nullty)
  -- ^ needs at least 1 argument
  -> [Expression outer grp commons schemas params from (nullty)]
  -- ^ or more
  -> Expression outer grp commons schemas params from (nullty)
greatest x xs = UnsafeExpression $ "GREATEST("
  <> commaSeparated (renderSQL <$> (x:xs)) <> ")"

-- | >>> printSQL $ least currentTimestamp [null_]
-- LEAST(CURRENT_TIMESTAMP, NULL)
least
  :: Expression outer grp commons schemas params from (nullty)
  -- ^ needs at least 1 argument
  -> [Expression outer grp commons schemas params from (nullty)]
  -- ^ or more
  -> Expression outer grp commons schemas params from (nullty)
least x xs = UnsafeExpression $ "LEAST("
  <> commaSeparated (renderSQL <$> (x:xs)) <> ")"

-- | >>> printSQL $ unsafeBinaryOp "OR" true false
-- (TRUE OR FALSE)
unsafeBinaryOp
  :: ByteString
  -- ^ operator
  -> Expression outer grp commons schemas params from (ty0)
  -> Expression outer grp commons schemas params from (ty1)
  -> Expression outer grp commons schemas params from (ty2)
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderSQL x <+> op <+> renderSQL y

-- | >>> printSQL $ unsafeUnaryOp "NOT" true
-- (NOT TRUE)
unsafeUnaryOp
  :: ByteString
  -- ^ operator
  -> Expression outer grp commons schemas params from (ty0)
  -> Expression outer grp commons schemas params from (ty1)
unsafeUnaryOp op x = UnsafeExpression $ parenthesized $
  op <+> renderSQL x

-- | >>> printSQL $ unsafeFunction "f" true
-- f(TRUE)
unsafeFunction
  :: ByteString
  -- ^ function
  -> Expression outer grp commons schemas params from (xty)
  -> Expression outer grp commons schemas params from (yty)
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderSQL x)

-- | Helper for defining variadic functions.
unsafeVariadicFunction
  :: SListI elems
  => ByteString
  -- ^ function
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from ret
unsafeVariadicFunction fun x = UnsafeExpression $
  fun <> parenthesized (commaSeparated (hcollapse (hmap (K . renderSQL) x)))

instance ty `In` PGNum
  => Num (Expression outer grp commons schemas params from (nullity ty)) where
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
  (Expression outer grp commons schemas params from (nullity ty)) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (ty `In` PGNum, ty `In` PGFloating) => Floating
  (Expression outer grp commons schemas params from (nullity ty)) where
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

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (nullity 'PGfloat4)
--   expression = atan2_ pi 2
-- in printSQL expression
-- :}
-- atan2(pi(), 2)
atan2_
  :: float `In` PGFloating
  => Expression outer grp commons schemas params from (nullity float)
  -- ^ numerator
  -> Expression outer grp commons schemas params from (nullity float)
  -- ^ denominator
  -> Expression outer grp commons schemas params from (nullity float)
atan2_ y x = UnsafeExpression $
  "atan2(" <> renderSQL y <> ", " <> renderSQL x <> ")"

-- When a `cast` is applied to an `Expression` of a known type, it
-- represents a run-time type conversion. The cast will succeed only if a
-- suitable type conversion operation has been defined.
--
-- | >>> printSQL $ true & cast int4
-- (TRUE :: int4)
cast
  :: TypeExpression schemas ty1
  -- ^ type to cast as
  -> Expression outer grp commons schemas params from ty0
  -- ^ value to convert
  -> Expression outer grp commons schemas params from ty1
cast ty x = UnsafeExpression $ parenthesized $
  renderSQL x <+> "::" <+> renderSQL ty

-- | integer division, truncates the result
--
-- >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (nullity 'PGint2)
--   expression = 5 `quot_` 2
-- in printSQL expression
-- :}
-- (5 / 2)
quot_
  :: int `In` PGIntegral
  => Expression outer grp commons schemas params from (nullity int)
  -- ^ numerator
  -> Expression outer grp commons schemas params from (nullity int)
  -- ^ denominator
  -> Expression outer grp commons schemas params from (nullity int)
quot_ = unsafeBinaryOp "/"

-- | remainder upon integer division
--
-- >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (nullity 'PGint2)
--   expression = 5 `rem_` 2
-- in printSQL expression
-- :}
-- (5 % 2)
rem_
  :: int `In` PGIntegral
  => Expression outer grp commons schemas params from (nullity int)
  -- ^ numerator
  -> Expression outer grp commons schemas params from (nullity int)
  -- ^ denominator
  -> Expression outer grp commons schemas params from (nullity int)
rem_ = unsafeBinaryOp "%"

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (nullity 'PGfloat4)
--   expression = trunc pi
-- in printSQL expression
-- :}
-- trunc(pi())
trunc
  :: frac `In` PGFloating
  => Expression outer grp commons schemas params from (nullity frac)
  -- ^ fractional number
  -> Expression outer grp commons schemas params from (nullity frac)
trunc = unsafeFunction "trunc"

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (nullity 'PGfloat4)
--   expression = round_ pi
-- in printSQL expression
-- :}
-- round(pi())
round_
  :: frac `In` PGFloating
  => Expression outer grp commons schemas params from (nullity frac)
  -- ^ fractional number
  -> Expression outer grp commons schemas params from (nullity frac)
round_ = unsafeFunction "round"

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (nullity 'PGfloat4)
--   expression = ceiling_ pi
-- in printSQL expression
-- :}
-- ceiling(pi())
ceiling_
  :: frac `In` PGFloating
  => Expression outer grp commons schemas params from (nullity frac)
  -- ^ fractional number
  -> Expression outer grp commons schemas params from (nullity frac)
ceiling_ = unsafeFunction "ceiling"

-- | A `Condition` is an `Expression`, which can evaluate
-- to `true`, `false` or `null_`. This is because SQL uses
-- a three valued logic.
type Condition outer grp commons schemas params from =
  Expression outer grp commons schemas params from ('Null 'PGbool)

-- | >>> printSQL true
-- TRUE
true :: Expression outer grp commons schemas params from (nullity 'PGbool)
true = UnsafeExpression "TRUE"

-- | >>> printSQL false
-- FALSE
false :: Expression outer grp commons schemas params from (nullity 'PGbool)
false = UnsafeExpression "FALSE"

-- | >>> printSQL $ not_ true
-- (NOT TRUE)
not_
  :: Expression outer grp commons schemas params from (nullity 'PGbool)
  -> Expression outer grp commons schemas params from (nullity 'PGbool)
not_ = unsafeUnaryOp "NOT"

-- | >>> printSQL $ true .&& false
-- (TRUE AND FALSE)
(.&&)
  :: Expression outer grp commons schemas params from (nullity 'PGbool)
  -> Expression outer grp commons schemas params from (nullity 'PGbool)
  -> Expression outer grp commons schemas params from (nullity 'PGbool)
infixr 3 .&&
(.&&) = unsafeBinaryOp "AND"

-- | >>> printSQL $ true .|| false
-- (TRUE OR FALSE)
(.||)
  :: Expression outer grp commons schemas params from (nullity 'PGbool)
  -> Expression outer grp commons schemas params from (nullity 'PGbool)
  -> Expression outer grp commons schemas params from (nullity 'PGbool)
infixr 2 .||
(.||) = unsafeBinaryOp "OR"

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (nullity 'PGint2)
--   expression = caseWhenThenElse [(true, 1), (false, 2)] 3
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN 1 WHEN FALSE THEN 2 ELSE 3 END
caseWhenThenElse
  :: [ ( Condition outer grp commons schemas params from
       , Expression outer grp commons schemas params from ty
     ) ]
  -- ^ whens and thens
  -> Expression outer grp commons schemas params from ty
  -- ^ else
  -> Expression outer grp commons schemas params from ty
caseWhenThenElse whenThens else_ = UnsafeExpression $ mconcat
  [ "CASE"
  , mconcat
    [ mconcat
      [ " WHEN ", renderSQL when_
      , " THEN ", renderSQL then_
      ]
    | (when_,then_) <- whenThens
    ]
  , " ELSE ", renderSQL else_
  , " END"
  ]

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (nullity 'PGint2)
--   expression = ifThenElse true 1 0
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN 1 ELSE 0 END
ifThenElse
  :: Condition outer grp commons schemas params from
  -> Expression outer grp commons schemas params from ty -- ^ then
  -> Expression outer grp commons schemas params from ty -- ^ else
  -> Expression outer grp commons schemas params from ty
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

-- | Comparison operations like `.==`, `./=`, `.>`, `.>=`, `.<` and `.<=`
-- will produce @NULL@s if one of their arguments is @NULL@.
--
-- >>> printSQL $ true .== null_
-- (TRUE = NULL)
(.==)
  :: Expression outer grp commons schemas params from (nullity0 ty) -- ^ lhs
  -> Expression outer grp commons schemas params from (nullity1 ty) -- ^ rhs
  -> Condition outer grp commons schemas params from
(.==) = unsafeBinaryOp "="
infix 4 .==

-- | >>> printSQL $ true ./= null_
-- (TRUE <> NULL)
(./=)
  :: Expression outer grp commons schemas params from (nullity0 ty) -- ^ lhs
  -> Expression outer grp commons schemas params from (nullity1 ty) -- ^ rhs
  -> Condition outer grp commons schemas params from
(./=) = unsafeBinaryOp "<>"
infix 4 ./=

-- | >>> printSQL $ true .>= null_
-- (TRUE >= NULL)
(.>=)
  :: Expression outer grp commons schemas params from (nullity0 ty) -- ^ lhs
  -> Expression outer grp commons schemas params from (nullity1 ty) -- ^ rhs
  -> Condition outer grp commons schemas params from
(.>=) = unsafeBinaryOp ">="
infix 4 .>=

-- | >>> printSQL $ true .< null_
-- (TRUE < NULL)
(.<)
  :: Expression outer grp commons schemas params from (nullity0 ty) -- ^ lhs
  -> Expression outer grp commons schemas params from (nullity1 ty) -- ^ rhs
  -> Condition outer grp commons schemas params from
(.<) = unsafeBinaryOp "<"
infix 4 .<

-- | >>> printSQL $ true .<= null_
-- (TRUE <= NULL)
(.<=)
  :: Expression outer grp commons schemas params from (nullity0 ty) -- ^ lhs
  -> Expression outer grp commons schemas params from (nullity1 ty) -- ^ rhs
  -> Condition outer grp commons schemas params from
(.<=) = unsafeBinaryOp "<="
infix 4 .<=

-- | >>> printSQL $ true .> null_
-- (TRUE > NULL)
(.>)
  :: Expression outer grp commons schemas params from (nullity0 ty) -- ^ lhs
  -> Expression outer grp commons schemas params from (nullity1 ty) -- ^ rhs
  -> Condition outer grp commons schemas params from
(.>) = unsafeBinaryOp ">"
infix 4 .>

-- | >>> printSQL currentDate
-- CURRENT_DATE
currentDate
  :: Expression outer grp commons schemas params from (nullity 'PGdate)
currentDate = UnsafeExpression "CURRENT_DATE"

-- | >>> printSQL currentTime
-- CURRENT_TIME
currentTime
  :: Expression outer grp commons schemas params from (nullity 'PGtimetz)
currentTime = UnsafeExpression "CURRENT_TIME"

-- | >>> printSQL currentTimestamp
-- CURRENT_TIMESTAMP
currentTimestamp
  :: Expression outer grp commons schemas params from (nullity 'PGtimestamptz)
currentTimestamp = UnsafeExpression "CURRENT_TIMESTAMP"

-- | >>> printSQL localTime
-- LOCALTIME
localTime
  :: Expression outer grp commons schemas params from (nullity 'PGtime)
localTime = UnsafeExpression "LOCALTIME"

-- | >>> printSQL localTimestamp
-- LOCALTIMESTAMP
localTimestamp
  :: Expression outer grp commons schemas params from (nullity 'PGtimestamp)
localTimestamp = UnsafeExpression "LOCALTIMESTAMP"

{-----------------------------------------
text
-----------------------------------------}

instance IsString
  (Expression outer grp commons schemas params from (nullity 'PGtext)) where
    fromString str = UnsafeExpression $
      "E\'" <> fromString (escape =<< str) <> "\'"
      where
        escape = \case
          '\NUL' -> "\\0"
          '\'' -> "''"
          '"' -> "\\\""
          '\b' -> "\\b"
          '\n' -> "\\n"
          '\r' -> "\\r"
          '\t' -> "\\t"
          '\\' -> "\\\\"
          c -> [c]

instance Semigroup
  (Expression outer grp commons schemas params from (nullity 'PGtext)) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression outer grp commons schemas params from (nullity 'PGtext)) where
    mempty = fromString ""
    mappend = (<>)

-- | >>> printSQL $ lower "ARRRGGG"
-- lower(E'ARRRGGG')
lower
  :: Expression outer grp commons schemas params from (nullity 'PGtext)
  -- ^ string to lower case
  -> Expression outer grp commons schemas params from (nullity 'PGtext)
lower = unsafeFunction "lower"

-- | >>> printSQL $ upper "eeee"
-- upper(E'eeee')
upper
  :: Expression outer grp commons schemas params from (nullity 'PGtext)
  -- ^ string to upper case
  -> Expression outer grp commons schemas params from (nullity 'PGtext)
upper = unsafeFunction "upper"

-- | >>> printSQL $ charLength "four"
-- char_length(E'four')
charLength
  :: Expression outer grp commons schemas params from (nullity 'PGtext)
  -- ^ string to measure
  -> Expression outer grp commons schemas params from (nullity 'PGint4)
charLength = unsafeFunction "char_length"

-- | The `like` expression returns true if the @string@ matches
-- the supplied @pattern@. If @pattern@ does not contain percent signs
-- or underscores, then the pattern only represents the string itself;
-- in that case `like` acts like the equals operator. An underscore (_)
-- in pattern stands for (matches) any single character; a percent sign (%)
-- matches any sequence of zero or more characters.
--
-- >>> printSQL $ "abc" `like` "a%"
-- (E'abc' LIKE E'a%')
like
  :: Expression outer grp commons schemas params from (nullity 'PGtext)
  -- ^ string
  -> Expression outer grp commons schemas params from (nullity 'PGtext)
  -- ^ pattern
  -> Expression outer grp commons schemas params from (nullity 'PGbool)
like = unsafeBinaryOp "LIKE"

-- | The key word ILIKE can be used instead of LIKE to make the
-- match case-insensitive according to the active locale.
--
-- >>> printSQL $ "abc" `ilike` "a%"
-- (E'abc' ILIKE E'a%')
ilike
  :: Expression outer grp commons schemas params from (nullity 'PGtext)
  -- ^ string
  -> Expression outer grp commons schemas params from (nullity 'PGtext)
  -- ^ pattern
  -> Expression outer grp commons schemas params from (nullity 'PGbool)
ilike = unsafeBinaryOp "ILIKE"

{-----------------------------------------
 -- json and jsonb support

See https://www.postgresql.org/docs/10/static/functions-json.html -- most
comments lifted directly from this page.

Table 9.44: json and jsonb operators
-----------------------------------------}

-- | Get JSON value (object field or array element) at a key.
(.->)
  :: (json `In` PGJsonType, key `In` PGJsonKey)
  => Expression outer grp commons schemas params from (nullity json)
  -> Expression outer grp commons schemas params from (nullity key)
  -> Expression outer grp commons schemas params from ('Null json)
infixl 8 .->
(.->) = unsafeBinaryOp "->"

-- | Get JSON value (object field or array element) at a key, as text.
(.->>)
  :: (json `In` PGJsonType, key `In` PGJsonKey)
  => Expression outer grp commons schemas params from (nullity json)
  -> Expression outer grp commons schemas params from (nullity key)
  -> Expression outer grp commons schemas params from ('Null 'PGtext)
infixl 8 .->>
(.->>) = unsafeBinaryOp "->>"

-- | Get JSON value at a specified path.
(.#>)
  :: (json `In` PGJsonType, PGTextArray "(.#>)" path)
  => Expression outer grp commons schemas params from (nullity json)
  -> Expression outer grp commons schemas params from (nullity path)
  -> Expression outer grp commons schemas params from ('Null json)
infixl 8 .#>
(.#>) = unsafeBinaryOp "#>"

-- | Get JSON value at a specified path as text.
(.#>>)
  :: (json `In` PGJsonType, PGTextArray "(.#>>)" path)
  => Expression outer grp commons schemas params from (nullity json)
  -> Expression outer grp commons schemas params from (nullity path)
  -> Expression outer grp commons schemas params from ('Null 'PGtext)
infixl 8 .#>>
(.#>>) = unsafeBinaryOp "#>>"

-- Additional jsonb operators

-- | Does the left JSON value contain the right JSON path/value entries at the
-- top level?
(.@>)
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Condition outer grp commons schemas params from
infixl 9 .@>
(.@>) = unsafeBinaryOp "@>"

-- | Are the left JSON path/value entries contained at the top level within the
-- right JSON value?
(.<@)
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Condition outer grp commons schemas params from
infixl 9 .<@
(.<@) = unsafeBinaryOp "<@"

-- | Does the string exist as a top-level key within the JSON value?
(.?)
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity 'PGtext)
  -> Condition outer grp commons schemas params from
infixl 9 .?
(.?) = unsafeBinaryOp "?"

-- | Do any of these array strings exist as top-level keys?
(.?|)
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity ('PGvararray ('NotNull 'PGtext)))
  -> Condition outer grp commons schemas params from
infixl 9 .?|
(.?|) = unsafeBinaryOp "?|"

-- | Do all of these array strings exist as top-level keys?
(.?&)
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity ('PGvararray ('NotNull 'PGtext)))
  -> Condition outer grp commons schemas params from
infixl 9 .?&
(.?&) = unsafeBinaryOp "?&"

-- | Concatenate two jsonb values into a new jsonb value.
instance Semigroup
  (Expression outer grp commons schemas params from (nullity 'PGjsonb)) where
    (<>) = unsafeBinaryOp "||"

-- | Delete a key or keys from a JSON object, or remove an array element.
--
-- If the right operand is..
--
-- @ text @: Delete key/value pair or string element from left operand. Key/value pairs
-- are matched based on their key value.
--
-- @ text[] @: Delete multiple key/value pairs or string elements
-- from left operand. Key/value pairs are matched based on their key value.
--
-- @ integer @: Delete the array element with specified index (Negative integers
-- count from the end). Throws an error if top level container is not an array.
(.-.)
  :: (key `In` '[ 'PGtext, 'PGvararray ('NotNull 'PGtext), 'PGint4, 'PGint2 ]) -- hlint error without parens here
  => Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity key)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
infixl 6 .-.
(.-.) = unsafeBinaryOp "-"

-- | Delete the field or element with specified path (for JSON arrays, negative
-- integers count from the end)
(#-.)
  :: PGTextArray "(#-.)" arrayty
  => Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity arrayty)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
infixl 6 #-.
(#-.) = unsafeBinaryOp "#-"

{-----------------------------------------
Table 9.45: JSON creation functions
-----------------------------------------}

-- | Literal binary JSON
jsonbLit
  :: JSON.ToJSON x
  => x -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbLit = cast jsonb . UnsafeExpression
  . singleQuotedUtf8 . toStrict . JSON.encode

-- | Literal JSON
jsonLit
  :: JSON.ToJSON x
  => x -> Expression outer grp commons schemas params from (nullity 'PGjson)
jsonLit = cast json . UnsafeExpression
  . singleQuotedUtf8 . toStrict . JSON.encode

-- | Returns the value as json. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid json value.
toJson
  :: Expression outer grp commons schemas params from (nullity ty)
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
toJson = unsafeFunction "to_json"

-- | Returns the value as jsonb. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid jsonb value.
toJsonb
  :: Expression outer grp commons schemas params from (nullity ty)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
toJsonb = unsafeFunction "to_jsonb"

-- | Returns the array as a JSON array. A PostgreSQL multidimensional array
-- becomes a JSON array of arrays.
arrayToJson
  :: PGArray "arrayToJson" arr
  => Expression outer grp commons schemas params from (nullity arr)
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
arrayToJson = unsafeFunction "array_to_json"

-- | Returns the row as a JSON object.
rowToJson
  :: Expression outer grp commons schemas params from (nullity ('PGcomposite ty))
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
rowToJson = unsafeFunction "row_to_json"

-- | Builds a possibly-heterogeneously-typed JSON array out of a variadic
-- argument list.
jsonBuildArray
  :: SListI elems
  => NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
jsonBuildArray = unsafeVariadicFunction "json_build_array"

-- | Builds a possibly-heterogeneously-typed (binary) JSON array out of a
-- variadic argument list.
jsonbBuildArray
  :: SListI elems
  => NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbBuildArray = unsafeVariadicFunction "jsonb_build_array"

unsafeRowFunction
  :: All Top elems
  => NP (Aliased (Expression outer grp commons schemas params from)) elems
  -> [ByteString]
unsafeRowFunction =
  (`appEndo` []) . hcfoldMap (Proxy :: Proxy Top)
  (\(col `As` name) -> Endo $ \xs ->
      renderAliasString name : renderSQL col : xs)
  where
    renderAliasString :: KnownSymbol alias => Alias alias -> ByteString
    renderAliasString = singleQuotedText . fromString . symbolVal

-- | Builds a possibly-heterogeneously-typed JSON object out of a variadic
-- argument list. The elements of the argument list must alternate between text
-- and values.
jsonBuildObject
  :: All Top elems
  => NP (Aliased (Expression outer grp commons schemas params from)) elems
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
jsonBuildObject
  = unsafeFunction "json_build_object"
  . UnsafeExpression
  . commaSeparated
  . unsafeRowFunction

-- | Builds a possibly-heterogeneously-typed (binary) JSON object out of a
-- variadic argument list. The elements of the argument list must alternate
-- between text and values.
jsonbBuildObject
  :: All Top elems
  => NP (Aliased (Expression outer grp commons schemas params from)) elems
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbBuildObject
  = unsafeFunction "jsonb_build_object"
  . UnsafeExpression
  . commaSeparated
  . unsafeRowFunction

-- | Builds a JSON object out of a text array. The array must have either
-- exactly one dimension with an even number of members, in which case they are
-- taken as alternating key/value pairs, or two dimensions such that each inner
-- array has exactly two elements, which are taken as a key/value pair.
jsonObject
  :: PGArrayOf "jsonObject" arr ('NotNull 'PGtext)
  => Expression outer grp commons schemas params from (nullity arr)
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
jsonObject = unsafeFunction "json_object"

-- | Builds a binary JSON object out of a text array. The array must have either
-- exactly one dimension with an even number of members, in which case they are
-- taken as alternating key/value pairs, or two dimensions such that each inner
-- array has exactly two elements, which are taken as a key/value pair.
jsonbObject
  :: PGArrayOf "jsonbObject" arr ('NotNull 'PGtext)
  => Expression outer grp commons schemas params from (nullity arr)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbObject = unsafeFunction "jsonb_object"

-- | This is an alternate form of 'jsonObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a JSON object.
jsonZipObject
  :: ( PGArrayOf "jsonZipObject" keysArray ('NotNull 'PGtext)
     , PGArrayOf "jsonZipObject" valuesArray ('NotNull 'PGtext))
  => Expression outer grp commons schemas params from (nullity keysArray)
  -> Expression outer grp commons schemas params from (nullity valuesArray)
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
jsonZipObject ks vs =
  unsafeVariadicFunction "json_object" (ks :* vs :* Nil)

-- | This is an alternate form of 'jsonObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a binary JSON
-- object.
jsonbZipObject
  :: ( PGArrayOf "jsonbZipObject" keysArray ('NotNull 'PGtext)
     , PGArrayOf "jsonbZipObject" valuesArray ('NotNull 'PGtext))
  => Expression outer grp commons schemas params from (nullity keysArray)
  -> Expression outer grp commons schemas params from (nullity valuesArray)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbZipObject ks vs =
  unsafeVariadicFunction "jsonb_object" (ks :* vs :* Nil)

{-----------------------------------------
Table 9.46: JSON processing functions
-----------------------------------------}

-- | Returns the number of elements in the outermost JSON array.
jsonArrayLength
  :: Expression outer grp commons schemas params from (nullity 'PGjson)
  -> Expression outer grp commons schemas params from (nullity 'PGint4)
jsonArrayLength = unsafeFunction "json_array_length"

-- | Returns the number of elements in the outermost binary JSON array.
jsonbArrayLength
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity 'PGint4)
jsonbArrayLength = unsafeFunction "jsonb_array_length"

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator).
jsonExtractPath
  :: SListI elems
  => Expression outer grp commons schemas params from (nullity 'PGjson)
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonExtractPath x xs =
  unsafeVariadicFunction "json_extract_path" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator).
jsonbExtractPath
  :: SListI elems
  => Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbExtractPath x xs =
  unsafeVariadicFunction "jsonb_extract_path" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator), as text.
jsonExtractPathAsText
  :: SListI elems
  => Expression outer grp commons schemas params from (nullity 'PGjson)
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
jsonExtractPathAsText x xs =
  unsafeVariadicFunction "json_extract_path_text" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator), as text.
jsonbExtractPathAsText
  :: SListI elems
  => Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbExtractPathAsText x xs =
  unsafeVariadicFunction "jsonb_extract_path_text" (x :* xs)

-- | Returns the type of the outermost JSON value as a text string. Possible
-- types are object, array, string, number, boolean, and null.
jsonTypeof
  :: Expression outer grp commons schemas params from (nullity 'PGjson)
  -> Expression outer grp commons schemas params from (nullity 'PGtext)
jsonTypeof = unsafeFunction "json_typeof"

-- | Returns the type of the outermost binary JSON value as a text string.
-- Possible types are object, array, string, number, boolean, and null.
jsonbTypeof
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity 'PGtext)
jsonbTypeof = unsafeFunction "jsonb_typeof"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonStripNulls
  :: Expression outer grp commons schemas params from (nullity 'PGjson)
  -> Expression outer grp commons schemas params from (nullity 'PGjson)
jsonStripNulls = unsafeFunction "json_strip_nulls"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonbStripNulls
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbStripNulls = unsafeFunction "jsonb_strip_nulls"

-- | @ jsonbSet target path new_value create_missing @
--
-- Returns target with the section designated by path replaced by new_value,
-- or with new_value added if create_missing is true ( default is true) and the
-- item designated by path does not exist. As with the path orientated
-- operators, negative integers that appear in path count from the end of JSON
-- arrays.
jsonbSet
  :: PGTextArray "jsonbSet" arr
  => Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity arr)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Maybe (Expression outer grp commons schemas params from (nullity 'PGbool))
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbSet tgt path val createMissing = case createMissing of
  Just m -> unsafeVariadicFunction "jsonb_set" (tgt :* path :* val :* m :* Nil)
  Nothing -> unsafeVariadicFunction "jsonb_set" (tgt :* path :* val :* Nil)

-- | @ jsonbInsert target path new_value insert_after @
--
-- Returns target with new_value inserted. If target section designated by
-- path is in a JSONB array, new_value will be inserted before target or after
-- if insert_after is true (default is false). If target section designated by
-- path is in JSONB object, new_value will be inserted only if target does not
-- exist. As with the path orientated operators, negative integers that appear
-- in path count from the end of JSON arrays.
jsonbInsert
  :: PGTextArray "jsonbInsert" arr
  => Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity arr)
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Maybe (Expression outer grp commons schemas params from (nullity 'PGbool))
  -> Expression outer grp commons schemas params from (nullity 'PGjsonb)
jsonbInsert tgt path val insertAfter = case insertAfter of
  Just i -> unsafeVariadicFunction "jsonb_insert" (tgt :* path :* val :* i :* Nil)
  Nothing -> unsafeVariadicFunction "jsonb_insert" (tgt :* path :* val :* Nil)

-- | Returns its argument as indented JSON text.
jsonbPretty
  :: Expression outer grp commons schemas params from (nullity 'PGjsonb)
  -> Expression outer grp commons schemas params from (nullity 'PGtext)
jsonbPretty = unsafeFunction "jsonb_pretty"

{-----------------------------------------
aggregation
-----------------------------------------}

class Aggregate expr1 expr2 aggr
  | aggr -> expr1, aggr -> expr2 where

  -- | A special aggregation that does not require an input
  --
  -- >>> :{
  -- let
  --   expression :: Expression '[] ('Grouped bys) commons schemas params from ('NotNull 'PGint8)
  --   expression = countStar
  -- in printSQL expression
  -- :}
  -- count(*)
  countStar :: aggr ('NotNull 'PGint8)

  -- | >>> :{
  -- let
  --   expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: nullity ty]] ('NotNull 'PGint8)
  --   expression = count (All #col)
  -- in printSQL expression
  -- :}
  -- count(ALL "col")
  count
    :: expr1 ty
    -- ^ what to count
    -> aggr ('NotNull 'PGint8)

  -- | >>> :{
  -- let
  --   expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: 'Null 'PGnumeric]] ('Null 'PGnumeric)
  --   expression = sum_ (Distinct #col)
  -- in printSQL expression
  -- :}
  -- sum(DISTINCT "col")
  sum_
    :: ty `In` PGNum
    => expr1 (nullity ty)
    -> aggr (nullity ty)

  -- | input values, including nulls, concatenated into an array
  arrayAgg
    :: expr1 ty
    -> aggr ('NotNull ('PGvararray ty))

  -- | aggregates values as a JSON array
  jsonAgg
    :: expr1 ty
    -> aggr ('NotNull 'PGjson)

  -- | aggregates values as a JSON array
  jsonbAgg
    :: expr1 ty
    -> aggr ('NotNull 'PGjsonb)

  -- | >>> :{
  -- let
  --   expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: nullity 'PGint4]] (nullity 'PGint4)
  --   expression = bitAnd (Distinct #col)
  -- in printSQL expression
  -- :}
  -- bit_and(DISTINCT "col")
  bitAnd
    :: int `In` PGIntegral
    => expr1 (nullity int)
    -- ^ what to aggregate
    -> aggr (nullity int)

  -- | >>> :{
  -- let
  --   expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: nullity 'PGint4]] (nullity 'PGint4)
  --   expression = bitOr (All #col)
  -- in printSQL expression
  -- :}
  -- bit_or(ALL "col")
  bitOr
    :: int `In` PGIntegral
    => expr1 (nullity int)
    -- ^ what to aggregate
    -> aggr (nullity int)

  -- | >>> :{
  -- let
  --   winFun :: WindowFunction '[] 'Ungrouped commons schemas params '[tab ::: '["col" ::: nullity 'PGbool]] (nullity 'PGbool)
  --   winFun = boolAnd #col
  -- in printSQL winFun
  -- :}
  -- bool_and("col")
  boolAnd
    :: expr1 (nullity 'PGbool)
    -- ^ what to aggregate
    -> aggr (nullity 'PGbool)

  -- | >>> :{
  -- let
  --   expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: nullity 'PGbool]] (nullity 'PGbool)
  --   expression = boolOr (All #col)
  -- in printSQL expression
  -- :}
  -- bool_or(ALL "col")
  boolOr
    :: expr1(nullity 'PGbool)
    -- ^ what to aggregate
    -> aggr (nullity 'PGbool)

  -- | synonym for `boolAnd`
  --
  -- >>> :{
  -- let
  --   expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: nullity 'PGbool]] (nullity 'PGbool)
  --   expression = every (Distinct #col)
  -- in printSQL expression
  -- :}
  -- every(DISTINCT "col")
  every
    :: expr1 (nullity 'PGbool)
    -- ^ what to aggregate
    -> aggr (nullity 'PGbool)

  -- | maximum value of expression across all input values
  max_
    :: expr1 (nullity ty)
    -- ^ what to maximize
    -> aggr (nullity ty)

  -- | minimum value of expression across all input values
  min_
    :: expr1 (nullity ty)
    -- ^ what to minimize
    -> aggr (nullity ty)

  -- | average aggregation
  avg
    :: expr1 (nullity ty)
    -- ^ what to average
    -> aggr (nullity (PGAvg ty))

  {- | correlation coefficient
  >>> :{
  let
    expression :: Expression '[] ('Grouped g) c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('NotNull 'PGfloat8)
    expression = corr (All (#y :*: #x))
  in printSQL expression
  :}
  corr(ALL "y", "x")
  -}
  corr
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  {- | population covariance
  >>> :{
  let
    expression :: Expression '[] ('Grouped g) c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('NotNull 'PGfloat8)
    expression = covarPop (All (#y :*: #x))
  in printSQL expression
  :}
  covar_pop(ALL "y", "x")
  -}
  covarPop
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  {- | sample covariance
  >>> :{
  let
    winFun :: WindowFunction '[] 'Ungrouped c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('NotNull 'PGfloat8)
    winFun = covarSamp (#y :*: #x)
  in printSQL winFun
  :}
  covar_samp("y", "x")
  -}
  covarSamp
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  {- | average of the independent variable (sum(X)/N)
  >>> :{
  let
    expression :: Expression '[] ('Grouped g) c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('NotNull 'PGfloat8)
    expression = regrAvgX (All (#y :*: #x))
  in printSQL expression
  :}
  regr_avgx(ALL "y", "x")
  -}
  regrAvgX
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  {- | average of the dependent variable (sum(Y)/N)
  >>> :{
  let
    winFun :: WindowFunction '[] 'Ungrouped c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('NotNull 'PGfloat8)
    winFun = regrAvgY (#y :*: #x)
  in printSQL winFun
  :}
  regr_avgy("y", "x")
  -}
  regrAvgY
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  {- | number of input rows in which both expressions are nonnull
  >>> :{
  let
    winFun :: WindowFunction '[] 'Ungrouped c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('NotNull 'PGint8)
    winFun = regrCount (#y :*: #x)
  in printSQL winFun
  :}
  regr_count("y", "x")
  -}
  regrCount
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGint8)

  {- | y-intercept of the least-squares-fit linear equation determined by the (X, Y) pairs
  >>> :{
  let
    expression :: Expression '[] ('Grouped g) c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('NotNull 'PGfloat8)
    expression = regrIntercept (All (#y :*: #x))
  in printSQL expression
  :}
  regr_intercept(ALL "y", "x")
  -}
  regrIntercept
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  -- | @regr_r2(Y, X)@, square of the correlation coefficient
  regrR2
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  -- | @regr_slope(Y, X)@, slope of the least-squares-fit linear equation
  -- determined by the (X, Y) pairs
  regrSlope
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  -- | @regr_sxx(Y, X)@, sum(X^2) - sum(X)^2/N
  -- (“sum of squares” of the independent variable)
  regrSxx
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  -- | @regr_sxy(Y, X)@, sum(X*Y) - sum(X) * sum(Y)/N
  -- (“sum of products” of independent times dependent variable)
  regrSxy
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  -- | @regr_syy(Y, X)@, sum(Y^2) - sum(Y)^2/N
  -- (“sum of squares” of the dependent variable)
  regrSyy
    :: expr2 (nullity 'PGfloat8)
    -> aggr (nullity 'PGfloat8)

  -- | historical alias for `stddevSamp`
  stddev
    :: expr1 (nullity ty)
    -> aggr (nullity (PGAvg ty))

  -- | population standard deviation of the input values
  stddevPop
    :: expr1 (nullity ty)
    -> aggr (nullity (PGAvg ty))

  -- | sample standard deviation of the input values
  stddevSamp
    :: expr1 (nullity ty)
    -> aggr (nullity (PGAvg ty))

  -- | historical alias for `varSamp`
  variance
    :: expr1 (nullity ty)
    -> aggr (nullity (PGAvg ty))

  -- | population variance of the input values
  -- (square of the population standard deviation)
  varPop
    :: expr1 (nullity ty)
    -> aggr (nullity (PGAvg ty))

  -- | sample variance of the input values
  -- (square of the sample standard deviation)
  varSamp
    :: expr1 (nullity ty)
    -> aggr (nullity (PGAvg ty))

data Distinction expr ty
  = All (expr ty)
  | Distinct (expr ty)
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData (Distinction (Expression outer grp commons schemas params from) ty)
instance RenderSQL (Distinction (Expression outer grp commons schemas params from) ty) where
  renderSQL = \case
    All x -> "ALL" <+> renderSQL x
    Distinct x -> "DISTINCT" <+> renderSQL x
instance RenderSQL
  (Distinction
    ( Expression outer grp commons schemas params from
      :*:
      Expression outer grp commons schemas params from) xty ) where
        renderSQL = \case
          All (x :*: x') ->
            "ALL" <+> renderSQL x <> ", " <> renderSQL x'
          Distinct (x :*: x') ->
            "DISTINCT" <+> renderSQL x <> ", " <> renderSQL x'

instance Aggregate
  (Distinction (Expression outer 'Ungrouped commons schemas params from))
  (Distinction (Expression outer 'Ungrouped commons schemas params from :*: Expression outer 'Ungrouped commons schemas params from))
  (Expression outer ('Grouped bys) commons schemas params from) where
    countStar = UnsafeExpression "count(*)"
    count = unsafeAggregate1 "count"
    sum_ = unsafeAggregate1 "sum"
    arrayAgg = unsafeAggregate1 "array_agg"
    jsonAgg = unsafeAggregate1 "json_agg"
    jsonbAgg = unsafeAggregate1 "jsonb_agg"
    bitAnd = unsafeAggregate1 "bit_and"
    bitOr = unsafeAggregate1 "bit_or"
    boolAnd = unsafeAggregate1 "bool_and"
    boolOr = unsafeAggregate1 "bool_or"
    every = unsafeAggregate1 "every"
    max_ = unsafeAggregate1 "max"
    min_ = unsafeAggregate1 "min"
    avg = unsafeAggregate1 "avg"
    corr = unsafeAggregate2 "corr"
    covarPop = unsafeAggregate2 "covar_pop"
    covarSamp = unsafeAggregate2 "covar_samp"
    regrAvgX = unsafeAggregate2 "regr_avgx"
    regrAvgY = unsafeAggregate2 "regr_avgy"
    regrCount = unsafeAggregate2 "regr_count"
    regrIntercept = unsafeAggregate2 "regr_intercept"
    regrR2 = unsafeAggregate2 "regr_r2"
    regrSlope = unsafeAggregate2 "regr_slope"
    regrSxx = unsafeAggregate2 "regr_sxx"
    regrSxy = unsafeAggregate2 "regr_sxy"
    regrSyy = unsafeAggregate2 "regr_syy"
    stddev = unsafeAggregate1 "stddev"
    stddevPop = unsafeAggregate1 "stddev_pop"
    stddevSamp = unsafeAggregate1 "stddev_samp"
    variance = unsafeAggregate1 "variance"
    varPop = unsafeAggregate1 "var_pop"
    varSamp = unsafeAggregate1 "var_samp"
instance Aggregate
  (Expression outer grp commons schemas params from)
  (Expression outer grp commons schemas params from :*: Expression outer grp commons schemas params from)
  (WindowFunction outer grp commons schemas params from) where
    countStar = UnsafeWindowFunction "count(*)"
    count = unsafeWindowFunction1 "count"
    sum_ = unsafeWindowFunction1 "sum"
    arrayAgg = unsafeWindowFunction1 "array_agg"
    jsonAgg = unsafeWindowFunction1 "json_agg"
    jsonbAgg = unsafeWindowFunction1 "jsonb_agg"
    bitAnd = unsafeWindowFunction1 "bit_and"
    bitOr = unsafeWindowFunction1 "bit_or"
    boolAnd = unsafeWindowFunction1 "bool_and"
    boolOr = unsafeWindowFunction1 "bool_or"
    every = unsafeWindowFunction1 "every"
    max_ = unsafeWindowFunction1 "max"
    min_ = unsafeWindowFunction1 "min"
    avg = unsafeWindowFunction1 "avg"
    corr = unsafeWindowFunction2 "corr"
    covarPop = unsafeWindowFunction2 "covar_pop"
    covarSamp = unsafeWindowFunction2 "covar_samp"
    regrAvgX = unsafeWindowFunction2 "regr_avgx"
    regrAvgY = unsafeWindowFunction2 "regr_avgy"
    regrCount = unsafeWindowFunction2 "regr_count"
    regrIntercept = unsafeWindowFunction2 "regr_intercept"
    regrR2 = unsafeWindowFunction2 "regr_r2"
    regrSlope = unsafeWindowFunction2 "regr_slope"
    regrSxx = unsafeWindowFunction2 "regr_sxx"
    regrSxy = unsafeWindowFunction2 "regr_sxy"
    regrSyy = unsafeWindowFunction2 "regr_syy"
    stddev = unsafeWindowFunction1 "stddev"
    stddevPop = unsafeWindowFunction1 "stddev_pop"
    stddevSamp = unsafeWindowFunction1 "stddev_samp"
    variance = unsafeWindowFunction1 "variance"
    varPop = unsafeWindowFunction1 "var_pop"
    varSamp = unsafeWindowFunction1 "var_samp"

-- | escape hatch to define aggregate functions
unsafeAggregate1
  :: ByteString -- ^ aggregate function
  -> Distinction (Expression outer 'Ungrouped commons schemas params from) xty
  -> Expression outer ('Grouped bys) commons schemas params from yty
unsafeAggregate1 fun x = UnsafeExpression $ mconcat
  [fun, "(", renderSQL x, ")"]

-- | escape hatch to define aggregate binary functions
unsafeAggregate2
  :: ByteString -- ^ aggregate function
  -> Distinction (Expression outer 'Ungrouped commons schemas params from :*: Expression outer 'Ungrouped commons schemas params from) xty
  -> Expression outer ('Grouped bys) commons schemas params from yty
unsafeAggregate2 fun xx = UnsafeExpression $ mconcat
  [fun, "(", renderSQL xx, ")"]

-- | A type family that calculates `PGAvg` type of a `PGType`.
type family PGAvg ty where
  PGAvg 'PGint2 = 'PGnumeric
  PGAvg 'PGint4 = 'PGnumeric
  PGAvg 'PGint8 = 'PGnumeric
  PGAvg 'PGnumeric = 'PGnumeric
  PGAvg 'PGfloat4 = 'PGfloat8
  PGAvg 'PGfloat8 = 'PGfloat8
  PGAvg 'PGinterval = 'PGinterval
  PGAvg pg = TypeError
    ('Text "Squeal type error: No average for " ':<>: 'ShowType pg)

{-----------------------------------------
type expressions
-----------------------------------------}

-- | `TypeExpression`s are used in `cast`s and `createTable` commands.
newtype TypeExpression (schemas :: SchemasType) (ty :: NullityType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (TypeExpression schemas ty) where
  renderSQL = renderTypeExpression

-- | The enum or composite type in a `Typedef` can be expressed by its alias.
typedef
  :: (Has sch schemas schema, Has td schema ('Typedef ty))
  => QualifiedAlias sch td
  -> TypeExpression schemas (nullity ty)
typedef = UnsafeTypeExpression . renderSQL

-- | The composite type corresponding to a `Table` definition can be expressed
-- by its alias.
typetable
  :: (Has sch schemas schema, Has tab schema ('Table table))
  => QualifiedAlias sch tab
  -> TypeExpression schemas (nullity ('PGcomposite (TableToRow table)))
typetable = UnsafeTypeExpression . renderSQL

-- | The composite type corresponding to a `View` definition can be expressed
-- by its alias.
typeview
  :: (Has sch schemas schema, Has vw schema ('View view))
  => QualifiedAlias sch vw
  -> TypeExpression schemas (nullity ('PGcomposite view))
typeview = UnsafeTypeExpression . renderSQL

-- | logical Boolean (true/false)
bool :: TypeExpression schemas (nullity 'PGbool)
bool = UnsafeTypeExpression "bool"
-- | signed two-byte integer
int2, smallint :: TypeExpression schemas (nullity 'PGint2)
int2 = UnsafeTypeExpression "int2"
smallint = UnsafeTypeExpression "smallint"
-- | signed four-byte integer
int4, int, integer :: TypeExpression schemas (nullity 'PGint4)
int4 = UnsafeTypeExpression "int4"
int = UnsafeTypeExpression "int"
integer = UnsafeTypeExpression "integer"
-- | signed eight-byte integer
int8, bigint :: TypeExpression schemas (nullity 'PGint8)
int8 = UnsafeTypeExpression "int8"
bigint = UnsafeTypeExpression "bigint"
-- | arbitrary precision numeric type
numeric :: TypeExpression schemas (nullity 'PGnumeric)
numeric = UnsafeTypeExpression "numeric"
-- | single precision floating-point number (4 bytes)
float4, real :: TypeExpression schemas (nullity 'PGfloat4)
float4 = UnsafeTypeExpression "float4"
real = UnsafeTypeExpression "real"
-- | double precision floating-point number (8 bytes)
float8, doublePrecision :: TypeExpression schemas (nullity 'PGfloat8)
float8 = UnsafeTypeExpression "float8"
doublePrecision = UnsafeTypeExpression "double precision"
-- | variable-length character string
text :: TypeExpression schemas (nullity 'PGtext)
text = UnsafeTypeExpression "text"
-- | fixed-length character string
char, character
  :: forall n schemas nullity. (KnownNat n, 1 <= n)
  => TypeExpression schemas (nullity ('PGchar n))
char = UnsafeTypeExpression $ "char(" <> renderNat @n <> ")"
character = UnsafeTypeExpression $  "character(" <> renderNat @n <> ")"
-- | variable-length character string
varchar, characterVarying
  :: forall n schemas nullity. (KnownNat n, 1 <= n)
  => TypeExpression schemas (nullity ('PGvarchar n))
varchar = UnsafeTypeExpression $ "varchar(" <> renderNat @n <> ")"
characterVarying = UnsafeTypeExpression $
  "character varying(" <> renderNat @n <> ")"
-- | binary data ("byte array")
bytea :: TypeExpression schemas (nullity 'PGbytea)
bytea = UnsafeTypeExpression "bytea"
-- | date and time (no time zone)
timestamp :: TypeExpression schemas (nullity 'PGtimestamp)
timestamp = UnsafeTypeExpression "timestamp"
-- | date and time, including time zone
timestampWithTimeZone :: TypeExpression schemas (nullity 'PGtimestamptz)
timestampWithTimeZone = UnsafeTypeExpression "timestamp with time zone"
-- | calendar date (year, month, day)
date :: TypeExpression schemas (nullity 'PGdate)
date = UnsafeTypeExpression "date"
-- | time of day (no time zone)
time :: TypeExpression schemas (nullity 'PGtime)
time = UnsafeTypeExpression "time"
-- | time of day, including time zone
timeWithTimeZone :: TypeExpression schemas (nullity 'PGtimetz)
timeWithTimeZone = UnsafeTypeExpression "time with time zone"
-- | time span
interval :: TypeExpression schemas (nullity 'PGinterval)
interval = UnsafeTypeExpression "interval"
-- | universally unique identifier
uuid :: TypeExpression schemas (nullity 'PGuuid)
uuid = UnsafeTypeExpression "uuid"
-- | IPv4 or IPv6 host address
inet :: TypeExpression schemas (nullity 'PGinet)
inet = UnsafeTypeExpression "inet"
-- | textual JSON data
json :: TypeExpression schemas (nullity 'PGjson)
json = UnsafeTypeExpression "json"
-- | binary JSON data, decomposed
jsonb :: TypeExpression schemas (nullity 'PGjsonb)
jsonb = UnsafeTypeExpression "jsonb"
-- | variable length array
vararray
  :: TypeExpression schemas pg
  -> TypeExpression schemas (nullity ('PGvararray pg))
vararray ty = UnsafeTypeExpression $ renderSQL ty <> "[]"
-- | fixed length array
--
-- >>> renderSQL (fixarray @'[2] json)
-- "json[2]"
fixarray
  :: forall dims schemas nullity pg. All KnownNat dims
  => TypeExpression schemas pg
  -> TypeExpression schemas (nullity ('PGfixarray dims pg))
fixarray ty = UnsafeTypeExpression $
  renderSQL ty <> renderDims @dims
  where
    renderDims :: forall ns. All KnownNat ns => ByteString
    renderDims =
      ("[" <>)
      . (<> "]")
      . ByteString.intercalate "]["
      . hcollapse
      $ hcmap (Proxy @KnownNat)
        (K . fromString . show . natVal)
        (hpure Proxy :: NP Proxy ns)

-- | `pgtype` is a demoted version of a `PGType`
class PGTyped schemas (ty :: NullityType) where
  pgtype :: TypeExpression schemas ty
instance PGTyped schemas (nullity 'PGbool) where pgtype = bool
instance PGTyped schemas (nullity 'PGint2) where pgtype = int2
instance PGTyped schemas (nullity 'PGint4) where pgtype = int4
instance PGTyped schemas (nullity 'PGint8) where pgtype = int8
instance PGTyped schemas (nullity 'PGnumeric) where pgtype = numeric
instance PGTyped schemas (nullity 'PGfloat4) where pgtype = float4
instance PGTyped schemas (nullity 'PGfloat8) where pgtype = float8
instance PGTyped schemas (nullity 'PGtext) where pgtype = text
instance (KnownNat n, 1 <= n)
  => PGTyped schemas (nullity ('PGchar n)) where pgtype = char @n
instance (KnownNat n, 1 <= n)
  => PGTyped schemas (nullity ('PGvarchar n)) where pgtype = varchar @n
instance PGTyped schemas (nullity 'PGbytea) where pgtype = bytea
instance PGTyped schemas (nullity 'PGtimestamp) where pgtype = timestamp
instance PGTyped schemas (nullity 'PGtimestamptz) where pgtype = timestampWithTimeZone
instance PGTyped schemas (nullity 'PGdate) where pgtype = date
instance PGTyped schemas (nullity 'PGtime) where pgtype = time
instance PGTyped schemas (nullity 'PGtimetz) where pgtype = timeWithTimeZone
instance PGTyped schemas (nullity 'PGinterval) where pgtype = interval
instance PGTyped schemas (nullity 'PGuuid) where pgtype = uuid
instance PGTyped schemas (nullity 'PGjson) where pgtype = json
instance PGTyped schemas (nullity 'PGjsonb) where pgtype = jsonb
instance PGTyped schemas ty
  => PGTyped schemas (nullity ('PGvararray ty)) where
    pgtype = vararray (pgtype @schemas @ty)
instance (All KnownNat dims, PGTyped schemas ty)
  => PGTyped schemas (nullity ('PGfixarray dims ty)) where
    pgtype = fixarray @dims (pgtype @schemas @ty)

{-----------------------------------------
Sorting
-----------------------------------------}

-- | `SortExpression`s are used by `sortBy` to optionally sort the results
-- of a `Query`. `Asc` or `Desc` set the sort direction of a `NotNull` result
-- column to ascending or descending. Ascending order puts smaller values
-- first, where "smaller" is defined in terms of the `.<` operator. Similarly,
-- descending order is determined with the `.>` operator. `AscNullsFirst`,
-- `AscNullsLast`, `DescNullsFirst` and `DescNullsLast` options are used to
-- determine whether nulls appear before or after non-null values in the sort
-- ordering of a `Null` result column.
data SortExpression outer grp commons schemas params from where
    Asc
      :: Expression outer grp commons schemas params from ('NotNull ty)
      -> SortExpression outer grp commons schemas params from
    Desc
      :: Expression outer grp commons schemas params from ('NotNull ty)
      -> SortExpression outer grp commons schemas params from
    AscNullsFirst
      :: Expression outer grp commons schemas params from  ('Null ty)
      -> SortExpression outer grp commons schemas params from
    AscNullsLast
      :: Expression outer grp commons schemas params from  ('Null ty)
      -> SortExpression outer grp commons schemas params from
    DescNullsFirst
      :: Expression outer grp commons schemas params from  ('Null ty)
      -> SortExpression outer grp commons schemas params from
    DescNullsLast
      :: Expression outer grp commons schemas params from  ('Null ty)
      -> SortExpression outer grp commons schemas params from
deriving instance Show (SortExpression outer grp commons schemas params from)

class OrderBy expr where
  orderBy
    :: [SortExpression outer grp commons schemas params from]
    -> expr outer grp commons schemas params from
    -> expr outer grp commons schemas params from

-- | Render a `SortExpression`.
instance RenderSQL (SortExpression outer grp commons schemas params from) where
  renderSQL = \case
    Asc expression -> renderSQL expression <+> "ASC"
    Desc expression -> renderSQL expression <+> "DESC"
    AscNullsFirst expression -> renderSQL expression
      <+> "ASC NULLS FIRST"
    DescNullsFirst expression -> renderSQL expression
      <+> "DESC NULLS FIRST"
    AscNullsLast expression -> renderSQL expression <+> "ASC NULLS LAST"
    DescNullsLast expression -> renderSQL expression <+> "DESC NULLS LAST"

data WindowDefinition outer grp commons schemas params from where
  WindowDefinition
    :: SListI bys
    => NP (Expression outer grp commons schemas params from) bys
    -> [SortExpression outer grp commons schemas params from]
    -> WindowDefinition outer grp commons schemas params from

instance OrderBy WindowDefinition where
  orderBy sortsR (WindowDefinition parts sortsL)
    = WindowDefinition parts (sortsL ++ sortsR)

instance RenderSQL (WindowDefinition outer commons schemas from grp params) where
  renderSQL (WindowDefinition part ord) =
    renderPartitionByClause part <> renderOrderByClause ord
    where
      renderPartitionByClause = \case
        Nil -> ""
        parts -> "PARTITION" <+> "BY" <+> renderCommaSeparated renderExpression parts
      renderOrderByClause = \case
        [] -> ""
        srts -> " ORDER" <+> "BY"
          <+> commaSeparated (renderSQL <$> srts)

partitionBy
  :: SListI bys
  => NP (Expression outer grp commons schemas params from) bys
  -> WindowDefinition outer grp commons schemas params from
partitionBy bys = WindowDefinition bys []

newtype WindowFunction
  (outer :: FromType)
  (grp :: Grouping)
  (commons :: FromType)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (from :: FromType)
  (ty :: NullityType)
    = UnsafeWindowFunction { renderWindowFunction :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (WindowFunction outer grp commons schemas params from ty) where
  renderSQL = renderWindowFunction

unsafeWindowFunction1
  :: ByteString
  -> Expression outer grp commons schemas params from ty0
  -> WindowFunction outer grp commons schemas params from ty1
unsafeWindowFunction1 fun x
  = UnsafeWindowFunction $ fun <> parenthesized (renderSQL x)

unsafeWindowFunction2
  :: ByteString
  -> (Expression outer grp commons schemas params from :*: Expression outer grp commons schemas params from) ty0
  -> WindowFunction outer grp commons schemas params from ty1
unsafeWindowFunction2 fun (x :*: x')
  = UnsafeWindowFunction $ fun <> parenthesized (renderSQL x <> ", " <> renderSQL x')

{- | rank of the current row with gaps; same as `rowNumber` of its first peer
>>> printSQL rank
rank()
-}
rank :: WindowFunction outer grp commons schemas params from ('NotNull 'PGint8)
rank = UnsafeWindowFunction "rank()"

{- | number of the current row within its partition, counting from 1
>>> printSQL rowNumber
row_number()
-}
rowNumber :: WindowFunction outer grp commons schemas params from ('NotNull 'PGint8)
rowNumber = UnsafeWindowFunction "row_number()"

{- | rank of the current row without gaps; this function counts peer groups
>>> printSQL denseRank
dense_rank()
-}
denseRank :: WindowFunction outer grp commons schemas params from ('NotNull 'PGint8)
denseRank = UnsafeWindowFunction "dense_rank()"

{- | relative rank of the current row: (rank - 1) / (total partition rows - 1)
>>> printSQL percentRank
percent_rank()
-}
percentRank :: WindowFunction outer grp commons schemas params from ('NotNull 'PGfloat8)
percentRank = UnsafeWindowFunction "percent_rank()"

{- | cumulative distribution: (number of partition rows
preceding or peer with current row) / total partition rows
>>> printSQL cumeDist
cume_dist()
-}
cumeDist :: WindowFunction outer grp commons schemas params from ('NotNull 'PGfloat8)
cumeDist = UnsafeWindowFunction "cume_dist()"

{- | integer ranging from 1 to the argument value,
dividing the partition as equally as possible
-}
ntile
  :: Expression outer grp commons schemas params from ('NotNull 'PGint4)
  -- ^ num buckets
  -> WindowFunction outer grp commons schemas params from ('NotNull 'PGint4)
ntile = unsafeWindowFunction1 "ntile"

{- | returns value evaluated at the row that is offset rows before the current
row within the partition; if there is no such row, instead return default
(which must be of the same type as value). Both offset and default are
evaluated with respect to the current row.
-}
lag
  :: Expression outer grp commons schemas params from ty
  -- ^ value
  -> Expression outer grp commons schemas params from ('NotNull 'PGint4)
  -- ^ offset
  -> Expression outer grp commons schemas params from ty
  -- ^ default
  -> WindowFunction outer grp commons schemas params from ty
lag value offset def = UnsafeWindowFunction $ "lag"
  <> parenthesized
  (commaSeparated ([renderSQL value, renderSQL offset, renderSQL def]))

{- | returns value evaluated at the row that is offset rows after the current
row within the partition; if there is no such row, instead return default
(which must be of the same type as value). Both offset and default are
evaluated with respect to the current row.
-}
lead
  :: Expression outer grp commons schemas params from ty
  -- ^ value
  -> Expression outer grp commons schemas params from ('NotNull 'PGint4)
  -- ^ offset
  -> Expression outer grp commons schemas params from ty
  -- ^ default
  -> WindowFunction outer grp commons schemas params from ty
lead value offset def = UnsafeWindowFunction $ "lag"
  <> parenthesized
  (commaSeparated ([renderSQL value, renderSQL offset, renderSQL def]))

{- | returns value evaluated at the row that is the
first row of the window frame
-}
firstValue
  :: Expression outer grp commons schemas params from ty
  -- ^ value
  -> WindowFunction outer grp commons schemas params from ty
firstValue = unsafeWindowFunction1 "first_value"

{- | returns value evaluated at the row that is the
last row of the window frame
-}
lastValue
  :: Expression outer grp commons schemas params from ty
  -- ^ value
  -> WindowFunction outer grp commons schemas params from ty
lastValue = unsafeWindowFunction1 "last_value"

{- | returns value evaluated at the row that is the nth
row of the window frame (counting from 1); null if no such row
-}
nthValue
  :: Expression outer grp commons schemas params from (nullity ty)
  -- ^ value
  -> Expression outer grp commons schemas params from ('NotNull 'PGint4)
  -- ^ nth
  -> WindowFunction outer grp commons schemas params from ('Null ty)
nthValue value nth = UnsafeWindowFunction $ "nth_value"
  <> parenthesized (commaSeparated [renderSQL value, renderSQL nth])

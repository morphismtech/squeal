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
  , Operator
  , (:-->)
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
  , cardinality
  , index
  , row
  , field
  , PGSubset (..)
    -- ** Functions
  , unsafeBinaryOp
  , unsafeUnaryOpL
  , unsafeUnaryOpR
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
  , litChar
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
  , between
  , notBetween
  , betweenSymmetric
  , notBetweenSymmetric
  , isDistinctFrom
  , isNotDistinctFrom
  , isTrue
  , isNotTrue
  , isFalse
  , isNotFalse
  , isUnknown
  , isNotUnknown
    -- ** Time
  , currentDate
  , currentTime
  , currentTimestamp
  , localTime
  , localTimestamp
  , TimeOp (..)
  , makeDate
  , makeTime
  , makeTimestamp
  , makeTimestamptz
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
    -- ** Text Search
  , (@@)
  , (.&)
  , (.|)
  , (.!)
  , (<->)
  , arrayToTSvector
  , tsvectorLength
  , numnode
  , plainToTSquery
  , phraseToTSquery
  , websearchToTSquery
  , queryTree
  , toTSquery
  , toTSvector
  , setWeight
  , strip
  , jsonToTSvector
  , jsonbToTSvector
  , tsDelete
  , tsFilter
  , tsHeadline
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
  , NP (..)
  , K (..)
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Function ((&))
import Data.Kind
import Data.Semigroup hiding (All)
import qualified Data.Aeson as JSON
import Data.String
import Data.Word
import Generics.SOP hiding (All, from)
import GHC.OverloadedLabels
import GHC.TypeLits
import Numeric
import Prelude hiding (id, (.))

import qualified Data.ByteString as ByteString
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

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

-- | An `Expr` is a closed `Expression`.
type Expr x
  = forall outer grp commons schemas params from
  . Expression outer grp commons schemas params from x
    -- ^ cannot reference aliases

type Operator x1 x2 y
  =  forall outer grp commons schemas params from
  .  Expression outer grp commons schemas params from x1
     -- ^ left input
  -> Expression outer grp commons schemas params from x2
     -- ^ right input
  -> Expression outer grp commons schemas params from y
     -- ^ output

type (:-->) x y
  =  forall outer grp commons schemas params from
  .  Expression outer grp commons schemas params from x
     -- ^ input
  -> Expression outer grp commons schemas params from y
     -- ^ output

type FunctionHet xs y
  =  forall outer grp commons schemas params from
  .  NP (Expression outer grp commons schemas params from) xs
     -- ^ inputs
  -> Expression outer grp commons schemas params from y
     -- ^ output

type FunctionHom x0 x1 y
  =  forall outer grp commons schemas params from
  .  [Expression outer grp commons schemas params from x0]
     -- ^ inputs
  -> Expression outer grp commons schemas params from x1
     -- ^ must have at least 1 input
  -> Expression outer grp commons schemas params from y
     -- ^ output

unsafeFunctionHom :: ByteString -> FunctionHom x0 x1 y
unsafeFunctionHom fun xs x = UnsafeExpression $ fun <> parenthesized
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
null_ :: Expr ('Null ty)
null_ = UnsafeExpression "NULL"

-- | analagous to `Just`
--
-- >>> printSQL $ notNull true
-- TRUE
notNull :: 'NotNull ty :--> 'Null ty
notNull = UnsafeExpression . renderSQL

-- | return the leftmost value which is not NULL
--
-- >>> printSQL $ coalesce [null_, true] false
-- COALESCE(NULL, TRUE, FALSE)
coalesce :: FunctionHom ('Null ty) ('NotNull ty) ('NotNull ty)
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
isNull :: 'Null ty :--> null 'PGbool
isNull x = UnsafeExpression $ renderSQL x <+> "IS NULL"

-- | >>> printSQL $ null_ & isNotNull
-- NULL IS NOT NULL
isNotNull :: 'Null ty :--> null 'PGbool
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
>>> let expr = nullIf (false *: param @1) :: Expression outer grp commons schemas '[ 'NotNull 'PGbool] from ('Null 'PGbool)
>>> printSQL expr
NULLIF(FALSE, ($1 :: bool))
-}
nullIf :: FunctionHet '[ 'NotNull ty, 'NotNull ty] ('Null ty)
nullIf = unsafeFunctionHet "NULLIF"

-- | >>> printSQL $ array [null_, false, true]
-- ARRAY[NULL, FALSE, TRUE]
array
  :: [Expression outer grp commons schemas params from ty]
  -- ^ array elements
  -> Expression outer grp commons schemas params from (null ('PGvararray ty))
array xs = UnsafeExpression $
  "ARRAY[" <> commaSeparated (renderSQL <$> xs) <> "]"

-- | >>> printSQL $ cardinality (array [null_, false, true])
-- cardinality(ARRAY[NULL, FALSE, TRUE])
cardinality :: null ('PGvararray ty) :--> null 'PGint8
cardinality = unsafeFunction "cardinality"

-- | >>> printSQL $ array [null_, false, true] & index 2
-- (ARRAY[NULL, FALSE, TRUE])[2]
index
  :: Word64 -- ^ index
  -> null ('PGvararray ty) :--> NullifyType ty
index n expr = UnsafeExpression $
  parenthesized (renderSQL expr) <> "[" <> fromString (show n) <> "]"

instance (KnownSymbol label, label `In` labels) => IsPGlabel label
  (Expression outer grp commons schemas params from (null ('PGenum labels))) where
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
  -> Expression outer grp commons schemas params from (null ('PGcomposite row))
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
  (Expression outer grp commons schemas params from (null ('PGvararray ty))) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression outer grp commons schemas params from (null ('PGvararray ty))) where
    mempty = array []
    mappend = (<>)

-- | >>> let expr = greatest [param @1] currentTimestamp :: Expression outer grp commons schemas '[ 'NotNull 'PGtimestamptz] from ('NotNull 'PGtimestamptz)
-- >>> printSQL expr
-- GREATEST(($1 :: timestamp with time zone), CURRENT_TIMESTAMP)
greatest :: FunctionHom ty ty ty
greatest = unsafeFunctionHom "GREATEST"

-- | >>> printSQL $ least [null_] currentTimestamp
-- LEAST(NULL, CURRENT_TIMESTAMP)
least :: FunctionHom ty ty ty
least = unsafeFunctionHom "LEAST"

-- | >>> printSQL $ unsafeBinaryOp "OR" true false
-- (TRUE OR FALSE)
unsafeBinaryOp :: ByteString {- ^ operator -} -> Operator ty0 ty1 ty2
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderSQL x <+> op <+> renderSQL y

-- | >>> printSQL $ unsafeUnaryOpL "NOT" true
-- (NOT TRUE)
unsafeUnaryOpL :: ByteString {- ^ operator -} -> x :--> y
unsafeUnaryOpL op x = UnsafeExpression $ parenthesized $ op <+> renderSQL x

unsafeUnaryOpR :: ByteString {- ^ operator -} -> x :--> y
unsafeUnaryOpR op x = UnsafeExpression $ parenthesized $ renderSQL x <+> op

-- | >>> printSQL $ unsafeFunction "f" true
-- f(TRUE)
unsafeFunction :: ByteString {- ^ function -} -> x :--> y
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderSQL x)

unsafeFunctionHet
  :: SListI xs
  => ByteString {- ^ function -}
  -> FunctionHet xs y
unsafeFunctionHet fun xs = UnsafeExpression $
  fun <> parenthesized (renderCommaSeparated renderSQL xs)

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
  => Num (Expression outer grp commons schemas params from (null ty)) where
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
  (Expression outer grp commons schemas params from (null ty)) where
    (/) = unsafeBinaryOp "/"
    fromRational
      = UnsafeExpression
      . fromString
      . ($ "")
      . showFFloat Nothing
      . fromRat @Double

instance (ty `In` PGNum, ty `In` PGFloating) => Floating
  (Expression outer grp commons schemas params from (null ty)) where
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
--   expression :: Expr (null 'PGfloat4)
--   expression = atan2_ (pi *: 2)
-- in printSQL expression
-- :}
-- atan2(pi(), 2)
atan2_
  :: float `In` PGFloating
  => FunctionHet '[ null float, null float] (null float)
atan2_ = unsafeFunctionHet "atan2"

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
--   expression :: Expression outer grp commons schemas params from (null 'PGint2)
--   expression = 5 `quot_` 2
-- in printSQL expression
-- :}
-- (5 / 2)
quot_
  :: int `In` PGIntegral
  => Operator (null int) (null int) (null int)
quot_ = unsafeBinaryOp "/"

-- | remainder upon integer division
--
-- >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (null 'PGint2)
--   expression = 5 `rem_` 2
-- in printSQL expression
-- :}
-- (5 % 2)
rem_
  :: int `In` PGIntegral
  => Operator (null int) (null int) (null int)
rem_ = unsafeBinaryOp "%"

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (null 'PGfloat4)
--   expression = trunc pi
-- in printSQL expression
-- :}
-- trunc(pi())
trunc :: frac `In` PGFloating => null frac :--> null frac
trunc = unsafeFunction "trunc"

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (null 'PGfloat4)
--   expression = round_ pi
-- in printSQL expression
-- :}
-- round(pi())
round_ :: frac `In` PGFloating => null frac :--> null frac
round_ = unsafeFunction "round"

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (null 'PGfloat4)
--   expression = ceiling_ pi
-- in printSQL expression
-- :}
-- ceiling(pi())
ceiling_ :: frac `In` PGFloating => null frac :--> null frac
ceiling_ = unsafeFunction "ceiling"

-- | A `Condition` is an `Expression`, which can evaluate
-- to `true`, `false` or `null_`. This is because SQL uses
-- a three valued logic.
type Condition outer grp commons schemas params from =
  Expression outer grp commons schemas params from ('Null 'PGbool)

-- | >>> printSQL true
-- TRUE
true :: Expr (null 'PGbool)
true = UnsafeExpression "TRUE"

-- | >>> printSQL false
-- FALSE
false :: Expr (null 'PGbool)
false = UnsafeExpression "FALSE"

-- | >>> printSQL $ not_ true
-- (NOT TRUE)
not_ :: null 'PGbool :--> null 'PGbool
not_ = unsafeUnaryOpL "NOT"

-- | >>> printSQL $ true .&& false
-- (TRUE AND FALSE)
(.&&) :: Operator (null 'PGbool) (null 'PGbool) (null 'PGbool)
infixr 3 .&&
(.&&) = unsafeBinaryOp "AND"

-- | >>> printSQL $ true .|| false
-- (TRUE OR FALSE)
(.||) :: Operator (null 'PGbool) (null 'PGbool) (null 'PGbool)
infixr 2 .||
(.||) = unsafeBinaryOp "OR"

-- | >>> :{
-- let
--   expression :: Expression outer grp commons schemas params from (null 'PGint2)
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
--   expression :: Expression outer grp commons schemas params from (null 'PGint2)
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
(.==) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.==) = unsafeBinaryOp "="
infix 4 .==

-- | >>> printSQL $ true ./= null_
-- (TRUE <> NULL)
(./=) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(./=) = unsafeBinaryOp "<>"
infix 4 ./=

-- | >>> printSQL $ true .>= null_
-- (TRUE >= NULL)
(.>=) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.>=) = unsafeBinaryOp ">="
infix 4 .>=

-- | >>> printSQL $ true .< null_
-- (TRUE < NULL)
(.<) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.<) = unsafeBinaryOp "<"
infix 4 .<

-- | >>> printSQL $ true .<= null_
-- (TRUE <= NULL)
(.<=) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.<=) = unsafeBinaryOp "<="
infix 4 .<=

-- | >>> printSQL $ true .> null_
-- (TRUE > NULL)
(.>) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
(.>) = unsafeBinaryOp ">"
infix 4 .>

{- | between
>>> printSQL $ true `between` (null_, false)
TRUE BETWEEN NULL AND FALSE
-}
between
  :: Expression outer grp commons schemas params from (null ty)
  -> ( Expression outer grp commons schemas params from (null ty)
     , Expression outer grp commons schemas params from (null ty) ) -- ^ bounds
  -> Condition outer grp commons schemas params from
between a (x,y) = UnsafeExpression $ renderSQL a <+> "BETWEEN"
  <+> renderSQL x <+> "AND" <+> renderSQL y

{- | not between
>>> printSQL $ true `notBetween` (null_, false)
TRUE NOT BETWEEN NULL AND FALSE
-}
notBetween
  :: Expression outer grp commons schemas params from (null ty)
  -> ( Expression outer grp commons schemas params from (null ty)
     , Expression outer grp commons schemas params from (null ty) ) -- ^ bounds
  -> Condition outer grp commons schemas params from
notBetween a (x,y) = UnsafeExpression $ renderSQL a <+> "NOT BETWEEN"
  <+> renderSQL x <+> "AND" <+> renderSQL y

{- | between, after sorting the comparison values
>>> printSQL $ true `betweenSymmetric` (null_, false)
TRUE BETWEEN SYMMETRIC NULL AND FALSE
-}
betweenSymmetric
  :: Expression outer grp commons schemas params from (null ty)
  -> ( Expression outer grp commons schemas params from (null ty)
     , Expression outer grp commons schemas params from (null ty) ) -- ^ bounds
  -> Condition outer grp commons schemas params from
betweenSymmetric a (x,y) = UnsafeExpression $ renderSQL a
  <+> "BETWEEN SYMMETRIC" <+> renderSQL x <+> "AND" <+> renderSQL y

{- | not between, after sorting the comparison values
>>> printSQL $ true `notBetweenSymmetric` (null_, false)
TRUE NOT BETWEEN SYMMETRIC NULL AND FALSE
-}
notBetweenSymmetric
  :: Expression outer grp commons schemas params from (null ty)
  -> ( Expression outer grp commons schemas params from (null ty)
     , Expression outer grp commons schemas params from (null ty) ) -- ^ bounds
  -> Condition outer grp commons schemas params from
notBetweenSymmetric a (x,y) = UnsafeExpression $ renderSQL a
  <+> "NOT BETWEEN SYMMETRIC" <+> renderSQL x <+> "AND" <+> renderSQL y

{- | not equal, treating null like an ordinary value
>>> printSQL $ true `isDistinctFrom` null_
(TRUE IS DISTINCT FROM NULL)
-}
isDistinctFrom :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
isDistinctFrom = unsafeBinaryOp "IS DISTINCT FROM"

{- | equal, treating null like an ordinary value
>>> printSQL $ true `isNotDistinctFrom` null_
(TRUE IS NOT DISTINCT FROM NULL)
-}
isNotDistinctFrom :: Operator (null0 ty) (null1 ty) ('NotNull 'PGbool)
isNotDistinctFrom = unsafeBinaryOp "IS NOT DISTINCT FROM"

{- | is true
>>> printSQL $ true & isTrue
(TRUE IS TRUE)
-}
isTrue :: null0 'PGbool :--> null1 'PGbool
isTrue = unsafeUnaryOpR "IS TRUE"

{- | is false or unknown
>>> printSQL $ true & isNotTrue
(TRUE IS NOT TRUE)
-}
isNotTrue :: null0 'PGbool :--> null1 'PGbool
isNotTrue = unsafeUnaryOpR "IS NOT TRUE"

{- | is false
>>> printSQL $ true & isFalse
(TRUE IS FALSE)
-}
isFalse :: null0 'PGbool :--> null1 'PGbool
isFalse = unsafeUnaryOpR "IS FALSE"

{- | is true or unknown
>>> printSQL $ true & isNotFalse
(TRUE IS NOT FALSE)
-}
isNotFalse :: null0 'PGbool :--> null1 'PGbool
isNotFalse = unsafeUnaryOpR "IS NOT FALSE"

{- | is unknown
>>> printSQL $ true & isUnknown
(TRUE IS UNKNOWN)
-}
isUnknown :: null0 'PGbool :--> null1 'PGbool
isUnknown = unsafeUnaryOpR "IS UNKNOWN"

{- | is true or false
>>> printSQL $ true & isNotUnknown
(TRUE IS NOT UNKNOWN)
-}
isNotUnknown :: null0 'PGbool :--> null1 'PGbool
isNotUnknown = unsafeUnaryOpR "IS NOT UNKNOWN"

-- | >>> printSQL currentDate
-- CURRENT_DATE
currentDate :: Expr (null 'PGdate)
currentDate = UnsafeExpression "CURRENT_DATE"

-- | >>> printSQL currentTime
-- CURRENT_TIME
currentTime :: Expr (null 'PGtimetz)
currentTime = UnsafeExpression "CURRENT_TIME"

-- | >>> printSQL currentTimestamp
-- CURRENT_TIMESTAMP
currentTimestamp :: Expr (null 'PGtimestamptz)
currentTimestamp = UnsafeExpression "CURRENT_TIMESTAMP"

-- | >>> printSQL localTime
-- LOCALTIME
localTime :: Expr (null 'PGtime)
localTime = UnsafeExpression "LOCALTIME"

-- | >>> printSQL localTimestamp
-- LOCALTIMESTAMP
localTimestamp :: Expr (null 'PGtimestamp)
localTimestamp = UnsafeExpression "LOCALTIMESTAMP"

{-----------------------------------------
text
-----------------------------------------}

escape :: Char -> String
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

instance IsString
  (Expression outer grp commons schemas params from (null 'PGtext)) where
    fromString str = UnsafeExpression $
      "E\'" <> fromString (escape =<< str) <> "\'"
instance IsString
  (Expression outer grp commons schemas params from (null 'PGtsvector)) where
    fromString str = cast tsvector . UnsafeExpression $
      "E\'" <> fromString (escape =<< str) <> "\'"
instance IsString
  (Expression outer grp commons schemas params from (null 'PGtsquery)) where
    fromString str = cast tsquery . UnsafeExpression $
      "E\'" <> fromString (escape =<< str) <> "\'"

instance Semigroup
  (Expression outer grp commons schemas params from (null 'PGtext)) where
    (<>) = unsafeBinaryOp "||"
instance Semigroup
  (Expression outer grp commons schemas params from (null 'PGtsvector)) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression outer grp commons schemas params from (null 'PGtext)) where
    mempty = fromString ""
    mappend = (<>)
instance Monoid
  (Expression outer grp commons schemas params from (null 'PGtsvector)) where
    mempty = fromString ""
    mappend = (<>)

-- | >>> printSQL $ lower "ARRRGGG"
-- lower(E'ARRRGGG')
lower :: null 'PGtext :--> null 'PGtext
lower = unsafeFunction "lower"

-- | >>> printSQL $ upper "eeee"
-- upper(E'eeee')
upper :: null 'PGtext :--> null 'PGtext
upper = unsafeFunction "upper"

-- | >>> printSQL $ charLength "four"
-- char_length(E'four')
charLength :: null 'PGtext :--> null 'PGint4
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
like :: Operator (null 'PGtext) (null 'PGtext) ('Null 'PGbool)
like = unsafeBinaryOp "LIKE"

-- | The key word ILIKE can be used instead of LIKE to make the
-- match case-insensitive according to the active locale.
--
-- >>> printSQL $ "abc" `ilike` "a%"
-- (E'abc' ILIKE E'a%')
ilike :: Operator (null 'PGtext) (null 'PGtext) ('Null 'PGbool)
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
  => Operator (null json) (null key) ('Null json)
infixl 8 .->
(.->) = unsafeBinaryOp "->"

-- | Get JSON value (object field or array element) at a key, as text.
(.->>)
  :: (json `In` PGJsonType, key `In` PGJsonKey)
  => Operator (null json) (null key) ('Null 'PGtext)
infixl 8 .->>
(.->>) = unsafeBinaryOp "->>"

-- | Get JSON value at a specified path.
(.#>)
  :: (json `In` PGJsonType, PGTextArray "(.#>)" path)
  => Operator (null json) (null path) ('Null json)
infixl 8 .#>
(.#>) = unsafeBinaryOp "#>"

-- | Get JSON value at a specified path as text.
(.#>>)
  :: (json `In` PGJsonType, PGTextArray "(.#>>)" path)
  => Operator (null json) (null path) ('Null 'PGtext)
infixl 8 .#>>
(.#>>) = unsafeBinaryOp "#>>"

-- Additional jsonb operators

class PGSubset container where
  (@>) :: Operator (null0 container) (null1 container) ('Null 'PGbool)
  (@>) = unsafeBinaryOp "@>"
  (<@) :: Operator (null0 container) (null1 container) ('Null 'PGbool)
  (<@) = unsafeBinaryOp "<@"
instance PGSubset 'PGjsonb
instance PGSubset 'PGtsquery
instance PGSubset ('PGvararray ty)

-- | Does the string exist as a top-level key within the JSON value?
(.?) :: Operator (null 'PGjsonb) (null 'PGtext) ('Null 'PGbool)
infixl 9 .?
(.?) = unsafeBinaryOp "?"

-- | Do any of these array strings exist as top-level keys?
(.?|) :: Operator
  (null 'PGjsonb)
  (null ('PGvararray ('NotNull 'PGtext)))
  ('Null 'PGbool)
infixl 9 .?|
(.?|) = unsafeBinaryOp "?|"

-- | Do all of these array strings exist as top-level keys?
(.?&) :: Operator
  (null 'PGjsonb)
  (null ('PGvararray ('NotNull 'PGtext)))
  ('Null 'PGbool)
infixl 9 .?&
(.?&) = unsafeBinaryOp "?&"

-- | Concatenate two jsonb values into a new jsonb value.
instance Semigroup
  (Expression outer grp commons schemas params from (null 'PGjsonb)) where
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
  => Operator (null 'PGjsonb) (null key) (null 'PGjsonb)
infixl 6 .-.
(.-.) = unsafeBinaryOp "-"

-- | Delete the field or element with specified path (for JSON arrays, negative
-- integers count from the end)
(#-.)
  :: PGTextArray "(#-.)" arrayty
  => Operator (null 'PGjsonb) (null arrayty) (null 'PGjsonb)
infixl 6 #-.
(#-.) = unsafeBinaryOp "#-"

{-----------------------------------------
Table 9.45: JSON creation functions
-----------------------------------------}

-- | Literal binary JSON
jsonbLit :: JSON.ToJSON x => x -> Expr (null 'PGjsonb)
jsonbLit = cast jsonb . UnsafeExpression
  . singleQuotedUtf8 . toStrict . JSON.encode

-- | Literal JSON
jsonLit :: JSON.ToJSON x => x -> Expr (null 'PGjson)
jsonLit = cast json . UnsafeExpression
  . singleQuotedUtf8 . toStrict . JSON.encode

-- | Returns the value as json. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid json value.
toJson :: null ty :--> null 'PGjson
toJson = unsafeFunction "to_json"

-- | Returns the value as jsonb. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid jsonb value.
toJsonb :: null ty :--> null 'PGjsonb
toJsonb = unsafeFunction "to_jsonb"

-- | Returns the array as a JSON array. A PostgreSQL multidimensional array
-- becomes a JSON array of arrays.
arrayToJson
  :: PGArray "arrayToJson" arr
  => null arr :--> null 'PGjson
arrayToJson = unsafeFunction "array_to_json"

-- | Returns the row as a JSON object.
rowToJson :: null ('PGcomposite ty) :--> null 'PGjson
rowToJson = unsafeFunction "row_to_json"

-- | Builds a possibly-heterogeneously-typed JSON array out of a variadic
-- argument list.
jsonBuildArray
  :: SListI elems
  => NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (null 'PGjson)
jsonBuildArray = unsafeVariadicFunction "json_build_array"

-- | Builds a possibly-heterogeneously-typed (binary) JSON array out of a
-- variadic argument list.
jsonbBuildArray
  :: SListI elems
  => NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
jsonbBuildArray = unsafeVariadicFunction "jsonb_build_array"

unsafeRowFunction
  :: SOP.SListI elems
  => NP (Aliased (Expression outer grp commons schemas params from)) elems
  -> [ByteString]
unsafeRowFunction =
  (`appEndo` []) . hcfoldMap (Proxy :: SOP.Proxy SOP.Top)
  (\(col `As` name) -> Endo $ \xs ->
      renderAliasString name : renderSQL col : xs)
  where
    renderAliasString :: KnownSymbol alias => Alias alias -> ByteString
    renderAliasString = singleQuotedText . fromString . symbolVal

-- | Builds a possibly-heterogeneously-typed JSON object out of a variadic
-- argument list. The elements of the argument list must alternate between text
-- and values.
jsonBuildObject
  :: SOP.SListI elems
  => NP (Aliased (Expression outer grp commons schemas params from)) elems
  -> Expression outer grp commons schemas params from (null 'PGjson)
jsonBuildObject
  = unsafeFunction "json_build_object"
  . UnsafeExpression
  . commaSeparated
  . unsafeRowFunction

-- | Builds a possibly-heterogeneously-typed (binary) JSON object out of a
-- variadic argument list. The elements of the argument list must alternate
-- between text and values.
jsonbBuildObject
  :: SOP.SListI elems
  => NP (Aliased (Expression outer grp commons schemas params from)) elems
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
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
  => null arr :--> null 'PGjson
jsonObject = unsafeFunction "json_object"

-- | Builds a binary JSON object out of a text array. The array must have either
-- exactly one dimension with an even number of members, in which case they are
-- taken as alternating key/value pairs, or two dimensions such that each inner
-- array has exactly two elements, which are taken as a key/value pair.
jsonbObject
  :: PGArrayOf "jsonbObject" arr ('NotNull 'PGtext)
  => null arr :--> null 'PGjsonb
jsonbObject = unsafeFunction "jsonb_object"

-- | This is an alternate form of 'jsonObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a JSON object.
jsonZipObject
  :: ( PGArrayOf "jsonZipObject" keysArray ('NotNull 'PGtext)
     , PGArrayOf "jsonZipObject" valuesArray ('NotNull 'PGtext))
  => Expression outer grp commons schemas params from (null keysArray)
  -> Expression outer grp commons schemas params from (null valuesArray)
  -> Expression outer grp commons schemas params from (null 'PGjson)
jsonZipObject ks vs =
  unsafeVariadicFunction "json_object" (ks :* vs :* Nil)

-- | This is an alternate form of 'jsonObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a binary JSON
-- object.
jsonbZipObject
  :: ( PGArrayOf "jsonbZipObject" keysArray ('NotNull 'PGtext)
     , PGArrayOf "jsonbZipObject" valuesArray ('NotNull 'PGtext))
  => Expression outer grp commons schemas params from (null keysArray)
  -> Expression outer grp commons schemas params from (null valuesArray)
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
jsonbZipObject ks vs =
  unsafeVariadicFunction "jsonb_object" (ks :* vs :* Nil)

{-----------------------------------------
Table 9.46: JSON processing functions
-----------------------------------------}

-- | Returns the number of elements in the outermost JSON array.
jsonArrayLength :: null 'PGjson :--> null 'PGint4
jsonArrayLength = unsafeFunction "json_array_length"

-- | Returns the number of elements in the outermost binary JSON array.
jsonbArrayLength :: null 'PGjsonb :--> null 'PGint4
jsonbArrayLength = unsafeFunction "jsonb_array_length"

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator).
jsonExtractPath
  :: SListI elems
  => Expression outer grp commons schemas params from (null 'PGjson)
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
jsonExtractPath x xs =
  unsafeVariadicFunction "json_extract_path" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator).
jsonbExtractPath
  :: SListI elems
  => Expression outer grp commons schemas params from (null 'PGjsonb)
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
jsonbExtractPath x xs =
  unsafeVariadicFunction "jsonb_extract_path" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator), as text.
jsonExtractPathAsText
  :: SListI elems
  => Expression outer grp commons schemas params from (null 'PGjson)
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (null 'PGjson)
jsonExtractPathAsText x xs =
  unsafeVariadicFunction "json_extract_path_text" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator), as text.
jsonbExtractPathAsText
  :: SListI elems
  => Expression outer grp commons schemas params from (null 'PGjsonb)
  -> NP (Expression outer grp commons schemas params from) elems
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
jsonbExtractPathAsText x xs =
  unsafeVariadicFunction "jsonb_extract_path_text" (x :* xs)

-- | Returns the type of the outermost JSON value as a text string. Possible
-- types are object, array, string, number, boolean, and null.
jsonTypeof :: null 'PGjson :--> null 'PGtext
jsonTypeof = unsafeFunction "json_typeof"

-- | Returns the type of the outermost binary JSON value as a text string.
-- Possible types are object, array, string, number, boolean, and null.
jsonbTypeof :: null 'PGjsonb :--> null 'PGtext
jsonbTypeof = unsafeFunction "jsonb_typeof"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonStripNulls :: null 'PGjson :--> null 'PGjson
jsonStripNulls = unsafeFunction "json_strip_nulls"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonbStripNulls :: null 'PGjsonb :--> null 'PGjsonb
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
  => Expression outer grp commons schemas params from (null 'PGjsonb)
  -> Expression outer grp commons schemas params from (null arr)
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
  -> Maybe (Expression outer grp commons schemas params from (null 'PGbool))
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
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
  => Expression outer grp commons schemas params from (null 'PGjsonb)
  -> Expression outer grp commons schemas params from (null arr)
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
  -> Maybe (Expression outer grp commons schemas params from (null 'PGbool))
  -> Expression outer grp commons schemas params from (null 'PGjsonb)
jsonbInsert tgt path val insertAfter = case insertAfter of
  Just i -> unsafeVariadicFunction "jsonb_insert" (tgt :* path :* val :* i :* Nil)
  Nothing -> unsafeVariadicFunction "jsonb_insert" (tgt :* path :* val :* Nil)

-- | Returns its argument as indented JSON text.
jsonbPretty :: null 'PGjsonb :--> null 'PGtext
jsonbPretty = unsafeFunction "jsonb_pretty"

(@@) :: Operator (null 'PGtsvector) (null 'PGtsquery) ('Null 'PGbool)
(@@) = unsafeBinaryOp "@@"

(.&) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(.&) = unsafeBinaryOp "&&"

(.|) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(.|) = unsafeBinaryOp "||"

(.!) :: null 'PGtsquery :--> null 'PGtsquery
(.!) = unsafeUnaryOpL "!!"

(<->) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(<->) = unsafeBinaryOp "<->"

arrayToTSvector
  ::   null ('PGvararray ('NotNull 'PGtext))
  :--> null 'PGtsvector
arrayToTSvector = unsafeFunction "array_to_tsvector"

tsvectorLength :: null 'PGtsvector :--> null 'PGint4
tsvectorLength = unsafeFunction "length"

numnode :: null 'PGtsquery :--> null 'PGint4
numnode = unsafeFunction "numnode"

plainToTSquery :: null 'PGtext :--> null 'PGtsquery
plainToTSquery = unsafeFunction "plainto_tsquery"

phraseToTSquery :: null 'PGtext :--> null 'PGtsquery
phraseToTSquery = unsafeFunction "phraseto_tsquery"

websearchToTSquery :: null 'PGtext :--> null 'PGtsquery
websearchToTSquery = unsafeFunction "websearch_to_tsquery"

queryTree :: null 'PGtsquery :--> null 'PGtext
queryTree = unsafeFunction "query_tree"

toTSquery :: null 'PGtext :--> null 'PGtsquery
toTSquery = unsafeFunction "to_tsquery"

toTSvector
  :: ty `In` '[ 'PGtext, 'PGjson, 'PGjsonb]
  => null ty :--> null 'PGtsquery
toTSvector = unsafeFunction "to_tsvector"

setWeight
  :: FunctionHet '[null 'PGtsvector, null ('PGchar 1)] (null 'PGtsvector)
setWeight = unsafeFunctionHet "set_weight"

litChar :: Char -> Expr (null ('PGchar 1))
litChar chr = UnsafeExpression $
  "E\'" <> fromString (escape =<< [chr]) <> "\'"

strip :: null 'PGtsvector :--> null 'PGtsvector
strip = unsafeFunction "strip"

jsonToTSvector :: FunctionHet '[null 'PGjson, null 'PGjson] (null 'PGtsvector)
jsonToTSvector = unsafeFunctionHet "json_to_tsvector"

jsonbToTSvector :: FunctionHet '[null 'PGjsonb, null 'PGjsonb] (null 'PGtsvector)
jsonbToTSvector = unsafeFunctionHet "jsonb_to_tsvector"

tsDelete :: FunctionHet
  '[null 'PGtsvector, null ('PGvararray ('NotNull 'PGtext))]
   (null 'PGtsvector)
tsDelete = unsafeFunctionHet "ts_delete"

tsFilter :: FunctionHet
  '[null 'PGtsvector, null ('PGvararray ('NotNull ('PGchar 1)))]
   (null 'PGtsvector)
tsFilter = unsafeFunctionHet "ts_filter"

tsHeadline
  :: document `In` '[ 'PGtext, 'PGjson, 'PGjsonb]
  => FunctionHet '[null document, null 'PGtsquery] (null 'PGtext)
tsHeadline = unsafeFunctionHet "ts_headline"
{-----------------------------------------
aggregation
-----------------------------------------}

type AggrFun x y
  =  forall outer bys commons schemas params from
  .  Distinction (Expression outer 'Ungrouped commons schemas params from) x
     -- ^ input
  -> Expression outer ('Grouped bys) commons schemas params from y
     -- ^ output

type AggrHet xs y
  =  forall outer bys commons schemas params from
  .  Distinction (NP (Expression outer 'Ungrouped commons schemas params from)) xs
     -- ^ inputs
  -> Expression outer ('Grouped bys) commons schemas params from y
     -- ^ output

unsafeAggregateHet :: SOP.SListI xs => ByteString -> AggrHet xs y
unsafeAggregateHet fun xs = UnsafeExpression $ mconcat
  [fun, "(", renderSQL xs, ")"]

{- |
`Aggregate` functions compute a single result from a set of input values.
`Aggregate` functions can be used as `GroupedBy` `Expression`s as well
as `WindowFunction`s.
-}
class Aggregate expr1 exprHet aggr
  | aggr -> expr1, aggr -> exprHet where

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
  --   expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: null ty]] ('NotNull 'PGint8)
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
  --   expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: 'Null 'PGnumeric]] ('NotNull 'PGnumeric)
  --   expression = sum_ (Distinct #col)
  -- in printSQL expression
  -- :}
  -- sum(DISTINCT "col")
  sum_
    :: ty `In` PGNum
    => expr1 (null ty)
    -> aggr ('NotNull ty)

  -- | input values, including nulls, concatenated into an array
  arrayAgg
    :: expr1 ty
    -> aggr ('Null ('PGvararray ty))

  -- | aggregates values as a JSON array
  jsonAgg
    :: expr1 ty
    -> aggr ('Null 'PGjson)

  -- | aggregates values as a JSON array
  jsonbAgg
    :: expr1 ty
    -> aggr ('Null 'PGjsonb)

  {- |
  the bitwise AND of all non-null input values, or null if none
  >>> :{
  let
    expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: null 'PGint4]] ('Null 'PGint4)
    expression = bitAnd (Distinct #col)
  in printSQL expression
  :}
  bit_and(DISTINCT "col")
  -}
  bitAnd
    :: int `In` PGIntegral
    => expr1 (null int)
    -- ^ what to aggregate
    -> aggr ('Null int)

  {- |
  the bitwise OR of all non-null input values, or null if none

  >>> :{
  let
    expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: null 'PGint4]] ('Null 'PGint4)
    expression = bitOr (All #col)
  in printSQL expression
  :}
  bit_or(ALL "col")
  -}
  bitOr
    :: int `In` PGIntegral
    => expr1 (null int)
    -- ^ what to aggregate
    -> aggr ('Null int)

  {- |
  true if all input values are true, otherwise false

  >>> :{
  let
    winFun :: WindowFunction '[] 'Ungrouped commons schemas params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    winFun = boolAnd #col
  in printSQL winFun
  :}
  bool_and("col")
  -}
  boolAnd
    :: expr1 (null 'PGbool)
    -- ^ what to aggregate
    -> aggr ('Null 'PGbool)

  {- |
  true if at least one input value is true, otherwise false

  >>> :{
  let
    expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    expression = boolOr (All #col)
  in printSQL expression
  :}
  bool_or(ALL "col")
  -}
  boolOr
    :: expr1 (null 'PGbool)
    -- ^ what to aggregate
    -> aggr ('Null 'PGbool)

  {- |
  equivalent to `boolAnd`

  >>> :{
  let
    expression :: Expression '[] ('Grouped bys) commons schemas params '[tab ::: '["col" ::: null 'PGbool]] ('Null 'PGbool)
    expression = every (Distinct #col)
  in printSQL expression
  :}
  every(DISTINCT "col")
  -}
  every
    :: expr1 (null 'PGbool)
    -- ^ what to aggregate
    -> aggr ('Null 'PGbool)

  {- |maximum value of expression across all input values-}
  max_
    :: expr1 (null ty)
    -- ^ what to maximize
    -> aggr ('Null ty)

  -- | minimum value of expression across all input values
  min_
    :: expr1 (null ty)
    -- ^ what to minimize
    -> aggr ('Null ty)

  -- | the average (arithmetic mean) of all input values
  avg
    :: expr1 (null ty)
    -- ^ what to average
    -> aggr ('Null (PGAvg ty))

  {- | correlation coefficient
  >>> :{
  let
    expression :: Expression '[] ('Grouped g) c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = corr (All (#y *: #x))
  in printSQL expression
  :}
  corr(ALL "y", "x")
  -}
  corr
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | population covariance
  >>> :{
  let
    expression :: Expression '[] ('Grouped g) c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = covarPop (All (#y *: #x))
  in printSQL expression
  :}
  covar_pop(ALL "y", "x")
  -}
  covarPop
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | sample covariance
  >>> :{
  let
    winFun :: WindowFunction '[] 'Ungrouped c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    winFun = covarSamp (#y *: #x)
  in printSQL winFun
  :}
  covar_samp("y", "x")
  -}
  covarSamp
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | average of the independent variable (sum(X)/N)
  >>> :{
  let
    expression :: Expression '[] ('Grouped g) c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = regrAvgX (All (#y *: #x))
  in printSQL expression
  :}
  regr_avgx(ALL "y", "x")
  -}
  regrAvgX
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | average of the dependent variable (sum(Y)/N)
  >>> :{
  let
    winFun :: WindowFunction '[] 'Ungrouped c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    winFun = regrAvgY (#y *: #x)
  in printSQL winFun
  :}
  regr_avgy("y", "x")
  -}
  regrAvgY
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  {- | number of input rows in which both expressions are nonnull
  >>> :{
  let
    winFun :: WindowFunction '[] 'Ungrouped c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGint8)
    winFun = regrCount (#y *: #x)
  in printSQL winFun
  :}
  regr_count("y", "x")
  -}
  regrCount
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGint8)

  {- | y-intercept of the least-squares-fit linear equation determined by the (X, Y) pairs
  >>> :{
  let
    expression :: Expression '[] ('Grouped g) c s p '[t ::: '["x" ::: 'NotNull 'PGfloat8, "y" ::: 'NotNull 'PGfloat8]] ('Null 'PGfloat8)
    expression = regrIntercept (All (#y *: #x))
  in printSQL expression
  :}
  regr_intercept(ALL "y", "x")
  -}
  regrIntercept
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_r2(Y, X)@, square of the correlation coefficient
  regrR2
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_slope(Y, X)@, slope of the least-squares-fit linear equation
  -- determined by the (X, Y) pairs
  regrSlope
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_sxx(Y, X)@, sum(X^2) - sum(X)^2/N
  -- (“sum of squares” of the independent variable)
  regrSxx
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_sxy(Y, X)@, sum(X*Y) - sum(X) * sum(Y)/N
  -- (“sum of products” of independent times dependent variable)
  regrSxy
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | @regr_syy(Y, X)@, sum(Y^2) - sum(Y)^2/N
  -- (“sum of squares” of the dependent variable)
  regrSyy
    :: exprHet '[null 'PGfloat8, null 'PGfloat8]
    -> aggr ('Null 'PGfloat8)

  -- | historical alias for `stddevSamp`
  stddev
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | population standard deviation of the input values
  stddevPop
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | sample standard deviation of the input values
  stddevSamp
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | historical alias for `varSamp`
  variance
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | population variance of the input values
  -- (square of the population standard deviation)
  varPop
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

  -- | sample variance of the input values
  -- (square of the sample standard deviation)
  varSamp
    :: expr1 (null ty)
    -> aggr ('Null (PGAvg ty))

{- |
`Distinction`s are used for the input of `Aggregate` `Expression`s.
`All` invokes the aggregate once for each input row.
`Distinct` invokes the aggregate once for each distinct value of the expression
(or distinct set of values, for multiple expressions) found in the input
-}
data Distinction (expr :: kind -> Type) (ty :: kind)
  = All (expr ty)
  | Distinct (expr ty)
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData (Distinction (Expression outer grp commons schemas params from) ty)
instance RenderSQL (Distinction (Expression outer grp commons schemas params from) ty) where
  renderSQL = \case
    All x -> "ALL" <+> renderSQL x
    Distinct x -> "DISTINCT" <+> renderSQL x
instance SOP.SListI tys => RenderSQL
  (Distinction (NP (Expression outer grp commons schemas params from)) tys) where
    renderSQL = \case
      All xs -> "ALL" <+> renderCommaSeparated renderSQL xs
      Distinct xs -> "DISTINCT" <+> renderCommaSeparated renderSQL xs

instance Aggregate
  (Distinction (Expression outer 'Ungrouped commons schemas params from))
  (Distinction (NP (Expression outer 'Ungrouped commons schemas params from)))
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
    corr = unsafeAggregateHet "corr"
    covarPop = unsafeAggregateHet "covar_pop"
    covarSamp = unsafeAggregateHet "covar_samp"
    regrAvgX = unsafeAggregateHet "regr_avgx"
    regrAvgY = unsafeAggregateHet "regr_avgy"
    regrCount = unsafeAggregateHet "regr_count"
    regrIntercept = unsafeAggregateHet "regr_intercept"
    regrR2 = unsafeAggregateHet "regr_r2"
    regrSlope = unsafeAggregateHet "regr_slope"
    regrSxx = unsafeAggregateHet "regr_sxx"
    regrSxy = unsafeAggregateHet "regr_sxy"
    regrSyy = unsafeAggregateHet "regr_syy"
    stddev = unsafeAggregate1 "stddev"
    stddevPop = unsafeAggregate1 "stddev_pop"
    stddevSamp = unsafeAggregate1 "stddev_samp"
    variance = unsafeAggregate1 "variance"
    varPop = unsafeAggregate1 "var_pop"
    varSamp = unsafeAggregate1 "var_samp"
instance Aggregate
  (Expression outer grp commons schemas params from)
  (NP (Expression outer grp commons schemas params from))
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
    corr = unsafeWindowFunctionHet "corr"
    covarPop = unsafeWindowFunctionHet "covar_pop"
    covarSamp = unsafeWindowFunctionHet "covar_samp"
    regrAvgX = unsafeWindowFunctionHet "regr_avgx"
    regrAvgY = unsafeWindowFunctionHet "regr_avgy"
    regrCount = unsafeWindowFunctionHet "regr_count"
    regrIntercept = unsafeWindowFunctionHet "regr_intercept"
    regrR2 = unsafeWindowFunctionHet "regr_r2"
    regrSlope = unsafeWindowFunctionHet "regr_slope"
    regrSxx = unsafeWindowFunctionHet "regr_sxx"
    regrSxy = unsafeWindowFunctionHet "regr_sxy"
    regrSyy = unsafeWindowFunctionHet "regr_syy"
    stddev = unsafeWindowFunction1 "stddev"
    stddevPop = unsafeWindowFunction1 "stddev_pop"
    stddevSamp = unsafeWindowFunction1 "stddev_samp"
    variance = unsafeWindowFunction1 "variance"
    varPop = unsafeWindowFunction1 "var_pop"
    varSamp = unsafeWindowFunction1 "var_samp"

-- | escape hatch to define aggregate functions
unsafeAggregate1
  :: ByteString -- ^ aggregate function
  -> AggrFun xty yty
unsafeAggregate1 fun x = UnsafeExpression $ mconcat
  [fun, "(", renderSQL x, ")"]

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

{-----------------------------------------
Sorting
-----------------------------------------}

-- | `SortExpression`s are used by `orderBy` to optionally sort the results
-- of a `Squeal.PostgreSQL.Query.Query`. `Asc` or `Desc`
-- set the sort direction of a `NotNull` result
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

{- |
The `orderBy` clause causes the result rows of a `Squeal.PostgreSQL.Query.TableExpression`
to be sorted according to the specified `SortExpression`(s).
If two rows are equal according to the leftmost expression,
they are compared according to the next expression and so on.
If they are equal according to all specified expressions,
they are returned in an implementation-dependent order.

You can also control the order in which rows are processed by window functions
using `orderBy` within `Squeal.PostgreSQL.Query.Over`.
-}
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

-- | A `WindowDefinition` is a set of table rows that are somehow related
-- to the current row
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

{- |
The `partitionBy` clause within `Squeal.PostgreSQL.Query.Over` divides the rows into groups,
or partitions, that share the same values of the `partitionBy` `Expression`(s).
For each row, the window function is computed across the rows that fall into
the same partition as the current row.
-}
partitionBy
  :: SListI bys
  => NP (Expression outer grp commons schemas params from) bys
  -> WindowDefinition outer grp commons schemas params from
partitionBy bys = WindowDefinition bys []

{- |
A window function performs a calculation across a set of table rows
that are somehow related to the current row. This is comparable to the type
of calculation that can be done with an aggregate function.
However, window functions do not cause rows to become grouped into a single
output row like non-window aggregate calls would.
Instead, the rows retain their separate identities.
Behind the scenes, the window function is able to access more than
just the current row of the query result.
-}
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

type WinFun0 x
  = forall outer grp commons schemas params from
  . WindowFunction outer grp commons schemas params from x
    -- ^ cannot reference aliases

type WinFun1 x y
  =  forall outer grp commons schemas params from
  .  Expression outer grp commons schemas params from x
     -- ^ input
  -> WindowFunction outer grp commons schemas params from y
     -- ^ output

type WinFunHet xs y
  =  forall outer grp commons schemas params from
  .  NP (Expression outer grp commons schemas params from) xs
     -- ^ inputs
  -> WindowFunction outer grp commons schemas params from y
     -- ^ output

unsafeWindowFunction1 :: ByteString -> WinFun1 x y
unsafeWindowFunction1 fun x
  = UnsafeWindowFunction $ fun <> parenthesized (renderSQL x)

unsafeWindowFunctionHet :: SOP.SListI xs => ByteString -> WinFunHet xs y
unsafeWindowFunctionHet fun xs = UnsafeWindowFunction $ fun <>
  parenthesized (renderCommaSeparated renderSQL xs)

{- | rank of the current row with gaps; same as `rowNumber` of its first peer
>>> printSQL rank
rank()
-}
rank :: WinFun0 ('NotNull 'PGint8)
rank = UnsafeWindowFunction "rank()"

{- | number of the current row within its partition, counting from 1
>>> printSQL rowNumber
row_number()
-}
rowNumber :: WinFun0 ('NotNull 'PGint8)
rowNumber = UnsafeWindowFunction "row_number()"

{- | rank of the current row without gaps; this function counts peer groups
>>> printSQL denseRank
dense_rank()
-}
denseRank :: WinFun0 ('NotNull 'PGint8)
denseRank = UnsafeWindowFunction "dense_rank()"

{- | relative rank of the current row: (rank - 1) / (total partition rows - 1)
>>> printSQL percentRank
percent_rank()
-}
percentRank :: WinFun0 ('NotNull 'PGfloat8)
percentRank = UnsafeWindowFunction "percent_rank()"

{- | cumulative distribution: (number of partition rows
preceding or peer with current row) / total partition rows
>>> printSQL cumeDist
cume_dist()
-}
cumeDist :: WinFun0 ('NotNull 'PGfloat8)
cumeDist = UnsafeWindowFunction "cume_dist()"

{- | integer ranging from 1 to the argument value,
dividing the partition as equally as possible
-}
ntile :: WinFun1 ('NotNull 'PGint4) ('NotNull 'PGint4)
ntile = unsafeWindowFunction1 "ntile"

{- | returns value evaluated at the row that is offset rows before the current
row within the partition; if there is no such row, instead return default
(which must be of the same type as value). Both offset and default are
evaluated with respect to the current row.
-}
lag :: WinFunHet '[ty, 'NotNull 'PGint4, ty] ty
lag = unsafeWindowFunctionHet "lag"

{- | returns value evaluated at the row that is offset rows after the current
row within the partition; if there is no such row, instead return default
(which must be of the same type as value). Both offset and default are
evaluated with respect to the current row.
-}
lead :: WinFunHet '[ty, 'NotNull 'PGint4, ty] ty
lead = unsafeWindowFunctionHet "lag"

{- | returns value evaluated at the row that is the
first row of the window frame
-}
firstValue :: WinFun1 ty ty
firstValue = unsafeWindowFunction1 "first_value"

{- | returns value evaluated at the row that is the
last row of the window frame
-}
lastValue :: WinFun1 ty ty
lastValue = unsafeWindowFunction1 "last_value"

{- | returns value evaluated at the row that is the nth
row of the window frame (counting from 1); null if no such row
-}
nthValue :: WinFunHet '[null ty, 'NotNull 'PGint4] ('Null ty)
nthValue = unsafeWindowFunctionHet "nth_value"

{-|
Create date from year, month and day fields

>>> printSQL (makeDate (1984 :* 7 *: 3))
make_date(1984, 7, 3)
-}
makeDate :: FunctionHet
  '[ null 'PGint4, null 'PGint4, null 'PGint4 ]
   ( null 'PGdate )
makeDate = unsafeFunctionHet "make_date"

{-|
Create time from hour, minute and seconds fields

>>> printSQL (makeTime (8 :* 15 *: 23.5))
make_time(8, 15, 23.5)
-}
makeTime :: FunctionHet
  '[ null 'PGint4, null 'PGint4, null 'PGfloat8 ]
   ( null 'PGtime )
makeTime = unsafeFunctionHet "make_time"

{-|
Create timestamp from year, month, day, hour, minute and seconds fields

>>> printSQL (makeTimestamp (2013 :* 7 :* 15 :* 8 :* 15 *: 23.5))
make_timestamp(2013, 7, 15, 8, 15, 23.5)
-}
makeTimestamp :: FunctionHet
  '[ null 'PGint4, null 'PGint4, null 'PGint4
   , null 'PGint4, null 'PGint4, null 'PGfloat8 ]
   ( null 'PGtimestamp )
makeTimestamp = unsafeFunctionHet "make_timestamp"

{-|
Create timestamp with time zone from
year, month, day, hour, minute and seconds fields;
the current time zone is used

>>> printSQL (makeTimestamptz (2013 :* 7 :* 15 :* 8 :* 15 *: 23.5))
make_timestamptz(2013, 7, 15, 8, 15, 23.5)
-}
makeTimestamptz :: FunctionHet
  '[ null 'PGint4, null 'PGint4, null 'PGint4
   , null 'PGint4, null 'PGint4, null 'PGfloat8 ]
   ( null 'PGtimestamptz )
makeTimestamptz = unsafeFunctionHet "make_timestamptz"

{-|
Affine space operations on time types.
-}
class TimeOp time diff | time -> diff where
  {-|
  >>> printSQL (makeDate (1984 :* 7 *: 3) !+ 365)
  (make_date(1984, 7, 3) + 365)
  -}
  (!+) :: Operator (null time) (null diff) (null time)
  (!+) = unsafeBinaryOp "+"
  {-|
  >>> printSQL (365 +! makeDate (1984 :* 7 *: 3))
  (365 + make_date(1984, 7, 3))
  -}
  (+!) :: Operator (null diff) (null time) (null time)
  (+!) = unsafeBinaryOp "+"
  {-|
  >>> printSQL (makeDate (1984 :* 7 *: 3) !- 365)
  (make_date(1984, 7, 3) - 365)
  -}
  (!-) :: Operator (null time) (null diff) (null time)
  (!-) = unsafeBinaryOp "-"
  {-|
  >>> printSQL (makeDate (1984 :* 7 *: 3) !-! currentDate)
  (make_date(1984, 7, 3) - CURRENT_DATE)
  -}
  (!-!) :: Operator (null time) (null time) (null diff)
  (!-!) = unsafeBinaryOp "-"
instance TimeOp 'PGtimestamp 'PGinterval
instance TimeOp 'PGtimestamptz 'PGinterval
instance TimeOp 'PGtime 'PGinterval
instance TimeOp 'PGtimetz 'PGinterval
instance TimeOp 'PGinterval 'PGinterval
instance TimeOp 'PGdate 'PGint4
infixl 6 !+
infixl 6 +!
infixl 6 !-
infixl 6 !-!

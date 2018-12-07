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
    Expression (UnsafeExpression, renderExpression)
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
  , unsafeAggregate, unsafeAggregateDistinct
  , sum_, sumDistinct
  , PGAvg (avg, avgDistinct)
  , bitAnd, bitOr, boolAnd, boolOr
  , bitAndDistinct, bitOrDistinct, boolAndDistinct, boolOrDistinct
  , countStar
  , count, countDistinct
  , every, everyDistinct
  , max_, maxDistinct, min_, minDistinct
    -- * Window functions
  , over
  , partitionBy
  , rank
    -- * Sorting
  , SortExpression (..)
  , renderSortExpression
  , OrderBy (..)
    -- * Types
  , TypeExpression (UnsafeTypeExpression, renderTypeExpression)
  , PGTyped (pgtype)
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
  , NP ((:*), Nil)
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
import GHC.OverloadedLabels
import GHC.TypeLits
import Prelude hiding (id, (.))

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
  (db :: DBType)
  (from :: FromType)
  (grouping :: Grouping)
  (params :: [NullityType])
  (ty :: NullityType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (Expression db from grouping params ty) where
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
    -- >>> let expr = parameter @1 int4 :: Expression (DBof schemas) from grp '[ 'Null 'PGint4] ('Null 'PGint4)
    -- >>> printSQL expr
    -- ($1 :: int4)
    parameter
      :: TypeExpression schemas ty
      -> Expression (commons :=> schemas) from grouping params ty
    parameter ty = UnsafeExpression $ parenthesized $
      "$" <> renderNat @n <+> "::"
        <+> renderTypeExpression ty
instance {-# OVERLAPPING #-} HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

-- | `param` takes a `Nat` using type application and for basic types,
-- infers a `TypeExpression`.
--
-- >>> let expr = param @1 :: Expression (commons :=> schemas) from grp '[ 'Null 'PGint4] ('Null 'PGint4)
-- >>> printSQL expr
-- ($1 :: int4)
param
  :: forall n db commons schemas params from grouping ty
   . (db ~ (commons :=> schemas), PGTyped schemas ty, HasParameter n params ty)
  => Expression db from grouping params ty -- ^ param
param = parameter @n (pgtype @schemas)

instance (HasUnique tab from row, Has col row ty)
  => IsLabel col (Expression db from 'Ungrouped params ty) where
    fromLabel = UnsafeExpression $ renderAlias (Alias @col)
instance (HasUnique tab from row, Has col row ty, column ~ (col ::: ty))
  => IsLabel col
    (Aliased (Expression db from 'Ungrouped params) column) where
    fromLabel = fromLabel @col `As` Alias
instance (HasUnique tab from row, Has col row ty, columns ~ '[col ::: ty])
  => IsLabel col
    (NP (Aliased (Expression db from 'Ungrouped params)) columns) where
    fromLabel = fromLabel @col :* Nil

instance (Has tab from row, Has col row ty)
  => IsQualified tab col (Expression db from 'Ungrouped params ty) where
    tab ! col = UnsafeExpression $
      renderAlias tab <> "." <> renderAlias col
instance (Has tab from row, Has col row ty, column ~ (col ::: ty))
  => IsQualified tab col
    (Aliased (Expression db from 'Ungrouped params) column) where
    tab ! col = tab ! col `As` col
instance (Has tab from row, Has col row ty, columns ~ '[col ::: ty])
  => IsQualified tab col
    (NP (Aliased (Expression db from 'Ungrouped params)) columns) where
    tab ! col = tab ! col :* Nil

instance
  ( HasUnique table from columns
  , Has column columns ty
  , GroupedBy table column bys
  ) => IsLabel column
    (Expression db from ('Grouped bys) params ty) where
      fromLabel = UnsafeExpression $ renderAlias (Alias @column)
instance
  ( HasUnique table from columns
  , Has column columns ty
  , GroupedBy table column bys
  ) => IsLabel column
    ( Aliased (Expression db from ('Grouped bys) params)
      (column ::: ty) ) where
      fromLabel = fromLabel @column `As` Alias @column
instance
  ( HasUnique table from columns
  , Has column columns ty
  , GroupedBy table column bys
  ) => IsLabel column
    ( NP (Aliased (Expression db from ('Grouped bys) params))
      '[column ::: ty] ) where
      fromLabel = fromLabel @column :* Nil

instance
  ( Has table from columns
  , Has column columns ty
  , GroupedBy table column bys
  ) => IsQualified table column
    (Expression db from ('Grouped bys) params ty) where
      table ! column = UnsafeExpression $
        renderAlias table <> "." <> renderAlias column
instance
  ( Has table from columns
  , Has column columns ty
  , GroupedBy table column bys
  ) => IsQualified table column
    (Aliased (Expression db from ('Grouped bys) params)
      (column ::: ty)) where
        table ! column = table ! column `As` column
instance
  ( Has table from columns
  , Has column columns ty
  , GroupedBy table column bys
  ) => IsQualified table column
    ( NP (Aliased (Expression db from ('Grouped bys) params))
      '[column ::: ty]) where
        table ! column = table ! column :* Nil

-- | analagous to `Nothing`
--
-- >>> printSQL null_
-- NULL
null_ :: Expression db rels grouping params ('Null ty)
null_ = UnsafeExpression "NULL"

-- | analagous to `Just`
--
-- >>> printSQL $ notNull true
-- TRUE
notNull
  :: Expression db rels grouping params ('NotNull ty)
  -> Expression db rels grouping params ('Null ty)
notNull = UnsafeExpression . renderExpression

-- | return the leftmost value which is not NULL
--
-- >>> printSQL $ coalesce [null_, true] false
-- COALESCE(NULL, TRUE, FALSE)
coalesce
  :: [Expression db from grouping params ('Null ty)]
  -- ^ @NULL@s may be present
  -> Expression db from grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression db from grouping params ('NotNull ty)
coalesce nullxs notNullx = UnsafeExpression $
  "COALESCE" <> parenthesized (commaSeparated
    ((renderExpression <$> nullxs) <> [renderExpression notNullx]))

-- | analagous to `Data.Maybe.fromMaybe` using @COALESCE@
--
-- >>> printSQL $ fromNull true null_
-- COALESCE(NULL, TRUE)
fromNull
  :: Expression db from grouping params ('NotNull ty)
  -- ^ what to convert @NULL@ to
  -> Expression db from grouping params ('Null ty)
  -> Expression db from grouping params ('NotNull ty)
fromNull notNullx nullx = coalesce [nullx] notNullx

-- | >>> printSQL $ null_ & isNull
-- NULL IS NULL
isNull
  :: Expression db from grouping params ('Null ty)
  -- ^ possibly @NULL@
  -> Condition db from grouping params
isNull x = UnsafeExpression $ renderExpression x <+> "IS NULL"

-- | >>> printSQL $ null_ & isNotNull
-- NULL IS NOT NULL
isNotNull
  :: Expression db from grouping params ('Null ty)
  -- ^ possibly @NULL@
  -> Condition db from grouping params
isNotNull x = UnsafeExpression $ renderExpression x <+> "IS NOT NULL"

-- | analagous to `maybe` using @IS NULL@
--
-- >>> printSQL $ matchNull true not_ null_
-- CASE WHEN NULL IS NULL THEN TRUE ELSE (NOT NULL) END
matchNull
  :: Expression db from grouping params (nullty)
  -- ^ what to convert @NULL@ to
  -> ( Expression db from grouping params ('NotNull ty)
       -> Expression db from grouping params (nullty) )
  -- ^ function to perform when @NULL@ is absent
  -> Expression db from grouping params ('Null ty)
  -> Expression db from grouping params (nullty)
matchNull y f x = ifThenElse (isNull x) y
  (f (UnsafeExpression (renderExpression x)))

{-| right inverse to `fromNull`, if its arguments are equal then
`nullIf` gives @NULL@.

>>> :set -XTypeApplications -XDataKinds
>>> let expr = nullIf false (param @1) :: Expression (commons :=> schema) from grp '[ 'NotNull 'PGbool] ('Null 'PGbool)
>>> printSQL expr
NULL IF (FALSE, ($1 :: bool))
-}
nullIf
  :: Expression db from grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression db from grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression db from grouping params ('Null ty)
nullIf x y = UnsafeExpression $ "NULL IF" <+> parenthesized
  (renderExpression x <> ", " <> renderExpression y)

-- | >>> printSQL $ array [null_, false, true]
-- ARRAY[NULL, FALSE, TRUE]
array
  :: [Expression db from grouping params ty]
  -- ^ array elements
  -> Expression db from grouping params (nullity ('PGvararray ty))
array xs = UnsafeExpression $
  "ARRAY[" <> commaSeparated (renderExpression <$> xs) <> "]"

-- | >>> printSQL $ array [null_, false, true] & index 2
-- (ARRAY[NULL, FALSE, TRUE])[2]
index
  :: Word64 -- ^ index
  -> Expression db from grouping params (nullity ('PGvararray ty)) -- ^ array
  -> Expression db from grouping params (NullifyType ty)
index n expr = UnsafeExpression $
  parenthesized (renderExpression expr) <> "[" <> fromString (show n) <> "]"

instance (KnownSymbol label, label `In` labels) => IsPGlabel label
  (Expression db from grouping params (nullity ('PGenum labels))) where
  label = UnsafeExpression $ renderLabel (PGlabel @label)

-- | A row constructor is an expression that builds a row value
-- (also called a composite value) using values for its member fields.
--
-- >>> :{
-- type Complex = 'PGcomposite
--   '[ "real"      ::: 'NotNull 'PGfloat8
--    , "imaginary" ::: 'NotNull 'PGfloat8 ]
-- :}
--
-- >>> let i = row (0 `as` #real :* 1 `as` #imaginary) :: Expression db from grp params ('NotNull Complex)
-- >>> printSQL i
-- ROW(0, 1)
row
  :: SListI row
  => NP (Aliased (Expression db from grouping params)) row
  -- ^ zero or more expressions for the row field values
  -> Expression db from grouping params (nullity ('PGcomposite row))
row exprs = UnsafeExpression $ "ROW" <> parenthesized
  (renderCommaSeparated (\ (expr `As` _) -> renderExpression expr) exprs)

-- | >>> :{
-- type Complex = 'PGcomposite
--   '[ "real"      ::: 'NotNull 'PGfloat8
--    , "imaginary" ::: 'NotNull 'PGfloat8 ]
-- type Schema = '["complex" ::: 'Typedef Complex]
-- type DB = DBof (Public Schema)
-- :}
--
-- >>> let i = row (0 `as` #real :* 1 `as` #imaginary) :: Expression DB from grp params ('NotNull Complex)
-- >>> printSQL $ i & field #complex #imaginary
-- (ROW(0, 1)::"complex")."imaginary"
field
  :: ( Has sch schemas schema
     , Has tydef schema ('Typedef ('PGcomposite row))
     , Has field row ty)
  => QualifiedAlias sch tydef -- ^ row type
  -> Alias field -- ^ field name
  -> Expression (commons :=> schemas) from grouping params ('NotNull ('PGcomposite row))
  -> Expression (commons :=> schemas) from grouping params ty
field td fld expr = UnsafeExpression $
  parenthesized (renderExpression expr <> "::" <> renderQualifiedAlias td)
    <> "." <> renderAlias fld

instance Semigroup
  (Expression db from grouping params (nullity ('PGvararray ty))) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression db from grouping params (nullity ('PGvararray ty))) where
    mempty = array []
    mappend = (<>)

-- | >>> let expr = greatest currentTimestamp [param @1] :: Expression (commons :=> schemas) rels grp '[ 'NotNull 'PGtimestamptz] ('NotNull 'PGtimestamptz)
-- >>> printSQL expr
-- GREATEST(CURRENT_TIMESTAMP, ($1 :: timestamp with time zone))
greatest
  :: Expression db from grouping params (nullty)
  -- ^ needs at least 1 argument
  -> [Expression db from grouping params (nullty)]
  -- ^ or more
  -> Expression db from grouping params (nullty)
greatest x xs = UnsafeExpression $ "GREATEST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

-- | >>> printSQL $ least currentTimestamp [null_]
-- LEAST(CURRENT_TIMESTAMP, NULL)
least
  :: Expression db from grouping params (nullty)
  -- ^ needs at least 1 argument
  -> [Expression db from grouping params (nullty)]
  -- ^ or more
  -> Expression db from grouping params (nullty)
least x xs = UnsafeExpression $ "LEAST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

-- | >>> printSQL $ unsafeBinaryOp "OR" true false
-- (TRUE OR FALSE)
unsafeBinaryOp
  :: ByteString
  -- ^ operator
  -> Expression db from grouping params (ty0)
  -> Expression db from grouping params (ty1)
  -> Expression db from grouping params (ty2)
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderExpression x <+> op <+> renderExpression y

-- | >>> printSQL $ unsafeUnaryOp "NOT" true
-- (NOT TRUE)
unsafeUnaryOp
  :: ByteString
  -- ^ operator
  -> Expression db from grouping params (ty0)
  -> Expression db from grouping params (ty1)
unsafeUnaryOp op x = UnsafeExpression $ parenthesized $
  op <+> renderExpression x

-- | >>> printSQL $ unsafeFunction "f" true
-- f(TRUE)
unsafeFunction
  :: ByteString
  -- ^ function
  -> Expression db from grouping params (xty)
  -> Expression db from grouping params (yty)
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderExpression x)

-- | Helper for defining variadic functions.
unsafeVariadicFunction
  :: SListI elems
  => ByteString
  -- ^ function
  -> NP (Expression db from grouping params) elems
  -> Expression db from grouping params ret
unsafeVariadicFunction fun x = UnsafeExpression $
  fun <> parenthesized (commaSeparated (hcollapse (hmap (K . renderExpression) x)))

instance ty `In` PGNum
  => Num (Expression db from grouping params (nullity ty)) where
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
  (Expression db from grouping params (nullity ty)) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (ty `In` PGNum, ty `In` PGFloating) => Floating
  (Expression db from grouping params (nullity ty)) where
    pi = UnsafeExpression "pi()"
    exp = unsafeFunction "exp"
    log = unsafeFunction "ln"
    sqrt = unsafeFunction "sqrt"
    b ** x = UnsafeExpression $
      "power(" <> renderExpression b <> ", " <> renderExpression x <> ")"
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
--   expression :: Expression db from grouping params (nullity 'PGfloat4)
--   expression = atan2_ pi 2
-- in printSQL expression
-- :}
-- atan2(pi(), 2)
atan2_
  :: float `In` PGFloating
  => Expression db from grouping params (nullity float)
  -- ^ numerator
  -> Expression db from grouping params (nullity float)
  -- ^ denominator
  -> Expression db from grouping params (nullity float)
atan2_ y x = UnsafeExpression $
  "atan2(" <> renderExpression y <> ", " <> renderExpression x <> ")"

-- When a `cast` is applied to an `Expression` of a known type, it
-- represents a run-time type conversion. The cast will succeed only if a
-- suitable type conversion operation has been defined.
--
-- | >>> printSQL $ true & cast int4
-- (TRUE :: int4)
cast
  :: TypeExpression schemas ty1
  -- ^ type to cast as
  -> Expression (commons :=> schemas) from grouping params ty0
  -- ^ value to convert
  -> Expression (commons :=> schemas) from grouping params ty1
cast ty x = UnsafeExpression $ parenthesized $
  renderExpression x <+> "::" <+> renderTypeExpression ty

-- | integer division, truncates the result
--
-- >>> :{
-- let
--   expression :: Expression db from grouping params (nullity 'PGint2)
--   expression = 5 `quot_` 2
-- in printSQL expression
-- :}
-- (5 / 2)
quot_
  :: int `In` PGIntegral
  => Expression db from grouping params (nullity int)
  -- ^ numerator
  -> Expression db from grouping params (nullity int)
  -- ^ denominator
  -> Expression db from grouping params (nullity int)
quot_ = unsafeBinaryOp "/"

-- | remainder upon integer division
--
-- >>> :{
-- let
--   expression :: Expression db from grouping params (nullity 'PGint2)
--   expression = 5 `rem_` 2
-- in printSQL expression
-- :}
-- (5 % 2)
rem_
  :: int `In` PGIntegral
  => Expression db from grouping params (nullity int)
  -- ^ numerator
  -> Expression db from grouping params (nullity int)
  -- ^ denominator
  -> Expression db from grouping params (nullity int)
rem_ = unsafeBinaryOp "%"

-- | >>> :{
-- let
--   expression :: Expression db from grouping params (nullity 'PGfloat4)
--   expression = trunc pi
-- in printSQL expression
-- :}
-- trunc(pi())
trunc
  :: frac `In` PGFloating
  => Expression db from grouping params (nullity frac)
  -- ^ fractional number
  -> Expression db from grouping params (nullity frac)
trunc = unsafeFunction "trunc"

-- | >>> :{
-- let
--   expression :: Expression db from grouping params (nullity 'PGfloat4)
--   expression = round_ pi
-- in printSQL expression
-- :}
-- round(pi())
round_
  :: frac `In` PGFloating
  => Expression db from grouping params (nullity frac)
  -- ^ fractional number
  -> Expression db from grouping params (nullity frac)
round_ = unsafeFunction "round"

-- | >>> :{
-- let
--   expression :: Expression db from grouping params (nullity 'PGfloat4)
--   expression = ceiling_ pi
-- in printSQL expression
-- :}
-- ceiling(pi())
ceiling_
  :: frac `In` PGFloating
  => Expression db from grouping params (nullity frac)
  -- ^ fractional number
  -> Expression db from grouping params (nullity frac)
ceiling_ = unsafeFunction "ceiling"

-- | A `Condition` is an `Expression`, which can evaluate
-- to `true`, `false` or `null_`. This is because SQL uses
-- a three valued logic.
type Condition db from grouping params =
  Expression db from grouping params ('Null 'PGbool)

-- | >>> printSQL true
-- TRUE
true :: Expression db from grouping params (nullity 'PGbool)
true = UnsafeExpression "TRUE"

-- | >>> printSQL false
-- FALSE
false :: Expression db from grouping params (nullity 'PGbool)
false = UnsafeExpression "FALSE"

-- | >>> printSQL $ not_ true
-- (NOT TRUE)
not_
  :: Expression db from grouping params (nullity 'PGbool)
  -> Expression db from grouping params (nullity 'PGbool)
not_ = unsafeUnaryOp "NOT"

-- | >>> printSQL $ true .&& false
-- (TRUE AND FALSE)
(.&&)
  :: Expression db from grouping params (nullity 'PGbool)
  -> Expression db from grouping params (nullity 'PGbool)
  -> Expression db from grouping params (nullity 'PGbool)
infixr 3 .&&
(.&&) = unsafeBinaryOp "AND"

-- | >>> printSQL $ true .|| false
-- (TRUE OR FALSE)
(.||)
  :: Expression db from grouping params (nullity 'PGbool)
  -> Expression db from grouping params (nullity 'PGbool)
  -> Expression db from grouping params (nullity 'PGbool)
infixr 2 .||
(.||) = unsafeBinaryOp "OR"

-- | >>> :{
-- let
--   expression :: Expression db from grouping params (nullity 'PGint2)
--   expression = caseWhenThenElse [(true, 1), (false, 2)] 3
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN 1 WHEN FALSE THEN 2 ELSE 3 END
caseWhenThenElse
  :: [ ( Condition db from grouping params
       , Expression db from grouping params ty
     ) ]
  -- ^ whens and thens
  -> Expression db from grouping params ty
  -- ^ else
  -> Expression db from grouping params ty
caseWhenThenElse whenThens else_ = UnsafeExpression $ mconcat
  [ "CASE"
  , mconcat
    [ mconcat
      [ " WHEN ", renderExpression when_
      , " THEN ", renderExpression then_
      ]
    | (when_,then_) <- whenThens
    ]
  , " ELSE ", renderExpression else_
  , " END"
  ]

-- | >>> :{
-- let
--   expression :: Expression db from grouping params (nullity 'PGint2)
--   expression = ifThenElse true 1 0
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN 1 ELSE 0 END
ifThenElse
  :: Condition db from grouping params
  -> Expression db from grouping params ty -- ^ then
  -> Expression db from grouping params ty -- ^ else
  -> Expression db from grouping params ty
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

-- | Comparison operations like `.==`, `./=`, `.>`, `.>=`, `.<` and `.<=`
-- will produce @NULL@s if one of their arguments is @NULL@.
--
-- >>> printSQL $ true .== null_
-- (TRUE = NULL)
(.==)
  :: Expression db from grouping params (nullity0 ty) -- ^ lhs
  -> Expression db from grouping params (nullity1 ty) -- ^ rhs
  -> Condition db from grouping params
(.==) = unsafeBinaryOp "="
infix 4 .==

-- | >>> printSQL $ true ./= null_
-- (TRUE <> NULL)
(./=)
  :: Expression db from grouping params (nullity0 ty) -- ^ lhs
  -> Expression db from grouping params (nullity1 ty) -- ^ rhs
  -> Condition db from grouping params
(./=) = unsafeBinaryOp "<>"
infix 4 ./=

-- | >>> printSQL $ true .>= null_
-- (TRUE >= NULL)
(.>=)
  :: Expression db from grouping params (nullity0 ty) -- ^ lhs
  -> Expression db from grouping params (nullity1 ty) -- ^ rhs
  -> Condition db from grouping params
(.>=) = unsafeBinaryOp ">="
infix 4 .>=

-- | >>> printSQL $ true .< null_
-- (TRUE < NULL)
(.<)
  :: Expression db from grouping params (nullity0 ty) -- ^ lhs
  -> Expression db from grouping params (nullity1 ty) -- ^ rhs
  -> Condition db from grouping params
(.<) = unsafeBinaryOp "<"
infix 4 .<

-- | >>> printSQL $ true .<= null_
-- (TRUE <= NULL)
(.<=)
  :: Expression db from grouping params (nullity0 ty) -- ^ lhs
  -> Expression db from grouping params (nullity1 ty) -- ^ rhs
  -> Condition db from grouping params
(.<=) = unsafeBinaryOp "<="
infix 4 .<=

-- | >>> printSQL $ true .> null_
-- (TRUE > NULL)
(.>)
  :: Expression db from grouping params (nullity0 ty) -- ^ lhs
  -> Expression db from grouping params (nullity1 ty) -- ^ rhs
  -> Condition db from grouping params
(.>) = unsafeBinaryOp ">"
infix 4 .>

-- | >>> printSQL currentDate
-- CURRENT_DATE
currentDate
  :: Expression db from grouping params (nullity 'PGdate)
currentDate = UnsafeExpression "CURRENT_DATE"

-- | >>> printSQL currentTime
-- CURRENT_TIME
currentTime
  :: Expression db from grouping params (nullity 'PGtimetz)
currentTime = UnsafeExpression "CURRENT_TIME"

-- | >>> printSQL currentTimestamp
-- CURRENT_TIMESTAMP
currentTimestamp
  :: Expression db from grouping params (nullity 'PGtimestamptz)
currentTimestamp = UnsafeExpression "CURRENT_TIMESTAMP"

-- | >>> printSQL localTime
-- LOCALTIME
localTime
  :: Expression db from grouping params (nullity 'PGtime)
localTime = UnsafeExpression "LOCALTIME"

-- | >>> printSQL localTimestamp
-- LOCALTIMESTAMP
localTimestamp
  :: Expression db from grouping params (nullity 'PGtimestamp)
localTimestamp = UnsafeExpression "LOCALTIMESTAMP"

{-----------------------------------------
text
-----------------------------------------}

instance IsString
  (Expression db from grouping params (nullity 'PGtext)) where
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
  (Expression db from grouping params (nullity 'PGtext)) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression db from grouping params (nullity 'PGtext)) where
    mempty = fromString ""
    mappend = (<>)

-- | >>> printSQL $ lower "ARRRGGG"
-- lower(E'ARRRGGG')
lower
  :: Expression db from grouping params (nullity 'PGtext)
  -- ^ string to lower case
  -> Expression db from grouping params (nullity 'PGtext)
lower = unsafeFunction "lower"

-- | >>> printSQL $ upper "eeee"
-- upper(E'eeee')
upper
  :: Expression db from grouping params (nullity 'PGtext)
  -- ^ string to upper case
  -> Expression db from grouping params (nullity 'PGtext)
upper = unsafeFunction "upper"

-- | >>> printSQL $ charLength "four"
-- char_length(E'four')
charLength
  :: Expression db from grouping params (nullity 'PGtext)
  -- ^ string to measure
  -> Expression db from grouping params (nullity 'PGint4)
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
  :: Expression db from grouping params (nullity 'PGtext)
  -- ^ string
  -> Expression db from grouping params (nullity 'PGtext)
  -- ^ pattern
  -> Expression db from grouping params (nullity 'PGbool)
like = unsafeBinaryOp "LIKE"

-- | The key word ILIKE can be used instead of LIKE to make the
-- match case-insensitive according to the active locale.
--
-- >>> printSQL $ "abc" `ilike` "a%"
-- (E'abc' ILIKE E'a%')
ilike
  :: Expression db from grouping params (nullity 'PGtext)
  -- ^ string
  -> Expression db from grouping params (nullity 'PGtext)
  -- ^ pattern
  -> Expression db from grouping params (nullity 'PGbool)
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
  => Expression db from grouping params (nullity json)
  -> Expression db from grouping params (nullity key)
  -> Expression db from grouping params ('Null json)
infixl 8 .->
(.->) = unsafeBinaryOp "->"

-- | Get JSON value (object field or array element) at a key, as text.
(.->>)
  :: (json `In` PGJsonType, key `In` PGJsonKey)
  => Expression db from grouping params (nullity json)
  -> Expression db from grouping params (nullity key)
  -> Expression db from grouping params ('Null 'PGtext)
infixl 8 .->>
(.->>) = unsafeBinaryOp "->>"

-- | Get JSON value at a specified path.
(.#>)
  :: (json `In` PGJsonType, PGTextArray "(.#>)" path)
  => Expression db from grouping params (nullity json)
  -> Expression db from grouping params (nullity path)
  -> Expression db from grouping params ('Null json)
infixl 8 .#>
(.#>) = unsafeBinaryOp "#>"

-- | Get JSON value at a specified path as text.
(.#>>)
  :: (json `In` PGJsonType, PGTextArray "(.#>>)" path)
  => Expression db from grouping params (nullity json)
  -> Expression db from grouping params (nullity path)
  -> Expression db from grouping params ('Null 'PGtext)
infixl 8 .#>>
(.#>>) = unsafeBinaryOp "#>>"

-- Additional jsonb operators

-- | Does the left JSON value contain the right JSON path/value entries at the
-- top level?
(.@>)
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity 'PGjsonb)
  -> Condition db from grouping params
infixl 9 .@>
(.@>) = unsafeBinaryOp "@>"

-- | Are the left JSON path/value entries contained at the top level within the
-- right JSON value?
(.<@)
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity 'PGjsonb)
  -> Condition db from grouping params
infixl 9 .<@
(.<@) = unsafeBinaryOp "<@"

-- | Does the string exist as a top-level key within the JSON value?
(.?)
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity 'PGtext)
  -> Condition db from grouping params
infixl 9 .?
(.?) = unsafeBinaryOp "?"

-- | Do any of these array strings exist as top-level keys?
(.?|)
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity ('PGvararray ('NotNull 'PGtext)))
  -> Condition db from grouping params
infixl 9 .?|
(.?|) = unsafeBinaryOp "?|"

-- | Do all of these array strings exist as top-level keys?
(.?&)
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity ('PGvararray ('NotNull 'PGtext)))
  -> Condition db from grouping params
infixl 9 .?&
(.?&) = unsafeBinaryOp "?&"

-- | Concatenate two jsonb values into a new jsonb value.
instance
  Semigroup (Expression db from grouping param (nullity 'PGjsonb)) where
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
  => Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity key)
  -> Expression db from grouping params (nullity 'PGjsonb)
infixl 6 .-.
(.-.) = unsafeBinaryOp "-"

-- | Delete the field or element with specified path (for JSON arrays, negative
-- integers count from the end)
(#-.)
  :: PGTextArray "(#-.)" arrayty
  => Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity arrayty)
  -> Expression db from grouping params (nullity 'PGjsonb)
infixl 6 #-.
(#-.) = unsafeBinaryOp "#-"

{-----------------------------------------
Table 9.45: JSON creation functions
-----------------------------------------}

-- | Literal binary JSON
jsonbLit
  :: JSON.ToJSON x
  => x -> Expression (commons :=> schemas) from grouping params (nullity 'PGjsonb)
jsonbLit = cast jsonb . UnsafeExpression
  . singleQuotedUtf8 . toStrict . JSON.encode

-- | Literal JSON
jsonLit
  :: JSON.ToJSON x
  => x -> Expression (commons :=> schemas) from grouping params (nullity 'PGjson)
jsonLit = cast json . UnsafeExpression
  . singleQuotedUtf8 . toStrict . JSON.encode

-- | Returns the value as json. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid json value.
toJson
  :: Expression db from grouping params (nullity ty)
  -> Expression db from grouping params (nullity 'PGjson)
toJson = unsafeFunction "to_json"

-- | Returns the value as jsonb. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid jsonb value.
toJsonb
  :: Expression db from grouping params (nullity ty)
  -> Expression db from grouping params (nullity 'PGjsonb)
toJsonb = unsafeFunction "to_jsonb"

-- | Returns the array as a JSON array. A PostgreSQL multidimensional array
-- becomes a JSON array of arrays.
arrayToJson
  :: PGArray "arrayToJson" arr
  => Expression db from grouping params (nullity arr)
  -> Expression db from grouping params (nullity 'PGjson)
arrayToJson = unsafeFunction "array_to_json"

-- | Returns the row as a JSON object.
rowToJson
  :: Expression db from grouping params (nullity ('PGcomposite ty))
  -> Expression db from grouping params (nullity 'PGjson)
rowToJson = unsafeFunction "row_to_json"

-- | Builds a possibly-heterogeneously-typed JSON array out of a variadic
-- argument list.
jsonBuildArray
  :: SListI elems
  => NP (Expression db from grouping params) elems
  -> Expression db from grouping params (nullity 'PGjson)
jsonBuildArray = unsafeVariadicFunction "json_build_array"

-- | Builds a possibly-heterogeneously-typed (binary) JSON array out of a
-- variadic argument list.
jsonbBuildArray
  :: SListI elems
  => NP (Expression db from grouping params) elems
  -> Expression db from grouping params (nullity 'PGjsonb)
jsonbBuildArray = unsafeVariadicFunction "jsonb_build_array"

unsafeRowFunction
  :: All Top elems
  => NP (Aliased (Expression db from grouping params)) elems
  -> [ByteString]
unsafeRowFunction =
  (`appEndo` []) . hcfoldMap (Proxy :: Proxy Top)
  (\(col `As` name) -> Endo $ \xs ->
      renderAliasString name : renderExpression col : xs)

-- | Builds a possibly-heterogeneously-typed JSON object out of a variadic
-- argument list. The elements of the argument list must alternate between text
-- and values.
jsonBuildObject
  :: All Top elems
  => NP (Aliased (Expression db from grouping params)) elems
  -> Expression db from grouping params (nullity 'PGjson)
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
  => NP (Aliased (Expression db from grouping params)) elems
  -> Expression db from grouping params (nullity 'PGjsonb)
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
  => Expression db from grouping params (nullity arr)
  -> Expression db from grouping params (nullity 'PGjson)
jsonObject = unsafeFunction "json_object"

-- | Builds a binary JSON object out of a text array. The array must have either
-- exactly one dimension with an even number of members, in which case they are
-- taken as alternating key/value pairs, or two dimensions such that each inner
-- array has exactly two elements, which are taken as a key/value pair.
jsonbObject
  :: PGArrayOf "jsonbObject" arr ('NotNull 'PGtext)
  => Expression db from grouping params (nullity arr)
  -> Expression db from grouping params (nullity 'PGjsonb)
jsonbObject = unsafeFunction "jsonb_object"

-- | This is an alternate form of 'jsonObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a JSON object.
jsonZipObject
  :: ( PGArrayOf "jsonZipObject" keysArray ('NotNull 'PGtext)
     , PGArrayOf "jsonZipObject" valuesArray ('NotNull 'PGtext))
  => Expression db from grouping params (nullity keysArray)
  -> Expression db from grouping params (nullity valuesArray)
  -> Expression db from grouping params (nullity 'PGjson)
jsonZipObject ks vs =
  unsafeVariadicFunction "json_object" (ks :* vs :* Nil)

-- | This is an alternate form of 'jsonObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a binary JSON
-- object.
jsonbZipObject
  :: ( PGArrayOf "jsonbZipObject" keysArray ('NotNull 'PGtext)
     , PGArrayOf "jsonbZipObject" valuesArray ('NotNull 'PGtext))
  => Expression db from grouping params (nullity keysArray)
  -> Expression db from grouping params (nullity valuesArray)
  -> Expression db from grouping params (nullity 'PGjsonb)
jsonbZipObject ks vs =
  unsafeVariadicFunction "jsonb_object" (ks :* vs :* Nil)

{-----------------------------------------
Table 9.46: JSON processing functions
-----------------------------------------}

-- | Returns the number of elements in the outermost JSON array.
jsonArrayLength
  :: Expression db from grouping params (nullity 'PGjson)
  -> Expression db from grouping params (nullity 'PGint4)
jsonArrayLength = unsafeFunction "json_array_length"

-- | Returns the number of elements in the outermost binary JSON array.
jsonbArrayLength
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity 'PGint4)
jsonbArrayLength = unsafeFunction "jsonb_array_length"

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator).
jsonExtractPath
  :: SListI elems
  => Expression db from grouping params (nullity 'PGjson)
  -> NP (Expression db from grouping params) elems
  -> Expression db from grouping params (nullity 'PGjsonb)
jsonExtractPath x xs =
  unsafeVariadicFunction "json_extract_path" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator).
jsonbExtractPath
  :: SListI elems
  => Expression db from grouping params (nullity 'PGjsonb)
  -> NP (Expression db from grouping params) elems
  -> Expression db from grouping params (nullity 'PGjsonb)
jsonbExtractPath x xs =
  unsafeVariadicFunction "jsonb_extract_path" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator), as text.
jsonExtractPathAsText
  :: SListI elems
  => Expression db from grouping params (nullity 'PGjson)
  -> NP (Expression db from grouping params) elems
  -> Expression db from grouping params (nullity 'PGjson)
jsonExtractPathAsText x xs =
  unsafeVariadicFunction "json_extract_path_text" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator), as text.
jsonbExtractPathAsText
  :: SListI elems
  => Expression db from grouping params (nullity 'PGjsonb)
  -> NP (Expression db from grouping params) elems
  -> Expression db from grouping params (nullity 'PGjsonb)
jsonbExtractPathAsText x xs =
  unsafeVariadicFunction "jsonb_extract_path_text" (x :* xs)

-- | Returns the type of the outermost JSON value as a text string. Possible
-- types are object, array, string, number, boolean, and null.
jsonTypeof
  :: Expression db from grouping params (nullity 'PGjson)
  -> Expression db from grouping params (nullity 'PGtext)
jsonTypeof = unsafeFunction "json_typeof"

-- | Returns the type of the outermost binary JSON value as a text string.
-- Possible types are object, array, string, number, boolean, and null.
jsonbTypeof
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity 'PGtext)
jsonbTypeof = unsafeFunction "jsonb_typeof"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonStripNulls
  :: Expression db from grouping params (nullity 'PGjson)
  -> Expression db from grouping params (nullity 'PGjson)
jsonStripNulls = unsafeFunction "json_strip_nulls"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonbStripNulls
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity 'PGjsonb)
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
  => Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity arr)
  -> Expression db from grouping params (nullity 'PGjsonb)
  -> Maybe (Expression db from grouping params (nullity 'PGbool))
  -> Expression db from grouping params (nullity 'PGjsonb)
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
  => Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity arr)
  -> Expression db from grouping params (nullity 'PGjsonb)
  -> Maybe (Expression db from grouping params (nullity 'PGbool))
  -> Expression db from grouping params (nullity 'PGjsonb)
jsonbInsert tgt path val insertAfter = case insertAfter of
  Just i -> unsafeVariadicFunction "jsonb_insert" (tgt :* path :* val :* i :* Nil)
  Nothing -> unsafeVariadicFunction "jsonb_insert" (tgt :* path :* val :* Nil)

-- | Returns its argument as indented JSON text.
jsonbPretty
  :: Expression db from grouping params (nullity 'PGjsonb)
  -> Expression db from grouping params (nullity 'PGtext)
jsonbPretty = unsafeFunction "jsonb_pretty"

{-----------------------------------------
aggregation
-----------------------------------------}

-- | escape hatch to define aggregate functions
unsafeAggregate
  :: ByteString -- ^ aggregate function
  -> Expression db from 'Ungrouped params (xty)
  -> Expression db from ('Grouped bys) params (yty)
unsafeAggregate fun x = UnsafeExpression $ mconcat
  [fun, "(", renderExpression x, ")"]

-- | escape hatch to define aggregate functions over distinct values
unsafeAggregateDistinct
  :: ByteString -- ^ aggregate function
  -> Expression db from 'Ungrouped params (xty)
  -> Expression db from ('Grouped bys) params (yty)
unsafeAggregateDistinct fun x = UnsafeExpression $ mconcat
  [fun, "(DISTINCT ", renderExpression x, ")"]

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: 'Null 'PGnumeric]] ('Grouped bys) params ('Null 'PGnumeric)
--   expression = sum_ #col
-- in printSQL expression
-- :}
-- sum("col")
sum_
  :: ty `In` PGNum
  => Expression db from 'Ungrouped params (nullity ty)
  -- ^ what to sum
  -> Expression db from ('Grouped bys) params (nullity ty)
sum_ = unsafeAggregate "sum"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGnumeric]] ('Grouped bys) params (nullity 'PGnumeric)
--   expression = sumDistinct #col
-- in printSQL expression
-- :}
-- sum(DISTINCT "col")
sumDistinct
  :: ty `In` PGNum
  => Expression db from 'Ungrouped params (nullity ty)
  -- ^ what to sum
  -> Expression db from ('Grouped bys) params (nullity ty)
sumDistinct = unsafeAggregateDistinct "sum"

-- | A constraint for `PGType`s that you can take averages of and the resulting
-- `PGType`.
class PGAvg ty avg | ty -> avg where
  avg, avgDistinct
    :: Expression db from 'Ungrouped params (nullity ty)
    -- ^ what to average
    -> Expression db from ('Grouped bys) params (nullity avg)
  avg = unsafeAggregate "avg"
  avgDistinct = unsafeAggregateDistinct "avg"
instance PGAvg 'PGint2 'PGnumeric
instance PGAvg 'PGint4 'PGnumeric
instance PGAvg 'PGint8 'PGnumeric
instance PGAvg 'PGnumeric 'PGnumeric
instance PGAvg 'PGfloat4 'PGfloat8
instance PGAvg 'PGfloat8 'PGfloat8
instance PGAvg 'PGinterval 'PGinterval

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitAnd #col
-- in printSQL expression
-- :}
-- bit_and("col")
bitAnd
  :: int `In` PGIntegral
  => Expression db from 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity int)
bitAnd = unsafeAggregate "bit_and"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitOr #col
-- in printSQL expression
-- :}
-- bit_or("col")
bitOr
  :: int `In` PGIntegral
  => Expression db from 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity int)
bitOr = unsafeAggregate "bit_or"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitAndDistinct #col
-- in printSQL expression
-- :}
-- bit_and(DISTINCT "col")
bitAndDistinct
  :: int `In` PGIntegral
  => Expression db from 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity int)
bitAndDistinct = unsafeAggregateDistinct "bit_and"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitOrDistinct #col
-- in printSQL expression
-- :}
-- bit_or(DISTINCT "col")
bitOrDistinct
  :: int `In` PGIntegral
  => Expression db from 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity int)
bitOrDistinct = unsafeAggregateDistinct "bit_or"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolAnd #col
-- in printSQL expression
-- :}
-- bool_and("col")
boolAnd
  :: Expression db from 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity 'PGbool)
boolAnd = unsafeAggregate "bool_and"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolOr #col
-- in printSQL expression
-- :}
-- bool_or("col")
boolOr
  :: Expression db from 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity 'PGbool)
boolOr = unsafeAggregate "bool_or"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolAndDistinct #col
-- in printSQL expression
-- :}
-- bool_and(DISTINCT "col")
boolAndDistinct
  :: Expression db from 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity 'PGbool)
boolAndDistinct = unsafeAggregateDistinct "bool_and"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolOrDistinct #col
-- in printSQL expression
-- :}
-- bool_or(DISTINCT "col")
boolOrDistinct
  :: Expression db from 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity 'PGbool)
boolOrDistinct = unsafeAggregateDistinct "bool_or"

-- | A special aggregation that does not require an input
--
-- >>> printSQL countStar
-- count(*)
countStar
  :: Expression db from ('Grouped bys) params ('NotNull 'PGint8)
countStar = UnsafeExpression $ "count(*)"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity ty]] (Grouped bys) params ('NotNull 'PGint8)
--   expression = count #col
-- in printSQL expression
-- :}
-- count("col")
count
  :: Expression db from 'Ungrouped params ty
  -- ^ what to count
  -> Expression db from ('Grouped bys) params ('NotNull 'PGint8)
count = unsafeAggregate "count"

-- | >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity ty]] (Grouped bys) params ('NotNull 'PGint8)
--   expression = countDistinct #col
-- in printSQL expression
-- :}
-- count(DISTINCT "col")
countDistinct
  :: Expression db from 'Ungrouped params ty
  -- ^ what to count
  -> Expression db from ('Grouped bys) params ('NotNull 'PGint8)
countDistinct = unsafeAggregateDistinct "count"

-- | synonym for `boolAnd`
--
-- >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = every #col
-- in printSQL expression
-- :}
-- every("col")
every
  :: Expression db from 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity 'PGbool)
every = unsafeAggregate "every"

-- | synonym for `boolAndDistinct`
--
-- >>> :{
-- let
--   expression :: Expression db '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = everyDistinct #col
-- in printSQL expression
-- :}
-- every(DISTINCT "col")
everyDistinct
  :: Expression db from 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity 'PGbool)
everyDistinct = unsafeAggregateDistinct "every"

-- | minimum and maximum aggregation
max_, min_, maxDistinct, minDistinct
  :: Expression db from 'Ungrouped params (nullity ty)
  -- ^ what to aggregate
  -> Expression db from ('Grouped bys) params (nullity ty)
max_ = unsafeAggregate "max"
min_ = unsafeAggregate "min"
maxDistinct = unsafeAggregateDistinct "max"
minDistinct = unsafeAggregateDistinct "min"

{-----------------------------------------
window functions
-----------------------------------------}

{- |
:{
let
  expr :: Expression db '["tab" ::: '["a" ::: ty]] 'Ungrouped params ('NotNull 'PGint8)
  expr = rank `over` (partitionBy (#a :* Nil))
in
  printSQL expr
:}
rank() OVER (PARTITION BY "a")
-}
rank :: Expression db from 'Framed params ('NotNull 'PGint8)
rank = UnsafeExpression "rank()"

over
  :: Aggregatable agg
  => Expression db from agg params ty
  -> WindowDefinition db params from grp
  -> Expression db from grp params ty
over expr windef = UnsafeExpression $
  renderExpression expr <+> "OVER"
  <+> parenthesized (renderWindowDefinition windef)

data WindowDefinition db params from grp where
  WindowDefinition
    :: SListI bys
    => NP (Expression db from grp params) bys
    -> [SortExpression db from grp params]
    -> WindowDefinition db params from grp

class OrderBy expr where
  orderBy
    :: [SortExpression db from grp params]
    -> expr db params from grp
    -> expr db params from grp
instance OrderBy WindowDefinition where
  orderBy sortsR (WindowDefinition parts sortsL)
    = WindowDefinition parts (sortsL ++ sortsR)

renderWindowDefinition
  :: WindowDefinition db from grp params
  -> ByteString
renderWindowDefinition (WindowDefinition part ord) =
  renderPartitionByClause part <> renderOrderByClause ord
  where
    renderPartitionByClause = \case
      Nil -> ""
      parts -> "PARTITION" <+> "BY" <+> renderCommaSeparated renderExpression parts
    renderOrderByClause = \case
      [] -> ""
      srts -> " ORDER" <+> "BY"
        <+> commaSeparated (renderSortExpression <$> srts)

partitionBy
  :: SListI bys
  => NP (Expression db from grp params) bys
  -> WindowDefinition db params from grp
partitionBy bys = WindowDefinition bys []

{-----------------------------------------
sort expressions
-----------------------------------------}

-- | `SortExpression`s are used by `sortBy` to optionally sort the results
-- of a `Query`. `Asc` or `Desc` set the sort direction of a `NotNull` result
-- column to ascending or descending. Ascending order puts smaller values
-- first, where "smaller" is defined in terms of the `.<` operator. Similarly,
-- descending order is determined with the `.>` operator. `AscNullsFirst`,
-- `AscNullsLast`, `DescNullsFirst` and `DescNullsLast` options are used to
-- determine whether nulls appear before or after non-null values in the sort
-- ordering of a `Null` result column.
data SortExpression db from grouping params where
  Asc
    :: Expression db from grouping params ('NotNull ty)
    -> SortExpression db from grouping params
  Desc
    :: Expression db from grouping params ('NotNull ty)
    -> SortExpression db from grouping params
  AscNullsFirst
    :: Expression db from grouping params  ('Null ty)
    -> SortExpression db from grouping params
  AscNullsLast
    :: Expression db from grouping params  ('Null ty)
    -> SortExpression db from grouping params
  DescNullsFirst
    :: Expression db from grouping params  ('Null ty)
    -> SortExpression db from grouping params
  DescNullsLast
    :: Expression db from grouping params  ('Null ty)
    -> SortExpression db from grouping params
deriving instance Show (SortExpression db from grouping params)

-- | Render a `SortExpression`.
renderSortExpression :: SortExpression db from grouping params -> ByteString
renderSortExpression = \case
  Asc expression -> renderExpression expression <+> "ASC"
  Desc expression -> renderExpression expression <+> "DESC"
  AscNullsFirst expression -> renderExpression expression
    <+> "ASC NULLS FIRST"
  DescNullsFirst expression -> renderExpression expression
    <+> "DESC NULLS FIRST"
  AscNullsLast expression -> renderExpression expression <+> "ASC NULLS LAST"
  DescNullsLast expression -> renderExpression expression <+> "DESC NULLS LAST"

{-----------------------------------------
type expressions
-----------------------------------------}

-- | `TypeExpression`s are used in `cast`s and `createTable` commands.
newtype TypeExpression (schemas :: SchemasType) (ty :: NullityType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | The enum or composite type in a `Typedef` can be expressed by its alias.
typedef
  :: (Has sch schemas schema, Has td schema ('Typedef ty))
  => QualifiedAlias sch td
  -> TypeExpression schemas (nullity ty)
typedef = UnsafeTypeExpression . renderQualifiedAlias

-- | The composite type corresponding to a `Table` definition can be expressed
-- by its alias.
typetable
  :: (Has sch schemas schema, Has tab schema ('Table table))
  => QualifiedAlias sch tab
  -> TypeExpression schemas (nullity ('PGcomposite (TableToRow table)))
typetable = UnsafeTypeExpression . renderQualifiedAlias

-- | The composite type corresponding to a `View` definition can be expressed
-- by its alias.
typeview
  :: (Has sch schemas schema, Has vw schema ('View view))
  => QualifiedAlias sch vw
  -> TypeExpression schemas (nullity ('PGcomposite view))
typeview = UnsafeTypeExpression . renderQualifiedAlias

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
vararray ty = UnsafeTypeExpression $ renderTypeExpression ty <> "[]"
-- | fixed length array
--
-- >>> renderTypeExpression (fixarray @2 json)
-- "json[2]"
fixarray
  :: forall n schemas nullity pg. KnownNat n
  => TypeExpression schemas pg
  -> TypeExpression schemas (nullity ('PGfixarray n pg))
fixarray ty = UnsafeTypeExpression $
  renderTypeExpression ty <> "[" <> renderNat @n <> "]"

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
instance (KnownNat n, PGTyped schemas ty)
  => PGTyped schemas (nullity ('PGfixarray n ty)) where
    pgtype = fixarray @n (pgtype @schemas @ty)

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
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MagicHash
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
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
  , row
    -- ** Functions
  , unsafeBinaryOp
  , unsafeUnaryOp
  , unsafeFunction
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
  , Condition
  , true
  , false
  , not_
  , (.&&)
  , (.||)
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
    -- * Types
  , TypeExpression (UnsafeTypeExpression, renderTypeExpression)
  , PGTyped (pgtype)
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
import Data.Function ((&))
import Data.Semigroup
import Data.Ratio
import Data.String
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
  (schema :: SchemaType)
  (relations :: RelationsType)
  (grouping :: Grouping)
  (params :: [NullityType])
  (ty :: NullityType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (Expression schema relations grouping params ty) where
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
  (schema :: SchemaType)
  (params :: [NullityType])
  (ty :: NullityType)
  | n params -> ty where
    -- | `parameter` takes a `Nat` using type application and a `TypeExpression`.
    --
    -- >>> let expr = parameter @1 int4 :: Expression sch rels grp '[ 'Null 'PGint4] ('Null 'PGint4)
    -- >>> printSQL expr
    -- ($1 :: int4)
    parameter
      :: TypeExpression schema (PGTypeOf ty)
      -> Expression schema relations grouping params ty
    parameter ty = UnsafeExpression $ parenthesized $
      "$" <> renderNat (Proxy @n) <+> "::"
        <+> renderTypeExpression ty
instance {-# OVERLAPPING #-} HasParameter 1 schema (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) schema params ty)
  => HasParameter n schema (ty' : params) ty

-- | `param` takes a `Nat` using type application and for basic types,
-- infers a `TypeExpression`.
--
-- >>> let expr = param @1 :: Expression sch rels grp '[ 'Null 'PGint4] ('Null 'PGint4)
-- >>> printSQL expr
-- ($1 :: int4)
param
  :: forall n schema params relations grouping ty
   . (PGTyped schema (PGTypeOf ty), HasParameter n schema params ty)
  => Expression schema relations grouping params ty -- ^ param
param = parameter @n pgtype

instance (HasUnique relation relations columns, Has column columns ty)
  => IsLabel column (Expression schema relations 'Ungrouped params ty) where
    fromLabel = UnsafeExpression $ renderAlias (Alias @column)
instance (HasUnique relation relations columns, Has column columns ty)
  => IsLabel column
    (Aliased (Expression schema relations 'Ungrouped params) (column ::: ty)) where
    fromLabel = fromLabel @column `As` Alias @column
instance (HasUnique relation relations columns, Has column columns ty)
  => IsLabel column
    (NP (Aliased (Expression schema relations 'Ungrouped params)) '[column ::: ty]) where
    fromLabel = fromLabel @column :* Nil

instance (Has relation relations columns, Has column columns ty)
  => IsQualified relation column (Expression schema relations 'Ungrouped params ty) where
    relation ! column = UnsafeExpression $
      renderAlias relation <> "." <> renderAlias column
instance (Has relation relations columns, Has column columns ty)
  => IsQualified relation column
    (Aliased (Expression schema relations 'Ungrouped params) (column ::: ty)) where
    relation ! column = relation ! column `As` column
instance (Has relation relations columns, Has column columns ty)
  => IsQualified relation column
    (NP (Aliased (Expression schema relations 'Ungrouped params)) '[column ::: ty]) where
    relation ! column = relation ! column :* Nil

instance
  ( HasUnique relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsLabel column
    (Expression schema relations ('Grouped bys) params ty) where
      fromLabel = UnsafeExpression $ renderAlias (Alias @column)
instance
  ( HasUnique relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsLabel column
    ( Aliased (Expression schema relations ('Grouped bys) params)
      (column ::: ty) ) where
      fromLabel = fromLabel @column `As` Alias @column
instance
  ( HasUnique relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsLabel column
    ( NP (Aliased (Expression schema relations ('Grouped bys) params))
      '[column ::: ty] ) where
      fromLabel = fromLabel @column :* Nil

instance
  ( Has relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsQualified relation column
    (Expression schema relations ('Grouped bys) params ty) where
      relation ! column = UnsafeExpression $
        renderAlias relation <> "." <> renderAlias column
instance
  ( Has relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsQualified relation column
    (Aliased (Expression schema relations ('Grouped bys) params)
      (column ::: ty)) where
        relation ! column = relation ! column `As` column
instance
  ( Has relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsQualified relation column
    ( NP (Aliased (Expression schema relations ('Grouped bys) params))
      '[column ::: ty]) where
        relation ! column = relation ! column :* Nil

-- | analagous to `Nothing`
--
-- >>> printSQL null_
-- NULL
null_ :: Expression schema rels grouping params ('Null ty)
null_ = UnsafeExpression "NULL"

-- | analagous to `Just`
--
-- >>> printSQL $ notNull true
-- TRUE
notNull
  :: Expression schema rels grouping params ('NotNull ty)
  -> Expression schema rels grouping params ('Null ty)
notNull = UnsafeExpression . renderExpression

-- | return the leftmost value which is not NULL
--
-- >>> printSQL $ coalesce [null_, notNull true] false
-- COALESCE(NULL, TRUE, FALSE)
coalesce
  :: [Expression schema relations grouping params ('Null ty)]
  -- ^ @NULL@s may be present
  -> Expression schema relations grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression schema relations grouping params ('NotNull ty)
coalesce nullxs notNullx = UnsafeExpression $
  "COALESCE" <> parenthesized (commaSeparated
    ((renderExpression <$> nullxs) <> [renderExpression notNullx]))

-- | analagous to `Data.Maybe.fromMaybe` using @COALESCE@
--
-- >>> printSQL $ fromNull true null_
-- COALESCE(NULL, TRUE)
fromNull
  :: Expression schema relations grouping params ('NotNull ty)
  -- ^ what to convert @NULL@ to
  -> Expression schema relations grouping params ('Null ty)
  -> Expression schema relations grouping params ('NotNull ty)
fromNull notNullx nullx = coalesce [nullx] notNullx

-- | >>> printSQL $ null_ & isNull
-- NULL IS NULL
isNull
  :: Expression schema relations grouping params ('Null ty)
  -- ^ possibly @NULL@
  -> Condition schema relations grouping params
isNull x = UnsafeExpression $ renderExpression x <+> "IS NULL"

-- | >>> printSQL $ null_ & isNotNull
-- NULL IS NOT NULL
isNotNull
  :: Expression schema relations grouping params ('Null ty)
  -- ^ possibly @NULL@
  -> Condition schema relations grouping params
isNotNull x = UnsafeExpression $ renderExpression x <+> "IS NOT NULL"

-- | analagous to `maybe` using @IS NULL@
--
-- >>> printSQL $ matchNull true not_ null_
-- CASE WHEN NULL IS NULL THEN TRUE ELSE (NOT NULL) END
matchNull
  :: Expression schema relations grouping params (nullty)
  -- ^ what to convert @NULL@ to
  -> ( Expression schema relations grouping params ('NotNull ty)
       -> Expression schema relations grouping params (nullty) )
  -- ^ function to perform when @NULL@ is absent
  -> Expression schema relations grouping params ('Null ty)
  -> Expression schema relations grouping params (nullty)
matchNull y f x = ifThenElse (isNull x) y
  (f (UnsafeExpression (renderExpression x)))

{-| right inverse to `fromNull`, if its arguments are equal then
`nullIf` gives @NULL@.

>>> :set -XTypeApplications -XDataKinds
>>> let expr = nullIf false (param @1) :: Expression schema rels grp '[ 'NotNull 'PGbool] ('Null 'PGbool)
>>> printSQL expr
NULL IF (FALSE, ($1 :: bool))
-}
nullIf
  :: Expression schema relations grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression schema relations grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression schema relations grouping params ('Null ty)
nullIf x y = UnsafeExpression $ "NULL IF" <+> parenthesized
  (renderExpression x <> ", " <> renderExpression y)

-- | >>> printSQL $ array [null_, notNull false, notNull true]
-- ARRAY[NULL, FALSE, TRUE]
array
  :: [Expression schema relations grouping params ('Null ty)]
  -- ^ array elements
  -> Expression schema relations grouping params (nullity ('PGvararray ty))
array xs = UnsafeExpression $
  "ARRAY[" <> commaSeparated (renderExpression <$> xs) <> "]"

instance (KnownSymbol label, label `In` labels) => IsPGlabel label
  (Expression schema relations grouping params (nullity ('PGenum labels))) where
  label = UnsafeExpression $ renderLabel (PGlabel @label)

-- | A row constructor is an expression that builds a row value
-- (also called a composite value) using values for its member fields.
--
-- >>> type Complex = PGcomposite '["real" ::: 'PGfloat8, "imaginary" ::: 'PGfloat8]
-- >>> let i = row (0 `As` #real :* 1 `As` #imaginary :* Nil) :: Expression '[] '[] 'Ungrouped '[] ('NotNull Complex)
-- >>> printSQL i
-- ROW(0, 1)
row
  :: SListI (Nulls fields)
  => NP (Aliased (Expression schema relations grouping params)) (Nulls fields)
  -- ^ zero or more expressions for the row field values
  -> Expression schema relations grouping params (nullity ('PGcomposite fields))
row exprs = UnsafeExpression $ "ROW" <> parenthesized
  (renderCommaSeparated (\ (expr `As` _) -> renderExpression expr) exprs)

instance Has field fields ty => IsLabel field
  (   Expression schema relation grouping params (nullity ('PGcomposite fields))
   -> Expression schema relation grouping params ('Null ty) ) where
    fromLabel expr = UnsafeExpression $
      parenthesized (renderExpression expr) <> "." <>
        fromString (symbolVal (Proxy @field))

instance Semigroup
  (Expression schema relations grouping params (nullity ('PGvararray ty))) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression schema relations grouping params (nullity ('PGvararray ty))) where
    mempty = array []
    mappend = (<>)

-- | >>> let expr = greatest currentTimestamp [param @1] :: Expression sch rels grp '[ 'NotNull 'PGtimestamptz] ('NotNull 'PGtimestamptz)
-- >>> printSQL expr
-- GREATEST(CURRENT_TIMESTAMP, ($1 :: timestamp with time zone))
greatest
  :: Expression schema relations grouping params (nullty)
  -- ^ needs at least 1 argument
  -> [Expression schema relations grouping params (nullty)]
  -- ^ or more
  -> Expression schema relations grouping params (nullty)
greatest x xs = UnsafeExpression $ "GREATEST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

-- | >>> printSQL $ least currentTimestamp [null_]
-- LEAST(CURRENT_TIMESTAMP, NULL)
least
  :: Expression schema relations grouping params (nullty)
  -- ^ needs at least 1 argument
  -> [Expression schema relations grouping params (nullty)]
  -- ^ or more
  -> Expression schema relations grouping params (nullty)
least x xs = UnsafeExpression $ "LEAST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

-- | >>> printSQL $ unsafeBinaryOp "OR" true false
-- (TRUE OR FALSE)
unsafeBinaryOp
  :: ByteString
  -- ^ operator
  -> Expression schema relations grouping params (ty0)
  -> Expression schema relations grouping params (ty1)
  -> Expression schema relations grouping params (ty2)
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderExpression x <+> op <+> renderExpression y

-- | >>> printSQL $ unsafeUnaryOp "NOT" true
-- (NOT TRUE)
unsafeUnaryOp
  :: ByteString
  -- ^ operator
  -> Expression schema relations grouping params (ty0)
  -> Expression schema relations grouping params (ty1)
unsafeUnaryOp op x = UnsafeExpression $ parenthesized $
  op <+> renderExpression x

-- | >>> printSQL $ unsafeFunction "f" true
-- f(TRUE)
unsafeFunction
  :: ByteString
  -- ^ function
  -> Expression schema relations grouping params (xty)
  -> Expression schema relations grouping params (yty)
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderExpression x)

unsafeVariadicFunction
  :: ByteString
  -- ^ function
  -> [Expression schema relations grouping params xty]
  -> Expression schema relations grouping params (yty)
unsafeVariadicFunction fun x = UnsafeExpression $
  fun <> parenthesized (commaSeparated (map renderExpression x))

instance PGNum ty
  => Num (Expression schema relations grouping params (nullity ty)) where
    (+) = unsafeBinaryOp "+"
    (-) = unsafeBinaryOp "-"
    (*) = unsafeBinaryOp "*"
    abs = unsafeFunction "abs"
    signum = unsafeFunction "sign"
    fromInteger
      = UnsafeExpression
      . fromString
      . show

instance (PGNum ty, PGFloating ty) => Fractional
  (Expression schema relations grouping params (nullity ty)) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (PGNum ty, PGFloating ty) => Floating
  (Expression schema relations grouping params (nullity ty)) where
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
--   expression :: Expression schema relations grouping params (nullity 'PGfloat4)
--   expression = atan2_ pi 2
-- in printSQL expression
-- :}
-- atan2(pi(), 2)
atan2_
  :: PGFloating float
  => Expression schema relations grouping params (nullity float)
  -- ^ numerator
  -> Expression schema relations grouping params (nullity float)
  -- ^ denominator
  -> Expression schema relations grouping params (nullity float)
atan2_ y x = UnsafeExpression $
  "atan2(" <> renderExpression y <> ", " <> renderExpression x <> ")"

-- When a `cast` is applied to an `Expression` of a known type, it
-- represents a run-time type conversion. The cast will succeed only if a
-- suitable type conversion operation has been defined.
--
-- | >>> printSQL $ true & cast int4
-- (TRUE :: int4)
cast
  :: TypeExpression schema ty1
  -- ^ type to cast as
  -> Expression schema relations grouping params (nullity ty0)
  -- ^ value to convert
  -> Expression schema relations grouping params (nullity ty1)
cast ty x = UnsafeExpression $ parenthesized $
  renderExpression x <+> "::" <+> renderTypeExpression ty

-- | integer division, truncates the result
--
-- >>> :{
-- let
--   expression :: Expression schema relations grouping params (nullity 'PGint2)
--   expression = 5 `quot_` 2
-- in printSQL expression
-- :}
-- (5 / 2)
quot_
  :: PGIntegral int
  => Expression schema relations grouping params (nullity int)
  -- ^ numerator
  -> Expression schema relations grouping params (nullity int)
  -- ^ denominator
  -> Expression schema relations grouping params (nullity int)
quot_ = unsafeBinaryOp "/"

-- | remainder upon integer division
--
-- >>> :{
-- let
--   expression :: Expression schema relations grouping params (nullity 'PGint2)
--   expression = 5 `rem_` 2
-- in printSQL expression
-- :}
-- (5 % 2)
rem_
  :: PGIntegral int
  => Expression schema relations grouping params (nullity int)
  -- ^ numerator
  -> Expression schema relations grouping params (nullity int)
  -- ^ denominator
  -> Expression schema relations grouping params (nullity int)
rem_ = unsafeBinaryOp "%"

-- | >>> :{
-- let
--   expression :: Expression schema relations grouping params (nullity 'PGfloat4)
--   expression = trunc pi
-- in printSQL expression
-- :}
-- trunc(pi())
trunc
  :: PGFloating frac
  => Expression schema relations grouping params (nullity frac)
  -- ^ fractional number
  -> Expression schema relations grouping params (nullity frac)
trunc = unsafeFunction "trunc"

-- | >>> :{
-- let
--   expression :: Expression schema relations grouping params (nullity 'PGfloat4)
--   expression = round_ pi
-- in printSQL expression
-- :}
-- round(pi())
round_
  :: PGFloating frac
  => Expression schema relations grouping params (nullity frac)
  -- ^ fractional number
  -> Expression schema relations grouping params (nullity frac)
round_ = unsafeFunction "round"

-- | >>> :{
-- let
--   expression :: Expression schema relations grouping params (nullity 'PGfloat4)
--   expression = ceiling_ pi
-- in printSQL expression
-- :}
-- ceiling(pi())
ceiling_
  :: PGFloating frac
  => Expression schema relations grouping params (nullity frac)
  -- ^ fractional number
  -> Expression schema relations grouping params (nullity frac)
ceiling_ = unsafeFunction "ceiling"

-- | A `Condition` is a boolean valued `Expression`. While SQL allows
-- conditions to have @NULL@, Squeal instead chooses to disallow @NULL@,
-- forcing one to handle the case of @NULL@ explicitly to produce
-- a `Condition`.
type Condition schema relations grouping params =
  Expression schema relations grouping params ('NotNull 'PGbool)

-- | >>> printSQL true
-- TRUE
true :: Condition schema relations grouping params
true = UnsafeExpression "TRUE"

-- | >>> printSQL false
-- FALSE
false :: Condition schema relations grouping params
false = UnsafeExpression "FALSE"

-- | >>> printSQL $ not_ true
-- (NOT TRUE)
not_
  :: Condition schema relations grouping params
  -> Condition schema relations grouping params
not_ = unsafeUnaryOp "NOT"

-- | >>> printSQL $ true .&& false
-- (TRUE AND FALSE)
(.&&)
  :: Condition schema relations grouping params
  -> Condition schema relations grouping params
  -> Condition schema relations grouping params
(.&&) = unsafeBinaryOp "AND"

-- | >>> printSQL $ true .|| false
-- (TRUE OR FALSE)
(.||)
  :: Condition schema relations grouping params
  -> Condition schema relations grouping params
  -> Condition schema relations grouping params
(.||) = unsafeBinaryOp "OR"

-- | >>> :{
-- let
--   expression :: Expression schema relations grouping params (nullity 'PGint2)
--   expression = caseWhenThenElse [(true, 1), (false, 2)] 3
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN 1 WHEN FALSE THEN 2 ELSE 3 END
caseWhenThenElse
  :: [ ( Condition schema relations grouping params
       , Expression schema relations grouping params (ty)
     ) ]
  -- ^ whens and thens
  -> Expression schema relations grouping params (ty)
  -- ^ else
  -> Expression schema relations grouping params (ty)
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
--   expression :: Expression schema relations grouping params (nullity 'PGint2)
--   expression = ifThenElse true 1 0
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN 1 ELSE 0 END
ifThenElse
  :: Condition schema relations grouping params
  -> Expression schema relations grouping params (ty) -- ^ then
  -> Expression schema relations grouping params (ty) -- ^ else
  -> Expression schema relations grouping params (ty)
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

-- | Comparison operations like `.==`, `./=`, `.>`, `.>=`, `.<` and `.<=`
-- will produce @NULL@s if one of their arguments is @NULL@.
--
-- >>> printSQL $ notNull true .== null_
-- (TRUE = NULL)
(.==)
  :: Expression schema relations grouping params (nullity ty) -- ^ lhs
  -> Expression schema relations grouping params (nullity ty) -- ^ rhs
  -> Expression schema relations grouping params (nullity 'PGbool)
(.==) = unsafeBinaryOp "="
infix 4 .==

-- | >>> printSQL $ notNull true ./= null_
-- (TRUE <> NULL)
(./=)
  :: Expression schema relations grouping params (nullity ty) -- ^ lhs
  -> Expression schema relations grouping params (nullity ty) -- ^ rhs
  -> Expression schema relations grouping params (nullity 'PGbool)
(./=) = unsafeBinaryOp "<>"
infix 4 ./=

-- | >>> printSQL $ notNull true .>= null_
-- (TRUE >= NULL)
(.>=)
  :: Expression schema relations grouping params (nullity ty) -- ^ lhs
  -> Expression schema relations grouping params (nullity ty) -- ^ rhs
  -> Expression schema relations grouping params (nullity 'PGbool)
(.>=) = unsafeBinaryOp ">="
infix 4 .>=

-- | >>> printSQL $ notNull true .< null_
-- (TRUE < NULL)
(.<)
  :: Expression schema relations grouping params (nullity ty) -- ^ lhs
  -> Expression schema relations grouping params (nullity ty) -- ^ rhs
  -> Expression schema relations grouping params (nullity 'PGbool)
(.<) = unsafeBinaryOp "<"
infix 4 .<

-- | >>> printSQL $ notNull true .<= null_
-- (TRUE <= NULL)
(.<=)
  :: Expression schema relations grouping params (nullity ty) -- ^ lhs
  -> Expression schema relations grouping params (nullity ty) -- ^ rhs
  -> Expression schema relations grouping params (nullity 'PGbool)
(.<=) = unsafeBinaryOp "<="
infix 4 .<=

-- | >>> printSQL $ notNull true .> null_
-- (TRUE > NULL)
(.>)
  :: Expression schema relations grouping params (nullity ty) -- ^ lhs
  -> Expression schema relations grouping params (nullity ty) -- ^ rhs
  -> Expression schema relations grouping params (nullity 'PGbool)
(.>) = unsafeBinaryOp ">"
infix 4 .>

-- | >>> printSQL currentDate
-- CURRENT_DATE
currentDate
  :: Expression schema relations grouping params (nullity 'PGdate)
currentDate = UnsafeExpression "CURRENT_DATE"

-- | >>> printSQL currentTime
-- CURRENT_TIME
currentTime
  :: Expression schema relations grouping params (nullity 'PGtimetz)
currentTime = UnsafeExpression "CURRENT_TIME"

-- | >>> printSQL currentTimestamp
-- CURRENT_TIMESTAMP
currentTimestamp
  :: Expression schema relations grouping params (nullity 'PGtimestamptz)
currentTimestamp = UnsafeExpression "CURRENT_TIMESTAMP"

-- | >>> printSQL localTime
-- LOCALTIME
localTime
  :: Expression schema relations grouping params (nullity 'PGtime)
localTime = UnsafeExpression "LOCALTIME"

-- | >>> printSQL localTimestamp
-- LOCALTIMESTAMP
localTimestamp
  :: Expression schema relations grouping params (nullity 'PGtimestamp)
localTimestamp = UnsafeExpression "LOCALTIMESTAMP"

{-----------------------------------------
text
-----------------------------------------}

instance IsString
  (Expression schema relations grouping params (nullity 'PGtext)) where
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
  (Expression schema relations grouping params (nullity 'PGtext)) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression schema relations grouping params (nullity 'PGtext)) where
    mempty = fromString ""
    mappend = (<>)

-- | >>> printSQL $ lower "ARRRGGG"
-- lower(E'ARRRGGG')
lower
  :: Expression schema relations grouping params (nullity 'PGtext)
  -- ^ string to lower case
  -> Expression schema relations grouping params (nullity 'PGtext)
lower = unsafeFunction "lower"

-- | >>> printSQL $ upper "eeee"
-- upper(E'eeee')
upper
  :: Expression schema relations grouping params (nullity 'PGtext)
  -- ^ string to upper case
  -> Expression schema relations grouping params (nullity 'PGtext)
upper = unsafeFunction "upper"

-- | >>> printSQL $ charLength "four"
-- char_length(E'four')
charLength
  :: Expression schema relations grouping params (nullity 'PGtext)
  -- ^ string to measure
  -> Expression schema relations grouping params (nullity 'PGint4)
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
  :: Expression schema relations grouping params (nullity 'PGtext)
  -- ^ string
  -> Expression schema relations grouping params (nullity 'PGtext)
  -- ^ pattern
  -> Expression schema relations grouping params (nullity 'PGbool)
like = unsafeBinaryOp "LIKE"

{-----------------------------------------
json and jsonb operators
see https://www.postgresql.org/docs/10/static/functions-json.html
-----------------------------------------}

type IsJSONKey key = key `In` '[ 'PGint2, 'PGint4, 'PGtext ]
type IsJSON json = json `In` '[ 'PGjson, 'PGjsonb ]

class IsArray arr => IsArrayOf arr ty
instance arrTy ~ expectTy => IsArrayOf ('PGvararray arrTy) expectTy
instance arrTy ~ expectTy => IsArrayOf ('PGfixarray n arrTy) expectTy

-- IsArray will generate nicer error messages when it is all that is required.
class IsArray arr
instance IsArray ('PGvararray x)
instance IsArray ('PGfixarray n x)

type IsPGtextArray arr = IsArrayOf arr 'PGtext

-- | Get JSON value (object field or array element) at a key.
(.->)
  :: (IsJSON json, IsJSONKey key)
  => Expression schema relations grouping params (jnull json)
  -> Expression schema relations grouping params (knull key)
  -> Expression schema relations grouping params ('Null json)
(.->) = unsafeBinaryOp "->"

-- | Get JSON value (object field or array element) at a key, as text.
(.->>)
  :: (IsJSON json, IsJSONKey key)
  => Expression schema relations grouping params (jnull json)
  -> Expression schema relations grouping params (knull key)
  -> Expression schema relations grouping params ('Null 'PGtext)
(.->>) = unsafeBinaryOp "->>"

-- | Get JSON value at a specified path.
(.#>)
  :: (IsJSON json, IsPGtextArray path)
  => Expression schema relations grouping params (jnull json)
  -> Expression schema relations grouping params (knull path)
  -> Expression schema relations grouping params ('Null json)
(.#>) = unsafeBinaryOp "#>"

-- | Get JSON value at a specified path as text.
(.#>>)
  :: (IsJSON json, IsPGtextArray path)
  => Expression schema relations grouping params (jnull json)
  -> Expression schema relations grouping params (knull path)
  -> Expression schema relations grouping params ('Null 'PGtext)
(.#>>) = unsafeBinaryOp "#>"

-- Additional jsonb operators

-- | Or operator for nulls; if either argument is null, then the result is null.
type family Null2 a b where
  Null2 'Null a = 'Null
  Null2 a 'Null = 'Null
  Null2 'NotNull 'NotNull = 'NotNull

-- | See 'Null2'
type family NullOfN xs where
  NullOfN ('Null ty:xs) = 'Null
  NullOfN (x:xs)        = NullOfN xs
  NullOfN '[]           = 'NotNull

-- | Does the left JSON value contain the right JSON path/value entries at the
-- top level?
(.@>)
  :: Expression schema relations grouping params (null0 'PGjsonb)
  -> Expression schema relations grouping params (null1 'PGjsonb)
  -> Expression schema relations grouping params (Null2 null0 null1 'PGbool)
(.@>) = unsafeBinaryOp "@>"

-- | Are the left JSON path/value entries contained at the top level within the
-- right JSON value?
(.<@)
  :: Expression schema relations grouping params (null0 'PGjsonb)
  -> Expression schema relations grouping params (null1 'PGjsonb)
  -> Expression schema relations grouping params (Null2 null0 null1 'PGbool)
(.<@) = unsafeBinaryOp "<@"

-- | Does the string exist as a top-level key within the JSON value?
(.?)
  :: Expression schema relations grouping params (null0 'PGjsonb)
  -> Expression schema relations grouping params (null1 'PGtext)
  -> Expression schema relations grouping params (Null2 null0 null1 'PGbool)
(.?) = unsafeBinaryOp "?"

-- | Do any of these array strings exist as top-level keys?
(.?|)
  :: Expression schema relations grouping params (null0 'PGjsonb)
  -> Expression schema relations grouping params (null1 ('PGvararray 'PGtext))
  -> Expression schema relations grouping params (Null2 null0 null1 'PGbool)
(.?|) = unsafeBinaryOp "?|"

-- | Do all of these array strings exist as top-level keys?
(.?&)
  :: Expression schema relations grouping params (null0 'PGjsonb)
  -> Expression schema relations grouping params (null1 ('PGvararray 'PGtext))
  -> Expression schema relations grouping params (Null2 null0 null1 'PGbool)
(.?&) = unsafeBinaryOp "?&"

-- | Concatenate two jsonb values into a new jsonb value
(.||.)
  :: Expression schema relations grouping params (null0 'PGjsonb)
  -> Expression schema relations grouping params (null1 'PGjsonb)
  -> Expression schema relations grouping params (Null2 null0 null1 'PGjsonb)
(.||.) = unsafeBinaryOp "||"

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
  :: (key `In` '[ 'PGtext, 'PGvararray 'PGtext, 'PGint4, 'PGint2 ]) -- hlint error without parens here
  => Expression schema relations grouping params (null0 'PGjsonb)
  -> Expression schema relations grouping params (null1 key)
  -> Expression schema relations grouping params (Null2 null0 null1 'PGjsonb)
(.-.) = unsafeBinaryOp "-"

-- | Delete the field or element with specified path (for JSON arrays, negative
-- integers count from the end)
(#-.)
  :: IsPGtextArray arrayty
  => Expression schema relations grouping params (null0 'PGjsonb)
  -> Expression schema relations grouping params (null1 arrayty)
  -> Expression schema relations grouping params (Null2 null0 null1 'PGjsonb)
(#-.) = unsafeBinaryOp "#-"

toJson
  :: Expression schema relations grouping params (nullity ty)
  -> Expression schema relations grouping params (nullity 'PGjson)
toJson = unsafeFunction "to_json"

toJsonb
  :: Expression schema relations grouping params (nullity ty)
  -> Expression schema relations grouping params (nullity 'PGjsonb)
toJsonb = unsafeUnaryOp "to_jsonb"

arrayToJson
  :: IsArray arr
  => Expression schema relations grouping params (nullity arr)
  -> Expression schema relations grouping params (nullity 'PGjson)
arrayToJson = unsafeFunction "array_to_json"

rowToJson
  :: Expression schema relations grouping params (nullity ('PGcomposite ty))
  -> Expression schema relations grouping params (nullity 'PGjson)
rowToJson = unsafeFunction "row_to_json"

-- TODO: jsonBuildArray, jsonbBuildArray, jsonObject, jsonbObject
-- probably something like
-- :: NP (Expression s r g p) xs -> Expression s r g p ('NotNull 'PGjson[b])

{-----------------------------------------
aggregation
-----------------------------------------}

-- | escape hatch to define aggregate functions
unsafeAggregate
  :: ByteString -- ^ aggregate function
  -> Expression schema relations 'Ungrouped params (xty)
  -> Expression schema relations ('Grouped bys) params (yty)
unsafeAggregate fun x = UnsafeExpression $ mconcat
  [fun, "(", renderExpression x, ")"]

-- | escape hatch to define aggregate functions over distinct values
unsafeAggregateDistinct
  :: ByteString -- ^ aggregate function
  -> Expression schema relations 'Ungrouped params (xty)
  -> Expression schema relations ('Grouped bys) params (yty)
unsafeAggregateDistinct fun x = UnsafeExpression $ mconcat
  [fun, "(DISTINCT ", renderExpression x, ")"]

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: 'Null 'PGnumeric]] ('Grouped bys) params ('Null 'PGnumeric)
--   expression = sum_ #col
-- in printSQL expression
-- :}
-- sum("col")
sum_
  :: PGNum ty
  => Expression schema relations 'Ungrouped params (nullity ty)
  -- ^ what to sum
  -> Expression schema relations ('Grouped bys) params (nullity ty)
sum_ = unsafeAggregate "sum"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGnumeric]] ('Grouped bys) params (nullity 'PGnumeric)
--   expression = sumDistinct #col
-- in printSQL expression
-- :}
-- sum(DISTINCT "col")
sumDistinct
  :: PGNum ty
  => Expression schema relations 'Ungrouped params (nullity ty)
  -- ^ what to sum
  -> Expression schema relations ('Grouped bys) params (nullity ty)
sumDistinct = unsafeAggregateDistinct "sum"

-- | A constraint for `PGType`s that you can take averages of and the resulting
-- `PGType`.
class PGAvg ty avg | ty -> avg where
  avg, avgDistinct
    :: Expression schema relations 'Ungrouped params (nullity ty)
    -- ^ what to average
    -> Expression schema relations ('Grouped bys) params (nullity avg)
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
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitAnd #col
-- in printSQL expression
-- :}
-- bit_and("col")
bitAnd
  :: PGIntegral int
  => Expression schema relations 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity int)
bitAnd = unsafeAggregate "bit_and"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitOr #col
-- in printSQL expression
-- :}
-- bit_or("col")
bitOr
  :: PGIntegral int
  => Expression schema relations 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity int)
bitOr = unsafeAggregate "bit_or"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitAndDistinct #col
-- in printSQL expression
-- :}
-- bit_and(DISTINCT "col")
bitAndDistinct
  :: PGIntegral int
  => Expression schema relations 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity int)
bitAndDistinct = unsafeAggregateDistinct "bit_and"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitOrDistinct #col
-- in printSQL expression
-- :}
-- bit_or(DISTINCT "col")
bitOrDistinct
  :: PGIntegral int
  => Expression schema relations 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity int)
bitOrDistinct = unsafeAggregateDistinct "bit_or"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolAnd #col
-- in printSQL expression
-- :}
-- bool_and("col")
boolAnd
  :: Expression schema relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity 'PGbool)
boolAnd = unsafeAggregate "bool_and"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolOr #col
-- in printSQL expression
-- :}
-- bool_or("col")
boolOr
  :: Expression schema relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity 'PGbool)
boolOr = unsafeAggregate "bool_or"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolAndDistinct #col
-- in printSQL expression
-- :}
-- bool_and(DISTINCT "col")
boolAndDistinct
  :: Expression schema relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity 'PGbool)
boolAndDistinct = unsafeAggregateDistinct "bool_and"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolOrDistinct #col
-- in printSQL expression
-- :}
-- bool_or(DISTINCT "col")
boolOrDistinct
  :: Expression schema relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity 'PGbool)
boolOrDistinct = unsafeAggregateDistinct "bool_or"

-- | A special aggregation that does not require an input
--
-- >>> printSQL countStar
-- count(*)
countStar
  :: Expression schema relations ('Grouped bys) params ('NotNull 'PGint8)
countStar = UnsafeExpression $ "count(*)"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity ty]] (Grouped bys) params ('NotNull 'PGint8)
--   expression = count #col
-- in printSQL expression
-- :}
-- count("col")
count
  :: Expression schema relations 'Ungrouped params ty
  -- ^ what to count
  -> Expression schema relations ('Grouped bys) params ('NotNull 'PGint8)
count = unsafeAggregate "count"

-- | >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity ty]] (Grouped bys) params ('NotNull 'PGint8)
--   expression = countDistinct #col
-- in printSQL expression
-- :}
-- count(DISTINCT "col")
countDistinct
  :: Expression schema relations 'Ungrouped params ty
  -- ^ what to count
  -> Expression schema relations ('Grouped bys) params ('NotNull 'PGint8)
countDistinct = unsafeAggregateDistinct "count"

-- | synonym for `boolAnd`
--
-- >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = every #col
-- in printSQL expression
-- :}
-- every("col")
every
  :: Expression schema relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity 'PGbool)
every = unsafeAggregate "every"

-- | synonym for `boolAndDistinct`
--
-- >>> :{
-- let
--   expression :: Expression schema '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = everyDistinct #col
-- in printSQL expression
-- :}
-- every(DISTINCT "col")
everyDistinct
  :: Expression schema relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity 'PGbool)
everyDistinct = unsafeAggregateDistinct "every"

-- | minimum and maximum aggregation
max_, min_, maxDistinct, minDistinct
  :: Expression schema relations 'Ungrouped params (nullity ty)
  -- ^ what to aggregate
  -> Expression schema relations ('Grouped bys) params (nullity ty)
max_ = unsafeAggregate "max"
min_ = unsafeAggregate "min"
maxDistinct = unsafeAggregateDistinct "max"
minDistinct = unsafeAggregateDistinct "min"

{-----------------------------------------
type expressions
-----------------------------------------}

-- | `TypeExpression`s are used in `cast`s and `createTable` commands.
newtype TypeExpression (schema :: SchemaType) (ty :: PGType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance (Has alias schema ('Typedef ty))
  => IsLabel alias (TypeExpression schema ty) where
    fromLabel = UnsafeTypeExpression (renderAlias (fromLabel @alias))

-- | logical Boolean (true/false)
bool :: TypeExpression schema 'PGbool
bool = UnsafeTypeExpression "bool"
-- | signed two-byte integer
int2, smallint :: TypeExpression schema 'PGint2
int2 = UnsafeTypeExpression "int2"
smallint = UnsafeTypeExpression "smallint"
-- | signed four-byte integer
int4, int, integer :: TypeExpression schema 'PGint4
int4 = UnsafeTypeExpression "int4"
int = UnsafeTypeExpression "int"
integer = UnsafeTypeExpression "integer"
-- | signed eight-byte integer
int8, bigint :: TypeExpression schema 'PGint8
int8 = UnsafeTypeExpression "int8"
bigint = UnsafeTypeExpression "bigint"
-- | arbitrary precision numeric type
numeric :: TypeExpression schema 'PGnumeric
numeric = UnsafeTypeExpression "numeric"
-- | single precision floating-point number (4 bytes)
float4, real :: TypeExpression schema 'PGfloat4
float4 = UnsafeTypeExpression "float4"
real = UnsafeTypeExpression "real"
-- | double precision floating-point number (8 bytes)
float8, doublePrecision :: TypeExpression schema 'PGfloat8
float8 = UnsafeTypeExpression "float8"
doublePrecision = UnsafeTypeExpression "double precision"
-- | variable-length character string
text :: TypeExpression schema 'PGtext
text = UnsafeTypeExpression "text"
-- | fixed-length character string
char, character
  :: (KnownNat n, 1 <= n)
  => proxy n
  -> TypeExpression schema ('PGchar n)
char p = UnsafeTypeExpression $ "char(" <> renderNat p <> ")"
character p = UnsafeTypeExpression $  "character(" <> renderNat p <> ")"
-- | variable-length character string
varchar, characterVarying
  :: (KnownNat n, 1 <= n)
  => proxy n
  -> TypeExpression schema ('PGvarchar n)
varchar p = UnsafeTypeExpression $ "varchar(" <> renderNat p <> ")"
characterVarying p = UnsafeTypeExpression $
  "character varying(" <> renderNat p <> ")"
-- | binary data ("byte array")
bytea :: TypeExpression schema 'PGbytea
bytea = UnsafeTypeExpression "bytea"
-- | date and time (no time zone)
timestamp :: TypeExpression schema 'PGtimestamp
timestamp = UnsafeTypeExpression "timestamp"
-- | date and time, including time zone
timestampWithTimeZone :: TypeExpression schema 'PGtimestamptz
timestampWithTimeZone = UnsafeTypeExpression "timestamp with time zone"
-- | calendar date (year, month, day)
date :: TypeExpression schema 'PGdate
date = UnsafeTypeExpression "date"
-- | time of day (no time zone)
time :: TypeExpression schema 'PGtime
time = UnsafeTypeExpression "time"
-- | time of day, including time zone
timeWithTimeZone :: TypeExpression schema 'PGtimetz
timeWithTimeZone = UnsafeTypeExpression "time with time zone"
-- | time span
interval :: TypeExpression schema 'PGinterval
interval = UnsafeTypeExpression "interval"
-- | universally unique identifier
uuid :: TypeExpression schema 'PGuuid
uuid = UnsafeTypeExpression "uuid"
-- | IPv4 or IPv6 host address
inet :: TypeExpression schema 'PGinet
inet = UnsafeTypeExpression "inet"
-- | textual JSON data
json :: TypeExpression schema 'PGjson
json = UnsafeTypeExpression "json"
-- | binary JSON data, decomposed
jsonb :: TypeExpression schema 'PGjsonb
jsonb = UnsafeTypeExpression "jsonb"
-- | variable length array
vararray
  :: TypeExpression schema pg
  -> TypeExpression schema ('PGvararray pg)
vararray ty = UnsafeTypeExpression $ renderTypeExpression ty <> "[]"
-- | fixed length array
--
-- >>> renderTypeExpression (fixarray (Proxy @2) json)
-- "json[2]"
fixarray
  :: KnownNat n
  => proxy n
  -> TypeExpression schema pg
  -> TypeExpression schema ('PGfixarray n pg)
fixarray p ty = UnsafeTypeExpression $
  renderTypeExpression ty <> "[" <> renderNat p <> "]"

-- | `pgtype` is a demoted version of a `PGType`
class PGTyped schema (ty :: PGType) where pgtype :: TypeExpression schema ty
instance PGTyped schema 'PGbool where pgtype = bool
instance PGTyped schema 'PGint2 where pgtype = int2
instance PGTyped schema 'PGint4 where pgtype = int4
instance PGTyped schema 'PGint8 where pgtype = int8
instance PGTyped schema 'PGnumeric where pgtype = numeric
instance PGTyped schema 'PGfloat4 where pgtype = float4
instance PGTyped schema 'PGfloat8 where pgtype = float8
instance PGTyped schema 'PGtext where pgtype = text
instance (KnownNat n, 1 <= n)
  => PGTyped schema ('PGchar n) where pgtype = char (Proxy @n)
instance (KnownNat n, 1 <= n)
  => PGTyped schema ('PGvarchar n) where pgtype = varchar (Proxy @n)
instance PGTyped schema 'PGbytea where pgtype = bytea
instance PGTyped schema 'PGtimestamp where pgtype = timestamp
instance PGTyped schema 'PGtimestamptz where pgtype = timestampWithTimeZone
instance PGTyped schema 'PGdate where pgtype = date
instance PGTyped schema 'PGtime where pgtype = time
instance PGTyped schema 'PGtimetz where pgtype = timeWithTimeZone
instance PGTyped schema 'PGinterval where pgtype = interval
instance PGTyped schema 'PGuuid where pgtype = uuid
instance PGTyped schema 'PGjson where pgtype = json
instance PGTyped schema 'PGjsonb where pgtype = jsonb
instance PGTyped schema ty => PGTyped schema ('PGvararray ty) where
  pgtype = vararray (pgtype @schema @ty)
instance (KnownNat n, PGTyped schema ty) => PGTyped schema ('PGfixarray n ty) where
  pgtype = fixarray (Proxy @n) (pgtype @schema @ty)

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
  , HasParameter (param)
    -- ** Null
  , null_
  , notNull
  , coalesce
  , fromNull
  , isNull
  , isNotNull
  , matchNull
  , nullIf
    -- ** Arrays, Enums, Composites
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
    -- * Tables
  , Table (UnsafeTable, renderTable)
  , View (UnsafeView, renderView)
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
  (relations :: RelationsType)
  (grouping :: Grouping)
  (params :: [NullityType])
  (ty :: NullityType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

{- | A `HasParameter` constraint is used to indicate a value that is
supplied externally to a SQL statement.
`Squeal.PostgreSQL.PQ.manipulateParams`,
`Squeal.PostgreSQL.PQ.queryParams` and
`Squeal.PostgreSQL.PQ.traversePrepared` support specifying data values
separately from the SQL command string, in which case `param`s are used to
refer to the out-of-line data values.
-}
class (PGTyped (PGTypeOf ty), KnownNat n) => HasParameter
  (n :: Nat)
  (params :: [NullityType])
  (ty :: NullityType)
  | n params -> ty where
    param :: Expression relations grouping params ty
    param = UnsafeExpression $ parenthesized $
      "$" <> renderNat (Proxy @n) <+> "::"
        <+> renderTypeExpression (pgtype @(PGTypeOf ty))
instance {-# OVERLAPPING #-} PGTyped (PGTypeOf ty1)
  => HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

instance (HasUnique relation relations columns, Has column columns ty)
  => IsLabel column (Expression relations 'Ungrouped params ty) where
    fromLabel = UnsafeExpression $ renderAlias (Alias @column)
instance (HasUnique relation relations columns, Has column columns ty)
  => IsLabel column
    (Aliased (Expression relations 'Ungrouped params) (column ::: ty)) where
    fromLabel = fromLabel @column `As` Alias @column
instance (HasUnique relation relations columns, Has column columns ty)
  => IsLabel column
    (NP (Aliased (Expression relations 'Ungrouped params)) '[column ::: ty]) where
    fromLabel = fromLabel @column :* Nil

instance (Has relation relations columns, Has column columns ty)
  => IsQualified relation column (Expression relations 'Ungrouped params ty) where
    relation ! column = UnsafeExpression $
      renderAlias relation <> "." <> renderAlias column
instance (Has relation relations columns, Has column columns ty)
  => IsQualified relation column
    (Aliased (Expression relations 'Ungrouped params) (column ::: ty)) where
    relation ! column = relation ! column `As` column
instance (Has relation relations columns, Has column columns ty)
  => IsQualified relation column
    (NP (Aliased (Expression relations 'Ungrouped params)) '[column ::: ty]) where
    relation ! column = relation ! column :* Nil

instance
  ( HasUnique relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsLabel column
    (Expression relations ('Grouped bys) params ty) where
      fromLabel = UnsafeExpression $ renderAlias (Alias @column)
instance
  ( HasUnique relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsLabel column
    ( Aliased (Expression relations ('Grouped bys) params)
      (column ::: ty) ) where
      fromLabel = fromLabel @column `As` Alias @column
instance
  ( HasUnique relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsLabel column
    ( NP (Aliased (Expression relations ('Grouped bys) params))
      '[column ::: ty] ) where
      fromLabel = fromLabel @column :* Nil

instance
  ( Has relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsQualified relation column
    (Expression relations ('Grouped bys) params ty) where
      relation ! column = UnsafeExpression $
        renderAlias relation <> "." <> renderAlias column
instance
  ( Has relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsQualified relation column
    (Aliased (Expression relations ('Grouped bys) params)
      (column ::: ty)) where
        relation ! column = relation ! column `As` column
instance
  ( Has relation relations columns
  , Has column columns ty
  , GroupedBy relation column bys
  ) => IsQualified relation column
    ( NP (Aliased (Expression relations ('Grouped bys) params))
      '[column ::: ty]) where
        relation ! column = relation ! column :* Nil

-- | analagous to `Nothing`
--
-- >>> renderExpression $ null_
-- "NULL"
null_ :: Expression rels grouping params ('Null ty)
null_ = UnsafeExpression "NULL"

-- | analagous to `Just`
--
-- >>> renderExpression $ notNull true
-- "TRUE"
notNull
  :: Expression rels grouping params ('NotNull ty)
  -> Expression rels grouping params ('Null ty)
notNull = UnsafeExpression . renderExpression

-- | return the leftmost value which is not NULL
--
-- >>> renderExpression $ coalesce [null_, notNull true] false
-- "COALESCE(NULL, TRUE, FALSE)"
coalesce
  :: [Expression relations grouping params ('Null ty)]
  -- ^ @NULL@s may be present
  -> Expression relations grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression relations grouping params ('NotNull ty)
coalesce nullxs notNullx = UnsafeExpression $
  "COALESCE" <> parenthesized (commaSeparated
    ((renderExpression <$> nullxs) <> [renderExpression notNullx]))

-- | analagous to `Data.Maybe.fromMaybe` using @COALESCE@
--
-- >>> renderExpression $ fromNull true null_
-- "COALESCE(NULL, TRUE)"
fromNull
  :: Expression relations grouping params ('NotNull ty)
  -- ^ what to convert @NULL@ to
  -> Expression relations grouping params ('Null ty)
  -> Expression relations grouping params ('NotNull ty)
fromNull notNullx nullx = coalesce [nullx] notNullx

-- | >>> renderExpression $ null_ & isNull
-- "NULL IS NULL"
isNull
  :: Expression relations grouping params ('Null ty)
  -- ^ possibly @NULL@
  -> Condition relations grouping params
isNull x = UnsafeExpression $ renderExpression x <+> "IS NULL"

-- | >>> renderExpression $ null_ & isNotNull
-- "NULL IS NOT NULL"
isNotNull
  :: Expression relations grouping params ('Null ty)
  -- ^ possibly @NULL@
  -> Condition relations grouping params
isNotNull x = UnsafeExpression $ renderExpression x <+> "IS NOT NULL"

-- | analagous to `maybe` using @IS NULL@
--
-- >>> renderExpression $ matchNull true not_ null_
-- "CASE WHEN NULL IS NULL THEN TRUE ELSE (NOT NULL) END"
matchNull
  :: Expression relations grouping params (nullty)
  -- ^ what to convert @NULL@ to
  -> ( Expression relations grouping params ('NotNull ty)
       -> Expression relations grouping params (nullty) )
  -- ^ function to perform when @NULL@ is absent
  -> Expression relations grouping params ('Null ty)
  -> Expression relations grouping params (nullty)
matchNull y f x = ifThenElse (isNull x) y
  (f (UnsafeExpression (renderExpression x)))

-- | right inverse to `fromNull`, if its arguments are equal then
-- `nullIf` gives @NULL@.
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> renderExpression @_ @_ @'[_] $ fromNull false (nullIf false (param @1))
-- "COALESCE(NULL IF (FALSE, ($1 :: bool)), FALSE)"
nullIf
  :: Expression relations grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression relations grouping params ('NotNull ty)
  -- ^ @NULL@ is absent
  -> Expression relations grouping params ('Null ty)
nullIf x y = UnsafeExpression $ "NULL IF" <+> parenthesized
  (renderExpression x <> ", " <> renderExpression y)

-- | >>> renderExpression $ array [null_, notNull false, notNull true]
-- "ARRAY[NULL, FALSE, TRUE]"
array
  :: [Expression relations grouping params ('Null ty)]
  -- ^ array elements
  -> Expression relations grouping params (nullity ('PGvararray ty))
array xs = UnsafeExpression $
  "ARRAY[" <> commaSeparated (renderExpression <$> xs) <> "]"

instance (KnownSymbol label, label `In` labels) => IsPGlabel label
  (Expression relations grouping params (nullity ('PGenum labels))) where
  label = UnsafeExpression $ renderLabel (PGlabel @label)

row
  :: (SListI (Nulls fields))
  => NP (Aliased (Expression relation grouping params)) (Nulls fields)
  -> Expression relation grouping params (nullity ('PGcomposite fields))
row exprs = UnsafeExpression $ "ROW" <> parenthesized
  (renderCommaSeparated (\ (expr `As` _) -> renderExpression expr) exprs)

instance Has field fields ty => IsLabel field
  (   Expression relation grouping params (nullity ('PGcomposite fields))
   -> Expression relation grouping params ('Null ty) ) where
    fromLabel expr = UnsafeExpression $
      parenthesized (renderExpression expr) <> "." <>
        fromString (symbolVal (Proxy @field))

instance Semigroup
  (Expression relations grouping params (nullity ('PGvararray ty))) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression relations grouping params (nullity ('PGvararray ty))) where
    mempty = array []
    mappend = (<>)

-- | >>> renderExpression @_ @_ @'[_] $ greatest currentTimestamp [param @1]
-- "GREATEST(CURRENT_TIMESTAMP, ($1 :: timestamp with time zone))"
greatest
  :: Expression relations grouping params (nullty)
  -- ^ needs at least 1 argument
  -> [Expression relations grouping params (nullty)]
  -- ^ or more
  -> Expression relations grouping params (nullty)
greatest x xs = UnsafeExpression $ "GREATEST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

-- | >>> renderExpression $ least currentTimestamp [null_]
-- "LEAST(CURRENT_TIMESTAMP, NULL)"
least
  :: Expression relations grouping params (nullty)
  -- ^ needs at least 1 argument
  -> [Expression relations grouping params (nullty)]
  -- ^ or more
  -> Expression relations grouping params (nullty)
least x xs = UnsafeExpression $ "LEAST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

-- | >>> renderExpression $ unsafeBinaryOp "OR" true false
-- "(TRUE OR FALSE)"
unsafeBinaryOp
  :: ByteString
  -- ^ operator
  -> Expression relations grouping params (ty0)
  -> Expression relations grouping params (ty1)
  -> Expression relations grouping params (ty2)
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderExpression x <+> op <+> renderExpression y

-- | >>> renderExpression $ unsafeUnaryOp "NOT" true
-- "(NOT TRUE)"
unsafeUnaryOp
  :: ByteString
  -- ^ operator
  -> Expression relations grouping params (ty0)
  -> Expression relations grouping params (ty1)
unsafeUnaryOp op x = UnsafeExpression $ parenthesized $
  op <+> renderExpression x

-- | >>> renderExpression $ unsafeFunction "f" true
-- "f(TRUE)"
unsafeFunction
  :: ByteString
  -- ^ function
  -> Expression relations grouping params (xty)
  -> Expression relations grouping params (yty)
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderExpression x)

instance PGNum ty
  => Num (Expression relations grouping params (nullity ty)) where
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
  (Expression relations grouping params (nullity ty)) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (PGNum ty, PGFloating ty) => Floating
  (Expression relations grouping params (nullity ty)) where
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
--   expression :: Expression relations grouping params (nullity 'PGfloat4)
--   expression = atan2_ pi 2
-- in renderExpression expression
-- :}
-- "atan2(pi(), 2)"
atan2_
  :: PGFloating float
  => Expression relations grouping params (nullity float)
  -- ^ numerator
  -> Expression relations grouping params (nullity float)
  -- ^ denominator
  -> Expression relations grouping params (nullity float)
atan2_ y x = UnsafeExpression $
  "atan2(" <> renderExpression y <> ", " <> renderExpression x <> ")"

-- When a `cast` is applied to an `Expression` of a known type, it
-- represents a run-time type conversion. The cast will succeed only if a
-- suitable type conversion operation has been defined.
--
-- | >>> renderExpression $ true & cast int4
-- "(TRUE :: int4)"
cast
  :: TypeExpression ty1
  -- ^ type to cast as
  -> Expression relations grouping params (nullity ty0)
  -- ^ value to convert
  -> Expression relations grouping params (nullity ty1)
cast ty x = UnsafeExpression $ parenthesized $
  renderExpression x <+> "::" <+> renderTypeExpression ty

-- | integer division, truncates the result
--
-- >>> :{
-- let
--   expression :: Expression relations grouping params (nullity 'PGint2)
--   expression = 5 `quot_` 2
-- in renderExpression expression
-- :}
-- "(5 / 2)"
quot_
  :: PGIntegral int
  => Expression relations grouping params (nullity int)
  -- ^ numerator
  -> Expression relations grouping params (nullity int)
  -- ^ denominator
  -> Expression relations grouping params (nullity int)
quot_ = unsafeBinaryOp "/"

-- | remainder upon integer division
--
-- >>> :{
-- let
--   expression :: Expression relations grouping params (nullity 'PGint2)
--   expression = 5 `rem_` 2
-- in renderExpression expression
-- :}
-- "(5 % 2)"
rem_
  :: PGIntegral int
  => Expression relations grouping params (nullity int)
  -- ^ numerator
  -> Expression relations grouping params (nullity int)
  -- ^ denominator
  -> Expression relations grouping params (nullity int)
rem_ = unsafeBinaryOp "%"

-- | >>> :{
-- let
--   expression :: Expression relations grouping params (nullity 'PGfloat4)
--   expression = trunc pi
-- in renderExpression expression
-- :}
-- "trunc(pi())"
trunc
  :: PGFloating frac
  => Expression relations grouping params (nullity frac)
  -- ^ fractional number
  -> Expression relations grouping params (nullity frac)
trunc = unsafeFunction "trunc"

-- | >>> :{
-- let
--   expression :: Expression relations grouping params (nullity 'PGfloat4)
--   expression = round_ pi
-- in renderExpression expression
-- :}
-- "round(pi())"
round_
  :: PGFloating frac
  => Expression relations grouping params (nullity frac)
  -- ^ fractional number
  -> Expression relations grouping params (nullity frac)
round_ = unsafeFunction "round"

-- | >>> :{
-- let
--   expression :: Expression relations grouping params (nullity 'PGfloat4)
--   expression = ceiling_ pi
-- in renderExpression expression
-- :}
-- "ceiling(pi())"
ceiling_
  :: PGFloating frac
  => Expression relations grouping params (nullity frac)
  -- ^ fractional number
  -> Expression relations grouping params (nullity frac)
ceiling_ = unsafeFunction "ceiling"

-- | A `Condition` is a boolean valued `Expression`. While SQL allows
-- conditions to have @NULL@, Squeal instead chooses to disallow @NULL@,
-- forcing one to handle the case of @NULL@ explicitly to produce
-- a `Condition`.
type Condition relations grouping params =
  Expression relations grouping params ('NotNull 'PGbool)

-- | >>> renderExpression true
-- "TRUE"
true :: Condition relations grouping params
true = UnsafeExpression "TRUE"

-- | >>> renderExpression false
-- "FALSE"
false :: Condition relations grouping params
false = UnsafeExpression "FALSE"

-- | >>> renderExpression $ not_ true
-- "(NOT TRUE)"
not_
  :: Condition relations grouping params
  -> Condition relations grouping params
not_ = unsafeUnaryOp "NOT"

-- | >>> renderExpression $ true .&& false
-- "(TRUE AND FALSE)"
(.&&)
  :: Condition relations grouping params
  -> Condition relations grouping params
  -> Condition relations grouping params
(.&&) = unsafeBinaryOp "AND"

-- | >>> renderExpression $ true .|| false
-- "(TRUE OR FALSE)"
(.||)
  :: Condition relations grouping params
  -> Condition relations grouping params
  -> Condition relations grouping params
(.||) = unsafeBinaryOp "OR"

-- | >>> :{
-- let
--   expression :: Expression relations grouping params (nullity 'PGint2)
--   expression = caseWhenThenElse [(true, 1), (false, 2)] 3
-- in renderExpression expression
-- :}
-- "CASE WHEN TRUE THEN 1 WHEN FALSE THEN 2 ELSE 3 END"
caseWhenThenElse
  :: [ ( Condition relations grouping params
       , Expression relations grouping params (ty)
     ) ]
  -- ^ whens and thens
  -> Expression relations grouping params (ty)
  -- ^ else
  -> Expression relations grouping params (ty)
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
--   expression :: Expression relations grouping params (nullity 'PGint2)
--   expression = ifThenElse true 1 0
-- in renderExpression expression
-- :}
-- "CASE WHEN TRUE THEN 1 ELSE 0 END"
ifThenElse
  :: Condition relations grouping params
  -> Expression relations grouping params (ty) -- ^ then
  -> Expression relations grouping params (ty) -- ^ else
  -> Expression relations grouping params (ty)
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

-- | Comparison operations like `.==`, `./=`, `.>`, `.>=`, `.<` and `.<=`
-- will produce @NULL@s if one of their arguments is @NULL@.
--
-- >>> renderExpression $ notNull true .== null_
-- "(TRUE = NULL)"
(.==)
  :: Expression relations grouping params (nullity ty) -- ^ lhs
  -> Expression relations grouping params (nullity ty) -- ^ rhs
  -> Expression relations grouping params (nullity 'PGbool)
(.==) = unsafeBinaryOp "="
infix 4 .==

-- | >>> renderExpression $ notNull true ./= null_
-- "(TRUE <> NULL)"
(./=)
  :: Expression relations grouping params (nullity ty) -- ^ lhs
  -> Expression relations grouping params (nullity ty) -- ^ rhs
  -> Expression relations grouping params (nullity 'PGbool)
(./=) = unsafeBinaryOp "<>"
infix 4 ./=

-- | >>> renderExpression $ notNull true .>= null_
-- "(TRUE >= NULL)"
(.>=)
  :: Expression relations grouping params (nullity ty) -- ^ lhs
  -> Expression relations grouping params (nullity ty) -- ^ rhs
  -> Expression relations grouping params (nullity 'PGbool)
(.>=) = unsafeBinaryOp ">="
infix 4 .>=

-- | >>> renderExpression $ notNull true .< null_
-- "(TRUE < NULL)"
(.<)
  :: Expression relations grouping params (nullity ty) -- ^ lhs
  -> Expression relations grouping params (nullity ty) -- ^ rhs
  -> Expression relations grouping params (nullity 'PGbool)
(.<) = unsafeBinaryOp "<"
infix 4 .<

-- | >>> renderExpression $ notNull true .<= null_
-- "(TRUE <= NULL)"
(.<=)
  :: Expression relations grouping params (nullity ty) -- ^ lhs
  -> Expression relations grouping params (nullity ty) -- ^ rhs
  -> Expression relations grouping params (nullity 'PGbool)
(.<=) = unsafeBinaryOp "<="
infix 4 .<=

-- | >>> renderExpression $ notNull true .> null_
-- "(TRUE > NULL)"
(.>)
  :: Expression relations grouping params (nullity ty) -- ^ lhs
  -> Expression relations grouping params (nullity ty) -- ^ rhs
  -> Expression relations grouping params (nullity 'PGbool)
(.>) = unsafeBinaryOp ">"
infix 4 .>

-- | >>> renderExpression currentDate
-- "CURRENT_DATE"
currentDate
  :: Expression relations grouping params (nullity 'PGdate)
currentDate = UnsafeExpression "CURRENT_DATE"

-- | >>> renderExpression currentTime
-- "CURRENT_TIME"
currentTime
  :: Expression relations grouping params (nullity 'PGtimetz)
currentTime = UnsafeExpression "CURRENT_TIME"

-- | >>> renderExpression currentTimestamp
-- "CURRENT_TIMESTAMP"
currentTimestamp
  :: Expression relations grouping params (nullity 'PGtimestamptz)
currentTimestamp = UnsafeExpression "CURRENT_TIMESTAMP"

-- | >>> renderExpression localTime
-- "LOCALTIME"
localTime
  :: Expression relations grouping params (nullity 'PGtime)
localTime = UnsafeExpression "LOCALTIME"

-- | >>> renderExpression localTimestamp
-- "LOCALTIMESTAMP"
localTimestamp
  :: Expression relations grouping params (nullity 'PGtimestamp)
localTimestamp = UnsafeExpression "LOCALTIMESTAMP"

{-----------------------------------------
text
-----------------------------------------}

instance IsString
  (Expression relations grouping params (nullity 'PGtext)) where
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
  (Expression relations grouping params (nullity 'PGtext)) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression relations grouping params (nullity 'PGtext)) where
    mempty = fromString ""
    mappend = (<>)

-- | >>> renderExpression $ lower "ARRRGGG"
-- "lower(E'ARRRGGG')"
lower
  :: Expression relations grouping params (nullity 'PGtext)
  -- ^ string to lower case
  -> Expression relations grouping params (nullity 'PGtext)
lower = unsafeFunction "lower"

-- | >>> renderExpression $ upper "eeee"
-- "upper(E'eeee')"
upper
  :: Expression relations grouping params (nullity 'PGtext)
  -- ^ string to upper case
  -> Expression relations grouping params (nullity 'PGtext)
upper = unsafeFunction "upper"

-- | >>> renderExpression $ charLength "four"
-- "char_length(E'four')"
charLength
  :: Expression relations grouping params (nullity 'PGtext)
  -- ^ string to measure
  -> Expression relations grouping params (nullity 'PGint4)
charLength = unsafeFunction "char_length"

-- | The `like` expression returns true if the @string@ matches
-- the supplied @pattern@. If @pattern@ does not contain percent signs
-- or underscores, then the pattern only represents the string itself;
-- in that case `like` acts like the equals operator. An underscore (_)
-- in pattern stands for (matches) any single character; a percent sign (%)
-- matches any sequence of zero or more characters.
--
-- >>> renderExpression $ "abc" `like` "a%"
-- "(E'abc' LIKE E'a%')"
like
  :: Expression relations grouping params (nullity 'PGtext)
  -- ^ string
  -> Expression relations grouping params (nullity 'PGtext)
  -- ^ pattern
  -> Expression relations grouping params (nullity 'PGbool)
like = unsafeBinaryOp "LIKE"

{-----------------------------------------
aggregation
-----------------------------------------}

-- | escape hatch to define aggregate functions
unsafeAggregate
  :: ByteString -- ^ aggregate function
  -> Expression relations 'Ungrouped params (xty)
  -> Expression relations ('Grouped bys) params (yty)
unsafeAggregate fun x = UnsafeExpression $ mconcat
  [fun, "(", renderExpression x, ")"]

-- | escape hatch to define aggregate functions over distinct values
unsafeAggregateDistinct
  :: ByteString -- ^ aggregate function
  -> Expression relations 'Ungrouped params (xty)
  -> Expression relations ('Grouped bys) params (yty)
unsafeAggregateDistinct fun x = UnsafeExpression $ mconcat
  [fun, "(DISTINCT ", renderExpression x, ")"]

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: 'Null 'PGnumeric]] ('Grouped bys) params ('Null 'PGnumeric)
--   expression = sum_ #col
-- in renderExpression expression
-- :}
-- "sum(\"col\")"
sum_
  :: PGNum ty
  => Expression relations 'Ungrouped params (nullity ty)
  -- ^ what to sum
  -> Expression relations ('Grouped bys) params (nullity ty)
sum_ = unsafeAggregate "sum"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGnumeric]] ('Grouped bys) params (nullity 'PGnumeric)
--   expression = sumDistinct #col
-- in renderExpression expression
-- :}
-- "sum(DISTINCT \"col\")"
sumDistinct
  :: PGNum ty
  => Expression relations 'Ungrouped params (nullity ty)
  -- ^ what to sum
  -> Expression relations ('Grouped bys) params (nullity ty)
sumDistinct = unsafeAggregateDistinct "sum"

-- | A constraint for `PGType`s that you can take averages of and the resulting
-- `PGType`.
class PGAvg ty avg | ty -> avg where
  avg, avgDistinct
    :: Expression relations 'Ungrouped params (nullity ty)
    -- ^ what to average
    -> Expression relations ('Grouped bys) params (nullity avg)
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
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitAnd #col
-- in renderExpression expression
-- :}
-- "bit_and(\"col\")"
bitAnd
  :: PGIntegral int
  => Expression relations 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity int)
bitAnd = unsafeAggregate "bit_and"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitOr #col
-- in renderExpression expression
-- :}
-- "bit_or(\"col\")"
bitOr
  :: PGIntegral int
  => Expression relations 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity int)
bitOr = unsafeAggregate "bit_or"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitAndDistinct #col
-- in renderExpression expression
-- :}
-- "bit_and(DISTINCT \"col\")"
bitAndDistinct
  :: PGIntegral int
  => Expression relations 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity int)
bitAndDistinct = unsafeAggregateDistinct "bit_and"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGint4]] (Grouped bys) params (nullity 'PGint4)
--   expression = bitOrDistinct #col
-- in renderExpression expression
-- :}
-- "bit_or(DISTINCT \"col\")"
bitOrDistinct
  :: PGIntegral int
  => Expression relations 'Ungrouped params (nullity int)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity int)
bitOrDistinct = unsafeAggregateDistinct "bit_or"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolAnd #col
-- in renderExpression expression
-- :}
-- "bool_and(\"col\")"
boolAnd
  :: Expression relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity 'PGbool)
boolAnd = unsafeAggregate "bool_and"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolOr #col
-- in renderExpression expression
-- :}
-- "bool_or(\"col\")"
boolOr
  :: Expression relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity 'PGbool)
boolOr = unsafeAggregate "bool_or"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolAndDistinct #col
-- in renderExpression expression
-- :}
-- "bool_and(DISTINCT \"col\")"
boolAndDistinct
  :: Expression relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity 'PGbool)
boolAndDistinct = unsafeAggregateDistinct "bool_and"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = boolOrDistinct #col
-- in renderExpression expression
-- :}
-- "bool_or(DISTINCT \"col\")"
boolOrDistinct
  :: Expression relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity 'PGbool)
boolOrDistinct = unsafeAggregateDistinct "bool_or"

-- | A special aggregation that does not require an input
--
-- >>> renderExpression countStar
-- "count(*)"
countStar
  :: Expression relations ('Grouped bys) params ('NotNull 'PGint8)
countStar = UnsafeExpression $ "count(*)"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity ty]] (Grouped bys) params ('NotNull 'PGint8)
--   expression = count #col
-- in renderExpression expression
-- :}
-- "count(\"col\")"
count
  :: Expression relations 'Ungrouped params ty
  -- ^ what to count
  -> Expression relations ('Grouped bys) params ('NotNull 'PGint8)
count = unsafeAggregate "count"

-- | >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity ty]] (Grouped bys) params ('NotNull 'PGint8)
--   expression = countDistinct #col
-- in renderExpression expression
-- :}
-- "count(DISTINCT \"col\")"
countDistinct
  :: Expression relations 'Ungrouped params ty
  -- ^ what to count
  -> Expression relations ('Grouped bys) params ('NotNull 'PGint8)
countDistinct = unsafeAggregateDistinct "count"

-- | synonym for `boolAnd`
--
-- >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = every #col
-- in renderExpression expression
-- :}
-- "every(\"col\")"
every
  :: Expression relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity 'PGbool)
every = unsafeAggregate "every"

-- | synonym for `boolAndDistinct`
--
-- >>> :{
-- let
--   expression :: Expression '[tab ::: '["col" ::: nullity 'PGbool]] (Grouped bys) params (nullity 'PGbool)
--   expression = everyDistinct #col
-- in renderExpression expression
-- :}
-- "every(DISTINCT \"col\")"
everyDistinct
  :: Expression relations 'Ungrouped params (nullity 'PGbool)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity 'PGbool)
everyDistinct = unsafeAggregateDistinct "every"

-- | minimum and maximum aggregation
max_, min_, maxDistinct, minDistinct
  :: Expression relations 'Ungrouped params (nullity ty)
  -- ^ what to aggregate
  -> Expression relations ('Grouped bys) params (nullity ty)
max_ = unsafeAggregate "max"
min_ = unsafeAggregate "min"
maxDistinct = unsafeAggregateDistinct "max"
minDistinct = unsafeAggregateDistinct "min"

{-----------------------------------------
tables
-----------------------------------------}

-- | A `Table` from a table expression is a way
-- to call a table reference by its alias.
newtype Table
  (schema :: SchemaType)
  (columns :: RelationType)
    = UnsafeTable { renderTable :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance
  ( Has alias schema ('Table table)
  , relation ~ TableToRelation table
  ) => IsLabel alias (Table schema relation) where
    fromLabel = UnsafeTable $ renderAlias (Alias @alias)

-- | A `View` from a table expression is a way
-- to call a table reference by its alias.
newtype View
  (schema :: SchemaType)
  (columns :: RelationType)
    = UnsafeView { renderView :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance
  ( Has alias schema ('View columns)
  ) => IsLabel alias (View schema columns) where
    fromLabel = UnsafeView $ renderAlias (Alias @alias)

{-----------------------------------------
type expressions
-----------------------------------------}

-- | `TypeExpression`s are used in `cast`s and `createTable` commands.
newtype TypeExpression (ty :: PGType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | logical Boolean (true/false)
bool :: TypeExpression 'PGbool
bool = UnsafeTypeExpression "bool"
-- | signed two-byte integer
int2, smallint :: TypeExpression 'PGint2
int2 = UnsafeTypeExpression "int2"
smallint = UnsafeTypeExpression "smallint"
-- | signed four-byte integer
int4, int, integer :: TypeExpression 'PGint4
int4 = UnsafeTypeExpression "int4"
int = UnsafeTypeExpression "int"
integer = UnsafeTypeExpression "integer"
-- | signed eight-byte integer
int8, bigint :: TypeExpression 'PGint8
int8 = UnsafeTypeExpression "int8"
bigint = UnsafeTypeExpression "bigint"
-- | arbitrary precision numeric type
numeric :: TypeExpression 'PGnumeric
numeric = UnsafeTypeExpression "numeric"
-- | single precision floating-point number (4 bytes)
float4, real :: TypeExpression 'PGfloat4
float4 = UnsafeTypeExpression "float4"
real = UnsafeTypeExpression "real"
-- | double precision floating-point number (8 bytes)
float8, doublePrecision :: TypeExpression 'PGfloat8
float8 = UnsafeTypeExpression "float8"
doublePrecision = UnsafeTypeExpression "double precision"
-- | variable-length character string
text :: TypeExpression 'PGtext
text = UnsafeTypeExpression "text"
-- | fixed-length character string
char, character
  :: (KnownNat n, 1 <= n)
  => proxy n
  -> TypeExpression ('PGchar n)
char p = UnsafeTypeExpression $ "char(" <> renderNat p <> ")"
character p = UnsafeTypeExpression $  "character(" <> renderNat p <> ")"
-- | variable-length character string
varchar, characterVarying
  :: (KnownNat n, 1 <= n)
  => proxy n
  -> TypeExpression ('PGvarchar n)
varchar p = UnsafeTypeExpression $ "varchar(" <> renderNat p <> ")"
characterVarying p = UnsafeTypeExpression $
  "character varying(" <> renderNat p <> ")"
-- | binary data ("byte array")
bytea :: TypeExpression 'PGbytea
bytea = UnsafeTypeExpression "bytea"
-- | date and time (no time zone)
timestamp :: TypeExpression 'PGtimestamp
timestamp = UnsafeTypeExpression "timestamp"
-- | date and time, including time zone
timestampWithTimeZone :: TypeExpression 'PGtimestamptz
timestampWithTimeZone = UnsafeTypeExpression "timestamp with time zone"
-- | calendar date (year, month, day)
date :: TypeExpression 'PGdate
date = UnsafeTypeExpression "date"
-- | time of day (no time zone)
time :: TypeExpression 'PGtime
time = UnsafeTypeExpression "time"
-- | time of day, including time zone
timeWithTimeZone :: TypeExpression 'PGtimetz
timeWithTimeZone = UnsafeTypeExpression "time with time zone"
-- | time span
interval :: TypeExpression 'PGinterval
interval = UnsafeTypeExpression "interval"
-- | universally unique identifier
uuid :: TypeExpression 'PGuuid
uuid = UnsafeTypeExpression "uuid"
-- | IPv4 or IPv6 host address
inet :: TypeExpression 'PGinet
inet = UnsafeTypeExpression "inet"
-- | textual JSON data
json :: TypeExpression 'PGjson
json = UnsafeTypeExpression "json"
-- | binary JSON data, decomposed
jsonb :: TypeExpression 'PGjsonb
jsonb = UnsafeTypeExpression "jsonb"
-- | variable length array
vararray
  :: TypeExpression pg
  -> TypeExpression ('PGvararray pg)
vararray ty = UnsafeTypeExpression $ renderTypeExpression ty <> "[]"
-- | fixed length array
--
-- >>> renderTypeExpression (fixarray (Proxy @2) json)
-- "json[2]"
fixarray
  :: KnownNat n
  => proxy n
  -> TypeExpression pg
  -> TypeExpression ('PGfixarray n pg)
fixarray p ty = UnsafeTypeExpression $
  renderTypeExpression ty <> "[" <> renderNat p <> "]"

-- | `pgtype` is a demoted version of a `PGType`
class PGTyped (ty :: PGType) where pgtype :: TypeExpression ty
instance PGTyped 'PGbool where pgtype = bool
instance PGTyped 'PGint2 where pgtype = int2
instance PGTyped 'PGint4 where pgtype = int4
instance PGTyped 'PGint8 where pgtype = int8
instance PGTyped 'PGnumeric where pgtype = numeric
instance PGTyped 'PGfloat4 where pgtype = float4
instance PGTyped 'PGfloat8 where pgtype = float8
instance PGTyped 'PGtext where pgtype = text
instance (KnownNat n, 1 <= n)
  => PGTyped ('PGchar n) where pgtype = char (Proxy @n)
instance (KnownNat n, 1 <= n)
  => PGTyped ('PGvarchar n) where pgtype = varchar (Proxy @n)
instance PGTyped 'PGbytea where pgtype = bytea
instance PGTyped 'PGtimestamp where pgtype = timestamp
instance PGTyped 'PGtimestamptz where pgtype = timestampWithTimeZone
instance PGTyped 'PGdate where pgtype = date
instance PGTyped 'PGtime where pgtype = time
instance PGTyped 'PGtimetz where pgtype = timeWithTimeZone
instance PGTyped 'PGinterval where pgtype = interval
instance PGTyped 'PGuuid where pgtype = uuid
instance PGTyped 'PGjson where pgtype = json
instance PGTyped 'PGjsonb where pgtype = jsonb
instance PGTyped ty => PGTyped ('PGvararray ty) where
  pgtype = vararray (pgtype @ty)
instance (KnownNat n, PGTyped ty) => PGTyped ('PGfixarray n ty) where
  pgtype = fixarray (Proxy @n) (pgtype @ty)

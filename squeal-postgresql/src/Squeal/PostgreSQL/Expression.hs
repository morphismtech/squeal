{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MagicHash
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression 
  ( -- * Column Expressions
    Expression (UnsafeExpression, renderExpression)
  , HasParameter (param)
  , HasColumn (getColumn)
  , Column (Column)
  , renderColumn
  , GroupedBy (getGroup1, getGroup2)
    -- ** Default
  , def
  , unDef
    -- ** Null
  , null_
  , unNull
  , coalesce
  , fromNull
  , isNull
  , isn'tNull
  , matchNull
  , nullIf
  -- ** Functions
  , unsafeBinaryOp
  , unsafeUnaryOp
  , unsafeFunction
  , atan2_
  , cast
  , div_
  , mod_
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
  , unsafeAggregate
  , unsafeAggregateDistinct
  , sum_
  , sumDistinct
  , PGAvg (avg, avgDistinct)
  , bitAnd
  , bitOr
  , boolAnd
  , boolOr
  , countStar
  , countDistinctStar
  , count
  , every
  , jsonAgg
  , jsonbAgg
  , jsonObjectAgg
  , jsonbObjectAgg
  , max_
  , min_
    -- * Tables
  , Table (UnsafeTable, renderTable)
  , HasTable (getTable)
    -- * TypeExpression
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
  , serial2
  , smallserial
  , serial4
  , serial
  , serial8
  , bigserial
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
  , json
  , jsonb
  , notNull
  , default_
    -- * Re-export
  , (&)
  , NP ((:*), Nil)
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Monoid hiding (All)
import Data.Ratio
import Data.String
import Generics.SOP hiding (from)
import GHC.OverloadedLabels
import GHC.TypeLits
import Prelude hiding (id, (.))

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Schema

{-----------------------------------------
column expressions
-----------------------------------------}

{- | `Expression`s are used in a variety of contexts,
such as in the target list of the `select` command,
as new column values in `insertInto` or `update`,
or in search `Condition`s in a number of commands.

The expression syntax allows the calculation of
values from primitive expression using arithmetic, logical,
and other operations.
-}
newtype Expression
  (tables :: TablesType)
  (grouping :: Grouping)
  (params :: [ColumnType])
  (ty :: ColumnType)
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
class (PGTyped (BaseType ty), KnownNat n)
  => HasParameter (n :: Nat) params ty | n params -> ty where
    param :: Expression tables grouping params ty
    param = UnsafeExpression $ parenthesized $
      "$" <> renderNat (Proxy @n) <+> "::"
        <+> renderTypeExpression (pgtype @(BaseType ty))
instance {-# OVERLAPPING #-} PGTyped (BaseType ty1)
  => HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

{- | A `HasColumn` constraint indicates an unqualified column reference.
`getColumn` can only be unambiguous when the `TableExpression` the column
references is unique, in which case the column may be referenced using
@-XOverloadedLabels@. Otherwise, combined with a `HasTable` constraint, the
qualified column reference operator `!` may be used.
-}
class KnownSymbol column => HasColumn column columns ty
  | column columns -> ty where
    getColumn
      :: HasUnique table tables columns
      => Alias column
      -> Expression tables 'Ungrouped params ty
    getColumn column = UnsafeExpression $ renderAlias column
instance {-# OVERLAPPING #-} KnownSymbol column
  => HasColumn column ((column ::: optionality ty) ': tys) ('Required ty)
instance {-# OVERLAPPABLE #-} (KnownSymbol column, HasColumn column table ty)
  => HasColumn column (ty' ': table) ty

instance (HasColumn column columns ty, HasUnique table tables columns)
  => IsLabel column (Expression tables 'Ungrouped params ty) where
    fromLabel = getColumn (Alias @column)

instance (HasTable table tables columns, HasColumn column columns ty)
  => IsTableColumn table column (Expression tables 'Ungrouped params ty) where
    table ! column = UnsafeExpression $
      renderAlias table <> "." <> renderAlias column

data Column
  (columns :: ColumnsType)
  (columnty :: (Symbol,ColumnType))
    where
      Column
        :: HasColumn column columns ty
        => Alias column
        -> Column columns (column ::: ty)
deriving instance Show (Column columns columnty)
deriving instance Eq (Column columns columnty)
deriving instance Ord (Column columns columnty)

renderColumn :: Column columns columnty -> ByteString
renderColumn (Column column) = renderAlias column

class (KnownSymbol table, KnownSymbol column)
  => GroupedBy table column bys where
    getGroup1
      :: (HasUnique table tables columns, HasColumn column columns ty)
      => Alias column
      -> Expression tables ('Grouped bys) params ty
    getGroup1 column = UnsafeExpression $ renderAlias column
    getGroup2
      :: (HasTable table tables columns, HasColumn column columns ty)
      => Alias table
      -> Alias column
      -> Expression tables ('Grouped bys) params ty
    getGroup2 table column = UnsafeExpression $
      renderAlias table <> "." <> renderAlias column
instance {-# OVERLAPPING #-} (KnownSymbol table, KnownSymbol column)
  => GroupedBy table column ('(table,column) ': bys)
instance {-# OVERLAPPABLE #-}
  ( KnownSymbol table
  , KnownSymbol column
  , GroupedBy table column bys
  ) => GroupedBy table column (tabcol ': bys)
  
instance
  ( HasUnique table tables columns
  , HasColumn column columns ty
  , GroupedBy table column bys
  ) => IsLabel column
    (Expression tables ('Grouped bys) params ty) where
      fromLabel = getGroup1 (Alias @column)
  
instance
  ( HasTable table tables columns
  , HasColumn column columns ty
  , GroupedBy table column bys
  ) => IsTableColumn table column
    (Expression tables ('Grouped bys) params ty) where (!) = getGroup2

-- | >>> renderExpression def
-- "DEFAULT"
def :: Expression '[] 'Ungrouped params ('Optional (nullity ty))
def = UnsafeExpression "DEFAULT"

-- | >>> renderExpression $ unDef false
-- "FALSE"
unDef
  :: Expression '[] 'Ungrouped params ('Required (nullity ty))
  -> Expression '[] 'Ungrouped params ('Optional (nullity ty))
unDef = UnsafeExpression . renderExpression

-- | analagous to `Nothing`
--
-- >>> renderExpression $ null_
-- "NULL"
null_ :: Expression tables grouping params (optionality ('Null ty))
null_ = UnsafeExpression "NULL"

-- | analagous to `Just`
--
-- >>> renderExpression $ unNull true
-- "TRUE"
unNull
  :: Expression tables grouping params (optionality ('NotNull ty))
  -> Expression tables grouping params (optionality ('Null ty))
unNull = UnsafeExpression . renderExpression

-- | return the leftmost value which is not NULL
--
-- >>> renderExpression $ coalesce [null_, unNull true] false
-- "COALESCE(NULL, TRUE, FALSE)"
coalesce
  :: [Expression tables grouping params ('Required ('Null ty))]
  -> Expression tables grouping params ('Required ('NotNull ty))
  -> Expression tables grouping params ('Required ('NotNull ty))
coalesce nullxs notNullx = UnsafeExpression $
  "COALESCE" <> parenthesized (commaSeparated
    ((renderExpression <$> nullxs) <> [renderExpression notNullx]))

-- | analagous to `fromMaybe` using @COALESCE@
--
-- >>> renderExpression $ fromNull true null_
-- "COALESCE(NULL, TRUE)"
fromNull
  :: Expression tables grouping params ('Required ('NotNull ty))
  -> Expression tables grouping params ('Required ('Null ty))
  -> Expression tables grouping params ('Required ('NotNull ty))
fromNull notNullx nullx = coalesce [nullx] notNullx

-- | >>> renderExpression $ null_ & isNull
-- "NULL IS NULL"
isNull
  :: Expression tables grouping params ('Required ('Null ty))
  -> Condition tables grouping params
isNull x = UnsafeExpression $ renderExpression x <+> "IS NULL"

-- | >>> renderExpression $ null_ & isn'tNull
-- "NULL IS NOT NULL"
isn'tNull
  :: Expression tables grouping params ('Required ('Null ty))
  -> Condition tables grouping params
isn'tNull x = UnsafeExpression $ renderExpression x <+> "IS NOT NULL"

-- | analagous to `maybe` using @IS NULL@
--
-- >>> renderExpression $ matchNull true not_ null_
-- "CASE WHEN NULL IS NULL THEN TRUE ELSE (NOT NULL) END"
matchNull
  :: Expression tables grouping params ('Required nullty)
  -> ( Expression tables grouping params ('Required ('NotNull ty))
       -> Expression tables grouping params ('Required nullty) )
  -> Expression tables grouping params ('Required ('Null ty))
  -> Expression tables grouping params ('Required nullty)
matchNull y f x = ifThenElse (isNull x) y
  (f (UnsafeExpression (renderExpression x)))

-- | right inverse to `fromNull`
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> renderExpression @_ @_ @'[_] $ fromNull false (nullIf false (param @1))
-- "COALESCE(NULL IF (FALSE, ($1 :: bool)), FALSE)"
nullIf
  :: Expression tables grouping params ('Required ('NotNull ty))
  -> Expression tables grouping params ('Required ('NotNull ty))
  -> Expression tables grouping params ('Required ('Null ty))
nullIf x y = UnsafeExpression $ "NULL IF" <+> parenthesized
  (renderExpression x <> ", " <> renderExpression y)

-- | >>> renderExpression @_ @_ @'[_] $ greatest currentTimestamp [param @1]
-- "GREATEST(CURRENT_TIMESTAMP, ($1 :: timestamp with time zone))"
greatest
  :: Expression tables grouping params ('Required nullty)
  -> [Expression tables grouping params ('Required nullty)]
  -> Expression tables grouping params ('Required nullty)
greatest x xs = UnsafeExpression $ "GREATEST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

-- | >>> renderExpression $ least currentTimestamp [null_]
-- "LEAST(CURRENT_TIMESTAMP, NULL)"
least
  :: Expression tables grouping params ('Required nullty)
  -> [Expression tables grouping params ('Required nullty)]
  -> Expression tables grouping params ('Required nullty)
least x xs = UnsafeExpression $ "LEAST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

unsafeBinaryOp
  :: ByteString
  -> Expression tables grouping params ('Required ty0)
  -> Expression tables grouping params ('Required ty1)
  -> Expression tables grouping params ('Required ty2)
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderExpression x <+> op <+> renderExpression y

unsafeUnaryOp
  :: ByteString
  -> Expression tables grouping params ('Required ty0)
  -> Expression tables grouping params ('Required ty1)
unsafeUnaryOp op x = UnsafeExpression $ parenthesized $
  op <+> renderExpression x

unsafeFunction
  :: ByteString
  -> Expression tables grouping params ('Required xty)
  -> Expression tables grouping params ('Required yty)
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderExpression x)

instance PGNum ty
  => Num (Expression tables grouping params ('Required (nullity ty))) where
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
  (Expression tables grouping params ('Required (nullity ty))) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (PGNum ty, PGFloating ty) => Floating
  (Expression tables grouping params ('Required (nullity ty))) where
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

atan2_
  :: PGFloating float
  => Expression tables grouping params ('Required (nullity float))
  -> Expression tables grouping params ('Required (nullity float))
  -> Expression tables grouping params ('Required (nullity float))
atan2_ y x = UnsafeExpression $
  "atan2(" <> renderExpression y <> ", " <> renderExpression x <> ")"

cast
  :: TypeExpression ('Required ('Null ty1))
  -> Expression tables grouping params ('Required (nullity ty0))
  -> Expression tables grouping params ('Required (nullity ty1))
cast ty x = UnsafeExpression $ parenthesized $
  renderExpression x <+> "::" <+> renderTypeExpression ty

div_
  :: PGIntegral int
  => Expression tables grouping params ('Required (nullity int))
  -> Expression tables grouping params ('Required (nullity int))
  -> Expression tables grouping params ('Required (nullity int))
div_ = unsafeBinaryOp "/"

mod_
  :: PGIntegral int
  => Expression tables grouping params ('Required (nullity int))
  -> Expression tables grouping params ('Required (nullity int))
  -> Expression tables grouping params ('Required (nullity int))
mod_ = unsafeBinaryOp "%"

trunc
  :: PGFloating frac
  => Expression tables grouping params ('Required (nullity frac))
  -> Expression tables grouping params ('Required (nullity frac))
trunc = unsafeFunction "trunc"

round_
  :: PGFloating frac
  => Expression tables grouping params ('Required (nullity frac))
  -> Expression tables grouping params ('Required (nullity frac))
round_ = unsafeFunction "round"

ceiling_
  :: PGFloating frac
  => Expression tables grouping params ('Required (nullity frac))
  -> Expression tables grouping params ('Required (nullity frac))
ceiling_ = unsafeFunction "ceiling"

type Condition tables grouping params =
  Expression tables grouping params ('Required ('NotNull 'PGbool))

true :: Condition tables grouping params
true = UnsafeExpression "TRUE"

false :: Condition tables grouping params
false = UnsafeExpression "FALSE"

not_
  :: Condition tables grouping params
  -> Condition tables grouping params
not_ = unsafeUnaryOp "NOT"

(.&&)
  :: Condition tables grouping params
  -> Condition tables grouping params
  -> Condition tables grouping params
(.&&) = unsafeBinaryOp "AND"

(.||)
  :: Condition tables grouping params
  -> Condition tables grouping params
  -> Condition tables grouping params
(.||) = unsafeBinaryOp "OR"

caseWhenThenElse
  :: [ ( Condition tables grouping params
       , Expression tables grouping params ('Required ty)
     ) ]
  -> Expression tables grouping params ('Required ty)
  -> Expression tables grouping params ('Required ty)
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

ifThenElse
  :: Condition tables grouping params
  -> Expression tables grouping params ('Required ty)
  -> Expression tables grouping params ('Required ty)
  -> Expression tables grouping params ('Required ty)
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

(.==)
  :: Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity 'PGbool))
(.==) = unsafeBinaryOp "="
infix 4 .==

(./=)
  :: Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity 'PGbool))
(./=) = unsafeBinaryOp "<>"
infix 4 ./=

(.>=)
  :: Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity 'PGbool))
(.>=) = unsafeBinaryOp ">="
infix 4 .>=

(.<)
  :: Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity 'PGbool))
(.<) = unsafeBinaryOp "<"
infix 4 .<

(.<=)
  :: Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity 'PGbool))
(.<=) = unsafeBinaryOp "<="
infix 4 .<=

(.>)
  :: Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity ty))
  -> Expression tables grouping params ('Required (nullity 'PGbool))
(.>) = unsafeBinaryOp ">"
infix 4 .>

currentDate
  :: Expression tables grouping params ('Required (nullity 'PGdate))
currentDate = UnsafeExpression "CURRENT_DATE"

currentTime
  :: Expression tables grouping params ('Required (nullity 'PGtimetz))
currentTime = UnsafeExpression "CURRENT_TIME"

currentTimestamp
  :: Expression tables grouping params ('Required (nullity 'PGtimestamptz))
currentTimestamp = UnsafeExpression "CURRENT_TIMESTAMP"

localTime
  :: Expression tables grouping params ('Required (nullity 'PGtime))
localTime = UnsafeExpression "LOCALTIME"

localTimestamp
  :: Expression tables grouping params ('Required (nullity 'PGtimestamp))
localTimestamp = UnsafeExpression "LOCALTIMESTAMP"

{-----------------------------------------
text
-----------------------------------------}

instance IsString
  (Expression tables grouping params ('Required (nullity 'PGtext))) where
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

instance Monoid
  (Expression tables grouping params ('Required (nullity 'PGtext))) where
    mempty = fromString ""
    mappend = unsafeBinaryOp "||"

lower
  :: Expression tables grouping params ('Required (nullity 'PGtext))
  -> Expression tables grouping params ('Required (nullity 'PGtext))
lower = unsafeFunction "lower"

upper
  :: Expression tables grouping params ('Required (nullity 'PGtext))
  -> Expression tables grouping params ('Required (nullity 'PGtext))
upper = unsafeFunction "upper"

charLength
  :: Expression tables grouping params ('Required (nullity 'PGtext))
  -> Expression tables grouping params ('Required (nullity 'PGint4))
charLength = unsafeFunction "char_length"

like
  :: Expression tables grouping params ('Required (nullity 'PGtext))
  -> Expression tables grouping params ('Required (nullity 'PGtext))
  -> Expression tables grouping params ('Required (nullity 'PGbool))
like = unsafeBinaryOp "LIKE"

{-----------------------------------------
aggregation
-----------------------------------------}

unsafeAggregate, unsafeAggregateDistinct
  :: ByteString
  -> Expression tables 'Ungrouped params ('Required xty)
  -> Expression tables ('Grouped bys) params ('Required yty)
unsafeAggregate fun x = UnsafeExpression $ mconcat
  [fun, "(", renderExpression x, ")"]
unsafeAggregateDistinct fun x = UnsafeExpression $ mconcat
  [fun, "(DISTINCT ", renderExpression x, ")"]

sum_, sumDistinct
  :: PGNum ty
  => Expression tables 'Ungrouped params ('Required (nullity ty))
  -> Expression tables ('Grouped bys) params ('Required (nullity ty))
sum_ = unsafeAggregate "sum"
sumDistinct = unsafeAggregateDistinct "sum"

class PGAvg ty avg | ty -> avg where
  avg, avgDistinct
    :: Expression tables 'Ungrouped params ('Required (nullity ty))
    -> Expression tables ('Grouped bys) params ('Required (nullity avg))
  avg = unsafeAggregate "avg"
  avgDistinct = unsafeAggregateDistinct "avg"
instance PGAvg 'PGint2 'PGnumeric
instance PGAvg 'PGint4 'PGnumeric
instance PGAvg 'PGint8 'PGnumeric
instance PGAvg 'PGnumeric 'PGnumeric
instance PGAvg 'PGfloat4 'PGfloat8
instance PGAvg 'PGfloat8 'PGfloat8
instance PGAvg 'PGinterval 'PGinterval

bitAnd, bitOr
  :: PGIntegral int
  => Expression tables 'Ungrouped params ('Required (nullity int))
  -> Expression tables ('Grouped bys) params ('Required (nullity int))
bitAnd = unsafeAggregate "bit_and"
bitOr = unsafeAggregate "bit_or"

boolAnd, boolOr
  :: Expression tables 'Ungrouped params ('Required (nullity 'PGbool))
  -> Expression tables ('Grouped bys) params ('Required (nullity 'PGbool))
boolAnd = unsafeAggregate "bool_and"
boolOr = unsafeAggregate "bool_or"

countStar, countDistinctStar
  :: Expression tables ('Grouped bys) params ('Required ('NotNull 'PGint8))
countStar = UnsafeExpression $ "count(*)"
countDistinctStar = UnsafeExpression $ "count(DISTINCT *)"

count
  :: Expression tables 'Ungrouped params ('Required ty)
  -> Expression tables ('Grouped bys) params ('Required ('NotNull 'PGint8))
count = unsafeAggregate "count"
  
every
  :: Expression tables 'Ungrouped params ('Required (nullity 'PGbool))
  -> Expression tables ('Grouped bys) params ('Required (nullity 'PGbool))
every = unsafeAggregate "every"

jsonAgg
  :: Expression tables 'Ungrouped params ('Required ty)
  -> Expression tables ('Grouped bys) params ('Required ('NotNull 'PGjson))
jsonAgg = unsafeAggregate "json_agg"

jsonbAgg
  :: Expression tables 'Ungrouped params ('Required ty)
  -> Expression tables ('Grouped bys) params ('Required ('NotNull 'PGjsonb))
jsonbAgg = unsafeAggregate "jsonb_agg"

jsonObjectAgg
  :: Expression tables 'Ungrouped params ('Required ('NotNull keyty))
  -> Expression tables 'Ungrouped params ('Required valty)
  -> Expression tables ('Grouped bys) params ('Required ('NotNull 'PGjson))
jsonObjectAgg k v = UnsafeExpression $
  "json_object_agg(" <> renderExpression k
    <> ", " <> renderExpression v <> ")"

jsonbObjectAgg
  :: Expression tables 'Ungrouped params ('Required ('NotNull keyty))
  -> Expression tables 'Ungrouped params ('Required valty)
  -> Expression tables ('Grouped bys) params ('Required ('NotNull 'PGjsonb))
jsonbObjectAgg k v = UnsafeExpression $
  "jsonb_object_agg("
    <> renderExpression k <> ", " <> renderExpression v <> ")"

max_, min_
  :: Expression tables 'Ungrouped params ('Required (nullity ty))
  -> Expression tables ('Grouped bys) params ('Required (nullity ty))
max_ = unsafeAggregate "max"
min_ = unsafeAggregate "min"

{-----------------------------------------
tables
-----------------------------------------}

newtype Table
  (schema :: TablesType)
  (columns :: ColumnsType)
    = UnsafeTable { renderTable :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

class KnownSymbol table => HasTable table tables columns
  | table tables -> columns where
    getTable :: Alias table -> Table tables columns
    getTable table = UnsafeTable $ renderAlias table
instance {-# OVERLAPPING #-} KnownSymbol table
  => HasTable table ((table ::: columns) ': tables) columns
instance {-# OVERLAPPABLE #-}
  (KnownSymbol table, HasTable table schema columns)
    => HasTable table (table' ': schema) columns

instance HasTable table schema columns
  => IsLabel table (Table schema columns) where
    fromLabel = getTable (Alias @table)

{-----------------------------------------
type expressions
-----------------------------------------}

newtype TypeExpression (ty :: ColumnType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

bool :: TypeExpression ('Required ('Null 'PGbool))
bool = UnsafeTypeExpression "bool"
int2 :: TypeExpression ('Required ('Null 'PGint2))
int2 = UnsafeTypeExpression "int2"
smallint :: TypeExpression ('Required ('Null 'PGint2))
smallint = UnsafeTypeExpression "smallint"
int4 :: TypeExpression ('Required ('Null 'PGint4))
int4 = UnsafeTypeExpression "int4"
int :: TypeExpression ('Required ('Null 'PGint4))
int = UnsafeTypeExpression "int"
integer :: TypeExpression ('Required ('Null 'PGint4))
integer = UnsafeTypeExpression "integer"
int8 :: TypeExpression ('Required ('Null 'PGint8))
int8 = UnsafeTypeExpression "int8"
bigint :: TypeExpression ('Required ('Null 'PGint8))
bigint = UnsafeTypeExpression "bigint"
numeric :: TypeExpression ('Required ('Null 'PGnumeric))
numeric = UnsafeTypeExpression "numeric"
float4 :: TypeExpression ('Required ('Null 'PGfloat4))
float4 = UnsafeTypeExpression "float4"
real :: TypeExpression ('Required ('Null 'PGfloat4))
real = UnsafeTypeExpression "real"
float8 :: TypeExpression ('Required ('Null 'PGfloat8))
float8 = UnsafeTypeExpression "float8"
doublePrecision :: TypeExpression ('Required ('Null 'PGfloat8))
doublePrecision = UnsafeTypeExpression "double precision"
serial2 :: TypeExpression ('Optional ('NotNull 'PGint2))
serial2 = UnsafeTypeExpression "serial2"
smallserial :: TypeExpression ('Optional ('NotNull 'PGint2))
smallserial = UnsafeTypeExpression "smallserial"
serial4 :: TypeExpression ('Optional ('NotNull 'PGint4))
serial4 = UnsafeTypeExpression "serial4"
serial :: TypeExpression ('Optional ('NotNull 'PGint4))
serial = UnsafeTypeExpression "serial"
serial8 :: TypeExpression ('Optional ('NotNull 'PGint8))
serial8 = UnsafeTypeExpression "serial8"
bigserial :: TypeExpression ('Optional ('NotNull 'PGint8))
bigserial = UnsafeTypeExpression "bigserial"
text :: TypeExpression ('Required ('Null 'PGtext))
text = UnsafeTypeExpression "text"
char
  :: (KnownNat n, 1 <= n)
  => proxy n
  -> TypeExpression ('Required ('Null ('PGchar n)))
char p = UnsafeTypeExpression $ "char(" <> renderNat p <> ")"
character
  :: (KnownNat n, 1 <= n)
  => proxy n
  -> TypeExpression ('Required ('Null ('PGchar n)))
character p = UnsafeTypeExpression $  "character(" <> renderNat p <> ")"
varchar
  :: (KnownNat n, 1 <= n)
  => proxy n
  -> TypeExpression ('Required ('Null ('PGvarchar n)))
varchar p = UnsafeTypeExpression $ "varchar(" <> renderNat p <> ")"
characterVarying
  :: (KnownNat n, 1 <= n)
  => proxy n
  -> TypeExpression ('Required ('Null ('PGvarchar n)))
characterVarying p = UnsafeTypeExpression $
  "character varying(" <> renderNat p <> ")"
bytea :: TypeExpression ('Required ('Null ('PGbytea)))
bytea = UnsafeTypeExpression "bytea"
timestamp :: TypeExpression ('Required ('Null ('PGtimestamp)))
timestamp = UnsafeTypeExpression "timestamp"
timestampWithTimeZone :: TypeExpression ('Required ('Null ('PGtimestamptz)))
timestampWithTimeZone = UnsafeTypeExpression "timestamp with time zone"
date :: TypeExpression ('Required ('Null ('PGdate)))
date = UnsafeTypeExpression "date"
time :: TypeExpression ('Required ('Null ('PGtime)))
time = UnsafeTypeExpression "time"
timeWithTimeZone :: TypeExpression ('Required ('Null ('PGtimetz)))
timeWithTimeZone = UnsafeTypeExpression "time with time zone"
interval :: TypeExpression ('Required ('Null ('PGinterval)))
interval = UnsafeTypeExpression "interval"
uuid :: TypeExpression ('Required ('Null ('PGuuid)))
uuid = UnsafeTypeExpression "uuid"
json :: TypeExpression ('Required ('Null ('PGjson)))
json = UnsafeTypeExpression "json"
jsonb :: TypeExpression ('Required ('Null ('PGjsonb)))
jsonb = UnsafeTypeExpression "jsonb"

notNull
  :: TypeExpression (optionality ('Null ty))
  -> TypeExpression (optionality ('NotNull ty))
notNull ty = UnsafeTypeExpression $ renderTypeExpression ty <+> "NOT NULL"
default_
  :: Expression '[] 'Ungrouped '[] ('Required ty)
  -> TypeExpression ('Required ty)
  -> TypeExpression ('Optional ty)
default_ x ty = UnsafeTypeExpression $
  renderTypeExpression ty <+> "DEFAULT" <+> renderExpression x

class PGTyped (ty :: PGType) where
  pgtype :: TypeExpression ('Required ('Null ty))
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

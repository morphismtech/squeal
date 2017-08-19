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

module Squeal.PostgreSQL.Statement 
  ( -- * Column Expressions
    Expression (UnsafeExpression, renderExpression)
  , HasParameter (param)
  , HasColumn (getColumn)
  , Column (Column)
  , renderColumn
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
  , (&&*)
  , (||*)
  , caseWhenThenElse
  , ifThenElse
  , (==*)
  , (/=*)
  , (>=*)
  , (*<)
  , (*<=)
  , (>*)
    -- ** Time
  , currentDate
  , currentTime
  , currentTimestamp
  , localTime
  , localTimestamp
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
    -- ** Froms and Joins
  , FromClause (..)
  , renderFromClause
    -- ** Table Expressions
  , TableExpression (..)
  , renderTableExpression
  , from
  , where_
  , group
  , having
  , orderBy
  , limit
  , offset
  , By (By, By2)
  , renderBy
  , GroupByClause (NoGroups, Group)
  , renderGroupByClause
  , HavingClause (NoHaving, Having)
  , renderHavingClause
  , GroupedBy (getGroup1, getGroup2)
  , SortExpression (..)
  , renderSortExpression
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
    -- * Statements
    -- ** Queries
  , Query (UnsafeQuery, renderQuery)
  , select
  , selectDistinct
  , selectStar
  , selectDistinctStar
  , selectDotStar
  , selectDistinctDotStar
  , union
  , unionAll
  , intersect
  , intersectAll
  , except
  , exceptAll
    -- ** Data Definition Language
  , Definition (UnsafeDefinition, renderDefinition)
  , createTable
  , TableConstraint (UnsafeTableConstraint, renderTableConstraint)
  , check
  , unique
  , primaryKey
  , foreignKey
  , OnDelete (OnDeleteNoAction, OnDeleteRestrict, OnDeleteCascade)
  , renderOnDelete
  , OnUpdate (OnUpdateNoAction, OnUpdateRestrict, OnUpdateCascade)
  , renderOnUpdate
  , dropTable
  , alterTable
  , alterTableRename
  , AlterTable (UnsafeAlterTable, renderAlterTable)
  , addColumnDefault
  , addColumnNull
  , dropColumn
  , renameColumn
  , alterColumn
  , AlterColumn (UnsafeAlterColumn, renderAlterColumn)
  , setDefault
  , dropDefault
  , setNotNull
  , dropNotNull
  , alterType
    -- ** Data Manipulation Language
  , Manipulation (UnsafeManipulation, renderManipulation)
  , queryStatement
  , insertInto
  , ValuesClause (Values, ValuesQuery)
  , renderValuesClause
  , ReturningClause (ReturningStar, Returning)
  , renderReturningClause
  , ConflictClause (Conflict, OnConflictDoNothing, OnConflictDoUpdate)
  , renderConflictClause
  , UpdateExpression (Same, Set)
  , renderUpdateExpression
  , update
  , deleteFrom
    -- * Re-export
  , (&)
  , NP ((:*), Nil)
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Foldable
import Data.Function ((&))
import Data.Maybe
import Data.Monoid hiding (All)
import Data.Ratio
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits
import Prelude hiding (id, (.))

import qualified GHC.Generics as GHC
import qualified Data.ByteString as ByteString

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
  (params :: [ColumnType])
  (tables :: TablesType)
  (grouping :: Grouping)
  (ty :: ColumnType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

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
    param :: Expression params tables grouping ty
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
      -> Expression params tables 'Ungrouped ty
    getColumn column = UnsafeExpression $ renderAlias column
instance {-# OVERLAPPING #-} KnownSymbol column
  => HasColumn column ((column ::: optionality ty) ': tys) ('Required ty)
instance {-# OVERLAPPABLE #-} (KnownSymbol column, HasColumn column table ty)
  => HasColumn column (ty' ': table) ty

instance (HasColumn column columns ty, HasUnique table tables columns)
  => IsLabel column (Expression params tables 'Ungrouped ty) where
    fromLabel = getColumn (Alias @column)

instance (HasTable table tables columns, HasColumn column columns ty)
  => IsTableColumn table column (Expression params tables 'Ungrouped ty) where
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

-- | >>> renderExpression def
-- "DEFAULT"
def :: Expression params '[] 'Ungrouped ('Optional (nullity ty))
def = UnsafeExpression "DEFAULT"

-- | >>> renderExpression $ unDef false
-- "FALSE"
unDef
  :: Expression params '[] 'Ungrouped ('Required (nullity ty))
  -> Expression params '[] 'Ungrouped ('Optional (nullity ty))
unDef = UnsafeExpression . renderExpression

-- | analagous to `Nothing`
--
-- >>> renderExpression $ null_
-- "NULL"
null_ :: Expression params tables grouping (optionality ('Null ty))
null_ = UnsafeExpression "NULL"

-- | analagous to `Just`
--
-- >>> renderExpression $ unNull true
-- "TRUE"
unNull
  :: Expression params tables grouping (optionality ('NotNull ty))
  -> Expression params tables grouping (optionality ('Null ty))
unNull = UnsafeExpression . renderExpression

-- | return the leftmost value which is not NULL
--
-- >>> renderExpression $ coalesce [null_, unNull true] false
-- "COALESCE(NULL, TRUE, FALSE)"
coalesce
  :: [Expression params tables grouping ('Required ('Null ty))]
  -> Expression params tables grouping ('Required ('NotNull ty))
  -> Expression params tables grouping ('Required ('NotNull ty))
coalesce nullxs notNullx = UnsafeExpression $
  "COALESCE" <> parenthesized (commaSeparated
    ((renderExpression <$> nullxs) <> [renderExpression notNullx]))

-- | analagous to `fromMaybe` using @COALESCE@
--
-- >>> renderExpression $ fromNull true null_
-- "COALESCE(NULL, TRUE)"
fromNull
  :: Expression params tables grouping ('Required ('NotNull ty))
  -> Expression params tables grouping ('Required ('Null ty))
  -> Expression params tables grouping ('Required ('NotNull ty))
fromNull notNullx nullx = coalesce [nullx] notNullx

-- | >>> renderExpression $ null_ & isNull
-- "NULL IS NULL"
isNull
  :: Expression params tables grouping ('Required ('Null ty))
  -> Condition params tables grouping
isNull x = UnsafeExpression $ renderExpression x <+> "IS NULL"

-- | >>> renderExpression $ null_ & isn'tNull
-- "NULL IS NOT NULL"
isn'tNull
  :: Expression params tables grouping ('Required ('Null ty))
  -> Condition params tables grouping
isn'tNull x = UnsafeExpression $ renderExpression x <+> "IS NOT NULL"

-- | analagous to `maybe` using @IS NULL@
--
-- >>> renderExpression $ matchNull true not_ null_
-- "CASE WHEN NULL IS NULL THEN TRUE ELSE (NOT NULL) END"
matchNull
  :: Expression params tables grouping ('Required nullty)
  -> ( Expression params tables grouping ('Required ('NotNull ty))
       -> Expression params tables grouping ('Required nullty) )
  -> Expression params tables grouping ('Required ('Null ty))
  -> Expression params tables grouping ('Required nullty)
matchNull y f x = ifThenElse (isNull x) y
  (f (UnsafeExpression (renderExpression x)))

-- | right inverse to `fromNull`
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> renderExpression @'[_] $ fromNull false (nullIf false (param @1))
-- "COALESCE(NULL IF (FALSE, ($1 :: bool)), FALSE)"
nullIf
  :: Expression params tables grouping ('Required ('NotNull ty))
  -> Expression params tables grouping ('Required ('NotNull ty))
  -> Expression params tables grouping ('Required ('Null ty))
nullIf x y = UnsafeExpression $ "NULL IF" <+> parenthesized
  (renderExpression x <> ", " <> renderExpression y)

-- | >>> renderExpression @'[_] $ greatest currentTimestamp [param @1]
-- "GREATEST(CURRENT_TIMESTAMP, ($1 :: timestamp with time zone))"
greatest
  :: Expression params tables grouping ('Required nullty)
  -> [Expression params tables grouping ('Required nullty)]
  -> Expression params table grouping ('Required nullty)
greatest x xs = UnsafeExpression $ "GREATEST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

-- | >>> renderExpression $ least currentTimestamp [null_]
-- "LEAST(CURRENT_TIMESTAMP, NULL)"
least
  :: Expression params tables grouping ('Required nullty)
  -> [Expression params tables grouping ('Required nullty)]
  -> Expression params table grouping ('Required nullty)
least x xs = UnsafeExpression $ "LEAST("
  <> commaSeparated (renderExpression <$> (x:xs)) <> ")"

unsafeBinaryOp
  :: ByteString
  -> Expression params tables grouping ('Required ty0)
  -> Expression params tables grouping ('Required ty1)
  -> Expression params tables grouping ('Required ty2)
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderExpression x <+> op <+> renderExpression y

unsafeUnaryOp
  :: ByteString
  -> Expression params tables grouping ('Required ty0)
  -> Expression params tables grouping ('Required ty1)
unsafeUnaryOp op x = UnsafeExpression $ parenthesized $
  op <+> renderExpression x

unsafeFunction
  :: ByteString
  -> Expression params tables grouping ('Required xty)
  -> Expression params tables grouping ('Required yty)
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderExpression x)

instance PGNum ty
  => Num (Expression params tables grouping ('Required (nullity ty))) where
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
  (Expression params tables grouping ('Required (nullity ty))) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (PGNum ty, PGFloating ty) => Floating
  (Expression params tables grouping ('Required (nullity ty))) where
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
  => Expression params tables grouping ('Required (nullity float))
  -> Expression params tables grouping ('Required (nullity float))
  -> Expression params tables grouping ('Required (nullity float))
atan2_ y x = UnsafeExpression $
  "atan2(" <> renderExpression y <> ", " <> renderExpression x <> ")"

cast
  :: TypeExpression ('Required ('Null ty1))
  -> Expression params tables grouping ('Required (nullity ty0))
  -> Expression params tables grouping ('Required (nullity ty1))
cast ty x = UnsafeExpression $ parenthesized $
  renderExpression x <+> "::" <+> renderTypeExpression ty

div_
  :: PGIntegral int
  => Expression params tables grouping ('Required (nullity int))
  -> Expression params tables grouping ('Required (nullity int))
  -> Expression params tables grouping ('Required (nullity int))
div_ = unsafeBinaryOp "/"

mod_
  :: PGIntegral int
  => Expression params tables grouping ('Required (nullity int))
  -> Expression params tables grouping ('Required (nullity int))
  -> Expression params tables grouping ('Required (nullity int))
mod_ = unsafeBinaryOp "%"

trunc
  :: PGFloating frac
  => Expression params tables grouping ('Required (nullity frac))
  -> Expression params tables grouping ('Required (nullity frac))
trunc = unsafeFunction "trunc"

round_
  :: PGFloating frac
  => Expression params tables grouping ('Required (nullity frac))
  -> Expression params tables grouping ('Required (nullity frac))
round_ = unsafeFunction "round"

ceiling_
  :: PGFloating frac
  => Expression params tables grouping ('Required (nullity frac))
  -> Expression params tables grouping ('Required (nullity frac))
ceiling_ = unsafeFunction "ceiling"

instance IsString
  (Expression params tables grouping ('Required (nullity 'PGtext))) where
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

type Condition params tables grouping =
  Expression params tables grouping ('Required ('NotNull 'PGbool))

true :: Condition params tables grouping
true = UnsafeExpression "TRUE"

false :: Condition params tables grouping
false = UnsafeExpression "FALSE"

not_
  :: Condition params tables grouping
  -> Condition params tables grouping
not_ = unsafeUnaryOp "NOT"

(&&*)
  :: Condition params tables grouping
  -> Condition params tables grouping
  -> Condition params tables grouping
(&&*) = unsafeBinaryOp "AND"

(||*)
  :: Condition params tables grouping
  -> Condition params tables grouping
  -> Condition params tables grouping
(||*) = unsafeBinaryOp "OR"

caseWhenThenElse
  :: [ ( Condition params tables grouping
       , Expression params tables grouping ('Required ty)
     ) ]
  -> Expression params tables grouping ('Required ty)
  -> Expression params tables grouping ('Required ty)
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
  :: Condition params tables grouping
  -> Expression params tables grouping ('Required ty)
  -> Expression params tables grouping ('Required ty)
  -> Expression params tables grouping ('Required ty)
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

(==*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGbool))
(==*) = unsafeBinaryOp "="
infix 4 ==*

(/=*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGbool))
(/=*) = unsafeBinaryOp "<>"
infix 4 /=*

(>=*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGbool))
(>=*) = unsafeBinaryOp ">="
infix 4 >=*

(*<)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGbool))
(*<) = unsafeBinaryOp "<"
infix 4 *<

(*<=)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGbool))
(*<=) = unsafeBinaryOp "<="
infix 4 *<=

(>*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGbool))
(>*) = unsafeBinaryOp ">"
infix 4 >*

currentDate
  :: Expression params tables grouping ('Required (nullity 'PGdate))
currentDate = UnsafeExpression "CURRENT_DATE"

currentTime
  :: Expression params tables grouping ('Required (nullity 'PGtimetz))
currentTime = UnsafeExpression "CURRENT_TIME"

currentTimestamp
  :: Expression params tables grouping ('Required (nullity 'PGtimestamptz))
currentTimestamp = UnsafeExpression "CURRENT_TIMESTAMP"

localTime
  :: Expression params tables grouping ('Required (nullity 'PGtime))
localTime = UnsafeExpression "LOCALTIME"

localTimestamp
  :: Expression params tables grouping ('Required (nullity 'PGtimestamp))
localTimestamp = UnsafeExpression "LOCALTIMESTAMP"

data SortExpression params tables grouping where
  Asc
    :: Expression params tables grouping ('Required ('NotNull ty))
    -> SortExpression params tables grouping
  Desc
    :: Expression params tables grouping ('Required ('NotNull ty))
    -> SortExpression params tables grouping
  AscNullsFirst
    :: Expression params tables grouping ('Required ('Null ty))
    -> SortExpression params tables grouping
  AscNullsLast
    :: Expression params tables grouping ('Required ('Null ty))
    -> SortExpression params tables grouping
  DescNullsFirst
    :: Expression params tables grouping ('Required ('Null ty))
    -> SortExpression params tables grouping
  DescNullsLast
    :: Expression params tables grouping ('Required ('Null ty))
    -> SortExpression params tables grouping
deriving instance Show (SortExpression params tables grouping)

renderSortExpression :: SortExpression params tables grouping -> ByteString
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
aggregation
-----------------------------------------}

data By
  (tables :: TablesType)
  (by :: (Symbol,Symbol)) where
  By
    :: (HasUnique table tables columns, HasColumn column columns ty)
    => Alias column
    -> By tables '(table, column)
  By2
    :: (HasTable table tables columns, HasColumn column columns ty)
    => (Alias table, Alias column)
    -> By tables '(table, column)
deriving instance Show (By tables by)
deriving instance Eq (By tables by)
deriving instance Ord (By tables by)

renderBy :: By tables tabcolty -> ByteString
renderBy = \case
  By column -> renderAlias column
  By2 (table, column) -> renderAlias table <> "." <> renderAlias column

data GroupByClause tables grouping where
  NoGroups :: GroupByClause tables 'Ungrouped
  Group
    :: SListI bys
    => NP (By tables) bys
    -> GroupByClause tables ('Grouped bys)

renderGroupByClause :: GroupByClause tables grouping -> ByteString
renderGroupByClause = \case
  NoGroups -> ""
  Group Nil -> ""
  Group bys -> " GROUP BY" <+> renderCommaSeparated renderBy bys

data HavingClause params tables grouping where
  NoHaving :: HavingClause params tables 'Ungrouped
  Having
    :: [Condition params tables ('Grouped bys)]
    -> HavingClause params tables ('Grouped bys)
deriving instance Show (HavingClause params tables grouping)
deriving instance Eq (HavingClause params tables grouping)
deriving instance Ord (HavingClause params tables grouping)

renderHavingClause :: HavingClause params tables grouping -> ByteString
renderHavingClause = \case
  NoHaving -> ""
  Having [] -> ""
  Having conditions ->
    " HAVING" <+> commaSeparated (renderExpression <$> conditions)

class (KnownSymbol table, KnownSymbol column)
  => GroupedBy table column bys where
    getGroup1
      :: (HasUnique table tables columns, HasColumn column columns ty)
      => Alias column
      -> Expression params tables ('Grouped bys) ty
    getGroup1 column = UnsafeExpression $ renderAlias column
    getGroup2
      :: (HasTable table tables columns, HasColumn column columns ty)
      => Alias table
      -> Alias column
      -> Expression params tables ('Grouped bys) ty
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
    (Expression params tables ('Grouped bys) ty) where
      fromLabel = getGroup1 (Alias @column)

instance
  ( HasTable table tables columns
  , HasColumn column columns ty
  , GroupedBy table column bys
  ) => IsTableColumn table column
    (Expression params tables ('Grouped bys) ty) where (!) = getGroup2

-- data AllOrDistinct = All | Distinct
--   deriving (GHC.Generic,Show,Eq,Ord,Data)
-- instance NFData AllOrDistinct

-- renderAllOrDistinct :: AllOrDistinct -> ByteString
-- renderAllOrDistinct = \case
--   All -> "ALL"
--   Distinct -> "DISTINCT"

unsafeAggregate, unsafeAggregateDistinct
  :: ByteString
  -> Expression params tables 'Ungrouped ('Required xty)
  -> Expression params tables ('Grouped bys) ('Required yty)
unsafeAggregate fun x = UnsafeExpression $ mconcat
  [fun, "(", renderExpression x, ")"]
unsafeAggregateDistinct fun x = UnsafeExpression $ mconcat
  [fun, "(DISTINCT ", renderExpression x, ")"]

sum_, sumDistinct
  :: PGNum ty
  => Expression params tables 'Ungrouped ('Required (nullity ty))
  -> Expression params tables ('Grouped bys) ('Required (nullity ty))
sum_ = unsafeAggregate "sum"
sumDistinct = unsafeAggregateDistinct "sum"

class PGAvg ty avg | ty -> avg where
  avg, avgDistinct
    :: Expression params tables 'Ungrouped ('Required (nullity ty))
    -> Expression params tables ('Grouped bys) ('Required (nullity avg))
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
  => Expression params tables 'Ungrouped ('Required (nullity int))
  -> Expression params tables ('Grouped bys) ('Required (nullity int))
bitAnd = unsafeAggregate "bit_and"
bitOr = unsafeAggregate "bit_or"

boolAnd, boolOr
  :: Expression params tables 'Ungrouped ('Required (nullity 'PGbool))
  -> Expression params tables ('Grouped bys) ('Required (nullity 'PGbool))
boolAnd = unsafeAggregate "bool_and"
boolOr = unsafeAggregate "bool_or"

countStar, countDistinctStar
  :: Expression params table ('Grouped bys) ('Required ('NotNull 'PGint8))
countStar = UnsafeExpression $ "count(*)"
countDistinctStar = UnsafeExpression $ "count(DISTINCT *)"

count
  :: Expression params tables 'Ungrouped ('Required ty)
  -> Expression params tables ('Grouped bys) ('Required ('NotNull 'PGint8))
count = unsafeAggregate "count"
  
every
  :: Expression params tables 'Ungrouped ('Required (nullity 'PGbool))
  -> Expression params tables ('Grouped bys) ('Required (nullity 'PGbool))
every = unsafeAggregate "every"

jsonAgg
  :: Expression params tables 'Ungrouped ('Required ty)
  -> Expression params tables ('Grouped bys) ('Required ('NotNull 'PGjson))
jsonAgg = unsafeAggregate "json_agg"

jsonbAgg
  :: Expression params tables 'Ungrouped ('Required ty)
  -> Expression params tables ('Grouped bys) ('Required ('NotNull 'PGjsonb))
jsonbAgg = unsafeAggregate "jsonb_agg"

jsonObjectAgg
  :: Expression params tables 'Ungrouped ('Required ('NotNull keyty))
  -> Expression params tables 'Ungrouped ('Required valty)
  -> Expression params tables ('Grouped bys) ('Required ('NotNull 'PGjson))
jsonObjectAgg k v = UnsafeExpression $
  "json_object_agg(" <> renderExpression k
    <> ", " <> renderExpression v <> ")"

jsonbObjectAgg
  :: Expression params tables 'Ungrouped ('Required ('NotNull keyty))
  -> Expression params tables 'Ungrouped ('Required valty)
  -> Expression params tables ('Grouped bys) ('Required ('NotNull 'PGjsonb))
jsonbObjectAgg k v = UnsafeExpression $
  "jsonb_object_agg("
    <> renderExpression k <> ", " <> renderExpression v <> ")"

max_, min_
  :: Expression params tables 'Ungrouped ('Required (nullity ty))
  -> Expression params tables ('Grouped bys) ('Required (nullity ty))
max_ = unsafeAggregate "max"
min_ = unsafeAggregate "min"

{-----------------------------------------
tables
-----------------------------------------}

newtype Table
  (schema :: TablesType)
  (columns :: ColumnsType)
    = UnsafeTable { renderTable :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

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

data FromClause params schema tables where
  Table
    :: Aliased (Table schema) table
    -> FromClause params schema '[table]
  Subquery
    :: Aliased (Query schema params) table
    -> FromClause params schema '[table]
  CrossJoin
    :: FromClause params schema right
    -> FromClause params schema left
    -> FromClause params schema (Join left right)
  InnerJoin
    :: FromClause params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> FromClause params schema left
    -> FromClause params schema (Join left right)
  LeftOuterJoin
    :: FromClause params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> FromClause params schema left
    -> FromClause params schema (Join left (NullifyTables right))
  RightOuterJoin
    :: FromClause params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> FromClause params schema left
    -> FromClause params schema (Join (NullifyTables left) right)
  FullOuterJoin
    :: FromClause params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> FromClause params schema left
    -> FromClause params schema
        (Join (NullifyTables left) (NullifyTables right))

renderFromClause :: FromClause params schema tables -> ByteString
renderFromClause = \case
  Table table -> renderAliased renderTable table
  Subquery selection -> renderAliased (parenthesized . renderQuery) selection
  CrossJoin right left ->
    renderFromClause left <+> "CROSS JOIN" <+> renderFromClause right
  InnerJoin right on left -> renderJoin "INNER JOIN" right on left
  LeftOuterJoin right on left -> renderJoin "LEFT OUTER JOIN" right on left
  RightOuterJoin right on left -> renderJoin "RIGHT OUTER JOIN" right on left
  FullOuterJoin right on left -> renderJoin "FULL OUTER JOIN" right on left
  where
    renderJoin op right on left =
      renderFromClause left <+> op <+> renderFromClause right
      <+> "ON" <+> renderExpression on

data TableExpression
  (params :: [ColumnType])
  (schema :: TablesType)
  (tables :: TablesType)
  (grouping :: Grouping)
    = TableExpression
    { fromClause :: FromClause params schema tables
    , whereClause :: [Condition params tables 'Ungrouped]
    , groupByClause :: GroupByClause tables grouping
    , havingClause :: HavingClause params tables grouping
    , orderByClause :: [SortExpression params tables grouping]
    , limitClause :: [Word64]
    , offsetClause :: [Word64]
    }

renderTableExpression
  :: TableExpression params schema tables grouping
  -> ByteString
renderTableExpression
  (TableExpression tables whs' grps' hvs' srts' lims' offs') = mconcat
    [ "FROM ", renderFromClause tables
    , renderWheres whs'
    , renderGroupByClause grps'
    , renderHavingClause hvs'
    , renderSorts srts'
    , renderLimits lims'
    , renderOffsets offs'
    ]
    where
      renderWheres = \case
        [] -> ""
        wh:[] -> " WHERE" <+> renderExpression wh
        wh:whs -> " WHERE" <+> renderExpression (foldr (&&*) wh whs)
      renderSorts = \case
        [] -> ""
        srts -> " SORT BY"
          <+> commaSeparated (renderSortExpression <$> srts)
      renderLimits = \case
        [] -> ""
        lims -> " LIMIT" <+> fromString (show (minimum lims))
      renderOffsets = \case
        [] -> ""
        offs -> " OFFSET" <+> fromString (show (sum offs))

from
  :: FromClause params schema tables
  -> TableExpression params schema tables 'Ungrouped
from tables = TableExpression tables [] NoGroups NoHaving [] [] []

where_
  :: Condition params tables 'Ungrouped
  -> TableExpression params schema tables grouping
  -> TableExpression params schema tables grouping
where_ wh tables = tables {whereClause = wh : whereClause tables}

group
  :: SListI bys
  => NP (By tables) bys
  -> TableExpression params schema tables 'Ungrouped 
  -> TableExpression params schema tables ('Grouped bys)
group bys tables = TableExpression
  { fromClause = fromClause tables
  , whereClause = whereClause tables
  , groupByClause = Group bys
  , havingClause = Having []
  , orderByClause = []
  , limitClause = limitClause tables
  , offsetClause = offsetClause tables
  }

having
  :: Condition params tables ('Grouped bys)
  -> TableExpression params schema tables ('Grouped bys)
  -> TableExpression params schema tables ('Grouped bys)
having hv tables = tables
  { havingClause = case havingClause tables of Having hvs -> Having (hv:hvs) }

orderBy
  :: [SortExpression params tables grouping]
  -> TableExpression params schema tables grouping
  -> TableExpression params schema tables grouping
orderBy srts tables = tables {orderByClause = orderByClause tables ++ srts}

limit
  :: Word64
  -> TableExpression params schema tables grouping
  -> TableExpression params schema tables grouping
limit lim tables = tables {limitClause = lim : limitClause tables}

offset
  :: Word64
  -> TableExpression params schema tables grouping
  -> TableExpression params schema tables grouping
offset off tables = tables {offsetClause = off : offsetClause tables}

{-----------------------------------------
type expressions
-----------------------------------------}

newtype TypeExpression (ty :: ColumnType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

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
  :: Expression '[] '[] 'Ungrouped ('Required ty)
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

{-----------------------------------------
statements
-----------------------------------------}

newtype Definition
  (schema0 :: TablesType)
  (schema1 :: TablesType)
  = UnsafeDefinition { renderDefinition :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

instance Category Definition where
  id = UnsafeDefinition ";"
  ddl1 . ddl0 = UnsafeDefinition $
    renderDefinition ddl0 <+> renderDefinition ddl1

newtype Manipulation
  (schema :: TablesType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

newtype Query
  (schema :: TablesType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
    = UnsafeQuery { renderQuery :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

queryStatement
  :: Query schema params columns
  -> Manipulation schema params columns
queryStatement q = UnsafeManipulation $ renderQuery q <> ";"

union, unionAll, intersect, intersectAll, except, exceptAll
  :: Query schema params columns
  -> Query schema params columns
  -> Query schema params columns
q1 `union` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "UNION"
  <+> parenthesized (renderQuery q2)
q1 `unionAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "UNION" <+> "ALL"
  <+> parenthesized (renderQuery q2)
q1 `intersect` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "INTERSECT"
  <+> parenthesized (renderQuery q2)
q1 `intersectAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "INTERSECT" <+> "ALL"
  <+> parenthesized (renderQuery q2)
q1 `except` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "EXCEPT"
  <+> parenthesized (renderQuery q2)
q1 `exceptAll` q2 = UnsafeQuery $
  parenthesized (renderQuery q1)
  <+> "EXCEPT" <+> "ALL"
  <+> parenthesized (renderQuery q2)

{-----------------------------------------
SELECT statements
-----------------------------------------}

select
  :: SListI columns
  => NP (Aliased (Expression params tables grouping)) (column ': columns)
  -> TableExpression params schema tables grouping
  -> Query schema params (column ': columns)
select list tabs = UnsafeQuery $
  "SELECT"
  <+> renderCommaSeparated (renderAliased renderExpression) list
  <+> renderTableExpression tabs

selectDistinct
  :: SListI columns
  => NP (Aliased (Expression params tables grouping)) (column ': columns)
  -> TableExpression params schema tables grouping
  -> Query schema params (column ': columns)
selectDistinct list tabs = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderCommaSeparated (renderAliased renderExpression) list
  <+> renderTableExpression tabs

selectStar
  :: HasUnique table tables columns
  => TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectStar tabs = UnsafeQuery $ "SELECT" <+> "*" <+> renderTableExpression tabs

selectDistinctStar
  :: HasUnique table tables columns
  => TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectDistinctStar tabs = UnsafeQuery $
  "SELECT DISTINCT" <+> "*" <+> renderTableExpression tabs

selectDotStar
  :: HasTable table tables columns
  => Alias table
  -> TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectDotStar table tables = UnsafeQuery $
  "SELECT" <+> renderAlias table <> ".*" <+> renderTableExpression tables

selectDistinctDotStar
  :: HasTable table tables columns
  => Alias table
  -> TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectDistinctDotStar table tables = UnsafeQuery $
  "SELECT DISTINCT" <+> renderAlias table <> ".*"
  <+> renderTableExpression tables

{-----------------------------------------
CREATE statements
-----------------------------------------}

createTable
  :: (KnownSymbol table, SListI columns)
  => Alias table
  -> NP (Aliased TypeExpression) (column ': columns)
  -> [TableConstraint schema (column ': columns)]
  -> Definition schema (Create table (column ': columns) schema)
createTable table columns constraints = UnsafeDefinition $
  "CREATE TABLE" <+> renderAlias table
  <+> parenthesized
    ( renderCommaSeparated renderColumnDef columns
      <> renderConstraints constraints )
  <> ";"
  where
    renderColumnDef :: Aliased TypeExpression x -> ByteString
    renderColumnDef (ty `As` column) =
      renderAlias column <+> renderTypeExpression ty
    renderConstraints :: [TableConstraint schema columns] -> ByteString
    renderConstraints = \case
      [] -> ""
      _ -> ", " <> commaSeparated (renderTableConstraint <$> constraints)

newtype TableConstraint
  (schema :: TablesType)
  (columns :: ColumnsType)
    = UnsafeTableConstraint { renderTableConstraint :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

check
  :: Condition '[] '[table ::: columns] 'Ungrouped
  -> TableConstraint schema columns
check condition = UnsafeTableConstraint $
  "CHECK" <+> parenthesized (renderExpression condition)

renderColumn :: Column columns columnty -> ByteString
renderColumn (Column column) = renderAlias column

unique
  :: SListI subcolumns
  => NP (Column columns) subcolumns
  -> TableConstraint schema columns
unique columns = UnsafeTableConstraint $
  "UNIQUE" <+> parenthesized (renderCommaSeparated renderColumn columns)

primaryKey
  :: (SListI subcolumns, AllNotNull subcolumns)
  => NP (Column columns) subcolumns
  -> TableConstraint schema columns
primaryKey columns = UnsafeTableConstraint $
  "PRIMARY KEY" <+> parenthesized (renderCommaSeparated renderColumn columns)

foreignKey
  :: ( HasTable reftable schema refcolumns
     , SameTypes subcolumns refsubcolumns
     , NotAllNull subcolumns
     , SListI subcolumns
     , SListI refsubcolumns)
  => NP (Column columns) subcolumns
  -> Alias reftable
  -> NP (Column refcolumns) refsubcolumns
  -> OnDelete
  -> OnUpdate
  -> TableConstraint schema columns
foreignKey columns reftable refcolumns onDelete onUpdate =
  UnsafeTableConstraint $
    "FOREIGN KEY" <+> parenthesized (renderCommaSeparated renderColumn columns)
    <+> "REFERENCES" <+> renderAlias reftable
    <+> parenthesized (renderCommaSeparated renderColumn refcolumns)
    <+> renderOnDelete onDelete
    <+> renderOnUpdate onUpdate

data OnDelete
  = OnDeleteNoAction
  | OnDeleteRestrict
  | OnDeleteCascade
  deriving (GHC.Generic,Show,Eq,Ord,Data)
instance NFData OnDelete

renderOnDelete :: OnDelete -> ByteString
renderOnDelete = \case
  OnDeleteNoAction -> "ON DELETE NO ACTION"
  OnDeleteRestrict -> "ON DELETE RESTRICT"
  OnDeleteCascade -> "ON DELETE CASCADE"

data OnUpdate
  = OnUpdateNoAction
  | OnUpdateRestrict
  | OnUpdateCascade
  deriving (GHC.Generic,Show,Eq,Ord,Data)
instance NFData OnUpdate

renderOnUpdate :: OnUpdate -> ByteString
renderOnUpdate = \case
  OnUpdateNoAction -> "ON UPDATE NO ACTION"
  OnUpdateRestrict -> "ON UPDATE RESTRICT"
  OnUpdateCascade -> "ON UPDATE CASCADE"

{-----------------------------------------
DROP statements
-----------------------------------------}

dropTable
  :: KnownSymbol table
  => Alias table
  -> Definition schema (Drop table schema)
dropTable table = UnsafeDefinition $ "DROP TABLE" <+> renderAlias table <> ";"

{-----------------------------------------
ALTER statements
-----------------------------------------}

alterTable
  :: HasTable table schema columns0
  => Alias table
  -> AlterTable columns0 columns1
  -> Definition schema (Alter table schema columns1)
alterTable table alteration = UnsafeDefinition $
  "ALTER TABLE"
  <+> renderAlias table
  <+> renderAlterTable alteration
  <> ";"

alterTableRename
  :: (KnownSymbol table0, KnownSymbol table1)
  => Alias table0
  -> Alias table1
  -> Definition schema (Rename table0 table1 schema)
alterTableRename table0 table1 = UnsafeDefinition $
  "ALTER TABLE" <+> renderAlias table0
  <+> "RENAME TO" <+> renderAlias table1 <> ";"

newtype AlterTable
  (columns0 :: ColumnsType)
  (columns1 :: ColumnsType) =
    UnsafeAlterTable {renderAlterTable :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

addColumnDefault
  :: KnownSymbol column
  => Alias column
  -> TypeExpression ('Optional ty)
  -> AlterTable columns (Create column ('Optional ty) columns)
addColumnDefault column ty = UnsafeAlterTable $
  "ADD COLUMN" <+> renderAlias column <+> renderTypeExpression ty

addColumnNull
  :: KnownSymbol column
  => Alias column
  -> TypeExpression ('Required ('Null ty))
  -> AlterTable columns (Create column ('Required ('Null ty)) columns)
addColumnNull column ty = UnsafeAlterTable $
  "ADD COLUMN" <+> renderAlias column <+> renderTypeExpression ty

dropColumn
  :: KnownSymbol column
  => Alias column
  -> AlterTable columns (Drop column columns)
dropColumn column = UnsafeAlterTable $
  "DROP COLUMN" <+> renderAlias column

renameColumn
  :: (KnownSymbol column0, KnownSymbol column1)
  => Alias column0
  -> Alias column1
  -> AlterTable columns (Rename column0 column1 columns)
renameColumn column0 column1 = UnsafeAlterTable $
  "RENAME COLUMN" <+> renderAlias column0  <+> "TO" <+> renderAlias column1

alterColumn
  :: (KnownSymbol column, HasColumn column columns ty0)
  => Alias column
  -> AlterColumn ty0 ty1
  -> AlterTable columns (Alter column columns ty1)
alterColumn column alteration = UnsafeAlterTable $
  "ALTER COLUMN" <+> renderAlias column <+> renderAlterColumn alteration

newtype AlterColumn (ty0 :: ColumnType) (ty1 :: ColumnType) =
  UnsafeAlterColumn {renderAlterColumn :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,Data,NFData)

setDefault
  :: Expression '[] '[] 'Ungrouped ('Required ty)
  -> AlterColumn ('Required ty) ('Optional ty)
setDefault expression = UnsafeAlterColumn $
  "SET DEFAULT" <+> renderExpression expression

dropDefault :: AlterColumn ('Optional ty) ('Required ty)
dropDefault = UnsafeAlterColumn $ "DROP DEFAULT"

setNotNull
  :: AlterColumn (optionality ('Null ty)) (optionality ('NotNull ty))
setNotNull = UnsafeAlterColumn $ "SET NOT NULL"

dropNotNull
  :: AlterColumn (optionality ('NotNull ty)) (optionality ('Null ty))
dropNotNull = UnsafeAlterColumn $ "DROP NOT NULL"

alterType
  :: TypeExpression (optionality (nullity ty1))
  -> AlterColumn (optionality (nullity ty0)) (optionality (nullity ty1))
alterType ty = UnsafeAlterColumn $ "TYPE " <> renderTypeExpression ty

{-----------------------------------------
INSERT statements
-----------------------------------------}

insertInto
  :: (SListI columns, SListI results, HasTable table schema columns)
  => Alias table
  -> ValuesClause schema params columns
  -> ConflictClause params columns
  -> ReturningClause params columns results
  -> Manipulation schema params results
insertInto table insert conflict returning = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderAlias table
  <+> renderValuesClause insert
  <> renderConflictClause conflict
  <> renderReturningClause returning

data ValuesClause
  (schema :: TablesType)
  (params :: [ColumnType])
  (columns :: ColumnsType)
    = Values
        (NP (Aliased (Expression params '[] 'Ungrouped)) columns)
        [NP (Aliased (Expression params '[] 'Ungrouped)) columns]
    | ValuesQuery (Query schema params columns)

renderValuesClause
  :: SListI columns
  => ValuesClause schema params columns
  -> ByteString
renderValuesClause = \case
  Values row rows ->
    parenthesized (renderCommaSeparated renderAliasPart row)
    <+> "VALUES"
    <+> commaSeparated
      (parenthesized . renderCommaSeparated renderValuePart <$> row:rows)
    where
      renderAliasPart, renderValuePart
        :: Aliased (Expression params '[] 'Ungrouped) ty -> ByteString
      renderAliasPart (_ `As` name) = renderAlias name
      renderValuePart (value `As` _) = renderExpression value
  ValuesQuery q -> renderQuery q

data ReturningClause
  (params :: [ColumnType])
  (columns :: ColumnsType)
  (results :: ColumnsType)
  where
    ReturningStar :: ReturningClause params columns columns
    Returning
      :: NP
          (Aliased (Expression params '[table ::: columns] 'Ungrouped))
          results
      -> ReturningClause params columns results

renderReturningClause
  :: SListI results
  => ReturningClause params columns results
  -> ByteString
renderReturningClause = \case
  ReturningStar -> " RETURNING *;"
  Returning Nil -> ";"
  Returning results -> " RETURNING"
    <+> renderCommaSeparated (renderAliased renderExpression) results <> ";"

data ConflictClause params columns where
  Conflict :: ConflictClause params columns
  OnConflictDoNothing :: ConflictClause params columns
  OnConflictDoUpdate
    :: NP (Aliased (UpdateExpression params columns)) columns
    -> Maybe (Condition params '[table ::: columns] 'Ungrouped)
    -> ConflictClause params columns

renderConflictClause
  :: SListI columns
  => ConflictClause params columns
  -> ByteString
renderConflictClause = \case
  Conflict -> ""
  OnConflictDoNothing -> " ON CONFLICT DO NOTHING"
  OnConflictDoUpdate updates whMaybe
    -> " ON CONFLICT DO UPDATE SET"
      <+> renderCommaSeparatedMaybe renderUpdateExpression updates
      <> case whMaybe of
        Nothing -> ""
        Just wh -> " WHERE" <+> renderExpression wh

{-----------------------------------------
UPDATE statements
-----------------------------------------}

data UpdateExpression params columns ty
  = Same
  | Set (forall table. Expression params '[table ::: columns] 'Ungrouped ty)
deriving instance Show (UpdateExpression params columns ty)
deriving instance Eq (UpdateExpression params columns ty)
deriving instance Ord (UpdateExpression params columns ty)

renderUpdateExpression
  :: Aliased (UpdateExpression params columns) column
  -> Maybe ByteString
renderUpdateExpression = \case
  Same `As` _ -> Nothing
  Set expression `As` column -> Just $
    renderAlias column <+> "=" <+> renderExpression expression

update
  :: (HasTable table schema columns, SListI columns, SListI results)
  => Alias table
  -> NP (Aliased (UpdateExpression params columns)) columns
  -> Condition params '[tab ::: columns] 'Ungrouped
  -> ReturningClause params columns results
  -> Manipulation schema params results
update table columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderAlias table
  <+> "SET"
  <+> renderCommaSeparatedMaybe renderUpdateExpression columns
  <+> "WHERE" <+> renderExpression wh
  <> renderReturningClause returning

{-----------------------------------------
DELETE statements
-----------------------------------------}

deleteFrom
  :: HasTable table schema columns
  => Alias table
  -> Condition params '[table ::: columns] 'Ungrouped
  -> Manipulation schema params '[]
deleteFrom table wh = UnsafeManipulation $
  "DELETE FROM" <+> renderAlias table
  <+> "WHERE" <+> renderExpression wh <> ";"

{-----------------------------------------
render helper functions
-----------------------------------------}

parenthesized :: ByteString -> ByteString
parenthesized str = "(" <> str <> ")"

(<+>) :: ByteString -> ByteString -> ByteString
str1 <+> str2 = str1 <> " " <> str2

commaSeparated :: [ByteString] -> ByteString
commaSeparated = ByteString.intercalate ", "

renderCommaSeparated
  :: SListI xs
  => (forall x. expression x -> ByteString)
  -> NP expression xs -> ByteString
renderCommaSeparated render
  = commaSeparated
  . hcollapse
  . hmap (K . render)

renderCommaSeparatedMaybe
  :: SListI xs
  => (forall x. expression x -> Maybe ByteString)
  -> NP expression xs -> ByteString
renderCommaSeparatedMaybe render
  = commaSeparated
  . catMaybes
  . hcollapse
  . hmap (K . render)

renderNat :: KnownNat n => proxy n -> ByteString
renderNat (_ :: proxy n) = fromString (show (natVal' (proxy# :: Proxy# n)))

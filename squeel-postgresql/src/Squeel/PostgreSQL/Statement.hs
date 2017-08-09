{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , MagicHash
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeel.PostgreSQL.Statement where

import Control.Category
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.String
import Data.Word
import Generics.SOP
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits
import Prelude hiding (RealFrac(..), id, (.))

import qualified Data.ByteString as ByteString

import Squeel.PostgreSQL.Schema

{-----------------------------------------
column expressions
-----------------------------------------}

newtype Expression
  (params :: [ColumnType])
  (tables :: [(Symbol,[(Symbol,ColumnType)])])
  (grouping :: Grouping)
  (ty :: ColumnType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (Show,Eq)

class KnownNat n => HasParameter (n :: Nat) params ty | n params -> ty where
  param :: Proxy# n -> Expression params tables grouping ty
  param p = UnsafeExpression $ ("$" <>) $ fromString $ show $ natVal' p
instance {-# OVERLAPPING #-} HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

param1 :: Expression (ty1:tys) tables grouping ty1
param1 = param (proxy# :: Proxy# 1)
param2 :: Expression (ty1:ty2:tys) tables grouping ty2
param2 = param (proxy# :: Proxy# 2)
param3 :: Expression (ty1:ty2:ty3:tys) tables grouping ty3
param3 = param (proxy# :: Proxy# 3)
param4 :: Expression (ty1:ty2:ty3:ty4:tys) tables grouping ty4
param4 = param (proxy# :: Proxy# 4)
param5 :: Expression (ty1:ty2:ty3:ty4:ty5:tys) tables grouping ty5
param5 = param (proxy# :: Proxy# 5)

class KnownSymbol column => HasColumn column columns ty
  | column columns -> ty where
    getColumn
      :: Proxy# column
      -> Expression params '[table ::: columns] 'Ungrouped ty
    getColumn column = UnsafeExpression $ renderSymbol column
instance {-# OVERLAPPING #-} KnownSymbol column
  => HasColumn column ((column ::: optionality ty) ': tys) ('Required ty)
instance {-# OVERLAPPABLE #-} (KnownSymbol column, HasColumn column table ty)
  => HasColumn column (ty' ': table) ty

instance (HasColumn column columns ty, tables ~ '[table ::: columns])
  => IsLabel column (Expression params tables 'Ungrouped ty) where
    fromLabel = getColumn

instance (HasTable table tables columns, HasColumn column columns ty)
  => IsTableColumn table column (Expression params tables 'Ungrouped ty) where
    table &. column = UnsafeExpression $
      renderAlias table <> "." <> renderAlias column

def :: Expression params '[] grouping ('Optional (nullity ty))
def = UnsafeExpression "DEFAULT"

unDef
  :: Expression params '[] grouping ('Required (nullity ty))
  -> Expression params '[] grouping ('Optional (nullity ty))
unDef = UnsafeExpression . renderExpression

null :: Expression params tables grouping (optionality ('Null ty))
null = UnsafeExpression "NULL"

unNull
  :: Expression params tables grouping (optionality ('NotNull ty))
  -> Expression params tables grouping (optionality ('Null ty))
unNull = UnsafeExpression . renderExpression

coalesce
  :: [Expression params tables grouping ('Required ('Null x))]
  -> Expression params tables grouping ('Required ('NotNull x))
  -> Expression params tables grouping ('Required ('NotNull x))
coalesce nulls isn'tNull = UnsafeExpression $
  "COALESCE"
  <> parenthesized (commaSeparated (renderExpression <$> nulls))
  <> ", " <> renderExpression isn'tNull

unsafeBinaryOp
  :: ByteString
  -> Expression params tables grouping ty0
  -> Expression params tables grouping ty1
  -> Expression params tables grouping ty2
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderExpression x <+> op <+> renderExpression y

unsafeUnaryOp
  :: ByteString
  -> Expression params tables grouping ty0
  -> Expression params tables grouping ty1
unsafeUnaryOp op x = UnsafeExpression $ parenthesized $
  op <+> renderExpression x

unsafeFunction
  :: ByteString
  -> Expression params tables grouping xty
  -> Expression params tables grouping yty
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
      . (<> if decimal (Proxy :: Proxy ty) then "." else "")
      . fromString
      . show

instance PGFractional ty => Fractional
  (Expression params tables grouping ('Required (nullity ty))) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (PGFloating ty, PGCast 'PGNumeric ty, PGTyped ty) => Floating
  (Expression params tables grouping ('Required (nullity ty))) where
    pi = UnsafeExpression "pi()"
    exp = unsafeFunction "exp"
    log = unsafeFunction "ln"
    sqrt = unsafeFunction "sqrt"
    b ** x = UnsafeExpression $
      "power(" <> renderExpression b <> ", " <> renderExpression x <> ")"
    logBase b y = cast pgtype $ logBaseNumeric b y
      where
        logBaseNumeric
          :: Expression params tables grouping ('Required (nullity ty))
          -> Expression params tables grouping ('Required (nullity ty))
          -> Expression params tables grouping ('Required (nullity 'PGNumeric))
        logBaseNumeric b' y' = UnsafeExpression $ mconcat
          [ "log("
          , renderExpression b' <> "::numeric"
          , ", "
          , renderExpression y' <> "::numeric"
          , ")"
          ]
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

atan2
  :: PGFloating float
  => Expression params tables grouping ('Required (nullity float))
  -> Expression params tables grouping ('Required (nullity float))
  -> Expression params tables grouping ('Required (nullity float))
atan2 y x = UnsafeExpression $
  "atan2(" <> renderExpression y <> ", " <> renderExpression x <> ")"

class PGCast (ty0 :: PGType) (ty1 :: PGType) where
  cast
    :: TypeExpression ('Required ('Null ty1))
    -> Expression params tables grouping ('Required (nullity ty0))
    -> Expression params tables grouping ('Required (nullity ty1))
  cast ty x = UnsafeExpression $
    "(" <> renderExpression x <> "::" <> renderTypeExpression ty <> ")"
instance PGCast 'PGInt2 'PGInt2
instance PGCast 'PGInt2 'PGInt4
instance PGCast 'PGInt2 'PGInt8
instance PGCast 'PGInt4 'PGInt2
instance PGCast 'PGInt4 'PGInt4
instance PGCast 'PGInt4 'PGInt8
instance PGCast 'PGInt8 'PGInt2
instance PGCast 'PGInt8 'PGInt4
instance PGCast 'PGInt8 'PGInt8
instance PGCast 'PGInt8 'PGFloat4
instance PGCast 'PGInt8 'PGFloat8
instance PGCast 'PGInt8 'PGNumeric

div
  :: (PGNum ty, PGTyped ty, PGCast 'PGInt8 ty, PGCast ty 'PGInt8)
  => Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
div = unsafeBinaryOp "/"

mod
  :: (PGNum ty, PGTyped ty, PGCast 'PGInt8 ty, PGCast ty 'PGInt8)
  => Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
mod = unsafeBinaryOp "%"

truncate
  :: (PGFractional frac, PGCast 'PGInt4 int, PGTyped int)
  => Expression params tables grouping ('Required (nullity frac))
  -> Expression params tables grouping ('Required (nullity int))
truncate = cast pgtype . truncate'
  where
    truncate'
      :: Expression params tables grouping ('Required (nullity frac))
      -> Expression params tables grouping ('Required (nullity 'PGInt4))
    truncate' = unsafeFunction "trunc"

round
  :: (PGFractional frac, PGCast 'PGInt4 int, PGTyped int)
  => Expression params tables grouping ('Required (nullity frac))
  -> Expression params tables grouping ('Required (nullity int))
round = cast pgtype . round'
  where
    round'
      :: Expression params tables grouping ('Required (nullity frac))
      -> Expression params tables grouping ('Required (nullity 'PGInt4))
    round' = unsafeFunction "round"

ceiling
  :: (PGFractional frac, PGCast 'PGInt4 int, PGTyped int)
  => Expression params tables grouping ('Required (nullity frac))
  -> Expression params tables grouping ('Required (nullity int))
ceiling = cast pgtype . ceiling'
  where
    ceiling'
      :: Expression params tables grouping ('Required (nullity frac))
      -> Expression params tables grouping ('Required (nullity 'PGInt4))
    ceiling' = unsafeFunction "ceiling"

instance IsString
  (Expression params tables grouping ('Required (nullity 'PGText))) where
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
  Expression params tables grouping ('Required ('NotNull 'PGBool))

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
  :: [(Condition params tables grouping, Expression params tables grouping ty)]
  -> Expression params tables grouping ty
  -> Expression params tables grouping ty
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
  -> Expression params tables grouping ty
  -> Expression params tables grouping ty
  -> Expression params tables grouping ty
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

(==*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGBool))
(==*) = unsafeBinaryOp "="

(/=*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGBool))
(/=*) = unsafeBinaryOp "<>"

(>=*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGBool))
(>=*) = unsafeBinaryOp ">="

(<*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGBool))
(<*) = unsafeBinaryOp "<"

(<=*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGBool))
(<=*) = unsafeBinaryOp "<="

(>*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGBool))
(>*) = unsafeBinaryOp ">"

data SortExpression params tables grouping where
  Asc
    :: Expression params tables grouping ty
    -> SortExpression params tables grouping
  Desc
    :: Expression params tables grouping ty
    -> SortExpression params tables grouping

renderSortExpression :: SortExpression params tables grouping -> ByteString
renderSortExpression = \case
  Asc expression -> renderExpression expression <+> "ASC"
  Desc expression -> renderExpression expression <+> "DESC"

{-----------------------------------------
aggregation
-----------------------------------------}

data By
  (tables :: [(Symbol,[(Symbol,ColumnType)])])
  (tabcol :: (Symbol,Symbol)) where
  By
    :: (HasTable table tables columns, HasColumn column columns ty)
    => (Alias table, Alias column)
    -> By tables '(table, column)

renderBy :: By tables tabcolty -> ByteString
renderBy (By (table, column)) = renderAlias table <> "." <> renderAlias column

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

renderHavingClause :: HavingClause params tables grouping -> ByteString
renderHavingClause = \case
  NoHaving -> ""
  Having [] -> ""
  Having conditions ->
    " HAVING" <+> commaSeparated (renderExpression <$> conditions)

class (KnownSymbol table, KnownSymbol column)
  => GroupedBy table column bys where
    getGroup1
      :: (tables ~ '[table ::: columns], HasColumn column columns ty)
      => Proxy# column
      -> Expression params tables ('Grouped bys) ty
    getGroup1 column = UnsafeExpression $ fromString (symbolVal' column)
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
  ( tables ~ '[table ::: columns]
  , HasColumn column columns ty
  , GroupedBy table column bys
  ) => IsLabel column
    (Expression params tables ('Grouped bys) ty) where fromLabel = getGroup1

instance
  ( HasTable table tables columns
  , HasColumn column columns ty
  , GroupedBy table column bys
  ) => IsTableColumn table column
    (Expression params tables ('Grouped bys) ty) where (&.) = getGroup2

unsafeAggregate
  :: ByteString
  -> Expression params tables 'Ungrouped xty
  -> Expression params tables ('Grouped bys) yty
unsafeAggregate fun x = UnsafeExpression $ mconcat
  [fun, "(", renderExpression x, ")"]

sum_
  :: PGNum ty
  => Expression params tables 'Ungrouped ('Required (nullity ty))
  -> Expression params tables ('Grouped bys) ('Required (nullity ty))
sum_ = unsafeAggregate "sum"

{-----------------------------------------
tables
-----------------------------------------}

newtype Table
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (columns :: [(Symbol,ColumnType)])
    = UnsafeTable { renderTable :: ByteString }
    deriving (Show,Eq)

class KnownSymbol table => HasTable table tables columns
  | table tables -> columns where
    getTable :: Proxy# table -> Table tables columns
    getTable table = UnsafeTable $ fromString $ symbolVal' table
instance {-# OVERLAPPING #-} KnownSymbol table
  => HasTable table ((table ::: columns) ': tables) columns
instance {-# OVERLAPPABLE #-}
  (KnownSymbol table, HasTable table schema columns)
    => HasTable table (table' ': schema) columns

instance HasTable table schema columns
  => IsLabel table (Table schema columns) where
    fromLabel = getTable

data TableReference params schema tables where
  Table
    :: Aliased (Table schema) table
    -> TableReference params schema '[table]
  Subquery
    :: Aliased (Query schema params) table
    -> TableReference params schema '[table]
  CrossJoin
    :: TableReference params schema right
    -> TableReference params schema left
    -> TableReference params schema (Join left right)
  InnerJoin
    :: TableReference params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> TableReference params schema left
    -> TableReference params schema (Join left right)
  LeftOuterJoin
    :: TableReference params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> TableReference params schema left
    -> TableReference params schema (Join left (NullifyTables right))
  RightOuterJoin
    :: TableReference params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> TableReference params schema left
    -> TableReference params schema (Join (NullifyTables left) right)
  FullOuterJoin
    :: TableReference params schema right
    -> Condition params (Join left right) 'Ungrouped
    -> TableReference params schema left
    -> TableReference params schema
        (Join (NullifyTables left) (NullifyTables right))

renderTableReference :: TableReference params schema tables -> ByteString
renderTableReference = \case
  Table table -> renderAliased renderTable table
  Subquery selection -> renderAliased renderQuery selection
  CrossJoin right left ->
    renderTableReference left <+> "CROSS JOIN" <+> renderTableReference right
  InnerJoin right on left -> renderJoin "INNER JOIN" right on left
  LeftOuterJoin right on left -> renderJoin "LEFT OUTER JOIN" right on left
  RightOuterJoin right on left -> renderJoin "RIGHT OUTER JOIN" right on left
  FullOuterJoin right on left -> renderJoin "FULL OUTER JOIN" right on left
  where
    renderJoin op right on left =
      renderTableReference left <+> op <+> renderTableReference right
      <+> "ON" <+> renderExpression on

data TableExpression
  (params :: [ColumnType])
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (tables :: [(Symbol,[(Symbol,ColumnType)])])
  (grouping :: Grouping)
    = TableExpression
    { fromClause :: TableReference params schema tables
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
    [ "FROM ", renderTableReference tables
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
  :: TableReference params schema tables
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
  deriving (Show,Eq)

bool :: TypeExpression ('Required ('Null 'PGBool))
bool = UnsafeTypeExpression "bool"
int2 :: TypeExpression ('Required ('Null 'PGInt2))
int2 = UnsafeTypeExpression "int2"
smallint :: TypeExpression ('Required ('Null 'PGInt2))
smallint = UnsafeTypeExpression "smallint"
int4 :: TypeExpression ('Required ('Null 'PGInt4))
int4 = UnsafeTypeExpression "int4"
int :: TypeExpression ('Required ('Null 'PGInt4))
int = UnsafeTypeExpression "int"
integer :: TypeExpression ('Required ('Null 'PGInt4))
integer = UnsafeTypeExpression "integer"
int8 :: TypeExpression ('Required ('Null 'PGInt8))
int8 = UnsafeTypeExpression "int8"
bigint :: TypeExpression ('Required ('Null 'PGInt8))
bigint = UnsafeTypeExpression "bigint"
numeric :: TypeExpression ('Required ('Null 'PGNumeric))
numeric = UnsafeTypeExpression "numeric"
float4 :: TypeExpression ('Required ('Null 'PGFloat4))
float4 = UnsafeTypeExpression "float4"
real :: TypeExpression ('Required ('Null 'PGFloat4))
real = UnsafeTypeExpression "real"
float8 :: TypeExpression ('Required ('Null 'PGFloat8))
float8 = UnsafeTypeExpression "float8"
doublePrecision :: TypeExpression ('Required ('Null 'PGFloat8))
doublePrecision = UnsafeTypeExpression "double precision"
serial2 :: TypeExpression ('Optional ('NotNull 'PGInt2))
serial2 = UnsafeTypeExpression "serial2"
smallserial :: TypeExpression ('Optional ('NotNull 'PGInt2))
smallserial = UnsafeTypeExpression "smallserial"
serial4 :: TypeExpression ('Optional ('NotNull 'PGInt4))
serial4 = UnsafeTypeExpression "serial4"
serial :: TypeExpression ('Optional ('NotNull 'PGInt4))
serial = UnsafeTypeExpression "serial"
serial8 :: TypeExpression ('Optional ('NotNull 'PGInt8))
serial8 = UnsafeTypeExpression "serial8"
bigserial :: TypeExpression ('Optional ('NotNull 'PGInt8))
bigserial = UnsafeTypeExpression "bigserial"
money :: TypeExpression ('Required ('Null 'PGMoney))
money = UnsafeTypeExpression "money"
text :: TypeExpression ('Required ('Null 'PGText))
text = UnsafeTypeExpression "text"
char
  :: KnownNat n
  => proxy n
  -> TypeExpression ('Required ('Null ('PGChar n)))
char (_ :: proxy n) = UnsafeTypeExpression $
  "char(" <> fromString (show (natVal' (proxy# :: Proxy# n))) <> ")"
character
  :: KnownNat n
  => proxy n
  -> TypeExpression ('Required ('Null ('PGChar n)))
character (_ :: proxy n) = UnsafeTypeExpression $
  "character(" <> fromString (show (natVal' (proxy# :: Proxy# n))) <> ")"
varchar
  :: KnownNat n
  => proxy n
  -> TypeExpression ('Required ('Null ('PGVarChar n)))
varchar (_ :: proxy n) = UnsafeTypeExpression $
  "varchar(" <> fromString (show (natVal' (proxy# :: Proxy# n))) <> ")"
characterVarying
  :: KnownNat n
  => proxy n
  -> TypeExpression ('Required ('Null ('PGVarChar n)))
characterVarying (_ :: proxy n) = UnsafeTypeExpression $
  "character varying(" <> fromString (show (natVal' (proxy# :: Proxy# n))) <> ")"
bytea :: TypeExpression ('Required ('Null ('PGBytea)))
bytea = UnsafeTypeExpression "bytea"
timestamp :: TypeExpression ('Required ('Null ('PGTimestamp)))
timestamp = UnsafeTypeExpression "timestamp"
timestampWithTimeZone :: TypeExpression ('Required ('Null ('PGTimestampTZ)))
timestampWithTimeZone = UnsafeTypeExpression "timestamp with time zone"
date :: TypeExpression ('Required ('Null ('PGDate)))
date = UnsafeTypeExpression "date"
time :: TypeExpression ('Required ('Null ('PGTime)))
time = UnsafeTypeExpression "time"
timeWithTimeZone :: TypeExpression ('Required ('Null ('PGTimeTZ)))
timeWithTimeZone = UnsafeTypeExpression "time with time zone"
interval :: TypeExpression ('Required ('Null ('PGInterval)))
interval = UnsafeTypeExpression "interval"
uuid :: TypeExpression ('Required ('Null ('PGUuid)))
uuid = UnsafeTypeExpression "uuid"
json :: TypeExpression ('Required ('Null ('PGJson)))
json = UnsafeTypeExpression "json"
jsonb :: TypeExpression ('Required ('Null ('PGJsonb)))
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
instance PGTyped 'PGBool where pgtype = bool
instance PGTyped 'PGInt2 where pgtype = int2
instance PGTyped 'PGInt4 where pgtype = int4
instance PGTyped 'PGInt8 where pgtype = int8
instance PGTyped 'PGNumeric where pgtype = numeric
instance PGTyped 'PGFloat4 where pgtype = float4
instance PGTyped 'PGFloat8 where pgtype = float8
instance PGTyped 'PGMoney where pgtype = money
instance PGTyped 'PGText where pgtype = text
instance KnownNat n => PGTyped ('PGChar n) where pgtype = char (Proxy @n)
instance KnownNat n => PGTyped ('PGVarChar n) where pgtype = varchar (Proxy @n)
instance PGTyped 'PGBytea where pgtype = bytea
instance PGTyped 'PGTimestamp where pgtype = timestamp
instance PGTyped 'PGTimestampTZ where pgtype = timestampWithTimeZone
instance PGTyped 'PGDate where pgtype = date
instance PGTyped 'PGTime where pgtype = time
instance PGTyped 'PGTimeTZ where pgtype = timeWithTimeZone
instance PGTyped 'PGInterval where pgtype = interval
instance PGTyped 'PGUuid where pgtype = uuid
instance PGTyped 'PGJson where pgtype = json
instance PGTyped 'PGJsonb where pgtype = jsonb

{-----------------------------------------
statements
-----------------------------------------}

newtype Definition
  (schema0 :: [(Symbol,[(Symbol,ColumnType)])])
  (schema1 :: [(Symbol,[(Symbol,ColumnType)])])
  = UnsafeDefinition { renderDefinition :: ByteString }

instance Category Definition where
  id = UnsafeDefinition ";"
  ddl1 . ddl0 = UnsafeDefinition $
    renderDefinition ddl0 <+> renderDefinition ddl1

newtype Manipulation
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (params :: [ColumnType])
  (columns :: [(Symbol,ColumnType)])
    = UnsafeManipulation { renderManipulation :: ByteString }

newtype Query
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (params :: [ColumnType])
  (columns :: [(Symbol,ColumnType)])
    = UnsafeQuery { renderQuery :: ByteString }

query
  :: Query schema params columns
  -> Manipulation schema params columns
query q = UnsafeManipulation $ renderQuery q <> ";"

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
  => NP (Aliased (Expression params tables grouping)) columns
  -> TableExpression params schema tables grouping
  -> Query schema params columns
select list tabs = UnsafeQuery $
  "SELECT"
  <+> renderCommaSeparated (renderAliased renderExpression) list
  <+> renderTableExpression tabs

selectStar
  :: (tables ~ '[table ::: columns])
  => TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectStar tabs = UnsafeQuery $ "SELECT" <+> "*" <+> renderTableExpression tabs

selectDotStar
  :: HasTable table tables columns
  => Alias table
  -> TableExpression params schema tables 'Ungrouped
  -> Query schema params columns
selectDotStar table tables = UnsafeQuery $
  "SELECT" <+> renderAlias table <> ".*" <+> renderTableExpression tables

selectRow
  :: SListI columns
  => NP (Aliased (Expression params '[] 'Ungrouped)) columns
  -> Query schema params columns
selectRow row = UnsafeQuery $
  "SELECT" <+> renderCommaSeparated (renderAliased renderExpression) row

{-----------------------------------------
CREATE statements
-----------------------------------------}

createTable
  :: (KnownSymbol table, SListI columns)
  => Alias table
  -> NP (Aliased TypeExpression) columns
  -> Definition schema (Create table columns schema)
createTable table columns = UnsafeDefinition $
  "CREATE TABLE" <+> renderAlias table
  <+> parenthesized (renderCommaSeparated renderColumn columns) <> ";"
  where
    renderColumn :: Aliased TypeExpression x -> ByteString
    renderColumn (ty `As` column) =
      renderAlias column <+> renderTypeExpression ty

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

data AlterTable
  (columns0 :: [(Symbol, ColumnType)])
  (columns1 :: [(Symbol, ColumnType)]) =
    UnsafeAlterTable {renderAlterTable :: ByteString}

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
  -> AlterTable columns ((column ::: 'Required ('Null ty)) ': columns)
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

data AlterColumn (ty0 :: ColumnType) (ty1 :: ColumnType) =
  UnsafeAlterColumn {renderAlterColumn :: ByteString}

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
  :: PGCast ty0 ty1
  => TypeExpression (optionality (nullity ty1))
  -> AlterColumn (optionality (nullity ty0)) (optionality (nullity ty1))
alterType ty = UnsafeAlterColumn $ "TYPE " <> renderTypeExpression ty

{-----------------------------------------
INSERT statements
-----------------------------------------}

into
  :: (SListI columns, SListI results, HasTable table schema columns)
  => Alias table
  -> InsertClause schema params columns
  -> ConflictClause params columns
  -> ReturningClause params columns results
  -> Manipulation schema params results
into table insert conflict returning = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderAlias table
  <+> renderInsertClause insert
  <> renderConflictClause conflict
  <> renderReturningClause returning

data InsertClause
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (params :: [ColumnType])
  (columns :: [(Symbol,ColumnType)])
  = InsertValues
      (NP (Aliased (Expression params '[] 'Ungrouped)) columns)
      [NP (Aliased (Expression params '[] 'Ungrouped)) columns]
  | InsertQuery (Query schema params columns)

insertRow
  :: (NP (Aliased (Expression params '[] 'Ungrouped)) columns)
  -> InsertClause schema params columns
insertRow row = InsertValues row []

renderInsertClause
  :: SListI columns
  => InsertClause schema params columns
  -> ByteString
renderInsertClause = \case
  InsertValues row rows ->
    parenthesized (renderCommaSeparated renderAliasPart row)
    <+> "VALUES"
    <+> commaSeparated
      (parenthesized . renderCommaSeparated renderValuePart <$> row:rows)
    where
      renderAliasPart, renderValuePart
        :: Aliased (Expression params '[] 'Ungrouped) ty -> ByteString
      renderAliasPart (_ `As` name) = renderAlias name
      renderValuePart (value `As` _) = renderExpression value
  InsertQuery q -> renderQuery q

data ReturningClause
  (params :: [ColumnType])
  (columns :: [(Symbol,ColumnType)])
  (results :: [(Symbol,ColumnType)])
  where
    ReturningNil :: ReturningClause params columns '[]
    ReturningStar :: ReturningClause params columns columns
    Returning
      :: NP (Aliased (Expression params '[table ::: columns] 'Ungrouped)) results
      -> ReturningClause params columns results

renderReturningClause
  :: SListI results
  => ReturningClause params columns results
  -> ByteString
renderReturningClause = \case
  ReturningNil -> ";"
  ReturningStar -> " RETURNING *;"
  Returning results -> " RETURNING"
    <+> renderCommaSeparated (renderAliased renderExpression) results <> ";"

data ConflictClause params columns where
  Conflict :: ConflictClause params columns
  OnConflictDoNothing :: ConflictClause params columns
  OnConflictDoUpdate
    :: NP (Aliased (UpdateExpression params '[table ::: columns])) columns
    -> Condition params '[table ::: columns] 'Ungrouped
    -> ConflictClause params columns

renderConflictClause
  :: SListI columns
  => ConflictClause params columns
  -> ByteString
renderConflictClause = \case
  Conflict -> ""
  OnConflictDoNothing -> " ON CONFLICT DO NOTHING"
  OnConflictDoUpdate updates wh
    -> " ON CONFLICT DO UPDATE SET"
      <+> renderCommaSeparatedMaybe renderUpdateExpression updates
      <+> "WHERE" <+> renderExpression wh

{-----------------------------------------
UPDATE statements
-----------------------------------------}

data UpdateExpression params table ty
  = Same
  | Set (Expression params table 'Ungrouped ty)

renderUpdateExpression
  :: Aliased (UpdateExpression params '[table ::: columns]) column
  -> Maybe ByteString
renderUpdateExpression = \case
  Same `As` _ -> Nothing
  Set expression `As` column -> Just $
    renderAlias column <+> "=" <+> renderExpression expression

update
  :: (HasTable table schema columns, SListI columns, SListI results)
  => Alias table
  -> NP (Aliased (UpdateExpression params '[table ::: columns])) columns
  -> Condition params '[table ::: columns] 'Ungrouped
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

renderSymbol :: KnownSymbol str => Proxy# str -> ByteString
renderSymbol p = fromString $ symbolVal' p

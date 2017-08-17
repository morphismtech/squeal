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

module Squeal.PostgreSQL.Statement where

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

import Squeal.PostgreSQL.Schema

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

class (PGTyped (BaseType ty), KnownNat n)
  => HasParameter (n :: Nat) params ty | n params -> ty where
    param :: proxy n -> Expression params tables grouping ty
    param _ = UnsafeExpression $ parenthesized $
      "$" <> renderNat (Proxy @n) <+> "::"
        <+> renderTypeExpression (pgtype @(BaseType ty))
instance {-# OVERLAPPING #-} PGTyped (BaseType ty1)
  => HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

class KnownSymbol column => HasColumn column columns ty
  | column columns -> ty where
    getColumn
      :: Alias column
      -> Expression params '[table ::: columns] 'Ungrouped ty
    getColumn column = UnsafeExpression $ renderAlias column
instance {-# OVERLAPPING #-} KnownSymbol column
  => HasColumn column ((column ::: optionality ty) ': tys) ('Required ty)
instance {-# OVERLAPPABLE #-} (KnownSymbol column, HasColumn column table ty)
  => HasColumn column (ty' ': table) ty

instance (HasColumn column columns ty, tables ~ '[table ::: columns])
  => IsLabel column (Expression params tables 'Ungrouped ty) where
    fromLabel = getColumn (Alias @column)

instance (HasTable table tables columns, HasColumn column columns ty)
  => IsTableColumn table column (Expression params tables 'Ungrouped ty) where
    table ! column = UnsafeExpression $
      renderAlias table <> "." <> renderAlias column

def :: Expression params '[] 'Ungrouped ('Optional (nullity ty))
def = UnsafeExpression "DEFAULT"

unDef
  :: Expression params '[] 'Ungrouped ('Required (nullity ty))
  -> Expression params '[] 'Ungrouped ('Optional (nullity ty))
unDef = UnsafeExpression . renderExpression

null :: Expression params tables grouping (optionality ('Null ty))
null = UnsafeExpression "NULL"

unNull
  :: Expression params tables grouping (optionality ('NotNull ty))
  -> Expression params tables grouping (optionality ('Null ty))
unNull = UnsafeExpression . renderExpression

coalesce
  :: [Expression params tables grouping ('Required ('Null ty))]
  -> Expression params tables grouping ('Required ('NotNull ty))
  -> Expression params tables grouping ('Required ('NotNull ty))
coalesce nullxs notNullx = UnsafeExpression $
  "COALESCE" <> parenthesized (commaSeparated
    ((renderExpression <$> nullxs) <> [renderExpression notNullx]))

fromNull
  :: Expression params tables grouping ('Required ('NotNull ty))
  -> Expression params tables grouping ('Required ('Null ty))
  -> Expression params tables grouping ('Required ('NotNull ty))
fromNull notNullx nullx = coalesce [nullx] notNullx

isNull
  :: Expression params tables grouping ('Required ('Null ty))
  -> Condition params tables grouping
isNull x = UnsafeExpression $ renderExpression x <+> "IS NULL"

isn'tNull
  :: Expression params tables grouping ('Required ('Null ty))
  -> Condition params tables grouping
isn'tNull x = UnsafeExpression $ renderExpression x <+> "IS NOT NULL"

matchNull
  :: Expression params tables grouping ('Required nullty)
  -> ( Expression params tables grouping ('Required ('NotNull ty))
       -> Expression params tables grouping ('Required nullty) )
  -> Expression params tables grouping ('Required ('Null ty))
  -> Expression params tables grouping ('Required nullty)
matchNull y f x = ifThenElse (isNull x) y
  (f (UnsafeExpression (renderExpression y)))

nullIf
  :: Expression params tables grouping ('Required ('NotNull ty))
  -> Expression params tables grouping ('Required ('NotNull ty))
  -> Expression params tables grouping ('Required ('Null ty))
nullIf x y = UnsafeExpression $ parenthesized $
  renderExpression x <> ", " <> renderExpression y

greatest
  :: [Expression params tables grouping ('Required nullty)]
  -> Expression params table grouping ('Required nullty)
greatest xs = UnsafeExpression $ "GREATEST("
  <> commaSeparated (renderExpression <$> xs) <> ")"

least
  :: [Expression params tables grouping ('Required nullty)]
  -> Expression params table grouping ('Required nullty)
least xs = UnsafeExpression $ "LEAST("
  <> commaSeparated (renderExpression <$> xs) <> ")"

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
      . (<> if decimal (Proxy :: Proxy ty) then "." else "")
      . fromString
      . show

instance PGFloating ty => Fractional
  (Expression params tables grouping ('Required (nullity ty))) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (PGFloating ty, PGTyped ty) => Floating
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
          -> Expression params tables grouping ('Required (nullity 'PGnumeric))
        logBaseNumeric b' y' = UnsafeExpression $ mconcat
          [ "log("
          , renderExpression (cast numeric b')
          , ", "
          , renderExpression (cast numeric y')
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

cast
  :: TypeExpression ('Required ('Null ty1))
  -> Expression params tables grouping ('Required (nullity ty0))
  -> Expression params tables grouping ('Required (nullity ty1))
cast ty x = UnsafeExpression $ parenthesized $
  renderExpression x <+> "::" <+> renderTypeExpression ty

div
  :: PGIntegral int
  => Expression params tables grouping ('Required (nullity int))
  -> Expression params tables grouping ('Required (nullity int))
  -> Expression params tables grouping ('Required (nullity int))
div = unsafeBinaryOp "/"

mod
  :: PGIntegral int
  => Expression params tables grouping ('Required (nullity int))
  -> Expression params tables grouping ('Required (nullity int))
  -> Expression params tables grouping ('Required (nullity int))
mod = unsafeBinaryOp "%"

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

(<*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGbool))
(<*) = unsafeBinaryOp "<"
infix 4 <*

(<=*)
  :: Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity ty))
  -> Expression params tables grouping ('Required (nullity 'PGbool))
(<=*) = unsafeBinaryOp "<="
infix 4 <=*

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
  (tables :: [(Symbol,[(Symbol,ColumnType)])])
  (tabcol :: (Symbol,Symbol)) where
  By
    :: (tables ~ '[table ::: columns], HasColumn column columns ty)
    => Alias column
    -> By tables '(table, column)
  By2
    :: (HasTable table tables columns, HasColumn column columns ty)
    => (Alias table, Alias column)
    -> By tables '(table, column)

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
  ( tables ~ '[table ::: columns]
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

unsafeAggregate
  :: ByteString
  -> Expression params tables 'Ungrouped ('Required xty)
  -> Expression params tables ('Grouped bys) ('Required yty)
unsafeAggregate fun x = UnsafeExpression $ mconcat
  [fun, "(", renderExpression x, ")"]

sum_
  :: PGNum ty
  => Expression params tables 'Ungrouped ('Required (nullity ty))
  -> Expression params tables ('Grouped bys) ('Required (nullity ty))
sum_ = unsafeAggregate "sum"

avgInt
  :: PGIntegral int
  => Expression params tables 'Ungrouped ('Required (nullity int))
  -> Expression params tables ('Grouped bys) ('Required (nullity 'PGnumeric))
avgInt = unsafeAggregate "avg"

avgFloat
  :: PGFloating float
  => Expression params tables 'Ungrouped ('Required (nullity float))
  -> Expression params tables ('Grouped bys) ('Required (nullity 'PGfloat8))
avgFloat = unsafeAggregate "avg"

bitAnd
  :: PGIntegral int
  => Expression params tables 'Ungrouped ('Required (nullity int))
  -> Expression params tables ('Grouped bys) ('Required (nullity int))
bitAnd = unsafeAggregate "bit_and"

bitOr
  :: PGIntegral int
  => Expression params tables 'Ungrouped ('Required (nullity int))
  -> Expression params tables ('Grouped bys) ('Required (nullity int))
bitOr = unsafeAggregate "bit_or"

boolAnd
  :: Expression params tables 'Ungrouped ('Required (nullity 'PGbool))
  -> Expression params tables ('Grouped bys) ('Required (nullity 'PGbool))
boolAnd = unsafeAggregate "bool_and"

boolOr
  :: Expression params tables 'Ungrouped ('Required (nullity 'PGbool))
  -> Expression params tables ('Grouped bys) ('Required (nullity 'PGbool))
boolOr = unsafeAggregate "bool_or"

countStar
  :: Expression params table ('Grouped bys) ('Required ('NotNull 'PGint8))
countStar = UnsafeExpression "count(*)"

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
  "jsonb_object_agg(" <> renderExpression k
    <> ", " <> renderExpression v <> ")"

max_
  :: Expression params tables 'Ungrouped ('Required (nullity ty))
  -> Expression params tables ('Grouped bys) ('Required (nullity ty))
max_ = unsafeAggregate "max"

min_
  :: Expression params tables 'Ungrouped ('Required (nullity ty))
  -> Expression params tables ('Grouped bys) ('Required (nullity ty))
min_ = unsafeAggregate "min"

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
  Subquery selection -> renderAliased renderQuery selection
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
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (tables :: [(Symbol,[(Symbol,ColumnType)])])
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
  deriving (Show,Eq)

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
  :: KnownNat n
  => proxy n
  -> TypeExpression ('Required ('Null ('PGchar n)))
char p = UnsafeTypeExpression $ "char(" <> renderNat p <> ")"
character
  :: KnownNat n
  => proxy n
  -> TypeExpression ('Required ('Null ('PGchar n)))
character p = UnsafeTypeExpression $  "character(" <> renderNat p <> ")"
varchar
  :: KnownNat n
  => proxy n
  -> TypeExpression ('Required ('Null ('PGvarchar n)))
varchar p = UnsafeTypeExpression $ "varchar(" <> renderNat p <> ")"
characterVarying
  :: KnownNat n
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
instance KnownNat n => PGTyped ('PGchar n) where pgtype = char (Proxy @n)
instance KnownNat n => PGTyped ('PGvarchar n) where pgtype = varchar (Proxy @n)
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

selectStar
  :: tables ~ '[table ::: columns]
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

data TableConstraint
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (columns :: [(Symbol,ColumnType)])
  = UnsafeTableConstraint { renderTableConstraint :: ByteString }

check
  :: Condition '[] '[table ::: columns] 'Ungrouped
  -> TableConstraint schema columns
check condition = UnsafeTableConstraint $
  "CHECK" <+> parenthesized (renderExpression condition)

data Column
  (columns :: [(Symbol,ColumnType)])
  (columnty :: (Symbol,ColumnType))
    where
      Column
        :: HasColumn column columns ty
        => Alias column
        -> Column columns (column ::: ty)

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

renderOnDelete :: OnDelete -> ByteString
renderOnDelete = \case
  OnDeleteNoAction -> "ON DELETE NO ACTION"
  OnDeleteRestrict -> "ON DELETE RESTRICT"
  OnDeleteCascade -> "ON DELETE CASCADE"

data OnUpdate
  = OnUpdateNoAction
  | OnUpdateRestrict
  | OnUpdateCascade

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
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (params :: [ColumnType])
  (columns :: [(Symbol,ColumnType)])
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
  (columns :: [(Symbol,ColumnType)])
  (results :: [(Symbol,ColumnType)])
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

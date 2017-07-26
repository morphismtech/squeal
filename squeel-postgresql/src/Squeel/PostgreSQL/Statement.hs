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
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeel.PostgreSQL.Statement where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.String
import Generics.SOP
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits
import Prelude hiding (RealFrac(..))

import qualified Data.ByteString as ByteString

import Squeel.PostgreSQL.Schema

{-----------------------------------------
column expressions
-----------------------------------------}

newtype Expression
  (params :: [ColumnType])
  (tables :: [(Symbol,[(Symbol,ColumnType)])])
  (ty :: ColumnType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (Show,Eq)

class KnownNat n => HasParameter (n :: Nat) params ty | n params -> ty where
  param :: Proxy# n -> Expression params tables ty
  param p = UnsafeExpression $ ("$" <>) $ fromString $ show $ natVal' p
instance {-# OVERLAPPING #-} HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

param1 :: Expression (ty1:tys) tables ty1
param1 = param (proxy# :: Proxy# 1)
param2 :: Expression (ty1:ty2:tys) tables ty2
param2 = param (proxy# :: Proxy# 2)
param3 :: Expression (ty1:ty2:ty3:tys) tables ty3
param3 = param (proxy# :: Proxy# 3)
param4 :: Expression (ty1:ty2:ty3:ty4:tys) tables ty4
param4 = param (proxy# :: Proxy# 4)
param5 :: Expression (ty1:ty2:ty3:ty4:ty5:tys) tables ty5
param5 = param (proxy# :: Proxy# 5)

class KnownSymbol column => HasColumn column columns ty
  | column columns -> ty where
    getColumn :: Proxy# column -> Expression params '[table ::: columns] ty
    getColumn column = UnsafeExpression $ fromString $ symbolVal' column
instance {-# OVERLAPPING #-} KnownSymbol column
  => HasColumn column ((column ::: optionality ty) ': tys) ('Required ty)
instance {-# OVERLAPPABLE #-} (KnownSymbol column, HasColumn column table ty)
  => HasColumn column (ty' ': table) ty

instance (HasColumn column columns ty, tables ~ '[table ::: columns])
  => IsLabel column (Expression params tables ty) where
    fromLabel = getColumn

(&.)
  :: (HasTable table tables columns, HasColumn column columns ty)
  => Alias table -> Alias column -> Expression params tables ty
table &. column = UnsafeExpression $
  renderAlias table <> "." <> renderAlias column

def :: Expression params '[] ('Optional (nullity ty))
def = UnsafeExpression "DEFAULT"

notDef
  :: Expression params '[] ('Required (nullity ty))
  -> Expression params '[] ('Optional (nullity ty))
notDef = UnsafeExpression . renderExpression

null :: Expression params tables (optionality ('Null ty))
null = UnsafeExpression "NULL"

coalesce
  :: [Expression params tables ('Required ('Null x))]
  -> Expression params tables ('Required ('NotNull x))
  -> Expression params tables ('Required ('NotNull x))
coalesce nulls isn'tNull = UnsafeExpression $ mconcat
  [ "COALESCE("
  , ByteString.intercalate ", " $ map renderExpression nulls
  , ", ", renderExpression isn'tNull
  , ")"
  ]

unsafeBinaryOp
  :: ByteString
  -> Expression params tables ty0
  -> Expression params tables ty1
  -> Expression params tables ty2
unsafeBinaryOp op x y = UnsafeExpression $ mconcat
  ["(", renderExpression x, " ", op, " ", renderExpression y, ")"]

unsafeUnaryOp
  :: ByteString
  -> Expression params tables ty0
  -> Expression params tables ty1
unsafeUnaryOp op x = UnsafeExpression $ mconcat
  ["(", op, " ", renderExpression x, ")"]

unsafeFunction
  :: ByteString
  -> Expression params tables xty
  -> Expression params tables yty
unsafeFunction fun x = UnsafeExpression $ mconcat
  [fun, "(", renderExpression x, ")"]

instance PGNum ty
  => Num (Expression params tables ('Required (nullity ty))) where
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

instance PGFractional ty
  => Fractional (Expression params tables ('Required (nullity ty))) where
    (/) = unsafeBinaryOp "/"
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance (PGFloating ty, PGCast 'PGNumeric ty, PGTyped ty)
  => Floating (Expression params tables ('Required (nullity ty))) where
    pi = UnsafeExpression "pi()"
    exp = unsafeFunction "exp"
    log = unsafeFunction "ln"
    sqrt = unsafeFunction "sqrt"
    b ** x = UnsafeExpression $
      "power(" <> renderExpression b <> ", " <> renderExpression x <> ")"
    logBase b y = cast pgtype $ logBaseNumeric b y
      where
        logBaseNumeric
          :: Expression params tables ('Required (nullity ty))
          -> Expression params tables ('Required (nullity ty))
          -> Expression params tables ('Required (nullity 'PGNumeric))
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
  => Expression params tables ('Required (nullity float))
  -> Expression params tables ('Required (nullity float))
  -> Expression params tables ('Required (nullity float))
atan2 y x = UnsafeExpression $
  "atan2(" <> renderExpression y <> ", " <> renderExpression x <> ")"

class PGCast (ty0 :: PGType) (ty1 :: PGType) where
  cast
    :: TypeExpression ('Required ('Null ty1))
    -> Expression params tables ('Required (nullity ty0))
    -> Expression params tables ('Required (nullity ty1))
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
  => Expression params tables ('Required (nullity ty))
  -> Expression params tables ('Required (nullity ty))
  -> Expression params tables ('Required (nullity ty))
div = unsafeBinaryOp "/"

mod
  :: (PGNum ty, PGTyped ty, PGCast 'PGInt8 ty, PGCast ty 'PGInt8)
  => Expression params tables ('Required (nullity ty))
  -> Expression params tables ('Required (nullity ty))
  -> Expression params tables ('Required (nullity ty))
mod = unsafeBinaryOp "%"

truncate
  :: (PGFractional frac, PGCast 'PGInt4 int, PGTyped int)
  => Expression params tables ('Required (nullity frac))
  -> Expression params tables ('Required (nullity int))
truncate = cast pgtype . truncate'
  where
    truncate'
      :: Expression params tables ('Required (nullity frac))
      -> Expression params tables ('Required (nullity 'PGInt4))
    truncate' = unsafeFunction "trunc"

round
  :: (PGFractional frac, PGCast 'PGInt4 int, PGTyped int)
  => Expression params tables ('Required (nullity frac))
  -> Expression params tables ('Required (nullity int))
round = cast pgtype . round'
  where
    round'
      :: Expression params tables ('Required (nullity frac))
      -> Expression params tables ('Required (nullity 'PGInt4))
    round' = unsafeFunction "round"

ceiling
  :: (PGFractional frac, PGCast 'PGInt4 int, PGTyped int)
  => Expression params tables ('Required (nullity frac))
  -> Expression params tables ('Required (nullity int))
ceiling = cast pgtype . ceiling'
  where
    ceiling'
      :: Expression params tables ('Required (nullity frac))
      -> Expression params tables ('Required (nullity 'PGInt4))
    ceiling' = unsafeFunction "ceiling"

instance IsString (Expression params tables ('Required (nullity 'PGText))) where
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

type Predicate params tables = Expression params tables ('Required ('NotNull 'PGBool))

true :: Predicate params tables
true = UnsafeExpression "TRUE"

false :: Predicate params tables
false = UnsafeExpression "FALSE"

notB
  :: Predicate params tables
  -> Predicate params tables
notB = unsafeUnaryOp "NOT"

(&&*)
  :: Predicate params tables
  -> Predicate params tables
  -> Predicate params tables
(&&*) = unsafeBinaryOp "AND"

(||*)
  :: Predicate params tables
  -> Predicate params tables
  -> Predicate params tables
(||*) = unsafeBinaryOp "OR"

caseWhenThenElse
  :: [(Predicate params tables, Expression params tables ty)]
  -> Expression params tables ty
  -> Expression params tables ty
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
  :: Predicate params tables
  -> Expression params tables ty
  -> Expression params tables ty
  -> Expression params tables ty
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

(==*)
  :: Expression params tables ty
  -> Expression params tables ty
  -> Predicate params tables
(==*) = unsafeBinaryOp "="

(/=*)
  :: Expression params tables ty
  -> Expression params tables ty
  -> Predicate params tables
(/=*) = unsafeBinaryOp "<>"

(>=*)
  :: Expression params tables ty
  -> Expression params tables ty
  -> Predicate params tables
(>=*) = unsafeBinaryOp ">="

(<*)
  :: Expression params tables ty
  -> Expression params tables ty
  -> Predicate params tables
(<*) = unsafeBinaryOp "<"

(<=*)
  :: Expression params tables ty
  -> Expression params tables ty
  -> Predicate params tables
(<=*) = unsafeBinaryOp "<="

(>*)
  :: Expression params tables ty
  -> Expression params tables ty
  -> Predicate params tables
(>*) = unsafeBinaryOp ">"

{-----------------------------------------
tables
-----------------------------------------}

newtype Table
  (params :: [ColumnType])
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (columns :: [(Symbol,ColumnType)])
    = UnsafeTable { renderTable :: ByteString }
    deriving (Show,Eq)

class KnownSymbol table => HasTable table tables columns
  | table tables -> columns where
    getTable :: Proxy# table -> Table params tables columns
    getTable table = UnsafeTable $ fromString $ symbolVal' table
instance {-# OVERLAPPING #-} KnownSymbol table
  => HasTable table ((table ::: columns) ': tables) columns
instance {-# OVERLAPPABLE #-}
  (KnownSymbol table, HasTable table schema columns)
    => HasTable table (table' ': schema) columns

instance HasTable table schema columns
  => IsLabel table (Table params schema columns) where
    fromLabel = getTable

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
  :: TypeExpression ('Required ('Null ty))
  -> TypeExpression ('Required ('NotNull ty))
notNull ty = UnsafeTypeExpression $ renderTypeExpression ty <> " NOT NULL"
default_
  :: Expression '[] '[] ('Required ty)
  -> TypeExpression ('Required ty)
  -> TypeExpression ('Optional ty)
default_ x ty = UnsafeTypeExpression $
  renderTypeExpression ty <> " DEFAULT " <> renderExpression x

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

newtype Statement
  (params :: [ColumnType])
  (columns :: [(Symbol,ColumnType)])
  (schema0 :: [(Symbol,[(Symbol,ColumnType)])])
  (schema1 :: [(Symbol,[(Symbol,ColumnType)])])
    = UnsafeStatement { renderStatement :: ByteString }
    deriving (Show,Eq)

-- newtype PreparedStatement
--   (params :: [ColumnType])
--   (columns :: [(Symbol,ColumnType)])
--   (schema0 :: [(Symbol,[(Symbol,ColumnType)])])
--   (schema1 :: [(Symbol,[(Symbol,ColumnType)])])
--     = UnsafePreparedStatement { renderPreparedStatement :: ByteString }
--     deriving (Show,Eq)

{-----------------------------------------
SELECT statements
-----------------------------------------}

data Join params schema tables where
  Table
    :: Aliased (Table params schema) table
    -> Join params schema '[table]
  Subselect
    :: Aliased (Selection params schema) table
    -> Join params schema '[table]
  Cross
    :: Aliased (Table params schema) table
    -> Join params schema tables
    -> Join params schema (table ': tables)
  Inner
    :: Aliased (Table params schema) table
    -> Predicate params (table ': tables)
    -> Join params schema tables
    -> Join params schema (table ': tables)
  LeftOuter
    :: Aliased (Table params schema) right
    -> Predicate params '[left,right]
    -> Join params schema (tables)
    -> Join params schema (NullifyTable right ': left ': tables)
  RightOuter
    :: Aliased (Table params schema) right
    -> Predicate params '[left,right]
    -> Join params schema (left : tables)
    -> Join params schema (right ': NullifyTable left ': tables)
  FullOuter
    :: Aliased (Table params schema) right
    -> Predicate params '[left,right]
    -> Join params schema (left : tables)
    -> Join params schema
        (NullifyTable right ': NullifyTable left ': tables)

renderJoin :: Join params schema tables -> ByteString
renderJoin = \case
  Table table -> renderAliased renderTable table
  Subselect selection -> "SELECT " <> renderAliased renderSelection selection
  Cross table tables -> mconcat
    [ renderJoin tables
    , " CROSS JOIN "
    , renderAliased renderTable table
    ]
  Inner table on tables -> mconcat
    [ renderJoin tables
    , " INNER JOIN "
    , renderAliased renderTable table
    , " ON "
    , renderExpression on
    ]
  LeftOuter table on tables -> mconcat
    [ renderJoin tables
    , " LEFT OUTER JOIN "
    , renderAliased renderTable table
    , " ON "
    , renderExpression on
    ]
  RightOuter table on tables -> mconcat
    [ renderJoin tables
    , " RIGHT OUTER JOIN "
    , renderAliased renderTable table
    , " ON "
    , renderExpression on
    ]
  FullOuter table on tables -> mconcat
    [ renderJoin tables
    , " FULL OUTER JOIN "
    , renderAliased renderTable table
    , " ON "
    , renderExpression on
    ]

instance HasTable table schema columns
  => IsLabel table (Join params schema '[table ::: columns]) where
    fromLabel p = Table $ fromLabel p

data TableExpression
  (params :: [ColumnType])
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (tables :: [(Symbol,[(Symbol,ColumnType)])])
    = TableExpression
    { joinClause :: Join params schema tables
    , whereClause :: [Predicate params tables]
    , limitClause :: [Expression params '[] ('Required ('NotNull 'PGInt8))]
    , offsetClause :: [Expression params '[] ('Required ('NotNull 'PGInt8))]
    }

renderTableExpression :: TableExpression params schema tables -> ByteString
renderTableExpression (TableExpression tables whs' lims' offs') = mconcat
  [ renderJoin tables
  , renderWheres whs'
  , renderLimits lims'
  , renderOffsets offs'
  ] where
  renderWheres = \case
    [] -> ""
    wh:[] -> " WHERE " <> renderExpression wh
    wh:whs -> " WHERE " <> renderExpression (foldr (&&*) wh whs)
  renderLimits = \case
    [] -> ""
    lim:[] -> " LIMIT " <> renderExpression lim
    lims -> mconcat
      [ " LIMIT LEAST("
      , ByteString.intercalate ", " (renderExpression <$> reverse lims)
      , ")"
      ]
  renderOffsets = \case
    [] -> ""
    off:[] -> " OFFSET " <> renderExpression off
    off:offs -> " OFFSET " <> renderExpression (foldr (+) off offs)

instance (HasTable table schema columns, table ~ table')
  => IsLabel table (TableExpression params schema '[table' ::: columns]) where
    fromLabel p = join (fromLabel p)

join
  :: Join params schema tables
  -> TableExpression params schema tables
join tables = TableExpression tables [] [] []

where_
  :: Predicate params tables
  -> TableExpression params schema tables
  -> TableExpression params schema tables
where_ wh (TableExpression tabs whs lims offs) =
  TableExpression tabs (wh:whs) lims offs

limit
  :: Expression params '[] ('Required ('NotNull 'PGInt8))
  -> TableExpression params schema tables
  -> TableExpression params schema tables
limit lim (TableExpression tabs whs lims offs) =
  TableExpression tabs whs (lim:lims) offs

offset
  :: Expression params '[] ('Required ('NotNull 'PGInt8))
  -> TableExpression params schema tables
  -> TableExpression params schema tables
offset off (TableExpression tabs whs lims offs) =
  TableExpression tabs whs lims (off:offs)

newtype Selection
  (params :: [ColumnType])
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (columns :: [(Symbol,ColumnType)])
    = UnsafeSelection { renderSelection :: ByteString }
    deriving (Show,Eq)

starFrom
  :: tables ~ '[table ::: columns]
  => TableExpression params schema tables
  -> Selection params schema columns
starFrom tabs = UnsafeSelection $ "* FROM " <> renderTableExpression tabs

dotStarFrom
  :: HasTable table tables columns
  => Alias table
  -> TableExpression params schema tables
  -> Selection params schema columns
table `dotStarFrom` tables = UnsafeSelection $
  renderAlias table <> ".* FROM " <> renderTableExpression tables

from
  :: SListI columns
  => NP (Aliased (Expression params tables)) columns
  -> TableExpression params schema tables
  -> Selection params schema columns
list `from` tabs = UnsafeSelection $
  renderList list <> " FROM " <> renderTableExpression tabs
  where
    renderList
      = ByteString.intercalate ", "
      . hcollapse
      . hmap (K . renderAliased renderExpression)

select
  :: Selection params schema columns
  -> Statement params columns schema schema
select = UnsafeStatement . ("SELECT " <>) . (<> ";") . renderSelection

subselect
  :: Aliased (Selection params schema) table
  -> TableExpression params schema '[table]
subselect selection = join (Subselect selection)

{-----------------------------------------
CREATE statements
-----------------------------------------}

createTable
  :: (KnownSymbol table, SListI columns)
  => Alias table
  -> NP (Aliased TypeExpression) columns
  -> Statement '[] '[] schema (Create table columns schema)
createTable table columns = UnsafeStatement $ mconcat
  [ "CREATE TABLE "
  , renderAlias table
  , " ("
  , ByteString.intercalate ", " . hcollapse $
      hmap (K . renderColumn) columns
  , ");"
  ]
  where
    renderColumn :: Aliased TypeExpression x -> ByteString
    renderColumn (ty `As` column) =
      renderAlias column <> " " <> renderTypeExpression ty

{-----------------------------------------
DROP statements
-----------------------------------------}

dropTable
  :: KnownSymbol table
  => Alias table
  -> Statement '[] '[] schema (Drop table schema)
dropTable table = UnsafeStatement $
  "DROP TABLE " <> renderAlias table <> ";"

{-----------------------------------------
ALTER statements
-----------------------------------------}

alterTable
  :: HasTable table schema columns0
  => Alias table
  -> AlterTable columns0 columns1
  -> Statement '[] '[] schema (Alter table schema columns1)
alterTable table alteration = UnsafeStatement $
  "ALTER TABLE "
  <> renderAlias table
  <> " "
  <> renderAlterTable alteration
  <> ";"

alterTableRename
  :: (KnownSymbol table0, KnownSymbol table1)
  => Alias table0
  -> Alias table1
  -> Statement '[] '[] schema (Rename table0 table1 schema)
alterTableRename table0 table1 = UnsafeStatement $
  "ALTER TABLE "
  <> renderAlias table0
  <> " RENAME TO "
  <> renderAlias table1
  <> ";"

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
  "ADD COLUMN "
  <> renderAlias column
  <> " "
  <> renderTypeExpression ty

addColumnNull
  :: KnownSymbol column
  => Alias column
  -> TypeExpression ('Required ('Null ty))
  -> AlterTable columns ((column ::: 'Required ('Null ty)) ': columns)
addColumnNull column ty = UnsafeAlterTable $
  "ADD COLUMN "
  <> renderAlias column
  <> " "
  <> renderTypeExpression ty

dropColumn
  :: KnownSymbol column
  => Alias column
  -> AlterTable columns (Drop column columns)
dropColumn column = UnsafeAlterTable $
  "DROP COLUMN " <> renderAlias column

renameColumn
  :: (KnownSymbol column0, KnownSymbol column1)
  => Alias column0
  -> Alias column1
  -> AlterTable columns (Rename column0 column1 columns)
renameColumn column0 column1 = UnsafeAlterTable $
  "RENAME COLUMN " <> renderAlias column0
  <> " TO " <> renderAlias column1

alterColumn
  :: (KnownSymbol column, HasColumn column columns ty0)
  => Alias column
  -> AlterColumn ty0 ty1
  -> AlterTable columns (Alter column columns ty1)
alterColumn column alteration = UnsafeAlterTable $
  "ALTER COLUMN " <> renderAlias column
  <> " " <> renderAlterColumn alteration

data AlterColumn (ty0 :: ColumnType) (ty1 :: ColumnType) =
  UnsafeAlterColumn {renderAlterColumn :: ByteString}

setDefault
  :: Expression '[] '[] ('Required ty)
  -> AlterColumn ('Required ty) ('Optional ty)
setDefault expression = UnsafeAlterColumn $
  "SET DEFAULT " <> renderExpression expression

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

insertInto
  :: (SListI columns, HasTable table schema columns)
  => Alias table
  -> NP (Aliased (Expression params '[])) columns
  -> Statement params '[] schema schema
insertInto table expressions = UnsafeStatement $ "INSERT INTO "
  <> renderAlias table
  <> " (" <> ByteString.intercalate ", " aliases
  <> ") VALUES ("
  <> ByteString.intercalate ", " values
  <> ");"
  where
    aliases = hcollapse $ hmap
      (\ (_ `As` name) -> K (renderAlias name))
      expressions
    values = hcollapse $ hmap
      (\ (expression `As` _) -> K (renderExpression expression))
      expressions

insertIntoReturning
  :: (SListI columns, SListI results, HasTable table schema columns)
  => Alias table
  -> NP (Aliased (Expression params '[])) columns
  -> NP (Aliased (Expression params '[table ::: columns])) results
  -> Statement params results schema schema
insertIntoReturning table inserts results
  = UnsafeStatement . mconcat $
    [ "INSERT INTO "
    , renderAlias table
    , " (" <> ByteString.intercalate ", " aliases
    , ") VALUES ("
    , ByteString.intercalate ", " values
    , ") RETURNING "
    , renderList results
    , ";"
    ]
  where
    aliases = hcollapse $ hmap
      (\ (_ `As` name) -> K (renderAlias name))
      inserts
    values = hcollapse $ hmap
      (\ (expression `As` _) -> K (renderExpression expression))
      inserts
    renderList
      = ByteString.intercalate ", "
      . hcollapse
      . hmap (K . renderAliased renderExpression)

{-----------------------------------------
UPDATE statements
-----------------------------------------}

data UpdateExpression params table ty
  = Same
  | Set (Expression params table ty)

renderUpdateExpression
  :: Aliased (UpdateExpression params '[table ::: columns]) column
  -> Maybe ByteString
renderUpdateExpression = \case
  Same `As` _ -> Nothing
  Set expression `As` column -> Just $ mconcat
    [ renderAlias column
    , " = "
    , renderExpression expression
    ]

update
  :: (HasTable table schema columns, SListI columns)
  => Alias table
  -> NP (Aliased (UpdateExpression params '[table ::: columns])) columns
  -> Predicate params '[table ::: columns]
  -> Statement params '[] schema schema
update table columns wh = UnsafeStatement $ mconcat
  [ "UPDATE "
  , renderAlias table
  , " SET "
  , ByteString.intercalate ", " . catMaybes . hcollapse $
      hmap (K . renderUpdateExpression) columns
  , " WHERE ", renderExpression wh, ";"
  ]

updateReturning
  :: (SListI columns, SListI results, HasTable table schema columns)
  => Alias table
  -> NP (Aliased (UpdateExpression params '[table ::: columns])) columns
  -> Predicate params '[table ::: columns]
  -> NP (Aliased (Expression params '[table ::: columns])) results
  -> Statement params results schema schema
updateReturning table updates wh results = UnsafeStatement $ mconcat
  [ "UPDATE "
  , renderAlias table
  , " SET "
  , ByteString.intercalate ", " . catMaybes . hcollapse $
      hmap (K . renderUpdateExpression) updates
  , " WHERE ", renderExpression wh
  , " RETURNING ", renderList results
  , ";"
  ]
  where
    renderList
      = ByteString.intercalate ", "
      . hcollapse
      . hmap (K . renderAliased renderExpression)

upsertInto
  :: (SListI columns, HasTable table schema columns)
  => Alias table
  -> NP (Aliased (Expression params '[])) columns
  -> NP (Aliased (UpdateExpression params '[table ::: columns])) columns
  -> Predicate params '[table ::: columns]
  -> Statement params '[] schema schema
upsertInto table inserts updates wh = UnsafeStatement . mconcat $
  [ "INSERT INTO "
  , renderAlias table
  , " (" <> ByteString.intercalate ", " aliases
  , ") VALUES ("
  , ByteString.intercalate ", " values
  , ") ON CONFLICT UPDATE "
  , renderAlias table
  , " SET "
  , ByteString.intercalate ", " . catMaybes . hcollapse $
      hmap (K . renderUpdateExpression) updates
  , " WHERE ", renderExpression wh, ";"
  ] where
    aliases = hcollapse $ hmap
      (\ (_ `As` name) -> K (renderAlias name))
      inserts
    values = hcollapse $ hmap
      (\ (expression `As` _) -> K (renderExpression expression))
      inserts

upsertIntoReturning
  :: (SListI columns, SListI results, HasTable table schema columns)
  => Alias table
  -> NP (Aliased (Expression params '[])) columns
  -> NP (Aliased (UpdateExpression params '[table ::: columns])) columns
  -> Predicate params '[table ::: columns]
  -> NP (Aliased (Expression params '[table ::: columns])) results
  -> Statement params results schema schema
upsertIntoReturning table inserts updates wh results =
  UnsafeStatement . mconcat $
    [ "INSERT INTO "
    , renderAlias table
    , " (" <> ByteString.intercalate ", " aliases
    , ") VALUES ("
    , ByteString.intercalate ", " values
    , ") ON CONFLICT UPDATE "
    , renderAlias table
    , " SET "
    , ByteString.intercalate ", " . catMaybes . hcollapse $
        hmap (K . renderUpdateExpression) updates
    , " WHERE ", renderExpression wh
    , " RETURNING ", renderList results
    , ";"
    ] where
      aliases = hcollapse $ hmap
        (\ (_ `As` name) -> K (renderAlias name))
        inserts
      values = hcollapse $ hmap
        (\ (expression `As` _) -> K (renderExpression expression))
        inserts
      renderList
        = ByteString.intercalate ", "
        . hcollapse
        . hmap (K . renderAliased renderExpression)

{-----------------------------------------
DELETE statements
-----------------------------------------}

deleteFrom
  :: HasTable table schema columns
  => Alias table
  -> Predicate params '[table ::: columns]
  -> Statement params '[] schema schema
deleteFrom table wh = UnsafeStatement $ mconcat
  [ "DELETE FROM "
  , renderAlias table
  , " WHERE ", renderExpression wh, ";"
  ]

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , MagicHash
  , OverloadedStrings
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeel.PostgreSQL.Statement where

import Control.Category
import Data.Boolean
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Monoid
import Data.String
import Generics.SOP
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits
import Prelude hiding (id,(.))

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
Alias table &. Alias column = UnsafeExpression $
  fromString (symbolVal' table)
  <> "." <>
  fromString (symbolVal' column)

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

instance PGNum ty => Num (Expression params tables ('Required (nullity ty))) where
  (+) = unsafeBinaryOp "+"
  (-) = unsafeBinaryOp "-"
  (*) = unsafeBinaryOp "*"
  abs = unsafeUnaryOp "@"
  signum = unsafeFunction "sign"
  fromInteger = UnsafeExpression . fromString . show

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

instance Boolean (Expression params tables ('Required ('NotNull 'PGBool))) where
  true = UnsafeExpression "TRUE"
  false = UnsafeExpression "FALSE"
  notB = unsafeUnaryOp "NOT"
  (&&*) = unsafeBinaryOp "AND"
  (||*) = unsafeBinaryOp "OR"

type instance BooleanOf (Expression params tables ty) =
  Expression params tables ('Required ('NotNull 'PGBool))

caseWhenThenElse
  :: [(Expression params tables ('Required ('NotNull 'PGBool)), Expression params tables ty)]
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

instance IfB (Expression params tables ty) where
  ifB if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

instance EqB (Expression params tables (optionality ('NotNull ty))) where
  (==*) = unsafeBinaryOp "="
  (/=*) = unsafeBinaryOp "<>"

instance OrdB (Expression params tables (optionality ('NotNull ty))) where
  (>*) = unsafeBinaryOp ">"
  (>=*) = unsafeBinaryOp ">="
  (<*) = unsafeBinaryOp "<"
  (<=*) = unsafeBinaryOp "<="

{-----------------------------------------
table expressions
-----------------------------------------}

newtype TableExpression
  (params :: [ColumnType])
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (columns :: [(Symbol,ColumnType)])
    = UnsafeTableExpression { renderTableExpression :: ByteString }
    deriving (Show,Eq)

class KnownSymbol table => HasTable table tables columns
  | table tables -> columns where
    getTable :: Proxy# table -> TableExpression params tables columns
    getTable table = UnsafeTableExpression $ fromString $ symbolVal' table
instance {-# OVERLAPPING #-} KnownSymbol table
  => HasTable table ((table ::: columns) ': tables) columns
instance {-# OVERLAPPABLE #-}
  (KnownSymbol table, HasTable table schema columns)
    => HasTable table (table' ': schema) columns

instance HasTable table schema columns
  => IsLabel table (TableExpression params schema columns) where
    fromLabel = getTable

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

instance Category (Statement '[] '[]) where
  id = UnsafeStatement ";"
  statement2 . statement1 = UnsafeStatement $
    renderStatement statement1 <> " " <> renderStatement statement2

newtype PreparedStatement
  (params :: [ColumnType])
  (columns :: [(Symbol,ColumnType)])
  (schema0 :: [(Symbol,[(Symbol,ColumnType)])])
  (schema1 :: [(Symbol,[(Symbol,ColumnType)])])
    = UnsafePreparedStatement { renderPreparedStatement :: ByteString }
    deriving (Show,Eq)

{-----------------------------------------
SELECT statements
-----------------------------------------}

data Join params schema tables where
  Table
    :: Aliased (TableExpression params schema) table
    -> Join params schema '[table]
  Subselect
    :: Aliased (Selection params schema) table
    -> Join params schema '[table]
  Cross
    :: Aliased (TableExpression params schema) table
    -> Join params schema tables
    -> Join params schema (table ': tables)
  Inner
    :: Aliased (TableExpression params schema) table
    -> Expression params (table ': tables) ('Required ('NotNull 'PGBool))
    -> Join params schema tables
    -> Join params schema (table ': tables)
  LeftOuter
    :: Aliased (TableExpression params schema) right
    -> Expression params '[left,right] ('Required ('NotNull 'PGBool))
    -> Join params schema (tables)
    -> Join params schema (NullifyTable right ': left ': tables)
  RightOuter
    :: Aliased (TableExpression params schema) right
    -> Expression params '[left,right] ('Required ('NotNull 'PGBool))
    -> Join params schema (left : tables)
    -> Join params schema (right ': NullifyTable left ': tables)
  FullOuter
    :: Aliased (TableExpression params schema) right
    -> Expression params '[left,right] ('Required ('NotNull 'PGBool))
    -> Join params schema (left : tables)
    -> Join params schema
        (NullifyTable right ': NullifyTable left ': tables)

renderJoin :: Join params schema tables -> ByteString
renderJoin = \case
  Table table -> renderAliased renderTableExpression table
  Subselect selection -> "SELECT " <> renderAliased renderSelection selection
  Cross table tables -> mconcat
    [ renderJoin tables
    , " CROSS JOIN "
    , renderAliased renderTableExpression table
    ]
  Inner table on tables -> mconcat
    [ renderJoin tables
    , " INNER JOIN "
    , renderAliased renderTableExpression table
    , " ON "
    , renderExpression on
    ]
  LeftOuter table on tables -> mconcat
    [ renderJoin tables
    , " LEFT OUTER JOIN "
    , renderAliased renderTableExpression table
    , " ON "
    , renderExpression on
    ]
  RightOuter table on tables -> mconcat
    [ renderJoin tables
    , " RIGHT OUTER JOIN "
    , renderAliased renderTableExpression table
    , " ON "
    , renderExpression on
    ]
  FullOuter table on tables -> mconcat
    [ renderJoin tables
    , " FULL OUTER JOIN "
    , renderAliased renderTableExpression table
    , " ON "
    , renderExpression on
    ]

instance HasTable table schema columns
  => IsLabel table (Join params schema '[table ::: columns]) where
    fromLabel p = Table $ fromLabel p

data Clauses params tables = Clauses
  { whereClause :: Maybe (Expression params tables ('Required ('NotNull 'PGBool)))
  , limitClause :: Maybe (Expression params '[] ('Required ('NotNull 'PGInt8)))
  , offsetClause :: Maybe (Expression params '[] ('Required ('NotNull 'PGInt8)))
  }

instance Monoid (Clauses params tables) where
  mempty = Clauses Nothing Nothing Nothing
  Clauses wh1 lim1 off1 `mappend` Clauses wh2 lim2 off2 = Clauses
    { whereClause = case (wh1,wh2) of
        (Nothing,Nothing) -> Nothing
        (Just w1,Nothing) -> Just w1
        (Nothing,Just w2) -> Just w2
        (Just w1,Just w2) -> Just (w1 &&* w2)
    , limitClause = case (lim1,lim2) of
        (Nothing,Nothing) -> Nothing
        (Just l1,Nothing) -> Just l1
        (Nothing,Just l2) -> Just l2
        (Just l1,Just l2) -> Just (l1 `minB` l2)
    , offsetClause = case (off1,off2) of
        (Nothing,Nothing) -> Nothing
        (Just o1,Nothing) -> Just o1
        (Nothing,Just o2) -> Just o2
        (Just o1,Just o2) -> Just (o1 + o2)
    }

data From
  (params :: [ColumnType])
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (tables :: [(Symbol,[(Symbol,ColumnType)])])
    = From
    { fromJoin :: Join params schema tables
    , fromClauses :: Clauses params tables
    }

join
  :: Join params schema tables
  -> From params schema tables
join tables = From tables mempty

renderFrom :: From params schema tables -> ByteString
renderFrom (From tref (Clauses wh lim off))= mconcat
  [ renderJoin tref
  , maybe "" ((" WHERE " <>) . renderExpression) wh
  , maybe "" ((" LIMIT " <>) . renderExpression) lim
  , maybe "" ((" OFFSET " <>) . renderExpression) off
  ]

instance (HasTable table schema columns, table ~ table')
  => IsLabel table (From params schema '[table' ::: columns]) where
    fromLabel p = From (fromLabel p) mempty

where_
  :: Expression params tables ('Required ('NotNull 'PGBool))
  -> From params schema tables
  -> From params schema tables
where_ wh (From tabs fromClauses1) = From tabs
  (fromClauses1 <> Clauses (Just wh) Nothing Nothing)

limit
  :: Expression params '[] ('Required ('NotNull 'PGInt8))
  -> From params schema tables
  -> From params schema tables
limit lim (From tabs fromClauses1) = From tabs
  (fromClauses1 <> Clauses Nothing (Just lim) Nothing)

offset
  :: Expression params '[] ('Required ('NotNull 'PGInt8))
  -> From params schema tables
  -> From params schema tables
offset off (From tabs fromClauses1) = From tabs
  (fromClauses1 <> Clauses Nothing Nothing (Just off))

newtype Selection
  (params :: [ColumnType])
  (schema :: [(Symbol,[(Symbol,ColumnType)])])
  (columns :: [(Symbol,ColumnType)])
    = UnsafeSelection { renderSelection :: ByteString }
    deriving (Show,Eq)

starFrom
  :: tables ~ '[table ::: columns]
  => From params schema tables
  -> Selection params schema columns
starFrom tabs = UnsafeSelection $ "* FROM " <> renderFrom tabs

dotStarFrom
  :: HasTable table tables columns
  => Alias table
  -> From params schema tables
  -> Selection params schema columns
Alias tab `dotStarFrom` tabs = UnsafeSelection $
  fromString (symbolVal' tab) <> ".* FROM " <> renderFrom tabs

from
  :: SListI columns
  => NP (Aliased (Expression params tables)) columns
  -> From params schema tables
  -> Selection params schema columns
list `from` tabs = UnsafeSelection $
  renderList list <> " FROM " <> renderFrom tabs
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
  -> From params schema '[table]
subselect selection = From
  { fromJoin = Subselect selection
  , fromClauses = mempty
  }

{-----------------------------------------
INSERT statements
-----------------------------------------}

insertInto
  :: (SListI columns, HasTable table schema columns)
  => Alias table
  -> NP (Aliased (Expression params '[])) columns
  -> Statement params '[] schema schema
insertInto (Alias table) expressions = UnsafeStatement $ "INSERT INTO "
  <> fromString (symbolVal' table)
  <> " (" <> ByteString.intercalate ", " aliases
  <> ") VALUES ("
  <> ByteString.intercalate ", " values
  <> ");"
  where
    aliases = hcollapse $ hmap
      (\ (_ `As` Alias name) -> K (fromString (symbolVal' name)))
      expressions
    values = hcollapse $ hmap
      (\ (expression `As` _) -> K (renderExpression expression))
      expressions

{-----------------------------------------
CREATE statements
-----------------------------------------}

newtype TypeExpression (ty :: ColumnType)
  = UnsafeTypeExpression { renderTypeExpression :: ByteString }
  deriving (Show,Eq)

int2 :: TypeExpression ('Required ('Null 'PGInt2))
int2 = UnsafeTypeExpression "int2"
int4 :: TypeExpression ('Required ('Null 'PGInt4))
int4 = UnsafeTypeExpression "int4"
int8 :: TypeExpression ('Required ('Null 'PGInt8))
int8 = UnsafeTypeExpression "int8"
bool :: TypeExpression ('Required ('Null 'PGBool))
bool = UnsafeTypeExpression "bool"
text :: TypeExpression ('Required ('Null 'PGText))
text = UnsafeTypeExpression "text"
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
serial :: TypeExpression ('Optional ('NotNull 'PGInt4))
serial = UnsafeTypeExpression "serial"

createTable
  :: (KnownSymbol table, SListI columns)
  => Alias table
  -> NP (Aliased TypeExpression) columns
  -> Statement '[] '[] schema ((table ::: columns) ': schema)
createTable (Alias table) columns = UnsafeStatement $ mconcat
  [ "CREATE TABLE "
  , fromString $ symbolVal' table
  , " ("
  , ByteString.intercalate ", " . hcollapse $
      hmap (K . renderColumn) columns
  , ");"
  ]
  where
    renderColumn :: Aliased TypeExpression x -> ByteString
    renderColumn (ty `As` Alias column) =
      fromString (symbolVal' column) <> " " <> renderTypeExpression ty

{-----------------------------------------
DROP statements
-----------------------------------------}

class KnownSymbol table => DropTable table schema0 schema1
  | table schema0 -> schema1 where
    dropTable :: Alias table -> Statement '[] '[] schema0 schema1
    dropTable (Alias table) = UnsafeStatement $
      "DROP TABLE " <> fromString (symbolVal' table) <> ";"
instance {-# OVERLAPPING #-}
  (KnownSymbol table, table ~ table')
    => DropTable table ((table' ::: columns) ': schema) schema
instance {-# OVERLAPPABLE #-}
  DropTable table schema0 schema1
    => DropTable table (table' ': schema0) (table' ': schema1)

{-----------------------------------------
UPDATE statements
-----------------------------------------}

set :: g x -> (Maybe :.: g) x
set = Comp . Just

same :: (Maybe :.: g) x
same = Comp Nothing

update
  :: (HasTable table schema columns, SListI columns)
  => Alias table
  -> NP (Aliased (Maybe :.: Expression params '[table ::: columns])) columns
  -> Expression params '[table ::: columns] ('Required ('NotNull 'PGBool))
  -> Statement params '[] schema schema
update (Alias table) columns where' = UnsafeStatement $ mconcat
  [ "UPDATE "
  , fromString $ symbolVal' table
  , " SET "
  , ByteString.intercalate ", " . catMaybes . hcollapse $
      hmap (K . renderSet) columns
  , " WHERE ", renderExpression where'
  ] where
    renderSet
      :: Aliased (Maybe :.: Expression params tables) column
      -> Maybe ByteString
    renderSet = \case
      Comp (Just expression) `As` Alias column -> Just $ mconcat
        [ fromString $ symbolVal' column
        , " = "
        , renderExpression expression
        ]
      Comp Nothing `As` _ -> Nothing

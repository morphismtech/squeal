{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , FunctionalDependencies
  , LambdaCase
  , MagicHash
  , OverloadedStrings
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeel.PostgreSQL.Statement where

import Data.Boolean
import Data.ByteString (ByteString)
import Data.Monoid
import Data.String
import Data.Vinyl
import Data.Vinyl.Functor
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits

import qualified Data.ByteString as ByteString

import Squeel.PostgreSQL.Schema

newtype Expression
  (params :: [NullityType])
  (tables :: [(Symbol,[(Symbol,NullityType)])])
  (ty :: NullityType)
  = UnsafeExpression { renderExpression :: ByteString }

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
  => HasColumn column ((column ::: ty) ': tys) ty
instance {-# OVERLAPPABLE #-} (KnownSymbol column, HasColumn column table ty)
  => HasColumn column (ty' ': table) ty

instance (HasColumn column columns ty, tables ~ '[table ::: columns])
  => IsLabel column (Expression params tables ty) where
    fromLabel = getColumn

(.&.)
  :: (HasTable table tables columns, HasColumn column columns ty)
  => Alias table -> Alias column -> Expression params tables ty
Alias table .&. Alias column = UnsafeExpression $
  fromString (symbolVal' table)
  <> "." <>
  fromString (symbolVal' column)

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
unsafeFunction fn x = UnsafeExpression $ mconcat
  [fn, "(", renderExpression x, ")"]

instance PGNum ty => Num (Expression params tables (nullity ty)) where
  (+) = unsafeBinaryOp "+"
  (-) = unsafeBinaryOp "-"
  (*) = unsafeBinaryOp "*"
  abs = unsafeUnaryOp "@"
  signum = unsafeFunction "sign"
  fromInteger = UnsafeExpression . fromString . show

instance IsString (Expression params tables (nullity 'PGText)) where
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

instance Boolean (Expression params tables ('NotNull 'PGBool)) where
  true = UnsafeExpression "TRUE"
  false = UnsafeExpression "FALSE"
  notB = unsafeUnaryOp "NOT"
  (&&*) = unsafeBinaryOp "AND"
  (||*) = unsafeBinaryOp "OR"

type instance BooleanOf (Expression params tables ty) =
  Expression params tables ('NotNull 'PGBool)

caseWhenThenElse
  :: [(Expression params tables ('NotNull 'PGBool), Expression params tables ty)]
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

instance EqB (Expression params tables ty) where
  (==*) = unsafeBinaryOp "="
  (/=*) = unsafeBinaryOp "<>"

instance OrdB (Expression params tables ty) where
  (>*) = unsafeBinaryOp ">"
  (>=*) = unsafeBinaryOp ">="
  (<*) = unsafeBinaryOp "<"
  (<=*) = unsafeBinaryOp "<="

newtype JoinExpression
  (params :: [NullityType])
  (schema :: [(Symbol,[(Symbol,NullityType)])])
  (tables :: [(Symbol,[(Symbol,NullityType)])])
  = UnsafeJoinExpression { renderJoinExpression :: ByteString }

class KnownSymbol table => HasTable table tables columns
  | table tables -> columns where
    getTable :: Proxy# table -> JoinExpression params tables '[table ::: columns]
    getTable table = UnsafeJoinExpression $ fromString $ symbolVal' table
instance {-# OVERLAPPING #-} KnownSymbol table
  => HasTable table ((table ::: columns) ': tables) columns
instance {-# OVERLAPPABLE #-}
  (KnownSymbol table, HasTable table schema columns)
    => HasTable table (table' ': schema) columns

cross
  :: JoinExpression params schema '[table]
  -> JoinExpression params schema tables
  -> JoinExpression params schema (table ': tables)
cross tab tabs = UnsafeJoinExpression $
  renderJoinExpression tabs <> " CROSS JOIN " <> renderJoinExpression tab

inner
  :: JoinExpression params schema '[table]
  -> Expression params (table ': tables) ('NotNull 'PGBool)
  -> JoinExpression params schema tables
  -> JoinExpression params schema (table ': tables)
inner tab on tabs = UnsafeJoinExpression $
  renderJoinExpression tabs
  <> " INNER JOIN " <>
  renderJoinExpression tab
  <> " ON " <>
  renderExpression on

leftOuter
  :: JoinExpression params schema '[right]
  -> Expression params (right ': left : tables) ('NotNull 'PGBool)
  -> JoinExpression params schema (tables)
  -> JoinExpression params schema (NullifyTable right ': left ': tables)
leftOuter tab on tabs = UnsafeJoinExpression $
  renderJoinExpression tabs
  <> " LEFT OUTER JOIN " <>
  renderJoinExpression tab
  <> " ON " <>
  renderExpression on

rightOuter
  :: JoinExpression params schema '[right]
  -> Expression params (right ': tables) ('NotNull 'PGBool)
  -> JoinExpression params schema (left : tables)
  -> JoinExpression params schema (right ': NullifyTable left ': tables)
rightOuter tab on tabs = UnsafeJoinExpression $
  renderJoinExpression tabs
  <> " RIGHT OUTER JOIN " <>
  renderJoinExpression tab
  <> " ON " <>
  renderExpression on

fullOuter
  :: JoinExpression params schema '[right]
  -> Expression params (right ': left ': tables) ('NotNull 'PGBool)
  -> JoinExpression params schema (left : tables)
  -> JoinExpression params schema
      (NullifyTable right ': NullifyTable left ': tables)
fullOuter tab on tabs = UnsafeJoinExpression $
  renderJoinExpression tabs
  <> " FULL OUTER JOIN " <>
  renderJoinExpression tab
  <> " ON " <>
  renderExpression on

data Clauses params tables = Clauses
  { whereClause :: Maybe (Expression params tables ('NotNull 'PGBool))
  , limitClause :: Maybe (Expression params '[] ('NotNull 'PGInt8))
  , offsetClause :: Maybe (Expression params '[] ('NotNull 'PGInt8))
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

data FromExpression
  (params :: [NullityType])
  (schema :: [(Symbol,[(Symbol,NullityType)])])
  (tables :: [(Symbol,[(Symbol,NullityType)])])
  = FromExpression
  { joinExpression :: JoinExpression params schema tables
  , clauses :: Clauses params tables
  }

join
  :: JoinExpression params schema tables
  -> FromExpression params schema tables
join tref = FromExpression tref mempty

renderTabulation :: FromExpression params schema tables -> ByteString
renderTabulation (FromExpression tref (Clauses wh lim off))= mconcat
  [ renderJoinExpression tref
  , maybe "" ((" WHERE " <>) . renderExpression) wh
  , maybe "" ((" LIMIT " <>) . renderExpression) lim
  , maybe "" ((" OFFSET " <>) . renderExpression) off
  ]

instance (HasTable table schema columns, table ~ table')
  => IsLabel table (JoinExpression params schema '[table' ::: columns]) where
    fromLabel = getTable

instance (HasTable table schema columns, table ~ table')
  => IsLabel table (FromExpression params schema '[table' ::: columns]) where
    fromLabel p = FromExpression (getTable p) mempty

where_
  :: Expression params tables ('NotNull 'PGBool)
  -> FromExpression params schema tables
  -> FromExpression params schema tables
where_ wh (FromExpression tabs clauses1) = FromExpression tabs
  (clauses1 <> Clauses (Just wh) Nothing Nothing)

limit
  :: Expression params '[] ('NotNull 'PGInt8)
  -> FromExpression params schema tables
  -> FromExpression params schema tables
limit lim (FromExpression tabs clauses1) = FromExpression tabs
  (clauses1 <> Clauses Nothing (Just lim) Nothing)

offset
  :: Expression params '[] ('NotNull 'PGInt8)
  -> FromExpression params schema tables
  -> FromExpression params schema tables
offset off (FromExpression tabs clauses1) = FromExpression tabs
  (clauses1 <> Clauses Nothing Nothing (Just off))

newtype Selection
  (params :: [NullityType])
  (schema :: [(Symbol,[(Symbol,NullityType)])])
  (columns :: [(Symbol,NullityType)])
    = UnsafeSelection
    { renderSelection :: ByteString }

starFrom
  :: tables ~ '[table ::: columns]
  => FromExpression params schema tables
  -> Selection params schema columns
starFrom tabs = UnsafeSelection $ "* FROM " <> renderTabulation tabs

dotStarFrom
  :: HasTable table tables columns
  => Alias table
  -> FromExpression params schema tables
  -> Selection params schema columns
Alias tab `dotStarFrom` tabs = UnsafeSelection $
  fromString (symbolVal' tab) <> ".* FROM" <> renderTabulation tabs

from
  :: Rec (Aliased (Expression params tables)) columns
  -> FromExpression params schema tables
  -> Selection params schema columns
list `from` tabs = UnsafeSelection $
  renderList list <> " FROM " <> renderTabulation tabs
  where
    renderList
      = ByteString.intercalate ", "
      . recordToList
      . rmap (Const . renderAliased renderExpression)

newtype Statement
  (params :: [NullityType])
  (schema0 :: [(Symbol,[(Symbol,NullityType)])])
  (schema1 :: [(Symbol,[(Symbol,NullityType)])])
  (columns :: [(Symbol,NullityType)])
    = UnsafeStatement { renderStatement :: ByteString }

newtype PreparedStatement
  (params :: [NullityType])
  (schema0 :: [(Symbol,[(Symbol,NullityType)])])
  (schema1 :: [(Symbol,[(Symbol,NullityType)])])
  (columns :: [(Symbol,NullityType)])
    = UnsafePreparedStatement { renderPreparedStatement :: ByteString }

select
  :: Selection params schema columns
  -> Statement params schema schema columns
select = UnsafeStatement . ("SELECT " <>) . (<> ";") . renderSelection

subselect
  :: Aliased (Selection params schema) table
  -> FromExpression params schema '[table]
subselect selection = FromExpression
  { joinExpression = UnsafeJoinExpression . ("SELECT " <>) $
      renderAliased renderSelection selection
  , clauses = mempty
  }

insertInto
  :: HasTable table schema columns
  => Alias table
  -> Rec (Aliased (Expression params '[])) columns
  -> Statement params schema schema '[]
insertInto (Alias table) expressions = UnsafeStatement $ "INSERT INTO "
  <> fromString (symbolVal' table)
  <> " (" <> ByteString.intercalate ", " aliases
  <> ") VALUES ("
  <> ByteString.intercalate ", " values
  <> ");"
  where
    aliases = recordToList $ rmap
      (\ (_ `As` Alias name) -> Const (fromString (symbolVal' name)))
      expressions
    values = recordToList $ rmap
      (\ (expression `As` _) -> Const (renderExpression expression))
      expressions

createTable
    :: (KnownSymbol table, KnownColumns columns)
    => Alias table
    -> Proxy# columns
    -> Statement '[] schema ((table ::: columns) ': schema) '[]
createTable (Alias table) columns = UnsafeStatement $ mconcat
  [ "CREATE TABLE "
  , fromString $ symbolVal' table
  , " ("
  , renderColumns columns
  , ");"
  ]

class KnownSymbol table => DropTable table schema0 schema1
  | table schema0 -> schema1 where
    dropTable :: Alias table -> Statement '[] schema0 schema1 '[]
    dropTable (Alias table) = UnsafeStatement $
      "DROP TABLE " <> fromString (symbolVal' table) <> ";"
instance {-# OVERLAPPING #-}
  (KnownSymbol table, table ~ table')
    => DropTable table ((table' ::: columns) ': schema) schema
instance {-# OVERLAPPABLE #-}
  (KnownSymbol table, DropTable table schema0 schema1)
    => DropTable table (tabCols ': schema0) (tabCols ': schema1)

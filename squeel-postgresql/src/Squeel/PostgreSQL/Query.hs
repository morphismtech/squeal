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

module Squeel.PostgreSQL.Query where

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
  (params :: [PGType])
  (tables :: [(Symbol,[(Symbol,PGType)])])
  (ty :: PGType)
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

instance PGNum ty => Num (Expression params tables ty) where
  (+) = unsafeBinaryOp "+"
  (-) = unsafeBinaryOp "-"
  (*) = unsafeBinaryOp "*"
  abs = unsafeUnaryOp "@"
  signum = unsafeFunction "sign"
  fromInteger = UnsafeExpression . fromString . show

instance IsString (Expression params tables 'PGText) where
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

instance Boolean (Expression params tables 'PGBool) where
  true = UnsafeExpression "TRUE"
  false = UnsafeExpression "FALSE"
  notB = unsafeUnaryOp "NOT"
  (&&*) = unsafeBinaryOp "AND"
  (||*) = unsafeBinaryOp "OR"

type instance BooleanOf (Expression params tables ty) =
  Expression params tables 'PGBool

instance IfB (Expression params tables ty) where
  ifB if_ then_ else_ = UnsafeExpression $ mconcat
    [ "CASE WHEN ",renderExpression if_
    , " THEN ",renderExpression then_
    , " ELSE ",renderExpression else_
    , " END"
    ]

instance EqB (Expression params tables ty) where
  (==*) = unsafeBinaryOp "="
  (/=*) = unsafeBinaryOp "<>"

instance OrdB (Expression params tables ty) where
  (>*) = unsafeBinaryOp ">"
  (>=*) = unsafeBinaryOp ">="
  (<*) = unsafeBinaryOp "<"
  (<=*) = unsafeBinaryOp "<="

newtype TableRef
  (params :: [PGType])
  (schema :: [(Symbol,[(Symbol,PGType)])])
  (tables :: [(Symbol,[(Symbol,PGType)])])
  = UnsafeTableRef { renderTableRef :: ByteString }

class KnownSymbol table => HasTable table tables columns
  | table tables -> columns where
    getTable :: Proxy# table -> TableRef params tables '[table ::: columns]
    getTable table = UnsafeTableRef $ fromString $ symbolVal' table
instance {-# OVERLAPPING #-} KnownSymbol table
  => HasTable table ((table ::: columns) ': tables) columns
instance {-# OVERLAPPABLE #-}
  (KnownSymbol table, HasTable table schema columns)
    => HasTable table (table' ': schema) columns

crossJoin
  :: TableRef params schema '[table]
  -> TableRef params schema tables
  -> TableRef params schema (table ': tables)
crossJoin tab tabs = UnsafeTableRef $
  renderTableRef tabs <> " CROSS JOIN " <> renderTableRef tab

innerJoin
  :: TableRef params schema '[table]
  -> Expression params (table ': tables) 'PGBool
  -> TableRef params schema tables
  -> TableRef params schema (table ': tables)
innerJoin tab on tabs = UnsafeTableRef $
  renderTableRef tabs
  <> " INNER JOIN " <>
  renderTableRef tab
  <> " ON " <>
  renderExpression on

newtype Projection
  (params :: [PGType])
  (tables :: [(Symbol,[(Symbol,PGType)])])
  (columns :: [(Symbol,PGType)]) =
    UnsafeProjection { renderProjection :: ByteString }

star :: Projection params '[table ::: columns] columns
star = UnsafeProjection "*"

tableStar
  :: HasTable table tables columns
  => Alias table -> Projection params tables columns
tableStar (Alias table) = UnsafeProjection $
  fromString (symbolVal' table) <> ".*"

project
  :: Rec (Aliased (Expression params tables)) columns
  -> Projection params tables columns
project
  = UnsafeProjection
  . ByteString.intercalate ", "
  . recordToList
  . rmap (Const . renderAliased renderExpression)

data Clauses params tables = Clauses
  { whereClause :: Maybe (Expression params tables 'PGBool)
  , limitClause :: Maybe (Expression params '[] 'PGInt8)
  , offsetClause :: Maybe (Expression params '[] 'PGInt8)
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

data TableExpression
  (params :: [PGType])
  (schema :: [(Symbol,[(Symbol,PGType)])])
  (tables :: [(Symbol,[(Symbol,PGType)])])
  = TableExpression
  { tableRef :: TableRef params schema tables
  , clauses :: Clauses params tables
  }

tables
  :: TableRef params schema tables
  -> TableExpression params schema tables
tables tref = TableExpression tref mempty

renderTabulation :: TableExpression params schema tables -> ByteString
renderTabulation (TableExpression tref (Clauses wh lim off))= mconcat
  [ renderTableRef tref
  , maybe "" ((" WHERE " <>) . renderExpression) wh
  , maybe "" ((" LIMIT " <>) . renderExpression) lim
  , maybe "" ((" OFFSET " <>) . renderExpression) off
  ]

instance (HasTable table schema columns, table ~ table')
  => IsLabel table (TableRef params schema '[table' ::: columns]) where
    fromLabel = getTable

instance (HasTable table schema columns, table ~ table')
  => IsLabel table (TableExpression params schema '[table' ::: columns]) where
    fromLabel p = TableExpression (getTable p) mempty

where_
  :: Expression params tables 'PGBool
  -> TableExpression params schema tables
  -> TableExpression params schema tables
where_ wh (TableExpression tabs clauses1) = TableExpression tabs
  (clauses1 <> Clauses (Just wh) Nothing Nothing)

limit
  :: Expression params '[] 'PGInt8
  -> TableExpression params schema tables
  -> TableExpression params schema tables
limit lim (TableExpression tabs clauses1) = TableExpression tabs
  (clauses1 <> Clauses Nothing (Just lim) Nothing)

offset
  :: Expression params '[] 'PGInt8
  -> TableExpression params schema tables
  -> TableExpression params schema tables
offset off (TableExpression tabs clauses1) = TableExpression tabs
  (clauses1 <> Clauses Nothing Nothing (Just off))

newtype Selection
  (params :: [PGType])
  (schema :: [(Symbol,[(Symbol,PGType)])])
  (columns :: [(Symbol,PGType)])
    = UnsafeSelection
    { renderSelection :: ByteString }

from
  :: TableExpression params schema tables
  -> Projection params tables columns
  -> Selection params schema columns
from tabs projection = UnsafeSelection $
  renderProjection projection <> " FROM " <> renderTabulation tabs

newtype Query
  (params :: [PGType])
  (schema0 :: [(Symbol,[(Symbol,PGType)])])
  (schema1 :: [(Symbol,[(Symbol,PGType)])])
  (columns :: [(Symbol,PGType)])
    = UnsafeQuery { renderQuery :: ByteString }

newtype PreparedQuery
  (params :: [PGType])
  (schema0 :: [(Symbol,[(Symbol,PGType)])])
  (schema1 :: [(Symbol,[(Symbol,PGType)])])
  (columns :: [(Symbol,PGType)])
    = UnsafePreparedQuery { renderPreparedQuery :: ByteString }

select
  :: Selection params schema columns
  -> Query params schema schema columns
select = UnsafeQuery . ("SELECT " <>) . (<> ";") . renderSelection

subselect
  :: Aliased (Selection params schema) table
  -> TableExpression params schema '[table]
subselect selection = TableExpression
  { tableRef = UnsafeTableRef . ("SELECT " <>) $
      renderAliased renderSelection selection
  , clauses = mempty
  }

insertInto
  :: HasTable table schema columns
  => Alias table
  -> Rec (Aliased (Expression params '[])) columns
  -> Query params schema schema '[]
insertInto (Alias table) expressions = UnsafeQuery $ "INSERT INTO "
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

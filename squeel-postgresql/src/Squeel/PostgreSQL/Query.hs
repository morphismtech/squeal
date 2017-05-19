{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , MagicHash
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
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

instance HasColumn column columns ty
  => IsLabel column (Expression params '[table ::: columns] ty) where
    fromLabel = getColumn

(.&.)
  :: forall columns column params tables table ty
   . (HasTable table tables columns, HasColumn column columns ty)
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
crossJoin table tables = UnsafeTableRef $
  "(" <> renderTableRef tables <> " CROSS JOIN " <> renderTableRef table <> ")"

innerJoin
  :: TableRef params tables '[left]
  -> Expression params '[left,right] 'PGBool
  -> TableRef params tables (right ': rest)
  -> TableRef params tables (left ': right ': rest)
innerJoin table on tables = UnsafeTableRef $
  renderTableRef tables
  <> " INNER JOIN " <>
  renderTableRef table
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

data Tabulation
  (params :: [PGType])
  (schema :: [(Symbol,[(Symbol,PGType)])])
  (tables :: [(Symbol,[(Symbol,PGType)])])
  = Tabulation
  { fromClause :: TableRef params schema tables
  , whereClause :: Maybe (Expression params tables 'PGBool)
  , limitClause :: Maybe (Expression params '[] 'PGInt8)
  , offsetClause :: Maybe (Expression params '[] 'PGInt8)
  }

tabulation :: TableRef params schema tables -> Tabulation params schema tables
tabulation tab = Tabulation tab Nothing Nothing Nothing

renderTabulation :: Tabulation params schema tables -> ByteString
renderTabulation (Tabulation fr wh lim off)= mconcat
  [ renderTableRef fr
  , maybe "" ((" WHERE " <>) . renderExpression) wh
  , maybe "" ((" LIMIT " <>) . renderExpression) lim
  , maybe "" ((" OFFSET " <>) . renderExpression) off
  ]

instance HasTable table schema columns
  => IsLabel table (TableRef params schema '[table ::: columns]) where
    fromLabel = getTable

instance HasTable table schema columns
  => IsLabel table (Tabulation params schema '[table ::: columns]) where
    fromLabel p = Tabulation (getTable p) Nothing Nothing Nothing

where_
  :: Expression params tables 'PGBool
  -> Tabulation params schema tables
  -> Tabulation params schema tables
where_ condition tables = tables
  { whereClause = case whereClause tables of
      Nothing -> Just condition
      Just conditions -> Just (conditions &&* condition)
  }

limit
  :: Expression params '[] 'PGInt8
  -> Tabulation params schema tables
  -> Tabulation params schema tables
limit n tables = tables
  { limitClause = case limitClause tables of
      Nothing -> Just n
      Just n' -> Just (n' `minB` n)
  }

offset
  :: Expression params '[] 'PGInt8
  -> Tabulation params schema tables
  -> Tabulation params schema tables
offset n tables = tables
  { offsetClause = case offsetClause tables of
      Nothing -> Just n
      Just n' -> Just (n' + n)
  }

newtype Selection params schema columns = UnsafeSelection
  { renderSelection :: ByteString }

from
  :: Projection params tables columns
  -> Tabulation params schema tables
  -> Selection params schema columns
from projection tables = UnsafeSelection $
  renderProjection projection <> " FROM " <> renderTabulation tables

newtype Query params schema0 schema1 columns = UnsafeQuery
  { renderQuery :: ByteString }

newtype PreparedQuery params schema0 schema1 columns = UnsafePreparedQuery
  { renderPreparedQuery :: ByteString }

select
  :: Selection params schema columns
  -> Query params schema schema columns
select = UnsafeQuery . ("SELECT " <>) . (<> ";") . renderSelection

subselect
  :: Aliased (Selection params schema) table
  -> Tabulation params schema '[table]
subselect selection = Tabulation
  { fromClause = UnsafeTableRef . ("SELECT " <>) $
      renderAliased renderSelection selection
  , whereClause = Nothing
  , limitClause = Nothing
  , offsetClause = Nothing
  }

insertInto
  :: forall schema columns table params
   . HasTable table schema columns
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

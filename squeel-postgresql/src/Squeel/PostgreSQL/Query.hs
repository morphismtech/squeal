{-# LANGUAGE
    PolyKinds
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeOperators
#-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Squeel.PostgreSQL.Query where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Vinyl
import Data.Vinyl.Functor
import GHC.TypeLits

import qualified Data.ByteString.Char8 as Char8

import Squeel.PostgreSQL.Type

data Expression ps cols x =
  Expression { unExpression :: ByteString } deriving Show

param1 :: Expression (p1:ps) cols p1
param1 = Expression "$1"

param2 :: Expression (p1:p2:ps) cols p2
param2 = Expression "$2"

param3 :: Expression (p1:p2:p3:ps) cols p3
param3 = Expression "$3"

param4 :: Expression (p1:p2:p3:p4:ps) cols p4
param4 = Expression "$4"

param5 :: Expression (p1:p2:p3:p4:p5:ps) cols p5
param5 = Expression "$5"

whenThen :: (Expression ps cols x,Expression ps cols y) -> ByteString
whenThen (Expression when_,Expression then_) = mconcat
  ["  WHEN ",when_," THEN ",then_,"\n"]

case_
  :: [(Expression ps cols 'PGBool,Expression ps cols x)]
  -> Expression ps cols x
  -> Expression ps cols x
case_ whens (Expression else_) = Expression $ mconcat
  [ "CASE\n"
  , mconcat (map whenThen whens)
  , "  ELSE ",else_
  , "\nEND"
  ]

caseSwitch
  :: Expression ps cols x
  -> [(Expression ps cols x,Expression ps cols y)]
  -> Expression ps cols y
  -> Expression ps cols y
caseSwitch (Expression val) whens (Expression else_) = Expression $ mconcat
  [ "CASE ",val,"\n"
  , mconcat (map whenThen whens)
  , "  ELSE ",else_
  ,"\nEND"
  ]

column
  :: forall colName colType params columns
  .  Alias (Expression params columns) (colName ':= colType)
  -> Expression params columns colType
column = \case
  As x -> x
  Elem -> Expression $ Char8.pack (symbolVal (Proxy @colName))

true :: Expression ps cols 'PGBool
true = Expression "true"

false :: Expression ps cols 'PGBool
false = Expression "false"

(.==)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols 'PGBool
(.==) (Expression x1) (Expression x2) = Expression $ x1 <> "=" <> x2

(./=)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols 'PGBool
(./=) (Expression x1) (Expression x2) = Expression $ x1 <> "<>" <> x2

(.>)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols 'PGBool
(.>) (Expression x1) (Expression x2) = Expression $ x1 <> ">" <> x2

(.<)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols 'PGBool
(.<) (Expression x1) (Expression x2) = Expression $ x1 <> "<" <> x2

(.>=)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols 'PGBool
(.>=) (Expression x1) (Expression x2) = Expression $ x1 <> ">=" <> x2

(.<=)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols 'PGBool
(.<=) (Expression x1) (Expression x2) = Expression $ x1 <> "<=" <> x2

instance IsString (Expression ps cols 'PGText) where
  fromString str = Expression $ "\'" <> fromString str <> "\'"

instance Num (Expression ps cols 'PGInt4) where
  fromInteger n = Expression . fromString $ show n
  Expression x + Expression y = Expression $ x <> "+" <> y
  Expression x * Expression y = Expression $ x <> "*" <> y
  Expression x - Expression y = Expression $ x <> "+" <> y
  abs (Expression x) = Expression $ "@" <> x
  signum (Expression x) = Expression $ "sign(" <> x <> ")"

data Relation ps xs ys = Relation { unRelation :: ByteString } deriving Show

star :: Relation ps xs xs
star = Relation "*"

project :: Rec (Alias (Expression ps xs)) ys -> Relation ps xs ys
project
  = Relation
  . Char8.intercalate ", "
  . recordToList
  . rmap (Const . expressionString)
  where
    expressionString :: Alias (Expression ps xs) y -> ByteString
    expressionString = \case
      (As (Expression expr) :: Alias (Expression ps xs) y)
        -> expr <> " AS " <> Char8.pack (symbolVal (Proxy @(Name y)))
      (Elem :: Alias (Expression ps xs) y)
        -> Char8.pack (symbolVal (Proxy @(Name y)))

-- star :: Relation db params xs xs
-- star = Relation "SELECT * FROM "

-- table
--   :: forall tableName tableCols db params.
--     ( KnownSymbol tableName
--     , (tableName ':= tableCols) `Elem` Unnamed db ~ 'True
--     ) => Proxy tableName -> Relation db params '[] tableCols
-- table (_ :: Proxy tableName) = Relation $
--   "SELECT * FROM " <> Char8.pack (symbolVal (Proxy @tableName))

-- starFrom
--   :: forall tableName tableCols db params columns
--   .  Alias (Relation params columns db) (tableName ':= tableCols)
--   -> Relation params columns db tableCols
-- starFrom = \case
--   As x -> x
--   Elem -> Relation $
--     "SELECT * FROM " <> Char8.pack (symbolVal (Proxy @tableName))

-- from
--   :: Relation db params ys zs
--   -> Relation db params xs ys
--   -> Relation db params xs zs
-- from (Relation rel1) (Relation rel2) = Relation $ rel1 <> rel2

-- project :: Rec (Alias (Expression ps xs)) ys -> Relation db params xs ys
-- project exprs = Relation $
--   "SELECT " <> Char8.intercalate "," (exprStrings exprs) <> " FROM "
--   where
--     exprStrings :: Rec (Alias (Expression ps xs)) ys -> [ByteString]
--     exprStrings = \case
--       RNil -> []
--       (As (Expression expr) :: Alias (Expression _ _) (exprName ':= _))
--         :& exprs ->
--           (expr <> " As " <> Char8.pack (symbolVal (Proxy @exprName)))
--             : expressionStrings exprs

newtype Query
  (db0 :: [Table])
  (db1 :: [Table])
  (ps :: [PGType])
  (xs :: [Column])
    = Query { unQuery :: ByteString }

newtype PreparedQuery db0 db1 ps xs
  = PreparedQuery { unPreparedQuery :: ByteString }

-- select :: Relation ps ('[] :: [Column]) db xs -> Query db db ps xs
-- select (Relation x) = Query x

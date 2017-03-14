{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Squeel.PostgreSQL.Query.Expression where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.String

import Squeel.PostgreSQL.Type

data Expression (ps :: [PGType]) (cols :: [Column]) (x :: PGType) =
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
  :: [(Expression ps cols ('PGType "bool"),Expression ps cols x)]
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

(.==)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols ('PGType "bool")
(.==) (Expression x1) (Expression x2) = Expression $ x1 <> "=" <> x2

(./=)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols ('PGType "bool")
(./=) (Expression x1) (Expression x2) = Expression $ x1 <> "<>" <> x2

(.>)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols ('PGType "bool")
(.>) (Expression x1) (Expression x2) = Expression $ x1 <> ">" <> x2

(.<)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols ('PGType "bool")
(.<) (Expression x1) (Expression x2) = Expression $ x1 <> "<" <> x2

(.>=)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols ('PGType "bool")
(.>=) (Expression x1) (Expression x2) = Expression $ x1 <> ">=" <> x2

(.<=)
  :: Expression ps cols x
  -> Expression ps cols x
  -> Expression ps cols ('PGType "bool")
(.<=) (Expression x1) (Expression x2) = Expression $ x1 <> "<=" <> x2

instance IsString (Expression ps cols ('PGType "text")) where
  fromString str = Expression $ "\'" <> fromString str <> "\'"

instance Num (Expression ps cols ('PGType "int4")) where
  fromInteger n = Expression . fromString $ show n
  Expression x + Expression y = Expression $ x <> "+" <> y
  Expression x * Expression y = Expression $ x <> "*" <> y
  Expression x - Expression y = Expression $ x <> "+" <> y
  abs (Expression x) = Expression $ "@" <> x
  signum (Expression x) = Expression $ "sign(" <> x <> ")"

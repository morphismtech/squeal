{-# LANGUAGE
    DataKinds
  , MagicHash
  , OverloadedLabels
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Main (main) where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Function ((&))
import Data.Int
import Data.Monoid
import Generics.SOP
import Squeel.PostgreSQL

import qualified Data.ByteString.Char8 as Char8

main :: IO ()
main = do
  Char8.putStrLn "squeel"
  connectionString <- pure
    "host=localhost port=5432 dbname=exampledb"
  Char8.putStrLn $ "connecting to " <> connectionString
  connection0 :: Connection '[] <- connectdb connectionString
  Char8.putStrLn "setting up database"
  connection1 <- flip execPQ connection0 $
    pqExec
      ( createTable #students
        (  (text & notNull) `As` #name
        :* Nil ) )
    & pqThenExec
      ( createTable #table1
        (  (int4 & notNull) `As` #col1
        :* (int4 & notNull) `As` #col2
        :* Nil ) )
    & pqThenExec
      ( insertInto #table1 ( 1 `As` #col1 :* 2 `As` #col2 :* Nil ) )
    & pqThenExec
      ( insertInto #table1 ( 3 `As` #col1 :* 4 `As` #col2 :* Nil ) )
  Char8.putStrLn "querying"
  connection2 <- flip execPQ connection1 $ do
    Just result <- pqExecNil $ select $ starFrom #table1
    runMaybeT . runExceptT . flip runValue result $ do
      value00 <- getValue (RowNumber 0) (columnNumber @0)
      value01 <- getValue (RowNumber 0) (columnNumber @1)
      value10 <- getValue (RowNumber 1) (columnNumber @0)
      value11 <- getValue (RowNumber 1) (columnNumber @1)
      row0 <- getRow (RowNumber 0)
      row1 <- getRow (RowNumber 1)
      liftBase $ do
        print (value00 :: Int32)
        print (value01 :: Int32)
        print (value10 :: Int32)
        print (value11 :: Int32)
        print (row0 :: NP I '[Int32,Int32])
        print (row1 :: NP I '[Int32,Int32])
  Char8.putStrLn "tearing down database"
  connection3 :: Connection '[] <- flip execPQ connection2 $ pqExec
    (dropTable #table1 :: Statement '[] '[] Tables '["students" ::: StudentsColumns])
    & pqThenExec (dropTable #students)
  finish connection3

type Columns =
  '[ "col1" ::: 'Required ('NotNull 'PGInt4)
   , "col2" ::: 'Required ('NotNull 'PGInt4)
   ]
-- type SumAndCol1 = '[ "sum" ::: 'NotNull 'PGInt4, "col1" ::: 'NotNull 'PGInt4]
type StudentsColumns = '["name" ::: 'Required ('NotNull 'PGText)]
type Tables = '["students" ::: StudentsColumns, "table1" ::: Columns]
-- type OrderColumns =
--   [ "orderID"    ::: 'NotNull 'PGInt4
--   , "orderVal"   ::: 'NotNull 'PGText
--   , "customerID" ::: 'NotNull 'PGInt4
--   , "shipperID"  ::: 'NotNull 'PGInt4
--   ]
-- type CustomerColumns =
--   [ "customerID" ::: 'NotNull 'PGInt4, "customerVal" ::: 'NotNull 'PGFloat4 ]
-- type ShipperColumns =
--   [ "shipperID" ::: 'NotNull 'PGInt4, "shipperVal" ::: 'NotNull 'PGBool ]
-- type JoinTables =
--   [ "shippers"  ::: ShipperColumns
--   , "customers" ::: CustomerColumns
--   , "orders"    ::: OrderColumns
--   ]
-- type ValueColumns =
--   [ "orderVal"    ::: 'NotNull 'PGText
--   , "customerVal" ::: 'NotNull 'PGFloat4
--   , "shipperVal"  ::: 'NotNull 'PGBool
--   ]

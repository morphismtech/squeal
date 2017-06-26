{-# LANGUAGE
    DataKinds
  , MagicHash
  , OverloadedLabels
  , OverloadedStrings
  , TypeOperators
#-}

module Main (main) where

import Control.Monad.Base
import Data.Int
import Data.Monoid
import Data.Vinyl
import GHC.Exts
import Squeel.PostgreSQL

import qualified Data.ByteString.Char8 as Char8

main :: IO ()
main = do
  Char8.putStrLn "squeel"
  connectionString <- pure
    "host=localhost port=5432 dbname=exampledb"
  Char8.putStrLn $ "connecting to " <> connectionString
  connection0 <- connectdb connectionString
  Char8.putStrLn "creating tables"
  connection1 <- flip execPQ connection0 $ pqExec $
    createTable #table1 (proxy# :: Proxy# Columns)
  connection2 <- flip execPQ connection1 $ do
    _insertTable1Result <- pqExec $
      insertInto #table1 ( 1 `As` #col1 :& 2 `As` #col2 :& RNil )
      &>>
      insertInto #table1 ( 3 `As` #col1 :& 4 `As` #col2 :& RNil )
    Just selectTable1Result <- flip pqExecParams RNil $
      (select $ starFrom #table1 :: Statement '[] Tables Tables Columns)
    Just (Right value00) <- getvalue selectTable1Result (RowNumber 0) colNum0
    Just (Right value01) <- getvalue selectTable1Result (RowNumber 0) colNum1
    Just (Right value10) <- getvalue selectTable1Result (RowNumber 1) colNum0
    Just (Right value11) <- getvalue selectTable1Result (RowNumber 1) colNum1
    liftBase $ do
      print (value00 :: Int32)
      print (value01 :: Int32)
      print (value10 :: Int32)
      print (value11 :: Int32)
  finish connection2

type Columns = '[ "col1" ::: 'NotNull 'PGInt4, "col2" ::: 'NotNull 'PGInt4]
type Tables = '[ "table1" ::: Columns ]
-- type SumAndCol1 = '[ "sum" ::: 'NotNull 'PGInt4, "col1" ::: 'NotNull 'PGInt4]
-- type StudentsColumns = '["name" ::: 'NotNull 'PGText]
-- type StudentsTable = '["students" ::: StudentsColumns]
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

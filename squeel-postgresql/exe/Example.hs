{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
  , TypeSynonymInstances
#-}

module Main (main,col1,col2) where

import Control.Category ((>>>))
import Control.Monad.Base
import Data.Function ((&))
import Data.Int
import Data.Monoid
import Generics.SOP hiding (from)
import Squeel.PostgreSQL

import qualified Data.ByteString.Char8 as Char8
import qualified GHC.Generics as GHC

main :: IO ()
main = do
  Char8.putStrLn "squeel"
  connectionString <- pure
    "host=localhost port=5432 dbname=exampledb"
  Char8.putStrLn $ "connecting to " <> connectionString
  connection0 <- connectdb connectionString
  Char8.putStrLn "setting up database"
  connection1 <- runPQ (connection0 :: Connection '[]) $ pqExec $
    createTable #students
      (  (text & notNull) `As` #name
      :* Nil ) []
    >>>
    createTable #table1
      (  (int4 & notNull) `As` #col1
      :* (int4 & notNull) `As` #col2
      :* Nil ) []
  Char8.putStrLn "querying"
  connection2 <- runPQ (connection1 :: Connection Tables) $ do
    let
      statement :: Manipulation Tables
        '[ 'Required ('NotNull 'PGInt4)
         , 'Required ('NotNull 'PGInt4)
         , 'Required ('NotNull 'PGInt4)
         , 'Required ('NotNull 'PGInt4)
         ] '[]
      statement =
        insertInto #table1
          (Values
            (param1 `As` #col1 :* param2 `As` #col2 :* Nil)
            [param3 `As` #col1 :* param4 `As` #col2 :* Nil] )
        Conflict ReturningNil
    _ <- pqExecParams statement (1::Int32,2::Int32,3::Int32,4::Int32)
    Just result <- pqExecNil . query $
      selectStar (from (Table (#table1 `As` #table1)))
    value00 <- getValue (RowNumber 0) (columnNumber @0) result
    value01 <- getValue (RowNumber 0) (columnNumber @1) result
    value10 <- getValue (RowNumber 1) (columnNumber @0) result
    value11 <- getValue (RowNumber 1) (columnNumber @1) result
    row0 <- getRow (RowNumber 0) result
    row1 <- getRow (RowNumber 1) result
    liftBase $ do
      print (value00 :: Int32)
      print (value01 :: Int32)
      print (value10 :: Int32)
      print (value11 :: Int32)
      print (row0 :: ColumnsRecord)
      print (row1 :: ColumnsRecord)
  Char8.putStrLn "tearing down database"
  connection3 <- runPQ (connection2 :: Connection Tables) $ pqExec $
    dropTable #table1 >>> dropTable #students
  finish (connection3 :: Connection '[])

type Columns =
  '[ "col1" ::: 'Required ('NotNull 'PGInt4)
   , "col2" ::: 'Required ('NotNull 'PGInt4)
   ]
data ColumnsRecord = ColumnsRecord { col1 :: Int32, col2 :: Int32 }
  deriving (Show, GHC.Generic)
instance Generic ColumnsRecord
instance FromRow Columns ColumnsRecord

type StudentsColumns = '["name" ::: 'Required ('NotNull 'PGText)]
type Tables = '["students" ::: StudentsColumns, "table1" ::: Columns]

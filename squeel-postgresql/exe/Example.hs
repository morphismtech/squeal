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
  Char8.putStrLn "setting up schema"
  connection1 <- runPQ (connection0 :: Connection '[]) $ pqExec $
    createTable #students ((text & notNull) `As` #name :* Nil ) []
    >>>
    createTable #table1
      ((int4 & notNull) `As` #col1 :* (int4 & notNull) `As` #col2 :* Nil) []
  connection2 <- runPQ (connection1 :: Connection Schema) $ do
    let
      manipulation :: Manipulation Schema
        '[ 'Required ('NotNull 'PGint4)
         , 'Required ('NotNull 'PGint4)
         , 'Required ('NotNull 'PGint4)
         , 'Required ('NotNull 'PGint4)
         ] '[]
      manipulation =
        insertInto #table1
          ( Values
            (param (Proxy @1) `As` #col1 :* param (Proxy @2) `As` #col2 :* Nil)
            [param (Proxy @3) `As` #col1 :* param (Proxy @4) `As` #col2 :* Nil]
          ) Conflict ReturningNil
    liftBase $ Char8.putStrLn "manipulating"
    _ <- pqExecParams manipulation (1::Int32,2::Int32,3::Int32,4::Int32)
    liftBase $ Char8.putStrLn "querying"
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
      print (row0 :: Table1Row)
      print (row1 :: Table1Row)
  Char8.putStrLn "tearing down schema"
  connection3 <- runPQ (connection2 :: Connection Schema) $ pqExec $
    dropTable #table1 >>> dropTable #students
  finish (connection3 :: Connection '[])

data Table1Row = Table1Row { col1 :: Int32, col2 :: Int32 }
  deriving (Show, GHC.Generic)
instance Generic Table1Row

type Schema =
  '[ "students" ::: '["name" ::: 'Required ('NotNull 'PGtext)]
   , "table1" :::
       '[ "col1" ::: 'Required ('NotNull 'PGint4)
        , "col2" ::: 'Required ('NotNull 'PGint4)
        ]
   ]

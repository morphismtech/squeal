{-# LANGUAGE
    DataKinds
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Main (main) where

import Control.Category ((>>>))
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Int
import Data.Monoid
import Generics.SOP hiding (from)
import Squeel.PostgreSQL

import qualified Data.ByteString.Char8 as Char8

main :: IO ()
main = do
  Char8.putStrLn "squeel"
  connectionString <- pure
    "host=localhost port=5432 dbname=exampledb"
  Char8.putStrLn $ "connecting to " <> connectionString
  connection0 <- connectdb connectionString
  Char8.putStrLn "setting up database"
  connection1 <- flip execPQ (connection0 :: Connection '[]) $ pqExec $
    createTable #students
      (  (text & notNull) `As` #name
      :* Nil )
    >>>
    createTable #table1
      (  (int4 & notNull) `As` #col1
      :* (int4 & notNull) `As` #col2
      :* Nil )
  Char8.putStrLn "querying"
  connection2 <- flip execPQ (connection1 :: Connection Tables) $ do
    for_ [I i :* I (i+1) :* Nil | i <- [1::Int32,3..9]] $ pqExecParams
      ( into #table1
        (insertRow (param1 `As` #col1 :* param2 `As` #col2 :* Nil))
        Conflict ReturningNil )
    Just result <- pqExecNil . query $
      selectStar (from (Table (#table1 `As` #table1)))
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
  connection3 <- flip execPQ (connection2 :: Connection Tables) $ pqExec $
    dropTable #table1 >>> dropTable #students
  finish (connection3 :: Connection '[])

type Columns =
  '[ "col1" ::: 'Required ('NotNull 'PGInt4)
   , "col2" ::: 'Required ('NotNull 'PGInt4)
   ]
type StudentsColumns = '["name" ::: 'Required ('NotNull 'PGText)]
type Tables = '["students" ::: StudentsColumns, "table1" ::: Columns]

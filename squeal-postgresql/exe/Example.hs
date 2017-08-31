{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Main (main, main2) where

import Control.Monad (void)
import Control.Monad.Base (liftBase)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Text (Text)

import Squeal.PostgreSQL

import qualified Data.ByteString.Char8 as Char8
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

type Schema =
  '[ "users" :::
       '[ "id" ::: 'Optional ('NotNull 'PGint4)
        , "name" ::: 'Required ('NotNull 'PGtext)
        ]
   , "emails" :::
       '[ "id" ::: 'Optional ('NotNull 'PGint4)
        , "user_id" ::: 'Required ('NotNull 'PGint4)
        , "email" ::: 'Required ('Null 'PGtext)
        ]
   ]

setup :: Definition '[] Schema
setup = 
  createTable #users
    ( serial `As` #id :*
      (text & notNull) `As` #name :* Nil )
    [ primaryKey (Column #id :* Nil) ]
  >>>
  createTable #emails
    ( serial `As` #id :*
      (int & notNull) `As` #user_id :*
      text `As` #email :* Nil )
    [ primaryKey (Column #id :* Nil)
    , foreignKey (Column #user_id :* Nil) #users (Column #id :* Nil)
      OnDeleteCascade OnUpdateCascade ]

teardown :: Definition Schema '[]
teardown = dropTable #emails >>> dropTable #users

insertUser :: Manipulation Schema
  '[ 'Required ('NotNull 'PGtext)]
  '[ "fromOnly" ::: 'Required ('NotNull 'PGint4) ]
insertUser = insertInto #users
  ( Values (def `As` #id :* param @1 `As` #name :* Nil) [] )
  OnConflictDoNothing (Returning (#id `As` #fromOnly :* Nil))

insertEmail :: Manipulation Schema
  '[ 'Required ('NotNull 'PGint4), 'Required ('Null 'PGtext)] '[]
insertEmail = insertInto #emails ( Values
  ( def `As` #id :*
    param @1 `As` #user_id :*
    param @2 `As` #email :* Nil) [] )
  OnConflictDoNothing (Returning Nil)

getUsers :: Query Schema '[]
  '[ "userName" ::: 'Required ('NotNull 'PGtext)
   , "userEmail" ::: 'Required ('Null 'PGtext) ]
getUsers = select
  (#u ! #name `As` #userName :* #e ! #email `As` #userEmail :* Nil)
  ( from (Table (#users `As` #u)
    & InnerJoin (Table (#emails `As` #e))
      (#u ! #id .== #e ! #user_id)) )

data User = User { userName :: Text, userEmail :: Maybe Text }
  deriving (Show, GHC.Generic)
instance SOP.Generic User
instance SOP.HasDatatypeInfo User

users :: [User]
users = 
  [ User "Alice" (Just "alice@gmail.com")
  , User "Bob" Nothing
  , User "Carole" (Just "carole@hotmail.com")
  ]

session :: PQ Schema Schema IO ()
session = do
  liftBase $ Char8.putStrLn "manipulating"
  idResults <- traversePrepared insertUser (Only . userName <$> users)
  ids <- traverse (fmap fromOnly . getRow (RowNumber 0)) idResults
  traversePrepared_ insertEmail (zip (ids :: [Int32]) (userEmail <$> users))
  liftBase $ Char8.putStrLn "querying"
  usersResult <- runQuery getUsers
  usersRows <- getRows usersResult
  liftBase $ print (usersRows :: [User])

main :: IO ()
main = do
  Char8.putStrLn "squeal"
  connectionString <- pure
    "host=localhost port=5432 dbname=exampledb"
  Char8.putStrLn $ "connecting to " <> connectionString
  connection0 <- connectdb connectionString
  Char8.putStrLn "setting up schema"
  connection1 <- execPQ (define setup) connection0
  connection2 <- execPQ session connection1
  Char8.putStrLn "tearing down schema"
  connection3 <- execPQ (define teardown) connection2
  finish connection3

main2 :: IO ()
main2 = void $
  withConnection "host=localhost port=5432 dbname=exampledb" . runPQ $
    define setup
    & pqThen session
    & thenDefine teardown

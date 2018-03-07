{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , OverloadedLabels
  , OverloadedStrings
  , OverloadedLists
  , TypeApplications
  , TypeOperators
#-}

module Main (main, main2) where

import Control.Monad (void)
import Control.Monad.Base (liftBase, MonadBase)
import Data.Int (Int16, Int32)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)

import Squeal.PostgreSQL

import qualified Data.ByteString.Char8 as Char8
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

type Schema =
  '[ "users" :::
       '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "name" ::: 'NoDef :=> 'NotNull 'PGtext
        , "vec" ::: 'NoDef :=> 'NotNull ('PGarray 'PGint2)
        ]
   , "emails" :::
       '[  "pk_emails" ::: 'PrimaryKey '["id"]
        , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
        ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
        , "email" ::: 'NoDef :=> 'Null 'PGtext
        ]
   ]

setup :: Definition '[] Schema
setup = 
  createTable #users
    ( serial `As` #id :*
      (text & notNull) `As` #name :*
      (vararray int2 & notNull) `As` #vec :* Nil )
    ( primaryKey (Column #id :* Nil) `As` #pk_users :* Nil )
  >>>
  createTable #emails
    ( serial `As` #id :*
      (int & notNull) `As` #user_id :*
      text `As` #email :* Nil )
    ( primaryKey (Column #id :* Nil) `As` #pk_emails :*
      foreignKey (Column #user_id :* Nil) #users (Column #id :* Nil)
        OnDeleteCascade OnUpdateCascade `As` #fk_user_id :* Nil )

teardown :: Definition Schema '[]
teardown = dropTable #emails >>> dropTable #users

insertUser :: Manipulation Schema '[ 'NotNull 'PGtext, 'NotNull ('PGarray 'PGint2)]
  '[ "fromOnly" ::: 'NotNull 'PGint4 ]
insertUser = insertRows #users
  (Default `As` #id :* Set (param @1) `As` #name :* Set (param @2) `As` #vec :* Nil) []
  OnConflictDoNothing (Returning (#id `As` #fromOnly :* Nil))

insertEmail :: Manipulation Schema '[ 'NotNull 'PGint4, 'Null 'PGtext] '[]
insertEmail = insertRows #emails
  ( Default `As` #id :*
    Set (param @1) `As` #user_id :*
    Set (param @2) `As` #email :* Nil ) []
  OnConflictDoNothing (Returning Nil)

getUsers :: Query Schema '[]
  '[ "userName" ::: 'NotNull 'PGtext
   , "userEmail" ::: 'Null 'PGtext
   , "userVec" ::: 'NotNull ('PGarray 'PGint2)]
getUsers = select
  (#u ! #name `As` #userName :* #e ! #email `As` #userEmail :* #u ! #vec `As` #userVec :* Nil)
  ( from (table (#users `As` #u)
    & innerJoin (table (#emails `As` #e))
      (#u ! #id .== #e ! #user_id)) )

data User = User { userName :: Text, userEmail :: Maybe Text, userVec :: Vector (Maybe Int16) }
  deriving (Show, GHC.Generic)
instance SOP.Generic User
instance SOP.HasDatatypeInfo User

users :: [User]
users = 
  [ User "Alice" (Just "alice@gmail.com") [Nothing, Just 1]
  , User "Bob" Nothing [Just 2, Nothing]
  , User "Carole" (Just "carole@hotmail.com") [Just 3]
  ]

session :: (MonadBase IO pq, MonadPQ Schema pq) => pq ()
session = do
  liftBase $ Char8.putStrLn "manipulating"
  idResults <- traversePrepared insertUser ([(userName user, userVec user) | user <- users])
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
  withConnection "host=localhost port=5432 dbname=exampledb" . goPQ $
    define setup
    & pqThen session
    & thenDefine teardown
  where
    goPQ pq conn = do
      SOP.K x <- runPQ pq conn
      return (x, SOP.K (SOP.unK conn))

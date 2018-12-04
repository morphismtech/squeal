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
  '[ "users" ::: 'Table (
       '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "name" ::: 'NoDef :=> 'NotNull 'PGtext
        , "vec" ::: 'NoDef :=> 'NotNull ('PGvararray ('Null 'PGint2))
        ])
   , "emails" ::: 'Table (
       '[  "pk_emails" ::: 'PrimaryKey '["id"]
        , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
        ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
        , "email" ::: 'NoDef :=> 'Null 'PGtext
        ])
   ]

setup :: Definition '[] Schema
setup = 
  createTable #users
    ( serial `as` #id :*
      (text & notNullable) `as` #name :*
      (vararray int2 & notNullable) `as` #vec )
    ( primaryKey #id `as` #pk_users )
  >>>
  createTable #emails
    ( serial `as` #id :*
      (int & notNullable) `as` #user_id :*
      (text & nullable) `as` #email )
    ( primaryKey #id `as` #pk_emails :*
      foreignKey #user_id #users #id
        OnDeleteCascade OnUpdateCascade `as` #fk_user_id )

teardown :: Definition Schema '[]
teardown = dropTable #emails >>> dropTable #users

insertUser :: Manipulation Schema '[ 'NotNull 'PGtext, 'NotNull ('PGvararray ('Null 'PGint2))]
  '[ "fromOnly" ::: 'NotNull 'PGint4 ]
insertUser = insertInto #users
  (Values_ (defaultAs #id :* param @1 `as` #name :* param @2 `as` #vec))
  (OnConflict (OnConstraint #pk_users) DoNothing) (Returning (#id `as` #fromOnly))

insertEmail :: Manipulation Schema '[ 'NotNull 'PGint4, 'Null 'PGtext] '[]
insertEmail = insertInto #emails
  (Values_ (defaultAs #id :* param @1 `as` #user_id :* param @2 `as` #email))
  (OnConflict (OnConstraint #pk_emails) DoNothing) (Returning Nil)

getUsers :: Query Schema '[]
  '[ "userName" ::: 'NotNull 'PGtext
   , "userEmail" ::: 'Null 'PGtext
   , "userVec" ::: 'NotNull ('PGvararray ('Null 'PGint2))]
getUsers = select
  (#u ! #name `as` #userName :* #e ! #email `as` #userEmail :* #u ! #vec `as` #userVec)
  ( from (table (#users `as` #u)
    & innerJoin (table (#emails `as` #e))
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
  ids <- traverse (fmap fromOnly . getRow 0) idResults
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
main2 =
  void . withConnection "host=localhost port=5432 dbname=exampledb" $
    define setup
    & pqThen session
    & pqThen (define teardown)

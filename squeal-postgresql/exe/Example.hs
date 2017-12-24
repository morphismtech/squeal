{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , OverloadedLabels
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeOperators
#-}

module Main (main, main2) where

import Control.Monad (void)
import Control.Monad.Base (liftBase, MonadBase)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Text (Text)

import Squeal.PostgreSQL

import qualified Data.ByteString.Char8 as Char8
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

type Schema =
  '[ "users" :::
       '[ "pk_id" ::: 'PrimaryKey '["id"] ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "name" ::: 'NoDef :=> 'NotNull 'PGtext
        ]
   , "emails" :::
       '[  "pk_id" ::: 'PrimaryKey '["id"]
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
    ( #id serial :*
      #name (text & notNull) :* Nil )
    ( primaryKey (Column #id :* Nil) `As` #pk_id :* Nil )
  >>>
  createTable #emails
    ( #id serial :*
      #user_id (int & notNull) :*
      #email text :* Nil )
    ( primaryKey (Column #id :* Nil) `As` #pk_id :*
      foreignKey (Column #user_id :* Nil) #users (Column #id :* Nil)
        OnDeleteCascade OnUpdateCascade `As` #fk_user_id :* Nil )

teardown :: Definition Schema '[]
teardown = dropTable #emails >>> dropTable #users

insertUser :: Manipulation Schema '[ 'NotNull 'PGtext]
  '[ "fromOnly" ::: 'NotNull 'PGint4 ]
insertUser = insertRows #users
  (Default `As` #id :* Set (param @1) `As` #name :* Nil) []
  OnConflictDoNothing (Returning (#id `As` #fromOnly :* Nil))

insertEmail :: Manipulation Schema '[ 'NotNull 'PGint4, 'Null 'PGtext] '[]
insertEmail = insertRows #emails
  ( Default `As` #id :*
    Set (param @1) `As` #user_id :*
    Set (param @2) `As` #email :* Nil ) []
  OnConflictDoNothing (Returning Nil)

getUsers :: Query Schema '[]
  '[ "userName" ::: 'NotNull 'PGtext
   , "userEmail" ::: 'Null 'PGtext ]
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

session :: (MonadBase IO pq, MonadPQ Schema pq) => pq ()
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

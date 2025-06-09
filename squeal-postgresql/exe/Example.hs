{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , OverloadedLabels
  , OverloadedStrings
  , OverloadedLists
  , QualifiedDo
  , TypeApplications
  , TypeOperators
#-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main, upsertUser) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int16, Int32)
import Data.Text (Text)
import Data.Vector (Vector)

import Squeal.PostgreSQL

import qualified Control.Monad.Trans.Indexed.Do as Ix
import qualified Data.ByteString.Char8 as Char8
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

type UserSchema =
  '[ "users" ::: 'Table (
       '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "name" ::: 'NoDef :=> 'NotNull 'PGtext
        , "vec" ::: 'NoDef :=> 'NotNull ('PGvararray ('Null 'PGint2))
        ])
   , "emails" ::: 'Table (
       '[  "pk_emails" ::: 'PrimaryKey '["id"]
        , "fk_user_id" ::: 'ForeignKey '["user_id"] "user" "users" '["id"]
        ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
        , "email" ::: 'NoDef :=> 'Null 'PGtext
        ])
   ]

type PublicSchema = '[ "positive" ::: 'Typedef 'PGfloat4 ]

type Schemas
  = '[ "public" ::: PublicSchema, "user" ::: UserSchema ]

setup :: Definition (Public '[]) Schemas
setup =
  createDomain #positive real (#value .> 0 .&& (#value & isNotNull))
  >>>
  createSchema #user
  >>>
  createTable (#user ! #jokers)
    ( serial `as` #id :*
      (text & notNullable) `as` #name :*
      (vararray int2 & notNullable) `as` #vec )
    ( primaryKey #id `as` #pk_users )
  >>>
  alterTableRename (#user ! #jokers) #users
  >>>
  createTable (#user ! #emails)
    ( serial `as` #id :*
      columntypeFrom @Int32 `as` #user_id :*
      columntypeFrom @(Maybe Text) `as` #email )
    ( primaryKey #id `as` #pk_emails :*
      foreignKey #user_id (#user ! #users) #id
        (OnDelete Cascade) (OnUpdate Cascade) `as` #fk_user_id )

teardown :: Definition Schemas (Public '[])
teardown = dropType #positive >>> dropSchemaCascade #user

insertUser :: Statement Schemas (Text, VarArray (Vector (Maybe Int16))) (Only Int32)
insertUser = manipulation $ insertInto (#user ! #users)
  (Values_ (Default `as` #id :* Set (param @1) `as` #name :* Set (param @2) `as` #vec))
  (OnConflict (OnConstraint #pk_users) DoNothing) (Returning_ (#id `as` #fromOnly))

insertEmail :: Statement Schemas (Int32, Maybe Text) ()
insertEmail = manipulation $ insertInto_ (#user ! #emails)
  (Values_ (Default `as` #id :* Set (param @1) `as` #user_id :* Set (param @2) `as` #email))

getUsers :: Statement Schemas () User
getUsers = query $ select_
  (#u ! #name `as` #userName :* #e ! #email `as` #userEmail :* #u ! #vec `as` #userVec)
  ( from (table ((#user ! #users) `as` #u)
    & innerJoin (table ((#user ! #emails) `as` #e))
      (#u ! #id .== #e ! #user_id)) )

upsertUser :: Statement Schemas (Int32, String, VarArray [Maybe Int16]) ()
upsertUser = manipulation $ insertInto (#user ! #users `as` #u)
  (Values_ (Set (param @1) `as` #id :* setUser))
  (OnConflict (OnConstraint #pk_users) (DoUpdate setUser [#u ! #id .== param @1]))
  (Returning_ Nil)
  where
    setUser = Set (param @2) `as` #name :* Set (param @3) `as` #vec :* Nil

data User
  = User
  { userName :: Text
  , userEmail :: Maybe Text
  , userVec :: VarArray (Vector (Maybe Int16))
  } deriving (Show, GHC.Generic)
instance SOP.Generic User
instance SOP.HasDatatypeInfo User

users :: [User]
users =
  [ User "Alice" (Just "alice@gmail.com") (VarArray [Just 1,Just 2,Nothing])
  , User "Bob" Nothing (VarArray [Nothing,Just (-3)])
  , User "Carole" (Just "carole@hotmail.com") (VarArray [Just 3,Nothing, Just 4])
  ]

session :: (MonadResult pq, MonadIO pq, MonadPQ Schemas pq) => pq ()
session = do
  liftIO $ Char8.putStrLn "===> manipulating"
  userIdResults <- forPrepared insertUser
    [(userName user, userVec user) | user <- users]
  userIds <- traverse (fmap fromOnly . getRow 0) (userIdResults :: [Result (Only Int32)])
  forPrepared_ insertEmail
    (zip (userIds :: [Int32]) (userEmail <$> users))

  liftIO $ Char8.putStrLn "===> querying: users"
  usersResult <- execute getUsers
  usersRows <- getRows usersResult
  liftIO $ print (usersRows :: [User])

main :: IO ()
main = do
  let connStr = "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
  Char8.putStrLn "===> squeal"
  Char8.putStrLn $ "connecting to " <> connStr
  withConnection connStr $ Ix.do
    liftIO $ Char8.putStrLn "===> setting up schema"
    define setup
    liftIO $ Char8.putStrLn "===> setting up schema"
    session
    liftIO $ Char8.putStrLn "===> tearing down schema"
    define teardown

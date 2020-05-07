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

import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int16, Int32)
import Data.Text (Text)
import Data.Vector (Vector)

import Squeal.PostgreSQL

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

type OrgSchema =
  '[ "organizations" ::: 'Table (
        '[ "pk_organizations" ::: 'PrimaryKey '["id"] ] :=>
        '[ "id" ::: 'Def :=> 'NotNull 'PGint4
         , "name" ::: 'NoDef :=> 'NotNull 'PGtext
         ])
   , "members" ::: 'Table (
        '[ "fk_member" ::: 'ForeignKey '["member"] "user" "users" '["id"]
         , "fk_organization" ::: 'ForeignKey '["organization"] "org" "organizations" '["id"] ] :=>
        '[ "member" ::: 'NoDef :=> 'NotNull 'PGint4
         , "organization" ::: 'NoDef :=> 'NotNull 'PGint4 ])
   ]

type Schemas 
  = '[ "public" ::: PublicSchema, "user" ::: UserSchema, "org" ::: OrgSchema ]

setup :: Definition (Public '[]) Schemas
setup =
  createDomain #positive real (#value .> 0 .&& (#value & isNotNull))
  >>>
  createSchema #user
  >>>
  createSchema #org
  >>>
  createTable (#user ! #users)
    ( serial `as` #id :*
      (text & notNullable) `as` #name :*
      (vararray int2 & notNullable) `as` #vec )
    ( primaryKey #id `as` #pk_users )
  >>>
  createTable (#user ! #emails)
    ( serial `as` #id :*
      columntypeFrom @Int32 `as` #user_id :*
      columntypeFrom @(Maybe Text) `as` #email )
    ( primaryKey #id `as` #pk_emails :*
      foreignKey #user_id (#user ! #users) #id
        OnDeleteCascade OnUpdateCascade `as` #fk_user_id )
  >>>
  createTable (#org ! #organizations)
    ( serial `as` #id :*
      (text & notNullable) `as` #name )
    ( primaryKey #id `as` #pk_organizations )
  >>>
  createTable (#org ! #members)
    ( notNullable int4 `as` #member :*
      notNullable int4 `as` #organization )
    ( foreignKey #member (#user ! #users) #id
        OnDeleteCascade OnUpdateCascade `as` #fk_member :*
      foreignKey #organization (#org ! #organizations) #id
        OnDeleteCascade OnUpdateCascade `as` #fk_organization )
      
teardown :: Definition Schemas (Public '[])
teardown = dropType #positive >>> dropSchemaCascade #user >>> dropSchemaCascade #org

insertUser :: Manipulation_ Schemas (Text, VarArray (Vector (Maybe Int16))) (Only Int32)
insertUser = insertInto (#user ! #users)
  (Values_ (Default `as` #id :* Set (param @1) `as` #name :* Set (param @2) `as` #vec))
  (OnConflict (OnConstraint #pk_users) DoNothing) (Returning_ (#id `as` #fromOnly))

insertEmail :: Manipulation_ Schemas (Int32, Maybe Text) ()
insertEmail = insertInto_ (#user ! #emails)
  (Values_ (Default `as` #id :* Set (param @1) `as` #user_id :* Set (param @2) `as` #email))

getUsers :: Query_ Schemas () User
getUsers = select_
  (#u ! #name `as` #userName :* #e ! #email `as` #userEmail :* #u ! #vec `as` #userVec)
  ( from (table ((#user ! #users) `as` #u)
    & innerJoin (table ((#user ! #emails) `as` #e))
      (#u ! #id .== #e ! #user_id)) )

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

session :: (MonadIO pq, MonadPQ Schemas pq) => pq ()
session = do
  liftIO $ Char8.putStrLn "manipulating"
  idResults <- traversePrepared insertUser ([(userName user, userVec user) | user <- users])
  ids <- traverse (fmap fromOnly . getRow 0) (idResults :: [Result (Only Int32)])
  traversePrepared_ insertEmail (zip (ids :: [Int32]) (userEmail <$> users))
  liftIO $ Char8.putStrLn "querying"
  usersResult <- runQuery getUsers
  usersRows <- getRows usersResult
  liftIO $ print (usersRows :: [User])

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
  withConnection "host=localhost port=5432 dbname=exampledb" $
    define setup
    & pqThen session
    & pqThen (define teardown)

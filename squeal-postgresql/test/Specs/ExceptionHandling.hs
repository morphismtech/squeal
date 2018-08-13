{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module ExceptionHandling
  (
    specs
  )
where

import           Control.Monad               (void)
import           Control.Monad.Base          (MonadBase)
import qualified Data.ByteString.Char8       as Char8
import           Data.Int                    (Int16)
import           Data.Text                   (Text)
import           Data.Vector                 (Vector)
import qualified Generics.SOP                as SOP
import qualified GHC.Generics                as GHC
import           Squeal.PostgreSQL
import           Squeal.PostgreSQL.Migration
import           Test.Hspec

type Schema =
  '[ "users" ::: 'Table (
       '[ "pk_users" ::: 'PrimaryKey '["id"]
        , "unique_names" ::: 'Unique '["name"]
        ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "name" ::: 'NoDef :=> 'NotNull 'PGtext
        , "vec" ::: 'NoDef :=> 'NotNull ('PGvararray 'PGint2)
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

data User =
  User { userName  :: Text
       , userEmail :: Maybe Text
       , userVec   :: Vector (Maybe Int16) }
  deriving (Show, GHC.Generic)
instance SOP.Generic User
instance SOP.HasDatatypeInfo User

insertUser :: Manipulation Schema '[ 'NotNull 'PGtext, 'NotNull ('PGvararray 'PGint2)]
  '[ "fromOnly" ::: 'NotNull 'PGint4 ]
insertUser = insertRows #users
  (Default `as` #id :* Set (param @1) `as` #name :* Set (param @2) `as` #vec) []
  OnConflictDoRaise (Returning (#id `as` #fromOnly))

setup :: Definition '[] Schema
setup =
  createTable #users
    ( serial `as` #id :*
      (text & notNullable) `as` #name :*
      (vararray int2 & notNullable) `as` #vec )
    ( primaryKey #id `as` #pk_users
    :* unique #name `as` #unique_names )
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

migration :: Migration IO '[] Schema
migration = Migration { name = "test"
                      , up = void $ define setup
                      , down = void $ define teardown }

setupDB :: IO ()
setupDB = void . withConnection connectionString $
  manipulate (UnsafeManipulation "SET client_min_messages = warning;")
  & pqThen $ migrateUp $ single migration

dropDB :: IO ()
dropDB = void . withConnection connectionString $
  manipulate (UnsafeManipulation "SET client_min_messages = error;")
  & pqThen $ migrateDown $ single migration

connectionString :: Char8.ByteString
connectionString = "host=localhost port=5432 dbname=exampledb"

testUser :: User
testUser = User "TestUser" Nothing []

newUser :: (MonadBase IO m, MonadPQ Schema m) => User -> m ()
newUser u = void $ manipulateParams insertUser (userName u, userVec u)

insertUserTwice :: (MonadBase IO m, MonadPQ Schema m) => m ()
insertUserTwice = newUser testUser >> newUser testUser

specs :: SpecWith ()
specs = before_ setupDB $ after_ dropDB $
  describe "Exceptions" $ do
    it "Should be raised in transactions and cause rollback" $
      withConnection connectionString insertUserTwice
       `shouldThrow` (const True :: Selector SquealException)

    it "Should be raised outside of transactions" $
      withConnection connectionString (transactionally_ insertUserTwice)
       `shouldThrow` (const True :: Selector SquealException)

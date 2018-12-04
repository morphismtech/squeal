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
  ( specs
  , User (..)
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

data User =
  User { userName  :: Text
       , userEmail :: Maybe Text
       , userVec   :: Vector (Maybe Int16) }
  deriving (Show, GHC.Generic)
instance SOP.Generic User
instance SOP.HasDatatypeInfo User

insertUser :: Manipulation Schema '[ 'NotNull 'PGtext, 'NotNull ('PGvararray ('Null 'PGint2))]
  '[ "fromOnly" ::: 'NotNull 'PGint4 ]
insertUser = insertInto #users
  (Values_ (defaultAs #id :* param @1 `as` #name :* param @2 `as` #vec))
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
  migrateUp $ single migration

dropDB :: IO ()
dropDB = void . withConnection connectionString $
  migrateDown $ single migration

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

    let
      dupKeyErr = PQException FatalError (Just "23505")
        (Just "ERROR:  duplicate key value violates unique constraint \"unique_names\"\nDETAIL:  Key (name)=(TestUser) already exists.\n")

    it "should be thrown for unique constraint violation in a manipulation" $
      withConnection connectionString insertUserTwice
       `shouldThrow` (== dupKeyErr)

    it "should be rethrown for unique constraint violation in a manipulation by a transaction" $
      withConnection connectionString (transactionally_ insertUserTwice)
       `shouldThrow` (== dupKeyErr)

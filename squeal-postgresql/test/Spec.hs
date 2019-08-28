{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DuplicateRecordFields
  , FlexibleContexts
  , OverloadedLabels
  , OverloadedLists
  , OverloadedStrings
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
#-}

module Main (main) where

import Control.Concurrent.Async (replicateConcurrently)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Test.Hspec

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL

main :: IO ()
main = hspec spec

type Schemas = Public
  '[ "users" ::: 'Table (
     '[ "pk_users" ::: 'PrimaryKey '["id"]
      , "unique_names" ::: 'Unique '["name"]
      ] :=>
     '[ "id" ::: 'Def :=> 'NotNull 'PGint4
      , "name" ::: 'NoDef :=> 'NotNull 'PGtext ] ) ]

data User = User
  { userName  :: Text
  } deriving stock (Eq, Show, GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

insertUser :: Manipulation_ Schemas User ()
insertUser = insertInto_ #users
  (Values_ (Default `as` #id :* Set (param @1) `as` #name))

setup :: Definition (Public '[]) Schemas
setup =
  createTable #users
    ( serial `as` #id :*
      notNullable text `as` #name )
    ( primaryKey #id `as` #pk_users :*
      unique #name `as` #unique_names )

teardown :: Definition Schemas (Public '[])
teardown = dropTable #users

silence :: MonadPQ schemas pq => pq ()
silence = manipulate_ $
  UnsafeManipulation "SET client_min_messages TO WARNING;"

setupDB :: IO ()
setupDB = withConnection connectionString $
  silence & pqThen (define setup)

dropDB :: IO ()
dropDB = withConnection connectionString $
  silence & pqThen (define teardown)

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=exampledb"

spec :: Spec
spec = before_ setupDB . after_ dropDB $ do

  describe "Exceptions" $ do

    let
      testUser = User "TestUser"
      newUser = manipulateParams_ insertUser
      insertUserTwice = newUser testUser >> newUser testUser
      err23505 = PQException $ PQState FatalError (Just "23505")
        (Just "ERROR:  duplicate key value violates unique constraint \"unique_names\"\nDETAIL:  Key (name)=(TestUser) already exists.\n")

    it "should be thrown for constraint violation" $
      withConnection connectionString insertUserTwice
       `shouldThrow` (== err23505)

    it "should be rethrown for constraint violation in a transaction" $
      withConnection connectionString (transactionally_ insertUserTwice)
       `shouldThrow` (== err23505)

  describe "Pools" $

    it "should manage concurrent transactions" $ do
      pool <- createConnectionPool
        "host=localhost port=5432 dbname=exampledb" 1 0.5 10
      let
        query :: Query_ (Public '[]) () (Only Char)
        query = values_ (literal 'a' `as` #fromOnly)
        session = usingConnectionPool pool . transactionally_ $ do
          result <- runQuery query
          Just (Only chr) <- firstRow result
          return chr
      chrs <- replicateConcurrently 10 session
      chrs `shouldSatisfy` all (== 'a')

  describe "Ranges" $

    it "should correctly decode ranges" $ do

      rangesOut <- withConnection connectionString $ do
        let
          query :: Query_ (Public '[]) () (Only (Range Int32))
          query = values
            ( range int4range (atLeast 3) `as` #fromOnly )
            [ range int4range (3 <=..< 5) `as` #fromOnly
            , range int4range Empty `as` #fromOnly
            , range int4range whole `as` #fromOnly ]
        getRows =<< runQuery query
      (fromOnly <$> rangesOut :: [Range Int32]) `shouldBe`
        [ atLeast 3, 3 <=..< 5, Empty, whole ]

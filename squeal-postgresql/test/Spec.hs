{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DerivingVia
  , DuplicateRecordFields
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , TypeSynonymInstances
  , TypeInType
  , TypeOperators
#-}

module Main (main) where

import Control.Concurrent.Async (replicateConcurrently)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Test.Hspec

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL

main :: IO ()
main = hspec spec

type UsersConstraints =
  '[ "pk_users" ::: 'PrimaryKey '["id"]
   , "unique_names" ::: 'Unique '["name"] ]

type UsersColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext ]

type Schema = '[ "users" ::: 'Table (UsersConstraints :=> UsersColumns) ]

type Schemas = '[ "public" ::: Schema ]

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

silence :: MonadPQ db pq => pq ()
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

data Person = Person { name :: String, age :: Int32 }
  deriving (Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)
  -- deriving (FromValue PGperson) via (Composite Person)
  deriving (ToParam PersonDB PGperson) via (Composite Person)
type PGperson = 'PGcomposite
  '["name" ::: 'NotNull 'PGtext, "age" ::: 'NotNull 'PGint4]
type PersonDB = '["public" ::: '["person" ::: 'Typedef PGperson]]
type instance PG Person = PGperson
instance PGTyped PersonDB PGperson where
  pgtype = typedef #person
instance FromValue PGperson Person where
  fromValue = getComposite <$> fromValue @PGperson
instance OidOf PersonDB PGperson where
  oidOf = oidOfTypedef #person
instance OidOfArray PersonDB PGperson where
  oidOfArray = oidOfArrayTypedef #person

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
        qry :: Query_ (Public '[]) () (Only Char)
        qry = values_ (literal 'a' `as` #fromOnly)
        session = usingConnectionPool pool . transactionally_ $ do
          result <- runQuery qry
          Just (Only chr) <- firstRow result
          return chr
      chrs <- replicateConcurrently 10 session
      chrs `shouldSatisfy` all (== 'a')

  describe "Ranges" $

    it "should correctly decode ranges" $ do

      rangesOut <- withConnection connectionString $ do
        let
          qry :: Query_ (Public '[]) () (Only (Range Int32))
          qry = values
            ( range int4range (atLeast 3) `as` #fromOnly )
            [ range int4range (3 <=..< 5) `as` #fromOnly
            , range int4range Empty `as` #fromOnly
            , range int4range whole `as` #fromOnly ]
        getRows =<< runQuery qry
      (fromOnly <$> rangesOut :: [Range Int32]) `shouldBe`
        [ atLeast 3, 3 <=..< 5, Empty, whole ]

  describe "Parameters" $ do

    it "should run queries that don't reference all their parameters" $ do

      out <- withConnection connectionString $ do
        let
          qry :: Query_ (Public '[]) (Char,Int32) (Only Int32)
          qry = values_ (param @2 `as` #fromOnly)
        firstRow =<< runQueryParams qry ('a', 3 :: Int32)
      (fromOnly <$> out :: Maybe Int32) `shouldBe` Just 3

  describe "User Types" $ do

    it "should be definable" $ do

      let
        setupPerson :: Definition (Public '[]) PersonDB
        setupPerson = createTypeCompositeFrom @Person #person

        teardownPerson :: Definition PersonDB (Public '[])
        teardownPerson = dropType #person

        roundtrip :: Query_ PersonDB (Only Person) (Only Person)
        roundtrip = values_ (param @1 `as` #fromOnly)

        roundtrip_array :: Query_ PersonDB
          (Only (VarArray [Person])) (Only (VarArray [Person]))
        roundtrip_array = values_ (param @1 `as` #fromOnly)

        adam = Person "Adam" 6000
        lucy = Person "Lucy" 2420000
        people = VarArray [adam, lucy]

        session :: PQ PersonDB PersonDB IO ()
        session = do
          out <- firstRow =<< runQueryParams roundtrip (Only adam)
          out_array <- firstRow =<<
            runQueryParams roundtrip_array (Only people)
          liftIO $ do
            out `shouldBe` Just (Only adam)
            out_array `shouldBe` Just (Only people)

      withConnection connectionString $
        define setupPerson
        & pqThen session
        & pqThen (define teardownPerson)

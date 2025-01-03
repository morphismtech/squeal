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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main, main2, upsertUser) where

import Control.Monad.Except (MonadError (throwError))
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
         , "type" ::: 'NoDef :=> 'NotNull 'PGtext
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
  >>>
  createTable (#org ! #organizations)
    ( serial `as` #id :*
      (text & notNullable) `as` #name :*
      (text & notNullable) `as` #type )
    ( primaryKey #id `as` #pk_organizations )
  >>>
  createTable (#org ! #members)
    ( notNullable int4 `as` #member :*
      notNullable int4 `as` #organization )
    ( foreignKey #member (#user ! #users) #id
        (OnDelete Cascade) (OnUpdate Cascade) `as` #fk_member :*
      foreignKey #organization (#org ! #organizations) #id
        (OnDelete Cascade) (OnUpdate Cascade) `as` #fk_organization )

teardown :: Definition Schemas (Public '[])
teardown = dropType #positive >>> dropSchemaCascade #user >>> dropSchemaCascade #org

insertUser :: Manipulation_ Schemas (Text, VarArray (Vector (Maybe Int16))) (Only Int32)
insertUser = insertInto (#user ! #users)
  (Values_ (Default `as` #id :* Set (param @1) `as` #name :* Set (param @2) `as` #vec))
  (OnConflict (OnConstraint #pk_users) DoNothing) (Returning_ (#id `as` #fromOnly))

insertEmail :: Manipulation_ Schemas (Int32, Maybe Text) ()
insertEmail = insertInto_ (#user ! #emails)
  (Values_ (Default `as` #id :* Set (param @1) `as` #user_id :* Set (param @2) `as` #email))

insertOrganization :: Manipulation_ Schemas (Text, OrganizationType) (Only Int32)
insertOrganization = insertInto (#org ! #organizations)
  (Values_ (Default `as` #id :* Set (param @1) `as` #name :* Set (param @2) `as` #type))
  (OnConflict (OnConstraint #pk_organizations) DoNothing) (Returning_ (#id `as` #fromOnly))

getUsers :: Query_ Schemas () User
getUsers = select_
  (#u ! #name `as` #userName :* #e ! #email `as` #userEmail :* #u ! #vec `as` #userVec)
  ( from (table ((#user ! #users) `as` #u)
    & innerJoin (table ((#user ! #emails) `as` #e))
      (#u ! #id .== #e ! #user_id)) )

getOrganizations :: Query_ Schemas () Organization
getOrganizations = select_
  ( #o ! #id `as` #orgId :*
    #o ! #name `as` #orgName :*
    #o ! #type `as` #orgType
  )
  (from (table (#org ! #organizations `as` #o)))

getOrganizationsBy ::
  forall hsty.
  (ToPG Schemas hsty) =>
  Condition
    'Ungrouped
    '[]
    '[]
    Schemas
    '[NullPG hsty]
    '["o" ::: ["id" ::: NotNull PGint4, "name" ::: NotNull PGtext, "type" ::: NotNull PGtext]] ->
  Query_ Schemas (Only hsty) Organization
getOrganizationsBy condition =
  select_
    ( #o ! #id `as` #orgId :*
      #o ! #name `as` #orgName :*
      #o ! #type `as` #orgType
    )
    (
      from (table (#org ! #organizations `as` #o))
      & where_ condition
    )

upsertUser :: Manipulation_ Schemas (Int32, String, VarArray [Maybe Int16]) ()
upsertUser = insertInto (#user ! #users `as` #u)
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

data Organization
  = Organization
  { orgId :: Int32
  , orgName :: Text
  , orgType :: OrganizationType
  } deriving (Show, GHC.Generic)
instance SOP.Generic Organization
instance SOP.HasDatatypeInfo Organization

data OrganizationType
  = ForProfit
  | NonProfit
  deriving (Show, GHC.Generic)
instance SOP.Generic OrganizationType
instance SOP.HasDatatypeInfo OrganizationType

instance IsPG OrganizationType where
  type PG OrganizationType = 'PGtext
instance ToPG db OrganizationType where
  toPG = toPG . toText
    where
      toText ForProfit = "for-profit" :: Text
      toText NonProfit = "non-profit" :: Text

instance FromPG OrganizationType where
  fromPG = do
    value <- fromPG @Text
    fromText value
    where
      fromText "for-profit" = pure ForProfit
      fromText "non-profit" = pure NonProfit
      fromText value = throwError $ "Invalid organization type: \"" <> value <> "\""

organizations :: [Organization]
organizations =
  [ Organization { orgId = 1, orgName = "ACME", orgType = ForProfit }
  , Organization { orgId = 2, orgName = "Haskell Foundation", orgType = NonProfit }
  ]

session :: (MonadIO pq, MonadPQ Schemas pq) => pq ()
session = do
  liftIO $ Char8.putStrLn "===> manipulating"
  userIdResults <- traversePrepared insertUser [(userName user, userVec user) | user <- users]
  userIds <- traverse (fmap fromOnly . getRow 0) (userIdResults :: [Result (Only Int32)])
  traversePrepared_ insertEmail (zip (userIds :: [Int32]) (userEmail <$> users))

  orgIdResults <- traversePrepared
    insertOrganization
    [(orgName organization, orgType organization) | organization <- organizations]
  _ <- traverse (fmap fromOnly . getRow 0) (orgIdResults :: [Result (Only Int32)])

  liftIO $ Char8.putStrLn "===> querying: users"
  usersResult <- runQuery getUsers
  usersRows <- getRows usersResult
  liftIO $ print (usersRows :: [User])

  liftIO $ Char8.putStrLn "===> querying: organizations: all"
  organizationsResult1 <- runQuery getOrganizations
  organizationRows1 <- getRows organizationsResult1
  liftIO $ print (organizationRows1 :: [Organization])

  liftIO $ Char8.putStrLn "===> querying: organizations: by ID (2)"
  organizationsResult2 <- runQueryParams
    (getOrganizationsBy @Int32 ((#o ! #id) .== param @1)) (Only (2 :: Int32))
  organizationRows2 <- getRows organizationsResult2
  liftIO $ print (organizationRows2 :: [Organization])

  liftIO $ Char8.putStrLn "===> querying: organizations: by name (ACME)"
  organizationsResult3 <- runQueryParams
    (getOrganizationsBy @Text ((#o ! #name) .== param @1)) (Only ("ACME" :: Text))
  organizationRows3 <- getRows organizationsResult3
  liftIO $ print (organizationRows3 :: [Organization])

  liftIO $ Char8.putStrLn "===> querying: organizations: by type (non-profit)"
  organizationsResult4 <- runQueryParams
    (getOrganizationsBy @Text ((#o ! #type) .== param @1)) (Only NonProfit)
  organizationRows4 <- getRows organizationsResult4
  liftIO $ print (organizationRows4 :: [Organization])

main :: IO ()
main = do
  Char8.putStrLn "===> squeal"
  connectionString <- pure
    "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
  Char8.putStrLn $ "connecting to " <> connectionString
  connection0 <- connectdb connectionString

  Char8.putStrLn "===> setting up schema"
  connection1 <- execPQ (define setup) connection0
  connection2 <- execPQ session connection1

  Char8.putStrLn "===> tearing down schema"
  connection3 <- execPQ (define teardown) connection2
  finish connection3

main2 :: IO ()
main2 =
  withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $
    define setup
    & pqThen session
    & pqThen (define teardown)

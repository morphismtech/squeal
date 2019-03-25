{-|
Module: Squeal.PostgreSQL.Migration
Description: Squeal migrations
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

This module defines a `Migration` type to safely
change the schema of your database over time. Let's see an example!

>>> :set -XDataKinds -XOverloadedLabels
>>> :set -XOverloadedStrings -XFlexibleContexts -XTypeOperators
>>> :{
type UsersTable =
  '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext
   ]
:}

>>> :{
type EmailsTable =
  '[  "pk_emails" ::: 'PrimaryKey '["id"]
   , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
   ] :=>
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
   , "email" ::: 'NoDef :=> 'Null 'PGtext
   ]
:}

>>> :{
let
  makeUsers :: Migration IO (Public '[]) '["public" ::: '["users" ::: 'Table UsersTable]]
  makeUsers = Migration
    { name = "make users table"
    , up = void . define $
        createTable #users
        ( serial `as` #id :*
          (text & notNullable) `as` #name )
        ( primaryKey #id `as` #pk_users )
    , down = void . define $ dropTable #users
    }
:}

>>> :{
let
  makeEmails :: Migration IO '["public" ::: '["users" ::: 'Table UsersTable]]
    '["public" ::: '["users" ::: 'Table UsersTable, "emails" ::: 'Table EmailsTable]]
  makeEmails = Migration
    { name = "make emails table"
    , up = void . define $
        createTable #emails
          ( serial `as` #id :*
            (int & notNullable) `as` #user_id :*
            (text & nullable) `as` #email )
          ( primaryKey #id `as` #pk_emails :*
            foreignKey #user_id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_user_id )
    , down = void . define $ dropTable #emails
    }
:}

Now that we have a couple migrations we can chain them together.

>>> let migrations = makeUsers :>> makeEmails :>> Done

Finally, we can run the migration and even roll it back.

>>> import Control.Monad.IO.Class
>>> :{
withConnection "host=localhost port=5432 dbname=exampledb" $
  manipulate (UnsafeManipulation "SET client_min_messages TO WARNING;")
    -- suppress notices
  & pqThen (liftIO (putStrLn "Migrate"))
  & pqThen (migrateUp migrations)
  & pqThen (liftIO (putStrLn "Rollback"))
  & pqThen (migrateDown migrations)
:}
Migrate
Rollback
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Squeal.PostgreSQL.Migration
  ( -- * Migration
    Migration (..)
  , migrateUp
  , migrateDown
    -- * Migration table
  , MigrationsTable
  , createMigrations
  , insertMigration
  , deleteMigration
  , selectMigration
   -- * CLI
  , defaultMain
  ) where

import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Data.ByteString             (ByteString)
import           Data.Foldable               (traverse_)
import           Data.Function               ((&))
import           Data.List                   ((\\))
import           Data.Text                   (Text)
import qualified Data.Text.IO                as T (putStrLn)
import           Data.Time                   (UTCTime)
import           Generics.SOP                (K (..))
import qualified Generics.SOP                as SOP
import qualified GHC.Generics                as GHC
import           Squeal.PostgreSQL
import           System.Environment

-- | A `Migration` should contain an inverse pair of
-- `up` and `down` instructions and a unique `name`.
data Migration io schemas0 schemas1 = Migration
  { name :: Text -- ^ The `name` of a `Migration`.
    -- Each `name` in a `Migration` should be unique.
  , up   :: PQ schemas0 schemas1 io () -- ^ The `up` instruction of a `Migration`.
  , down :: PQ schemas1 schemas0 io () -- ^ The `down` instruction of a `Migration`.
  } deriving (GHC.Generic)

-- | Run `Migration`s by creating the `MigrationsTable`
-- if it does not exist and then in a transaction, for each each `Migration`
-- query to see if the `Migration` is executed. If not, then
-- execute the `Migration` and insert its row in the `MigrationsTable`.
migrateUp
  :: MonadBaseControl IO io
  => AlignedList (Migration io) schemas0 schemas1 -- ^ migrations to run
  -> PQ schemas0 schemas1 io ()
migrateUp migration = unsafePQ $
  define createMigrations
  & pqBind okResult
  & pqThen (transactionallySchema_ (upMigrations migration))
  where

    upMigrations
      :: MonadBaseControl IO io
      => AlignedList (Migration io) schemas0 schemas1
      -> PQ MigrationsSchemas MigrationsSchemas io ()
    upMigrations = \case
      Done -> return ()
      step :>> steps -> upMigration step & pqThen (upMigrations steps)

    upMigration
      :: MonadBase IO io
      => Migration io schemas0 schemas1
      -> PQ MigrationsSchemas MigrationsSchemas io ()
    upMigration step =
      queryExecuted step
      & pqBind ( \ executed -> unless (executed == 1)
        $ unsafePQ (up step)
        & pqThen (manipulateParams insertMigration (Only (name step)))
        -- insert execution record
        & pqBind okResult )

    queryExecuted
      :: MonadBase IO io
      => Migration io schemas0 schemas1
      -> PQ MigrationsSchemas MigrationsSchemas io Row
    queryExecuted step = do
      result <- runQueryParams selectMigration (Only (name step))
      okResult result
      ntuples result

-- | Rewind `Migration`s by creating the `MigrationsTable`
-- if it does not exist and then in a transaction, for each each `Migration`
-- query to see if the `Migration` is executed. If it is, then
-- rewind the `Migration` and delete its row in the `MigrationsTable`.
migrateDown
  :: MonadBaseControl IO io
  => AlignedList (Migration io) schemas0 schemas1 -- ^ migrations to rewind
  -> PQ schema1 schema0 io ()
migrateDown migrations = unsafePQ $
  define createMigrations
  & pqBind okResult
  & pqThen (transactionallySchema_ (downMigrations migrations))
  where

    downMigrations
      :: MonadBaseControl IO io
      => AlignedList (Migration io) schemas0 schemas1
      -> PQ MigrationsSchemas MigrationsSchemas io ()
    downMigrations = \case
      Done -> return ()
      step :>> steps -> downMigrations steps & pqThen (downMigration step)

    downMigration
      :: MonadBase IO io
      => Migration io schemas0 schemas1
      -> PQ MigrationsSchemas MigrationsSchemas io ()
    downMigration step =
      queryExecuted step
      & pqBind ( \ executed -> unless (executed == 0)
        $ unsafePQ (down step)
        & pqThen (manipulateParams deleteMigration (Only (name step)))
        -- delete execution record
        & pqBind okResult )

    queryExecuted
      :: MonadBase IO io
      => Migration io schemas0 schemas1
      -> PQ MigrationsSchemas MigrationsSchemas io Row
    queryExecuted step = do
      result <- runQueryParams selectMigration (Only (name step))
      okResult result
      ntuples result

okResult :: MonadBase IO io => K Result results -> PQ schema schema io ()
okResult result = do
  status <- resultStatus result
  when (not (status `elem` [CommandOk, TuplesOk])) $ do
    errorMessageMaybe <- resultErrorMessage result
    case errorMessageMaybe of
      Nothing  -> error "migration: unknown error"
      Just msg -> error ("migration: " <> show msg)

-- | The `TableType` for a Squeal migration.
type MigrationsTable =
  '[ "migrations_unique_name" ::: 'Unique '["name"]] :=>
  '[ "name"        ::: 'NoDef :=> 'NotNull 'PGtext
   , "executed_at" :::   'Def :=> 'NotNull 'PGtimestamptz
   ]

data MigrationRow =
  MigrationRow { migrationName :: Text
               , executedAt    :: UTCTime }
  deriving (GHC.Generic, Show)

instance SOP.Generic MigrationRow
instance SOP.HasDatatypeInfo MigrationRow

type MigrationsSchema = '["schema_migrations" ::: 'Table MigrationsTable]
type MigrationsSchemas = Public MigrationsSchema

-- | Creates a `MigrationsTable` if it does not already exist.
createMigrations :: Definition MigrationsSchemas MigrationsSchemas
createMigrations =
  createTableIfNotExists #schema_migrations
    ( (text & notNullable) `as` #name :*
      (timestampWithTimeZone & notNullable & default_ currentTimestamp)
        `as` #executed_at )
    ( unique #name `as` #migrations_unique_name )

-- | Inserts a `Migration` into the `MigrationsTable`
insertMigration :: Manipulation_ MigrationsSchemas (Only Text) ()
insertMigration = insertInto_ #schema_migrations . Values_ $
  (param @1) `as` #name :* defaultAs #executed_at

-- | Deletes a `Migration` from the `MigrationsTable`
deleteMigration :: Manipulation_ MigrationsSchemas (Only Text) ()
deleteMigration = deleteFrom_ #schema_migrations (#name .== param @1)

-- | Selects a `Migration` from the `MigrationsTable`, returning
-- the time at which it was executed.
selectMigration
  :: Query_ MigrationsSchemas (Only Text) (P ("executed_at" ::: UTCTime))
selectMigration = select_ #executed_at
  $ from (table (#schema_migrations))
  & where_ (#name .== param @1)

fetchAllMigrations :: Query_ MigrationsSchemas () MigrationRow
fetchAllMigrations = select_
  (#name `as` #migrationName :* #executed_at `as` #executedAt)
  (from (table #schema_migrations))

unsafePQ :: (Functor m) => PQ db0 db1 m x -> PQ db0' db1' m x
unsafePQ (PQ pq) = PQ $ fmap (SOP.K . SOP.unK) . pq . SOP.K . SOP.unK

data Command = Display | AllUp | AllDown deriving (GHC.Generic, Show)

defaultMain
  :: ByteString
  -- ^ connection string
  -> AlignedList (Migration IO) db0 db1
  -- ^ migrations
  -> IO ()
defaultMain connectTo migrations = do
  command <- readCommandFromArgs
  maybe (pure ()) (performCommand connectTo migrations) command

readCommandFromArgs :: IO (Maybe Command)
readCommandFromArgs = getArgs >>= \case
  ["migrate"]  -> pure . Just $ AllUp
  ["rollback"] -> pure . Just $ AllDown
  ["status"]   -> pure . Just $ Display
  _               -> displayUsage >> pure Nothing

displayUsage :: IO ()
displayUsage = do
  putStrLn "Invalid command. Use:"
  putStrLn "migrate    to run all available migrations"
  putStrLn "rollback   to rollback all available migrations"
  putStrLn "status     to display migrations run and migrations left to run"

getRunMigrationNames :: (MonadBase IO m) => PQ db0 db0 m [Text]
getRunMigrationNames =
  fmap migrationName <$> (unsafePQ (define createMigrations & pqThen (runQuery fetchAllMigrations)) >>= getRows)

displayListOfNames :: [Text] -> IO ()
displayListOfNames [] = T.putStrLn "\tNone"
displayListOfNames xs =
  let singleName n = T.putStrLn $ "\t- " <> n
  in traverse_ singleName xs

displayUnrunned :: [Text] -> IO ()
displayUnrunned unrunned =
  T.putStrLn "Migrations left to run:"
  >> displayListOfNames unrunned

displayRunned :: [Text] -> IO ()
displayRunned runned =
  T.putStrLn "Migrations already run:"
  >> displayListOfNames runned

performCommand
  :: ByteString
  -- ^ Connection string
  -> AlignedList (Migration IO) db0 db1
  -- ^ Known migrations
  -> Command
  -- ^ Operation to perform
  -> IO ()
performCommand connectTo migrations Display =
  withConnection connectTo $ do
    runNames <- getRunMigrationNames
    let names = extractList name migrations
        unrunNames = names \\ runNames
    liftBase $ displayRunned runNames >> displayUnrunned unrunNames
performCommand connectTo migrations AllUp =
  withConnection connectTo (migrateUp migrations)
  >> performCommand connectTo migrations Display
performCommand connectTo migrations AllDown =
  withConnection connectTo (migrateDown migrations)
  >> performCommand connectTo migrations Display

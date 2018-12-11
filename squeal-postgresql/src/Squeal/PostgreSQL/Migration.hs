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

>>> :{
let
  numMigrations
    :: Has "migrations" schemas MigrationsSchema
    => PQ schemas schemas IO ()
  numMigrations = do
    result <- runQuery (select Star (from (table (#migrations ! #schema_migrations `as` #m))))
    num <- ntuples result
    liftBase $ print num
:}

>>> :{
withConnection "host=localhost port=5432 dbname=exampledb" $
  manipulate (UnsafeManipulation "SET client_min_messages TO WARNING;")
    -- suppress notices
  & pqThen (migrateUp migrations)
  & pqThen numMigrations
  & pqThen (migrateDown migrations)
  & pqThen numMigrations
:}
Row 2
Row 0
-}

{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
  , PolyKinds
  , OverloadedLabels
  , TypeApplications
  , FlexibleContexts
  , TypeOperators
#-}

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
  ) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Generics.SOP (K(..))
import Data.Function ((&))
import Data.Text (Text)

import Squeal.PostgreSQL

-- | A `Migration` should contain an inverse pair of
-- `up` and `down` instructions and a unique `name`.
data Migration io schemas0 schemas1 = Migration
  { name :: Text -- ^ The `name` of a `Migration`.
    -- Each `name` in a `Migration` should be unique.
  , up :: PQ schemas0 schemas1 io () -- ^ The `up` instruction of a `Migration`.
  , down :: PQ schemas1 schemas0 io () -- ^ The `down` instruction of a `Migration`.
  }

-- | Run `Migration`s by creating the `MigrationsTable`
-- if it does not exist and then in a transaction, for each each `Migration`
-- query to see if the `Migration` is executed. If not, then
-- execute the `Migration` and insert its row in the `MigrationsTable`.
migrateUp
  :: MonadBaseControl IO io
  => AlignedList (Migration io) schemas0 schemas1 -- ^ migrations to run
  -> PQ
    ("migrations" ::: MigrationsSchema ': schemas0)
    ("migrations" ::: MigrationsSchema ': schemas1)
    io ()
migrateUp migration =
  define createMigrations
  & pqBind okResult
  & pqThen (transactionallySchema_ (upMigrations migration))
  where

    upMigrations
      :: MonadBaseControl IO io
      => AlignedList (Migration io) schemas0 schemas1
      -> PQ
        ("migrations" ::: MigrationsSchema ': schemas0)
        ("migrations" ::: MigrationsSchema ': schemas1)
        io ()
    upMigrations = \case
      Done -> return ()
      step :>> steps -> upMigration step & pqThen (upMigrations steps)

    upMigration
      :: MonadBase IO io
      => Migration io schemas0 schemas1 -> PQ
        ("migrations" ::: MigrationsSchema ': schemas0)
        ("migrations" ::: MigrationsSchema ': schemas1)
        io ()
    upMigration step =
      queryExecuted step
      & pqBind (\ executed ->
        if executed == 1 -- up step has already been executed
          then PQ (\ _ -> return (K ())) -- unsafely switch schemas
          else
            pqEmbed (up step) -- safely switch schemas
            & pqThen (manipulateParams insertMigration (Only (name step)))
            -- insert execution record
            & pqBind okResult)

    queryExecuted
      :: MonadBase IO io
      => Migration io schemas0 schemas1 -> PQ
        ("migrations" ::: MigrationsSchema ': schemas0)
        ("migrations" ::: MigrationsSchema ': schemas0)
        io Row
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
  -> PQ
    ("migrations" ::: MigrationsSchema ': schemas1)
    ("migrations" ::: MigrationsSchema ': schemas0)
    io ()
migrateDown migrations =
  define createMigrations
  & pqBind okResult
  & pqThen (transactionallySchema_ (downMigrations migrations))
  where

    downMigrations
      :: MonadBaseControl IO io
      => AlignedList (Migration io) schemas0 schemas1 -> PQ
        ("migrations" ::: MigrationsSchema ': schemas1)
        ("migrations" ::: MigrationsSchema ': schemas0)
        io ()
    downMigrations = \case
      Done -> return ()
      step :>> steps -> downMigrations steps & pqThen (downMigration step)

    downMigration
      :: MonadBase IO io
      => Migration io schemas0 schemas1 -> PQ
        ("migrations" ::: MigrationsSchema ': schemas1)
        ("migrations" ::: MigrationsSchema ': schemas0)
        io ()
    downMigration step =
      queryExecuted step
      & pqBind (\ executed ->
        if executed == 0 -- up step has not been executed
          then PQ (\ _ -> return (K ())) -- unsafely switch schemas
          else
            pqEmbed (down step) -- safely switch schemas
            & pqThen (manipulateParams deleteMigration (Only (name step)))
            -- delete execution record
            & pqBind okResult)

    queryExecuted
      :: MonadBase IO io
      => Migration io schemas0 schemas1 -> PQ
        ("migrations" ::: MigrationsSchema ': schemas1)
        ("migrations" ::: MigrationsSchema ': schemas1)
        io Row
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
      Nothing -> error "migrateDown: unknown error"
      Just msg -> error ("migrationDown: " <> show msg)

-- | The `TableType` for a Squeal migration.
type MigrationsTable =
  '[ "migrations_unique_name" ::: 'Unique '["name"]] :=>
  '[ "name"        ::: 'NoDef :=> 'NotNull 'PGtext
   , "executed_at" :::   'Def :=> 'NotNull 'PGtimestamptz
   ]

type MigrationsSchema = '["schema_migrations" ::: 'Table MigrationsTable]

-- | Creates a `MigrationsTable` if it does not already exist.
createMigrations
  :: Has "migrations" schemas MigrationsSchema
  => Definition schemas schemas
createMigrations =
  createSchemaIfNotExists #migrations >>>
  createTableIfNotExists (#migrations ! #schema_migrations)
    ( (text & notNullable) `as` #name :*
      (timestampWithTimeZone & notNullable & default_ currentTimestamp)
        `as` #executed_at )
    ( unique #name `as` #migrations_unique_name )

-- | Inserts a `Migration` into the `MigrationsTable`
insertMigration
  :: Has "migrations" schemas MigrationsSchema
  => Manipulation ('[] :=> schemas) '[ 'NotNull 'PGtext] '[]
insertMigration = insertInto_ (#migrations ! #schema_migrations) . Values_ $
  (param @1) `as` #name :* defaultAs #executed_at

-- | Deletes a `Migration` from the `MigrationsTable`
deleteMigration
  :: Has "migrations" schemas MigrationsSchema
  => Manipulation ('[] :=> schemas) '[ 'NotNull 'PGtext ] '[]
deleteMigration = deleteFrom_ (#migrations ! #schema_migrations) NoUsing (#name .== param @1)

-- | Selects a `Migration` from the `MigrationsTable`, returning
-- the time at which it was executed.
selectMigration
  :: Has "migrations" schemas MigrationsSchema
  => Query ('[] :=> schemas) '[ 'NotNull 'PGtext ]
    '[ "executed_at" ::: 'NotNull 'PGtimestamptz ]
selectMigration = select_
  (#executed_at `as` #executed_at)
  ( from (table ((#migrations ! #schema_migrations) `as` #m))
    & where_ (#name .== param @1))

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
  makeUsers :: Migration IO '[] '["users" ::: UsersTable]
  makeUsers = Migration
    { name = "make users table"
    , up = void . define $
        createTable #users
        ( serial `As` #id :*
          (text & notNull) `As` #name :* Nil )
        ( primaryKey (Column #id :* Nil) `As` #pk_users :* Nil )
    , down = void . define $ dropTable #users
    }
:}

>>> :{
let
  makeEmails :: Migration IO '["users" ::: UsersTable]
    '["users" ::: UsersTable, "emails" ::: EmailsTable]
  makeEmails = Migration
    { name = "make emails table"
    , up = void . define $
        createTable #emails
          ( serial `As` #id :*
            (int & notNull) `As` #user_id :*
            text `As` #email :* Nil )
          ( primaryKey (Column #id :* Nil) `As` #pk_emails :*
            foreignKey (Column #user_id :* Nil) #users (Column #id :* Nil)
              OnDeleteCascade OnUpdateCascade `As` #fk_user_id :* Nil )
    , down = void . define $ dropTable #emails
    }
:}

Now that we have a couple migrations we can chain them together.

>>> let migrations = makeUsers :>> makeEmails :>> Done

>>> :{
let
  numMigrations
    :: Has "schema_migrations" schema MigrationsTable
    => PQ schema schema IO ()
  numMigrations = do
    result <- runQuery (selectStar (from (table (#schema_migrations `As` #m))))
    num <- ntuples result
    liftBase $ print num
:}

>>> :{
withConnection "host=localhost port=5432 dbname=exampledb" $
  migrateUp migrations
  & pqThen numMigrations
  & pqThen (migrateDown migrations)
:}
NOTICE:  relation "schema_migrations" already exists, skipping
Row 2
NOTICE:  relation "schema_migrations" already exists, skipping
-}

{-# LANGUAGE
    ScopedTypeVariables
  , OverloadedStrings
  , DataKinds
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
    -- * Aligned lists
  , AlignedList (..)
  , single
    -- * Migration table
  , MigrationsTable
  , createMigrations
  , insertMigration
  , deleteMigration
  , selectMigration
  ) where

import Control.Category
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Monoid
import Generics.SOP (K(..))
import Data.Function ((&))
import Data.Text (Text)
import Prelude hiding (id, (.))

import Squeal.PostgreSQL

-- | A `Migration` of a `Migrations`, should contain an inverse pair of
-- `up` and `down` instructions and a unique `name`.
data Migration io schema0 schema1 = Migration
  { name :: Text -- ^ The `name` of a `Migration`.
    -- Each `name` in a `Migration` should be unique.
  , up :: PQ schema0 schema1 io () -- ^ The `up` instruction of a `Migration`.
  , down :: PQ schema1 schema0 io () -- ^ The `down` instruction of a `Migration`.
  }

-- | An `AlignedList` is a type-aligned list or free category.
data AlignedList p x0 x1 where
  Done :: AlignedList p x x
  (:>>) :: p x0 x1 -> AlignedList p x1 x2 -> AlignedList p x0 x2
infixr 7 :>>
instance Category (AlignedList p) where
  id = Done
  (.) list = \case
    Done -> list
    step :>> steps -> step :>> (steps >>> list)

single :: p x0 x1 -> AlignedList p x0 x1
single step = step :>> Done

-- | Run `Migration`s by creating the `MigrationsTable`
-- if it does not exist and then in a transaction, for each each `Migration`
-- query to see if the `Migration` is executed. If not, then
-- execute the `Migration` and insert its row in `MigrationsTable`.
migrateUp
  :: MonadBaseControl IO io
  => AlignedList (Migration io) schema0 schema1 -- ^ migrations to run
  -> PQ
    ("schema_migrations" ::: MigrationsTable ': schema0)
    ("schema_migrations" ::: MigrationsTable ': schema1)
    io ()
migrateUp migration =
  define createMigrations
  & pqBind okResult
  & pqThen (transactionallySchema_ (upMigrations migration))
  where

    upMigrations
      :: MonadBaseControl IO io
      => AlignedList (Migration io) schema0 schema1
      -> PQ
        ("schema_migrations" ::: MigrationsTable ': schema0)
        ("schema_migrations" ::: MigrationsTable ': schema1)
        io ()
    upMigrations = \case
      Done -> return ()
      step :>> steps -> upMigration step & pqThen (upMigrations steps)

    upMigration
      :: MonadBase IO io
      => Migration io schema0 schema1 -> PQ
        ("schema_migrations" ::: MigrationsTable ': schema0)
        ("schema_migrations" ::: MigrationsTable ': schema1)
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
      => Migration io schema0 schema1 -> PQ
        ("schema_migrations" ::: MigrationsTable ': schema0)
        ("schema_migrations" ::: MigrationsTable ': schema0)
        io Row
    queryExecuted step = do
      result <- runQueryParams selectMigration (Only (name step))
      okResult result
      ntuples result

-- | Rewind `Migration`s by creating the `MigrationsTable`
-- if it does not exist and then in a transaction, for each each `Migration`
-- query to see if the `Migration` is executed. If it is, then
-- rewind the `Migration` and delete its row in `MigrationsTable`.
migrateDown
  :: MonadBaseControl IO io
  => AlignedList (Migration io) schema0 schema1 -- ^ migrations to rewind
  -> PQ
    ("schema_migrations" ::: MigrationsTable ': schema1)
    ("schema_migrations" ::: MigrationsTable ': schema0)
    io ()
migrateDown migrations =
  define createMigrations
  & pqBind okResult
  & pqThen (transactionallySchema_ (downMigrations migrations))
  where

    downMigrations
      :: MonadBaseControl IO io
      => AlignedList (Migration io) schema0 schema1 -> PQ
        ("schema_migrations" ::: MigrationsTable ': schema1)
        ("schema_migrations" ::: MigrationsTable ': schema0)
        io ()
    downMigrations = \case
      Done -> return ()
      step :>> steps -> downMigrations steps & pqThen (downMigration step)

    downMigration
      :: MonadBase IO io
      => Migration io schema0 schema1 -> PQ
        ("schema_migrations" ::: MigrationsTable ': schema1)
        ("schema_migrations" ::: MigrationsTable ': schema0)
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
      => Migration io schema0 schema1 -> PQ
        ("schema_migrations" ::: MigrationsTable ': schema1)
        ("schema_migrations" ::: MigrationsTable ': schema1)
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

-- | Creates a `MigrationsTable` if it does not already exist.
createMigrations
  :: Has "schema_migrations" schema MigrationsTable
  => Definition schema schema
createMigrations =
  createTableIfNotExists #schema_migrations
    ( (text & notNull) `As` #name :*
      (timestampWithTimeZone & notNull & default_ currentTimestamp)
        `As` #executed_at :* Nil )
    ( unique (Column #name :* Nil) `As` #migrations_unique_name :* Nil )

-- | Inserts a `Migration` into the `MigrationsTable`
insertMigration
  :: Has "schema_migrations" schema MigrationsTable
  => Manipulation schema '[ 'NotNull 'PGtext] '[]
insertMigration = insertRow_ #schema_migrations
  ( Set (param @1) `As` #name :*
    Default `As` #executed_at :* Nil )

-- | Deletes a `Migration` from the `MigrationsTable`
deleteMigration
  :: Has "schema_migrations" schema MigrationsTable
  => Manipulation schema '[ 'NotNull 'PGtext ] '[]
deleteMigration = deleteFrom_ #schema_migrations (#name .== param @1)

-- | Selects a `Migration` from the `MigrationsTable`, returning
-- the time at which it was executed.
selectMigration
  :: Has "schema_migrations" schema MigrationsTable
  => Query schema '[ 'NotNull 'PGtext ]
    '[ "executed_at" ::: 'NotNull 'PGtimestamptz ]
selectMigration = select
  (#executed_at `As` #executed_at :* Nil)
  ( from (table (#schema_migrations `As` #m))
    & where_ (#name .== param @1))

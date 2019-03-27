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
  makeUsers :: Migration Definition (Public '[]) '["public" ::: '["users" ::: 'Table UsersTable]]
  makeUsers = Migration
    { name = "make users table"
    , up = createTable #users
        ( serial `as` #id :*
          (text & notNullable) `as` #name )
        ( primaryKey #id `as` #pk_users )
    , down = dropTable #users
    }
:}

>>> :{
let
  makeEmails :: Migration Definition '["public" ::: '["users" ::: 'Table UsersTable]]
    '["public" ::: '["users" ::: 'Table UsersTable, "emails" ::: 'Table EmailsTable]]
  makeEmails = Migration
    { name = "make emails table"
    , up = createTable #emails
          ( serial `as` #id :*
            (int & notNullable) `as` #user_id :*
            (text & nullable) `as` #email )
          ( primaryKey #id `as` #pk_emails :*
            foreignKey #user_id #users #id
              OnDeleteCascade OnUpdateCascade `as` #fk_user_id )
    , down = dropTable #emails
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
    liftIO $ print num
:}

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

We can also create a simple executable using `defaultMain`.

>>> let main = defaultMain "host=localhost port=5432 dbname=exampledb" migrations

>>> withArgs [] main
Invalid command: "". Use:
migrate    to run all available migrations
rollback   to rollback all available migrations
status     to display migrations run and migrations left to run

>>> withArgs ["status"] main
Migrations already run:
  None
Migrations left to run:
  - make users table
  - make emails table

>>> withArgs ["migrate"] main
Migrations already run:
  - make users table
  - make emails table
Migrations left to run:
  None

>>> withArgs ["rollback"] main
Migrations already run:
  None
Migrations left to run:
  - make users table
  - make emails table
-}
{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , OverloadedLabels
  , OverloadedStrings
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.Migration
  ( -- * Migration
    Migration (..)
  , Terminally (..)
  , terminally
  , pureMigration
  , Migratory (..)
    -- * Migration table
  , MigrationsTable
  , createMigrations
  , insertMigration
  , deleteMigration
  , selectMigration
   -- * CLI
  , defaultMain
  , MigrateCommand
  ) where

import           Control.Category
import           Control.Monad
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
import           Prelude hiding ((.), id)
import           Squeal.PostgreSQL
import           System.Environment
import           UnliftIO                    (MonadIO (..))

-- | A `Migration` is a named "isomorphism" over a given category.
-- It should contain an inverse pair of `up` and `down`
-- instructions and a unique `name`.
data Migration p schemas0 schemas1 = Migration
  { name :: Text -- ^ The `name` of a `Migration`.
    -- Each `name` in a `Migration` should be unique.
  , up   :: p schemas0 schemas1 -- ^ The `up` instruction of a `Migration`.
  , down :: p schemas1 schemas0 -- ^ The `down` instruction of a `Migration`.
  } deriving (GHC.Generic)

-- | `Terminally` turns an indexed monad transformer into a category by
-- restricting the return type to `()` and permuting the type variables.
newtype Terminally trans monad x0 x1 = Terminally
  { runTerminally :: trans x0 x1 monad () }
  deriving GHC.Generic

-- | A `pureMigration` turns a `Migration` involving only pure SQL
-- `Definition`s into a `Migration` that may involve `IO`.
pureMigration
  :: Migration Definition schema0 schema1
  -> Migration (Terminally PQ IO) schema0 schema1
pureMigration migration = Migration
  { name = name migration
  , up = terminally . define $ up migration
  , down = terminally . define $ down migration
  }

instance
  ( IndexedMonadTransPQ trans
  , Monad monad
  , forall x0 x1. Monad (trans x0 x1 monad) )
  => Category (Terminally trans monad) where
    id = Terminally (return ())
    Terminally g . Terminally f = Terminally $ pqThen g f

-- | terminally ignores the output of a computation, returning `()` and
-- wrapping it up in a `Terminally`.
terminally
  :: Functor (trans x0 x1 monad)
  => trans x0 x1 monad ignore
  -> Terminally trans monad x0 x1
terminally = Terminally . void

class Migratory p where
  migrateUp
    :: AlignedList (Migration p) schemas0 schemas1
    -> PQ schemas0 schemas1 IO ()
  migrateDown
    :: AlignedList (Migration p) schemas0 schemas1
    -> PQ schemas1 schemas0 IO ()

instance Migratory Definition where
  migrateUp = migrateUp . alignedMap pureMigration
  migrateDown = migrateDown . alignedMap pureMigration

instance Migratory (Terminally PQ IO) where

  migrateUp migration = unsafePQ $
    define createMigrations
    & pqBind okResult
    & pqThen (transactionallySchema_ (upMigrations migration))

    where

      upMigrations
        :: AlignedList (Migration (Terminally PQ IO)) schemas0 schemas1
        -> PQ MigrationsSchemas MigrationsSchemas IO ()
      upMigrations = \case
        Done -> return ()
        step :>> steps -> upMigration step & pqThen (upMigrations steps)

      upMigration
        :: Migration (Terminally PQ IO) schemas0 schemas1
        -> PQ MigrationsSchemas MigrationsSchemas IO ()
      upMigration step =
        queryExecuted step
        & pqBind ( \ executed -> unless (executed == 1)
          $ unsafePQ (runTerminally (up step))
          & pqThen (manipulateParams insertMigration (Only (name step)))
          -- insert execution record
          & pqBind okResult )

      queryExecuted
        :: Migration (Terminally PQ IO) schemas0 schemas1
        -> PQ MigrationsSchemas MigrationsSchemas IO Row
      queryExecuted step = do
        result <- runQueryParams selectMigration (Only (name step))
        okResult result
        ntuples result

  migrateDown migrations = unsafePQ $
    define createMigrations
    & pqBind okResult
    & pqThen (transactionallySchema_ (downMigrations migrations))

    where

      downMigrations
        :: AlignedList (Migration (Terminally PQ IO)) schemas0 schemas1
        -> PQ MigrationsSchemas MigrationsSchemas IO ()
      downMigrations = \case
        Done -> return ()
        step :>> steps -> downMigrations steps & pqThen (downMigration step)

      downMigration
        :: Migration (Terminally PQ IO) schemas0 schemas1
        -> PQ MigrationsSchemas MigrationsSchemas IO ()
      downMigration step =
        queryExecuted step
        & pqBind ( \ executed -> unless (executed == 0)
          $ unsafePQ (runTerminally (down step))
          & pqThen (manipulateParams deleteMigration (Only (name step)))
          -- delete execution record
          & pqBind okResult )

      queryExecuted
        :: Migration (Terminally PQ IO) schemas0 schemas1
        -> PQ MigrationsSchemas MigrationsSchemas IO Row
      queryExecuted step = do
        result <- runQueryParams selectMigration (Only (name step))
        okResult result
        ntuples result

okResult :: K Result results -> PQ schema schema IO ()
okResult result = do
  status <- resultStatus result
  when (not (status `elem` [CommandOk, TuplesOk])) $ do
    errorMessageMaybe <- resultErrorMessage result
    case errorMessageMaybe of
      Nothing  -> error "migration: unknown error"
      Just msg -> error ("migration: " <> show msg)

unsafePQ :: (Functor m) => PQ db0 db1 m x -> PQ db0' db1' m x
unsafePQ (PQ pq) = PQ $ fmap (SOP.K . SOP.unK) . pq . SOP.K . SOP.unK

-- | The `TableType` for a Squeal migration.
type MigrationsTable =
  '[ "migrations_unique_name" ::: 'Unique '["name"]] :=>
  '[ "name"        ::: 'NoDef :=> 'NotNull 'PGtext
   , "executed_at" :::   'Def :=> 'NotNull 'PGtimestamptz
   ]

data MigrationRow =
  MigrationRow { migrationName :: Text
               , migrationTime :: UTCTime }
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

-- | Inserts a `Migration` into the `MigrationsTable`, returning
-- the time at which it was inserted.
insertMigration
  :: Manipulation_ MigrationsSchemas (Only Text) (P ("executed_at" ::: UTCTime))
insertMigration = insertInto #schema_migrations
  (Values_ ((param @1) `as` #name :* defaultAs #executed_at))
  OnConflictDoRaise (Returning #executed_at)

-- | Deletes a `Migration` from the `MigrationsTable`, returning
-- the time at which it was inserted.
deleteMigration
  :: Manipulation_ MigrationsSchemas (Only Text) (P ("executed_at" ::: UTCTime))
deleteMigration = deleteFrom #schema_migrations
  NoUsing (#name .== param @1) (Returning #executed_at)

-- | Selects a `Migration` from the `MigrationsTable`, returning
-- the time at which it was inserted.
selectMigration
  :: Query_ MigrationsSchemas (Only Text) (P ("executed_at" ::: UTCTime))
selectMigration = select_ #executed_at
  $ from (table (#schema_migrations))
  & where_ (#name .== param @1)

selectMigrations :: Query_ MigrationsSchemas () MigrationRow
selectMigrations = select_
  (#name `as` #migrationName :* #executed_at `as` #migrationTime)
  (from (table #schema_migrations))

data MigrateCommand
  = MigrateStatus
  | MigrateUp
  | MigrateDown deriving (GHC.Generic, Show)

defaultMain
  :: Migratory p
  => ByteString
  -- ^ connection string
  -> AlignedList (Migration p) db0 db1
  -- ^ migrations
  -> IO ()
defaultMain connectTo migrations = do
  command <- readCommandFromArgs
  maybe (pure ()) performCommand command

  where

    performCommand :: MigrateCommand -> IO ()
    performCommand = \case
      MigrateStatus -> withConnection connectTo $
        suppressNotices >> migrateStatus
      MigrateUp -> withConnection connectTo $
        suppressNotices & pqThen (migrateUp migrations) & pqThen migrateStatus
      MigrateDown -> withConnection connectTo $
        suppressNotices & pqThen (migrateDown migrations) & pqThen migrateStatus

    migrateStatus :: PQ schema schema IO ()
    migrateStatus = unsafePQ $ do
      runNames <- getRunMigrationNames
      let names = extractList name migrations
          unrunNames = names \\ runNames
      liftIO $ displayRunned runNames >> displayUnrunned unrunNames

    suppressNotices :: PQ schema schema IO ()
    suppressNotices = void . manipulate $
      UnsafeManipulation "SET client_min_messages TO WARNING;"

    readCommandFromArgs :: IO (Maybe MigrateCommand)
    readCommandFromArgs = getArgs >>= \case
      ["migrate"] -> pure . Just $ MigrateUp
      ["rollback"] -> pure . Just $ MigrateDown
      ["status"] -> pure . Just $ MigrateStatus
      args -> displayUsage args >> pure Nothing

    displayUsage :: [String] -> IO ()
    displayUsage args = do
      putStrLn $ "Invalid command: \"" <> unwords args <> "\". Use:"
      putStrLn "migrate    to run all available migrations"
      putStrLn "rollback   to rollback all available migrations"
      putStrLn "status     to display migrations run and migrations left to run"

    getRunMigrationNames :: (MonadIO m) => PQ db0 db0 m [Text]
    getRunMigrationNames =
      fmap migrationName <$> (unsafePQ (define createMigrations & pqThen (runQuery selectMigrations)) >>= getRows)

    displayListOfNames :: [Text] -> IO ()
    displayListOfNames [] = T.putStrLn "  None"
    displayListOfNames xs =
      let singleName n = T.putStrLn $ "  - " <> n
      in traverse_ singleName xs

    displayUnrunned :: [Text] -> IO ()
    displayUnrunned unrunned =
      T.putStrLn "Migrations left to run:"
      >> displayListOfNames unrunned

    displayRunned :: [Text] -> IO ()
    displayRunned runned =
      T.putStrLn "Migrations already run:"
      >> displayListOfNames runned

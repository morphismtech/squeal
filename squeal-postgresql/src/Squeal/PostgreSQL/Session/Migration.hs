{-|
Module: Squeal.PostgreSQL.Session.Migration
Description: migrations
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

This module defines a `Migration` type to safely
change the schema of your database over time. Let's see an example!

First turn on some extensions.

>>> :set -XDataKinds -XOverloadedLabels
>>> :set -XOverloadedStrings -XFlexibleContexts -XTypeOperators

Next, let's define our `TableType`s.

>>> :{
type UsersTable =
  '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext
   ]
:}

>>> :{
type EmailsTable =
  '[ "pk_emails" ::: 'PrimaryKey '["id"]
   , "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["id"]
   ] :=>
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
   , "email" ::: 'NoDef :=> 'Null 'PGtext
   ]
:}

Now we can define some `Migration`s to make our tables.

`Migration`s are parameterized giving the option of a

* pure one-way `Migration` `Definition`
* impure one-way `Migration` @(@`Indexed` `PQ` `IO`@)@
* pure reversible `Migration` @(@`IsoQ` `Definition`@)@
* impure reversible `Migration` @(@`IsoQ` @(@`Indexed` `PQ` `IO`@)@@)@

For this example, we'll use pure reversible `Migration`s.

>>> :{
let
  makeUsers :: Migration (IsoQ Definition)
    '["public" ::: '[]]
    '["public" ::: '["users" ::: 'Table UsersTable]]
  makeUsers = Migration "make users table" IsoQ
    { up = createTable #users
        ( serial `as` #id :*
          notNullable text `as` #name )
        ( primaryKey #id `as` #pk_users )
    , down = dropTable #users
    }
:}

>>> :{
let
  makeEmails :: Migration (IsoQ Definition)
    '["public" ::: '["users" ::: 'Table UsersTable]]
    '["public" ::: '["users" ::: 'Table UsersTable, "emails" ::: 'Table EmailsTable]]
  makeEmails = Migration "make emails table" IsoQ
    { up = createTable #emails
          ( serial `as` #id :*
            notNullable int `as` #user_id :*
            nullable text `as` #email )
          ( primaryKey #id `as` #pk_emails :*
            foreignKey #user_id #users #id
              (OnDelete Cascade) (OnUpdate Cascade) `as` #fk_user_id )
    , down = dropTable #emails
    }
:}

Now that we have a couple migrations we can chain them together into a `Path`.

>>> let migrations = makeUsers :>> makeEmails :>> Done

Now run the migrations.

>>> import Control.Monad.IO.Class
>>> :{
withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $
  manipulate_ (UnsafeManipulation "SET client_min_messages TO WARNING;")
    -- suppress notices
  & pqThen (liftIO (putStrLn "Migrate"))
  & pqThen (migrateUp migrations)
  & pqThen (liftIO (putStrLn "Rollback"))
  & pqThen (migrateDown migrations)
:}
Migrate
Rollback

We can also create a simple executable using `mainMigrateIso`.

>>> let main = mainMigrateIso "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" migrations

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

In addition to enabling `Migration`s using pure SQL `Definition`s for
the `up` and `down` migrations, you can also perform impure `IO` actions
by using a `Migration`s over the `Indexed` `PQ` `IO` category.
-}

{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.Session.Migration
  ( -- * Migration
    Migration (..)
  , Migratory (..)
  , migrate
  , migrateUp
  , migrateDown
  , MigrationsTable
    -- * Executable
  , mainMigrate
  , mainMigrateIso
    -- * Re-export
  , IsoQ (..)
  ) where

import Control.Category
import Control.Category.Free
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List ((\\))
import Data.Quiver
import Data.Quiver.Functor
import Data.Text (Text)
import Data.Time (UTCTime)
import Prelude hiding ((.), id)
import System.Environment

import qualified Data.Text.IO as Text (putStrLn)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Definition.Constraint
import Squeal.PostgreSQL.Definition.Table
import Squeal.PostgreSQL.Expression.Comparison
import Squeal.PostgreSQL.Expression.Default
import Squeal.PostgreSQL.Expression.Parameter
import Squeal.PostgreSQL.Expression.Time
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Manipulation.Delete
import Squeal.PostgreSQL.Manipulation.Insert
import Squeal.PostgreSQL.Session
import Squeal.PostgreSQL.Session.Decode
import Squeal.PostgreSQL.Session.Encode
import Squeal.PostgreSQL.Session.Indexed
import Squeal.PostgreSQL.Session.Monad
import Squeal.PostgreSQL.Session.Result
import Squeal.PostgreSQL.Session.Statement
import Squeal.PostgreSQL.Session.Transaction.Unsafe
import Squeal.PostgreSQL.Query.From
import Squeal.PostgreSQL.Query.Select
import Squeal.PostgreSQL.Query.Table
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.Schema

-- | A `Migration` consists of a name and a migration definition.
data Migration def db0 db1 = Migration
  { migrationName :: Text -- ^ The name of a `Migration`.
    -- Each `migrationName` should be unique.
  , migrationDef :: def db0 db1 -- ^ The migration of a `Migration`.
  } deriving (GHC.Generic)
instance QFunctor Migration where
  qmap f (Migration n i) = Migration n (f i)

{- |
A `Migratory` `Category` can run or
possibly rewind a `Path` of `Migration`s.
-}
class (Category def, Category run) => Migratory def run | def -> run where
  {- | Run a `Path` of `Migration`s.-}
  runMigrations :: Path (Migration def) db0 db1 -> run db0 db1
-- | impure migrations
instance Migratory (Indexed PQ IO ()) (Indexed PQ IO ()) where
  runMigrations path = Indexed . unsafePQ . transactionally_ $ do
    define createMigrations
    qtoMonoid upMigration path
    where
      upMigration step = do
        executed <- do
          result <- executeParams selectMigration (migrationName step)
          ntuples (result :: Result UTCTime)
        unless (executed == 1) $ do
          _ <- unsafePQ . runIndexed $ migrationDef step
          executeParams_ insertMigration (migrationName step)
-- | pure migrations
instance Migratory Definition (Indexed PQ IO ()) where
  runMigrations = runMigrations . qmap (qmap ixDefine)
-- | impure rewinds
instance Migratory (OpQ (Indexed PQ IO ())) (OpQ (Indexed PQ IO ())) where
  runMigrations path = OpQ . Indexed . unsafePQ . transactionally_ $ do
    define createMigrations
    qtoMonoid @FoldPath downMigration (reversePath path)
    where
      downMigration (OpQ step) = do
        executed <- do
          result <- executeParams selectMigration (migrationName step)
          ntuples (result :: Result UTCTime)
        unless (executed == 0) $ do
          _ <- unsafePQ . runIndexed . getOpQ $ migrationDef step
          executeParams_ deleteMigration (migrationName step)
-- | pure rewinds
instance Migratory (OpQ Definition) (OpQ (Indexed PQ IO ())) where
  runMigrations = runMigrations . qmap (qmap (qmap ixDefine))
-- | impure rewindable migrations
instance Migratory
  (IsoQ (Indexed PQ IO ()))
  (IsoQ (Indexed PQ IO ())) where
    runMigrations path = IsoQ
      (runMigrations (qmap (qmap up) path))
      (getOpQ (runMigrations (qmap (qmap (OpQ . down)) path)))
-- | pure rewindable migrations
instance Migratory (IsoQ Definition) (IsoQ (Indexed PQ IO ())) where
  runMigrations = runMigrations . qmap (qmap (qmap ixDefine))

unsafePQ :: (Functor m) => PQ db0 db1 m x -> PQ db0' db1' m x
unsafePQ (PQ pq) = PQ $ fmap (SOP.K . SOP.unK) . pq . SOP.K . SOP.unK

-- | Run migrations.
migrate
  :: Migratory def (Indexed PQ IO ())
  => Path (Migration def) db0 db1
  -> PQ db0 db1 IO ()
migrate = runIndexed . runMigrations

-- | Run rewindable migrations.
migrateUp
  :: Migratory def (IsoQ (Indexed PQ IO ()))
  => Path (Migration def) db0 db1
  -> PQ db0 db1 IO ()
migrateUp = runIndexed . up . runMigrations

-- | Rewind migrations.
migrateDown
  :: Migratory def (IsoQ (Indexed PQ IO ()))
  => Path (Migration def) db0 db1
  -> PQ db1 db0 IO ()
migrateDown = runIndexed . down . runMigrations

ixDefine :: Definition db0 db1 -> Indexed PQ IO () db0 db1
ixDefine = indexedDefine

-- | The `TableType` for a Squeal migration.
type MigrationsTable =
  '[ "migrations_unique_name" ::: 'Unique '["name"]] :=>
  '[ "name"        ::: 'NoDef :=> 'NotNull 'PGtext
   , "executed_at" :::   'Def :=> 'NotNull 'PGtimestamptz
   ]

data MigrationRow =
  MigrationRow { name :: Text
               , executed_at :: UTCTime }
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
insertMigration :: Statement MigrationsSchemas Text ()
insertMigration = Manipulation aParam genericRow $
  insertInto_ #schema_migrations $
    Values_ (Set (param @1) `as` #name :* Default `as` #executed_at)

-- | Deletes a `Migration` from the `MigrationsTable`, returning
-- the time at which it was inserted.
deleteMigration :: Statement MigrationsSchemas Text ()
deleteMigration = Manipulation aParam genericRow $
  deleteFrom_ #schema_migrations (#name .== param @1)

-- | Selects a `Migration` from the `MigrationsTable`, returning
-- the time at which it was inserted.
selectMigration :: Statement MigrationsSchemas Text UTCTime
selectMigration = Query aParam #executed_at $
  select_ #executed_at
    $ from (table (#schema_migrations))
    & where_ (#name .== param @1)

selectMigrations :: Statement MigrationsSchemas () MigrationRow
selectMigrations = query $ select Star (from (table #schema_migrations))

{- | `mainMigrate` creates a simple executable
from a connection string and a `Path` of `Migration`s. -}
mainMigrate
  :: Migratory p (Indexed PQ IO ())
  => ByteString
  -- ^ connection string
  -> Path (Migration p) db0 db1
  -- ^ migrations
  -> IO ()
mainMigrate connectTo migrations = do
  command <- getArgs
  performCommand command

  where

    performCommand :: [String] -> IO ()
    performCommand = \case
      ["status"] -> withConnection connectTo $
        suppressNotices >> migrateStatus
      ["migrate"] -> withConnection connectTo $
        suppressNotices
        & pqThen (runIndexed (runMigrations migrations))
        & pqThen migrateStatus
      args -> displayUsage args

    migrateStatus :: PQ schema schema IO ()
    migrateStatus = unsafePQ $ do
      runNames <- getRunMigrationNames
      let names = qtoList migrationName migrations
          unrunNames = names \\ runNames
      liftIO $ displayRunned runNames >> displayUnrunned unrunNames

    suppressNotices :: PQ schema schema IO ()
    suppressNotices = manipulate_ $
      UnsafeManipulation "SET client_min_messages TO WARNING;"

    displayUsage :: [String] -> IO ()
    displayUsage args = do
      putStrLn $ "Invalid command: \"" <> unwords args <> "\". Use:"
      putStrLn "migrate    to run all available migrations"
      putStrLn "rollback   to rollback all available migrations"

{- | `mainMigrateIso` creates a simple executable
from a connection string and a `Path` of `Migration` `IsoQ`s. -}
mainMigrateIso
  :: Migratory (IsoQ def) (IsoQ (Indexed PQ IO ()))
  => ByteString
  -- ^ connection string
  -> Path (Migration (IsoQ def)) db0 db1
  -- ^ migrations
  -> IO ()
mainMigrateIso connectTo migrations = performCommand =<< getArgs

  where

    performCommand :: [String] -> IO ()
    performCommand = \case
      ["status"] -> withConnection connectTo $
        suppressNotices >> migrateStatus
      ["migrate"] -> withConnection connectTo $
        suppressNotices
        & pqThen (migrateUp migrations)
        & pqThen migrateStatus
      ["rollback"] -> withConnection connectTo $
        suppressNotices
        & pqThen (migrateDown migrations)
        & pqThen migrateStatus
      args -> displayUsage args

    migrateStatus :: PQ schema schema IO ()
    migrateStatus = unsafePQ $ do
      runNames <- getRunMigrationNames
      let names = qtoList migrationName migrations
          unrunNames = names \\ runNames
      liftIO $ displayRunned runNames >> displayUnrunned unrunNames

    suppressNotices :: PQ schema schema IO ()
    suppressNotices = manipulate_ $
      UnsafeManipulation "SET client_min_messages TO WARNING;"

    displayUsage :: [String] -> IO ()
    displayUsage args = do
      putStrLn $ "Invalid command: \"" <> unwords args <> "\". Use:"
      putStrLn "migrate    to run all available migrations"
      putStrLn "rollback   to rollback all available migrations"
      putStrLn "status     to display migrations run and migrations left to run"

getRunMigrationNames :: PQ db0 db0 IO [Text]
getRunMigrationNames =
  fmap name <$>
    (unsafePQ (define createMigrations
    & pqThen (execute selectMigrations)) >>= getRows)

displayListOfNames :: [Text] -> IO ()
displayListOfNames [] = Text.putStrLn "  None"
displayListOfNames xs =
  let singleName n = Text.putStrLn $ "  - " <> n
  in traverse_ singleName xs

displayUnrunned :: [Text] -> IO ()
displayUnrunned unrunned =
  Text.putStrLn "Migrations left to run:"
  >> displayListOfNames unrunned

displayRunned :: [Text] -> IO ()
displayRunned runned =
  Text.putStrLn "Migrations already run:"
  >> displayListOfNames runned

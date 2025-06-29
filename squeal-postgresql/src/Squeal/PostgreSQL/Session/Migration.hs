{-# LANGUAGE
DataKinds
, GADTs
, LambdaCase
, MagicHash
, OverloadedLabels
, PolyKinds
, QualifiedDo
, TypeApplications
, TypeOperators
#-}

module Squeal.PostgreSQL.Session.Migration
  ( Migration (..)
  , Migrate (..)
  ) where

import Control.Category.Free
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Indexed
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List ((\\))
import Data.Time (UTCTime)
import Data.Quiver
import Data.Quiver.Functor
import Prelude hiding ((.), id)
import System.Environment (getArgs)

import qualified Control.Monad.Trans.Indexed.Do as Ix

import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Definition.Constraint
import Squeal.PostgreSQL.Definition.Table
import Squeal.PostgreSQL.Definition.Schema
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
import Squeal.PostgreSQL.Session.Monad
import Squeal.PostgreSQL.Session.Result
import Squeal.PostgreSQL.Session.Statement
import Squeal.PostgreSQL.Session.Transaction.Unsafe
import Squeal.PostgreSQL.Query.From
import Squeal.PostgreSQL.Query.Select
import Squeal.PostgreSQL.Query.Table
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.Schema

-- | A `Migration` consists of a name and a migration definition.
data Migration def db0 db1 = Migration
  { migrationName :: String -- ^ The name of a `Migration`.
    -- Each `migrationName` should be unique.
  , migrationDef :: def db0 db1 -- ^ The definition of a `Migration`.
  }
instance QFunctor Migration where
  qmap f (Migration n i) = Migration n (f i)

data Migrate where
  MigrateUp :: Path (Migration Definition) db0 db1 -> Migrate
  MigrateUpDown :: Path (Migration (IsoQ Definition)) db0 db1 -> Migrate
  MigrateUpImpure :: Path (Migration (Indexed PQ IO ())) db0 db1 -> Migrate
  MigrateUpDownImpure :: Path (Migration (IsoQ (Indexed PQ IO ()))) db0 db1 -> Migrate

mainMigrate
  :: ByteString -- ^ connection string
  -> Migrate -- ^ migrations
  -> IO ()
mainMigrate connStr migrate = do
  let
    switchSchema :: PQ db0 db1 IO ()
    switchSchema = PQ $ \_ -> return ()

    upTx :: Migration Definition x y -> Migration (Indexed PQ IO ()) x y
    upTx (Migration name def) = Migration name . Indexed $ Ix.do
      runTransaction (Transactional defaultMode) $ Ix.do
        define def
        switchSchema
        executeParams_ insertMigration name
        switchSchema
      switchSchema
    
    upDownTx
      :: Migration (IsoQ Definition) x y
      -> Migration (IsoQ (Indexed PQ IO ())) x y
    upDownTx (Migration name def) = Migration name IsoQ
      { up = Indexed $ Ix.do
          runTransaction (Transactional defaultMode) $ Ix.do
            define (up def)
            switchSchema
            executeParams_ insertMigration name
            switchSchema
          switchSchema
      , down = Indexed $ Ix.do
          runTransaction (Transactional defaultMode) $ Ix.do
            define (down def)
            switchSchema
            executeParams_ deleteMigration name
            switchSchema
          switchSchema
      }

    displayInvalidCmd args = putStrLn $ "Invalid command: \"" <> unwords args <> "\". Use:"
    displayCmdStatus = putStrLn "status\tto display migrations run and migrations left to run"
    displayCmdMigrate = putStrLn "migrate\tto run all available migrations"
    displayCmdRollback = putStrLn "rollback\tto rollback all available migrations"

    displayUnrun unrun = do
      putStrLn "Migrate left to run:"
      displayListOfNames unrun

    displayRun run = do
      putStrLn "Migrate already run:"
      displayListOfNames run

    displayListOfNames [] = putStrLn "  None"
    displayListOfNames xs =
      let singleName n = putStrLn $ "  - " <> n
      in traverse_ singleName xs

  case migrate of
    MigrateUp migrations ->
      mainMigrate connStr (MigrateUpImpure (qmap upTx migrations))
    MigrateUpDown migrations ->
      mainMigrate connStr (MigrateUpDownImpure (qmap upDownTx migrations))
    MigrateUpImpure migrations -> withConnection connStr $ do
      liftIO getArgs >>= \case
        [] -> liftIO $ do
          displayCmdStatus
          displayCmdMigrate
        ["status"] -> do
          define createMigrations
          runMigrations <- execute selectMigrations
          runNames <- getRows (fst <$> runMigrations)
          let allNames = qtoList migrationName migrations
              unrunNames = allNames \\ runNames
          liftIO $ do
            displayRun runNames 
            displayUnrun unrunNames
        ["migrate"] -> return ()
        ["rollback"] -> return ()
        args -> liftIO $ do
          displayInvalidCmd args
          displayCmdStatus
          displayCmdMigrate
      define createMigrations
      let fooBar (Migration _ (Indexed m)) = Ix.do
            switchSchema
            m
            switchSchema
      qtoMonoid fooBar migrations
    MigrateUpDownImpure migrations -> undefined
      -- withConnection connStr $ do
      -- define createMigrations
      -- qtoMonoid (\(Indexed m) -> Ix.do {switchSchema; m; switchSchema}) migrations


-- {- | `mainMigrateIso` creates a simple executable
-- from a connection string and a `Path` of `Migration` `IsoQ`s. -}
-- mainMigrateIso
--   :: Migratory (IsoQ def) (IsoQ (Indexed PQ IO ()))
--   => ByteString
--   -- ^ connection string
--   -> Path (Migration (IsoQ def)) db0 db1
--   -- ^ migrations
--   -> IO ()
-- mainMigrateIso connectTo migrations = performCommand =<< getArgs

--   where

--     performCommand :: [String] -> IO ()
--     performCommand = \case
--       ["status"] -> withConnection connectTo $
--         suppressNotices >> migrateStatus
--       ["migrate"] -> withConnection connectTo $
--         suppressNotices
--         & thenIx (migrateUp migrations)
--         & thenIx migrateStatus
--       ["rollback"] -> withConnection connectTo $
--         suppressNotices
--         & thenIx (migrateDown migrations)
--         & thenIx migrateStatus
--       args -> displayUsage args

--     migrateStatus :: PQ schema schema IO ()
--     migrateStatus = switchSchema $ do
--       runNames <- getRunMigrationNames
--       let names = qtoList migrationName migrations
--           unrunNames = names \\ runNames
--       liftIO $ displayRunned runNames >> displayUnrun unrunNames

--     suppressNotices :: PQ schema schema IO ()
--     suppressNotices = execute_ . manipulation $
--       UnsafeManipulation "SET client_min_messages TO WARNING;"

--     displayUsage :: [String] -> IO ()
--     displayUsage args = do
--       putStrLn $ "Invalid command: \"" <> unwords args <> "\". Use:"
--       putStrLn "migrate    to run all available migrations"
--       putStrLn "rollback   to rollback all available migrations"
--       putStrLn "status     to display migrations run and migrations left to run"

-- getRunMigrationNames :: PQ db0 db0 IO [Text]
-- getRunMigrationNames =
--   fmap name <$>
--     (switchSchema (define createMigrations
--     & thenIx (execute selectMigrations)) >>= getRows)

-- displayListOfNames :: [Text] -> IO ()
-- displayListOfNames [] = Text.putStrLn "  None"
-- displayListOfNames xs =
--   let singleName n = Text.putStrLn $ "  - " <> n
--   in traverse_ singleName xs

-- displayUnrun :: [Text] -> IO ()
-- displayUnrun unrun =
--   Text.putStrLn "Migrate left to run:"
--   >> displayListOfNames unrun

-- displayRunned :: [Text] -> IO ()
-- displayRunned run =
--   Text.putStrLn "Migrate already run:"
--   >> displayListOfNames run

type MigrationsDB =
  '["squeal" ::: '["migrations" :::
    'Table ('["uq" ::: 'Unique '["name"]] :=>
      '["name" ::: 'NoDef :=> 'NotNull 'PGtext
       ,"at" ::: 'Def :=> 'NotNull 'PGtimestamptz])]]

-- | Creates a `MigrateTable` if it does not already exist.
createMigrations :: Definition MigrationsDB MigrationsDB
createMigrations =
  createSchemaIfNotExists #squeal >>>
  createTableIfNotExists (#squeal ! #migrations)
    ( (text & notNullable) `as` #name :*
      (timestampWithTimeZone & notNullable & default_ currentTimestamp)
        `as` #at )
    ( unique #name `as` #uq )

-- | Inserts a `Migration` into the `MigrateTable`, returning
-- the time at which it was inserted.
insertMigration :: Statement MigrationsDB String ()
insertMigration = Manipulation aParam genericRow $
  insertInto_ (#squeal ! #migrations) $
    Values_ (Set (param @1) `as` #name :* Default `as` #at)

-- | Deletes a `Migration` from the `MigrateTable`, returning
-- the time at which it was inserted.
deleteMigration :: Statement MigrationsDB String ()
deleteMigration = Manipulation aParam genericRow $
  deleteFrom_ (#squeal ! #migrations) (#name .== param @1)

-- | Selects a `Migration` from the `MigrateTable`, returning
-- the time at which it was inserted.
selectMigration :: Statement MigrationsDB String UTCTime
selectMigration = Query aParam #at $
  select_ #at
    $ from (table (#squeal ! #migrations))
    & where_ (#name .== param @1)

selectMigrations :: Statement MigrationsDB () (String, UTCTime)
selectMigrations = Query genericParams genericProductRow $
  select Star (from (table (#squeal ! #migrations)))

{----------

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
  & thenIx (liftIO (putStrLn "Migrate"))
  & thenIx (migrateUp migrations)
  & thenIx (liftIO (putStrLn "Rollback"))
  & thenIx (migrateDown migrations)
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
Migrate already run:
  None
Migrate left to run:
  - make users table
  - make emails table

>>> withArgs ["migrate"] main
Migrate already run:
  - make users table
  - make emails table
Migrate left to run:
  None

>>> withArgs ["rollback"] main
Migrate already run:
  None
Migrate left to run:
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
  , MigrateTable
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
import Control.Monad.Trans.Indexed
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
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
  runMigrate :: Path (Migration def) db0 db1 -> run db0 db1
-- | impure migrations
instance Migratory (Indexed PQ IO ()) (Indexed PQ IO ()) where
  runMigrate path = Indexed . switchSchema . runTransaction (Transactional defaultMode) $ do
    define createMigrations
    qtoMonoid upMigration path
    where
      upMigration step = do
        executed <- do
          result <- executeParams selectMigration (migrationName step)
          ntuples (result :: Result UTCTime)
        unless (executed == 1) $ do
          _ <- switchSchema . runIndexed $ migrationDef step
          executeParams_ insertMigration (migrationName step)
-- | pure migrations
instance Migratory Definition (Indexed PQ IO ()) where
  runMigrate = runMigrate . qmap (qmap ixDefine)
-- | impure rewinds
instance Migratory (OpQ (Indexed PQ IO ())) (OpQ (Indexed PQ IO ())) where
  runMigrate path = OpQ . Indexed . switchSchema . runTransaction (Transactional defaultMode)_ $ do
    define createMigrations
    qtoMonoid @FoldPath downMigration (reversePath path)
    where
      downMigration (OpQ step) = do
        executed <- do
          result <- executeParams selectMigration (migrationName step)
          ntuples (result :: Result UTCTime)
        unless (executed == 0) $ do
          _ <- switchSchema . runIndexed . getOpQ $ migrationDef step
          executeParams_ deleteMigration (migrationName step)
-- | pure rewinds
instance Migratory (OpQ Definition) (OpQ (Indexed PQ IO ())) where
  runMigrate = runMigrate . qmap (qmap (qmap ixDefine))
-- | impure rewindable migrations
instance Migratory
  (IsoQ (Indexed PQ IO ()))
  (IsoQ (Indexed PQ IO ())) where
    runMigrate path = IsoQ
      (runMigrate (qmap (qmap up) path))
      (getOpQ (runMigrate (qmap (qmap (OpQ . down)) path)))
-- | pure rewindable migrations
instance Migratory (IsoQ Definition) (IsoQ (Indexed PQ IO ())) where
  runMigrate = runMigrate . qmap (qmap (qmap ixDefine))

switchSchema :: (Functor m) => PQ db0 db1 m x -> PQ db0' db1' m x
switchSchema = coerce

-- | Run migrations.
migrate
  :: Migratory def (Indexed PQ IO ())
  => Path (Migration def) db0 db1
  -> PQ db0 db1 IO ()
migrate = runIndexed . runMigrate

-- | Run rewindable migrations.
migrateUp
  :: Migratory def (IsoQ (Indexed PQ IO ()))
  => Path (Migration def) db0 db1
  -> PQ db0 db1 IO ()
migrateUp = runIndexed . up . runMigrate

-- | Rewind migrations.
migrateDown
  :: Migratory def (IsoQ (Indexed PQ IO ()))
  => Path (Migration def) db0 db1
  -> PQ db1 db0 IO ()
migrateDown = runIndexed . down . runMigrate

ixDefine :: Definition db0 db1 -> Indexed PQ IO () db0 db1
ixDefine = Indexed . define

-- | The `TableType` for a Squeal migration.
type MigrateTable =
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

type MigrateSchema = '["schema_migrations" ::: 'Table MigrateTable]
type MigrateSchemas = Public MigrateSchema

-- | Creates a `MigrateTable` if it does not already exist.
createMigrations :: Definition MigrateSchemas MigrateSchemas
createMigrations =
  createTableIfNotExists #schema_migrations
    ( (text & notNullable) `as` #name :*
      (timestampWithTimeZone & notNullable & default_ currentTimestamp)
        `as` #executed_at )
    ( unique #name `as` #migrations_unique_name )

-- | Inserts a `Migration` into the `MigrateTable`, returning
-- the time at which it was inserted.
insertMigration :: Statement MigrateSchemas Text ()
insertMigration = Manipulation aParam genericRow $
  insertInto_ #schema_migrations $
    Values_ (Set (param @1) `as` #name :* Default `as` #executed_at)

-- | Deletes a `Migration` from the `MigrateTable`, returning
-- the time at which it was inserted.
deleteMigration :: Statement MigrateSchemas Text ()
deleteMigration = Manipulation aParam genericRow $
  deleteFrom_ #schema_migrations (#name .== param @1)

-- | Selects a `Migration` from the `MigrateTable`, returning
-- the time at which it was inserted.
selectMigration :: Statement MigrateSchemas Text UTCTime
selectMigration = Query aParam #executed_at $
  select_ #executed_at
    $ from (table (#schema_migrations))
    & where_ (#name .== param @1)

selectMigrations :: Statement MigrateSchemas () MigrationRow
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
        & thenIx (runIndexed (runMigrate migrations))
        & thenIx migrateStatus
      args -> displayUsage args

    migrateStatus :: PQ schema schema IO ()
    migrateStatus = switchSchema $ do
      runNames <- getRunMigrationNames
      let names = qtoList migrationName migrations
          unrunNames = names \\ runNames
      liftIO $ displayRunned runNames >> displayUnrun unrunNames

    suppressNotices :: PQ schema schema IO ()
    suppressNotices = execute_ . manipulation $
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
        & thenIx (migrateUp migrations)
        & thenIx migrateStatus
      ["rollback"] -> withConnection connectTo $
        suppressNotices
        & thenIx (migrateDown migrations)
        & thenIx migrateStatus
      args -> displayUsage args

    migrateStatus :: PQ schema schema IO ()
    migrateStatus = switchSchema $ do
      runNames <- getRunMigrationNames
      let names = qtoList migrationName migrations
          unrunNames = names \\ runNames
      liftIO $ displayRunned runNames >> displayUnrun unrunNames

    suppressNotices :: PQ schema schema IO ()
    suppressNotices = execute_ . manipulation $
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
    (switchSchema (define createMigrations
    & thenIx (execute selectMigrations)) >>= getRows)

displayListOfNames :: [Text] -> IO ()
displayListOfNames [] = Text.putStrLn "  None"
displayListOfNames xs =
  let singleName n = Text.putStrLn $ "  - " <> n
  in traverse_ singleName xs

displayUnrun :: [Text] -> IO ()
displayUnrun unrun =
  Text.putStrLn "Migrate left to run:"
  >> displayListOfNames unrun

displayRunned :: [Text] -> IO ()
displayRunned run =
  Text.putStrLn "Migrate already run:"
  >> displayListOfNames run

-----------}

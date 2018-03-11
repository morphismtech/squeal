{-# LANGUAGE
    ScopedTypeVariables
  , OverloadedStrings
  , DataKinds
  , GADTs
  , LambdaCase
  , OverloadedLabels
  , TypeApplications
  , FlexibleContexts
  , TypeOperators
#-}

module Squeal.PostgreSQL.Migration
  ( Migration (..)
  , Step (..)
  , skip
  , migrateUp
  , migrateDown
  , MigrationTable
  ) where

import Control.Category
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Function ((&))
import Prelude hiding (id, (.))

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Transaction

data Step io schema0 schema1 = Step
  { up :: PQ schema0 schema1 io ()
  , down :: PQ schema1 schema0 io ()
  }

skip :: Monad io => Step io schema schema -> Step io schema schema
skip _ = Step (return ()) (return ())

data Migration io schema0 schema1 where
  Done :: Migration io schema schema
  (:>>)
    :: Step io schema0 schema1
    -> Migration io schema1 schema2
    -> Migration io schema0 schema2
infixl 7 :>>
instance Category (Migration io) where
  id = Done
  (.) migration = \case
    Done -> migration
    step :>> steps -> step :>> (steps >>> migration)

migrateUp
  :: MonadBaseControl IO io
  => Migration io schema0 schema1
  -> PQ schema0 schema1 io ()
migrateUp migration = start & pqThen (upSteps migration & pqBind end)

migrateDown
  :: MonadBaseControl IO io
  => Migration io schema0 schema1
  -> PQ schema1 schema0 io ()
migrateDown migration = start & pqThen (downSteps migration & pqBind end)

type MigrationTable = "schema_migrations" :::
  '[ "unique_name" ::: 'Unique '["name"]] :=>
  '[ "name"        ::: 'NoDef :=> 'NotNull 'PGtext
   , "executed_at" :::   'Def :=> 'NotNull 'PGtimestamptz
   ]

createMigrations :: Definition '[] '[MigrationTable]
createMigrations = UnsafeDefinition $ renderDefinition $
  createTableIfNotExists #schema_migrations
    ( (text & notNull) `As` #name :*
      (timestampWithTimeZone & notNull) `As` #executed_at :* Nil )
    ( unique (Column #name :* Nil) `As` #unique_name :* Nil )

insertMigration :: Manipulation '[MigrationTable] '[ 'NotNull 'PGtext] '[]
insertMigration = insertRow_ #schema_migrations
  ( Set (param @1) `As` #name :*
    Default `As` #executed_at :* Nil )

deleteMigration :: Manipulation '[MigrationTable] '[ 'NotNull 'PGtext] '[]
deleteMigration = deleteFrom_ #schema_migrations (#name .== param @1)

selectMigration :: Query '[MigrationTable] '[ 'NotNull 'PGtext]
  '["executed_at" ::: 'NotNull 'PGtimestamptz]
selectMigration = select
  (#executed_at `As` #executed_at :* Nil)
  ( from (table (#schema_migrations `As` #m))
    & where_ (#name .== param @1))

start :: MonadBase IO io => PQ schema schema io ()
start = define createMigrations_
  & pqThen (begin (TransactionMode RepeatableRead ReadWrite) & void)
  where
    createMigrations_ :: Definition schema schema
    createMigrations_ = UnsafeDefinition $
      renderDefinition createMigrations

upSteps
  :: MonadBaseControl IO io
  => Migration io schema0 schema1
  -> PQ schema0 schema1 io ()
upSteps = \case
  Done -> return ()
  step :>> steps ->
    onExceptionRollback (up step)
    & pqThen (upSteps steps)

downSteps
  :: MonadBaseControl IO io
  => Migration io schema0 schema1
  -> PQ schema1 schema0 io ()
downSteps = \case
  Done -> return ()
  step :>> steps ->
    downSteps steps
    & pqThen (onExceptionRollback (down step))

end :: MonadBase IO io => result -> PQ schema schema io result
end result = commit >> return result

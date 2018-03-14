{-|
Module: Squeal.PostgreSQL.Migration
Description: Squeal migrations
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

`Squeal.PostgreSQL.Migration` defines a `Migration` type to safely
change the schema of your database over time.
-}

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
  ( -- * Migration
    Migration (..)
  , Step (..)
  , migrateUp
  , migrateDown
    -- * MigrationTable
  , MigrationTable
  , createMigration
  , insertStep
  , deleteStep
  , selectStep
  ) where

import Control.Category
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Generics.SOP (K(..))
import Data.Function ((&))
import Data.Text (Text)
import Prelude hiding (id, (.))

import Squeal.PostgreSQL

-- | A `Migration` is a schema-aligned sequence of `Step`s.
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

-- | A `Step` of a `Migration`, should contain an inverse pair of
-- `up` and `down` instructions and a unique `name`.
data Step io schema0 schema1 = Step
  { name :: Text -- ^ The `name` of a `Step`.
    -- Each `name` in a `Migration` should be unique.
  , up :: PQ schema0 schema1 io () -- ^ The `up` instruction of a `Step`.
  , down :: PQ schema1 schema0 io () -- ^ The `down` instruction of a `Step`.
  }


-- | Run a `Migration` by creating the `MigrationTable`
-- if it does not exist and then in a transaction, for each each `Step`
-- query to see if the `Step` is executed. If not, then
-- execute the `Step` and insert its row in `MigrationTable`.
migrateUp
  :: MonadBaseControl IO io
  => Migration io schema0 schema1 -- ^ migration to run
  -> PQ
    ("migration" ::: MigrationTable ': schema0)
    ("migration" ::: MigrationTable ': schema1)
    io ()
migrateUp migration =
  define createMigration
  & pqThen (begin defaultMode)
  & pqThen (upSteps migration)
  & pqThen (void commit)
  where

    upSteps
      :: MonadBaseControl IO io
      => Migration io schema0 schema1
      -> PQ
        ("migration" ::: MigrationTable ': schema0)
        ("migration" ::: MigrationTable ': schema1)
        io ()
    upSteps = \case
      Done -> return ()
      step :>> steps -> onExceptionRollback $
        upStep step
        & pqThen (upSteps steps)

    queryExecuted
      :: MonadBase IO io
      => Step io schema0 schema1 -> PQ
        ("migration" ::: MigrationTable ': schema0)
        ("migration" ::: MigrationTable ': schema0)
        io RowNumber
    queryExecuted =
      ntuples <=< runQueryParams selectStep . Only . name

    upStep
      :: MonadBase IO io
      => Step io schema0 schema1 -> PQ
        ("migration" ::: MigrationTable ': schema0)
        ("migration" ::: MigrationTable ': schema1)
        io ()
    upStep step =
      queryExecuted step
      & pqBind (\ (RowNumber executed) ->
        if executed == 1 -- up step has already been executed
          then PQ (\ _ -> return (K ())) -- unsafely switch schemas
          else
            pqEmbed (up step) -- safely switch schemas
            & pqThen (manipulateParams insertStep (Only (name step)) & void))
            -- insert up step execution record

-- | Rewind a `Migration` by creating the `MigrationTable`
-- if it does not exist and then in a transaction, for each each `Step`
-- query to see if the `Step` is executed. If it is, then
-- rewind the `Step` and delete its row in `MigrationTable`.
migrateDown
  :: MonadBaseControl IO io
  => Migration io schema0 schema1 -- ^ migration to rewind
  -> PQ
    ("migration" ::: MigrationTable ': schema1)
    ("migration" ::: MigrationTable ': schema0)
    io ()
migrateDown migration =
  define createMigration
  & pqThen (begin defaultMode)
  & pqThen (downSteps migration)
  & pqThen (void commit)
  where

    queryExecuted
      :: MonadBase IO io
      => Step io schema0 schema1 -> PQ
        ("migration" ::: MigrationTable ': schema1)
        ("migration" ::: MigrationTable ': schema1)
        io RowNumber
    queryExecuted =
      ntuples <=< runQueryParams selectStep . Only . name

    downStep
      :: MonadBase IO io
      => Step io schema0 schema1 -> PQ
        ("migration" ::: MigrationTable ': schema1)
        ("migration" ::: MigrationTable ': schema0)
        io ()
    downStep step =
      queryExecuted step
      & pqBind (\ (RowNumber executed) ->
        if executed == 0 -- up step has not been executed
          then PQ (\ _ -> return (K ())) -- unsafely switch schemas
          else
            pqEmbed (down step) -- safely switch schemas
            & pqThen (manipulateParams deleteStep (Only (name step)) & void))
            -- delete up step execution record

    downSteps
      :: MonadBaseControl IO io
      => Migration io schema0 schema1 -> PQ
        ("migration" ::: MigrationTable ': schema1)
        ("migration" ::: MigrationTable ': schema0)
        io ()
    downSteps = \case
      Done -> return ()
      step :>> steps -> onExceptionRollback $
        downSteps steps
        & pqThen (downStep step)

-- | The `TableType` for a Squeal migration.
type MigrationTable =
  '[ "unique_name" ::: 'Unique '["name"]] :=>
  '[ "name"        ::: 'NoDef :=> 'NotNull 'PGtext
   , "executed_at" :::   'Def :=> 'NotNull 'PGtimestamptz
   ]

-- | Creates a `MigrationTable` if it does not already exist.
createMigration
  :: Has "migration" schema MigrationTable
  => Definition schema schema
createMigration =
  createTableIfNotExists #migration
    ( (text & notNull) `As` #name :*
      (timestampWithTimeZone & notNull & default_ currentTimestamp)
        `As` #executed_at :* Nil )
    ( unique (Column #name :* Nil) `As` #unique_name :* Nil )

-- | Inserts a `Step` into the `MigrationTable`
insertStep
  :: Has "migration" schema MigrationTable
  => Manipulation schema '[ 'NotNull 'PGtext] '[]
insertStep = insertRow_ #migration
  ( Set (param @1) `As` #name :*
    Default `As` #executed_at :* Nil )

-- | Deletes a `Step` from the `MigrationTable`
deleteStep
  :: Has "migration" schema MigrationTable
  => Manipulation schema '[ 'NotNull 'PGtext ] '[]
deleteStep = deleteFrom_ #migration (#name .== param @1)

-- | Selects a `Step` from the `MigrationTable`, returning
-- the time at which it was executed.
selectStep
  :: Has "migration" schema MigrationTable
  => Query schema '[ 'NotNull 'PGtext ]
    '[ "executed_at" ::: 'NotNull 'PGtimestamptz ]
selectStep = select
  (#executed_at `As` #executed_at :* Nil)
  ( from (table (#migration `As` #m))
    & where_ (#name .== param @1))

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
  => Migration io schema0 schema1 -> PQ
    ("migration" ::: MigrationTable ': schema0)
    ("migration" ::: MigrationTable ': schema1)
    io ()
migrateUp migration = start & pqThen (upSteps migration & pqBind end)

migrateDown
  :: MonadBaseControl IO io
  => Migration io schema0 schema1 -> PQ
  ("migration" ::: MigrationTable ': schema1)
  ("migration" ::: MigrationTable ': schema0)
  io ()
migrateDown migration = start & pqThen (downSteps migration & pqBind end)

type MigrationTable =
  '[ "unique_name" ::: 'Unique '["name"]] :=>
  '[ "name"        ::: 'NoDef :=> 'NotNull 'PGtext
   , "executed_at" :::   'Def :=> 'NotNull 'PGtimestamptz
   ]

createMigration
  :: Has "migration" schema MigrationTable
  => Definition schema schema
createMigration =
  createTableIfNotExists #migration
    ( (text & notNull) `As` #name :*
      (timestampWithTimeZone & notNull & default_ currentTimestamp)
        `As` #executed_at :* Nil )
    ( unique (Column #name :* Nil) `As` #unique_name :* Nil )

insertStep
  :: Has "migration" schema MigrationTable
  => Manipulation schema '[ 'NotNull 'PGtext] '[]
insertStep = insertRow_ #migration
  ( Set (param @1) `As` #name :*
    Default `As` #executed_at :* Nil )

deleteStep
  :: Has "migration" schema MigrationTable
  => Manipulation schema '[ 'NotNull 'PGtext ] '[]
deleteStep = deleteFrom_ #migration (#name .== param @1)

selectStep
  :: Has "migration" schema MigrationTable
  => Query schema '[ 'NotNull 'PGtext ]
    '[ "executed_at" ::: 'NotNull 'PGtimestamptz ]
selectStep = select
  (#executed_at `As` #executed_at :* Nil)
  ( from (table (#migration `As` #m))
    & where_ (#name .== param @1))

start
  :: MonadBase IO io => PQ
    ("migration" ::: MigrationTable ': schema)
    ("migration" ::: MigrationTable ': schema)
    io ()
start = define createMigration
  & pqThen (begin (TransactionMode RepeatableRead ReadWrite) & void)

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
    pqEmbed (up step) & pqThen (upSteps steps)

downSteps
  :: MonadBaseControl IO io
  => Migration io schema0 schema1 -> PQ
    ("migration" ::: MigrationTable ': schema1)
    ("migration" ::: MigrationTable ': schema0)
    io ()
downSteps = \case
  Done -> return ()
  step :>> steps -> onExceptionRollback $
    downSteps steps & pqThen (pqEmbed (down step))

end :: MonadBase IO io => result -> PQ
  ("migration" ::: MigrationTable ': schema)
  ("migration" ::: MigrationTable ': schema)
  io result
end result = commit >> return result

avoid :: Monad m => PQ schema0 schema1 m ()
avoid = PQ $ \ _ -> return (K ())

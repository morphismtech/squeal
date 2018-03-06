{-# LANGUAGE
    ScopedTypeVariables
  , OverloadedStrings
#-}

module Squeal.PostgreSQL.Migration
  ( Migration
  , up
  , MigrationResult
  ) where

import Control.Category
import Control.Exception.Lifted
import Control.Monad
import Data.Function ((&))
import Generics.SOP
import Prelude hiding (id, (.))

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Transaction

data Migration schema0 schema1 = Migration
  { migrateUp :: PQ schema0 schema1 IO MigrationResult
  , migrateDown :: PQ schema1 schema0 IO MigrationResult
  }

instance Category Migration where
  id = Migration (return MigrationSuccess) (return MigrationSuccess)
  migration1 . migration0 = Migration
    { migrateUp = migrateUp migration0 & pqThen (migrateUp migration1)
    , migrateDown = migrateDown migration1 & pqThen (migrateDown migration0)
    }

up
  :: Migration schema0 schema1
  -> K Connection schema0
  -> IO (K MigrationResult schema1)
up (Migration mUp _) = runPQ $
  begin (TransactionMode RepeatableRead ReadWrite)
  & pqThen (restorably mUp)
  & pqBind ( \ result ->
    commit >> return result
  )
  where
    restorably u = PQ $ \ conn -> mask $ \ restore ->
      restore (runPQ u conn) `onException` LibPQ.exec (unK conn) "ROLLBACK"

data MigrationResult
  = MigrationSuccess
module Squeal.PostgreSQL.Migration
  ( Migration
--   , up
  , MigrationResult
  ) where

import Control.Category
import Control.Exception.Lifted
import Data.Function ((&))

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

-- up
--   :: Migration schema0 schema1
--   -> Connection schema0
--   -> IO (MigrationResult, Connection schema1)
-- up (Migration mUp _) conn0 = do
--   (begin _conn0) <- runPQ (begin (TransactionMode ReadCommitted ReadWrite)) conn0
--   & pqThen (Prelude.id mUp `onException` rollback)
--   & pqBind (\ result ->
--     commit & pqThen (return result))

data MigrationResult
  = MigrationSuccess
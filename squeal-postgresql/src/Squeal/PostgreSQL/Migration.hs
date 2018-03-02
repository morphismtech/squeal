module Squeal.PostgreSQL.Migration where

import Squeal.PostgreSQL.PQ

data Migration schema0 schema1 m x = Migration
  { migrateUp :: PQ schema0 schema1 m x
  , migrateDown :: PQ schema0 schema1 m x
  }


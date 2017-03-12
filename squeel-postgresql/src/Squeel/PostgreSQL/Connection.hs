{-# LANGUAGE
    DataKinds
#-}

module Squeel.PostgreSQL.Connection where

import qualified Database.PostgreSQL.LibPQ as LibPQ

newtype Connection db = Connection LibPQ.Connection

newtype PQ m db0 db1 x = PQ
  { runPQ :: Connection db0 -> m (x, Connection db1) }

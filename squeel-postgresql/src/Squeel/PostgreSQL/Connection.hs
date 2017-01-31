{-# LANGUAGE
    DataKinds
#-}

module Squeel.PostgreSQL.Connection where

import Control.Monad.IO.Class
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeel.PostgreSQL.Query
import Squeel.PostgreSQL.Value

newtype Connection db = Connection LibPQ.Connection

newtype PQ m db0 db1 x = PQ
  { runPQ :: Connection db0 -> m (x, Connection db1) }

exec
  :: (MonadIO io, Row xs x)
  => Query db0 db1 '[] xs
  -> PQ io db0 db1 [x]
exec = undefined

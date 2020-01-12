{-|
Module: Squeal.PostgreSQL.PQ.Exception
Description: Squeal exceptions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal exceptions
-}

module Squeal.PostgreSQL.PQ.Exception
  ( SquealException (..)
  , PQState (..)
  , LibPQ.ExecStatus
  , catchSqueal
  , handleSqueal
  , trySqueal
  ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Text (Text)
import UnliftIO (MonadUnliftIO (..), catch, handle, try)

import qualified Database.PostgreSQL.LibPQ as LibPQ

-- $setup
-- >>> import Squeal.PostgreSQL

-- | the state of LibPQ
data PQState = PQState
  { sqlExecStatus :: LibPQ.ExecStatus
  , sqlStateCode :: Maybe ByteString
    -- ^ https://www.postgresql.org/docs/current/static/errcodes-appendix.html
  , sqlErrorMessage :: Maybe ByteString
  } deriving (Eq, Show)

-- | `Exception`s that can be thrown by Squeal.
data SquealException
  = PQException PQState
  | ResultException Text
  | ParseException Text
  deriving (Eq, Show)
instance Exception SquealException

-- | Catch `SquealException`s.
catchSqueal
  :: MonadUnliftIO io
  => io a
  -> (SquealException -> io a) -- ^ handler
  -> io a
catchSqueal = catch

-- | Handle `SquealException`s.
handleSqueal
  :: MonadUnliftIO io
  => (SquealException -> io a) -- ^ handler
  -> io a -> io a
handleSqueal = handle

-- | `Either` return a `SquealException` or a result.
trySqueal
  :: MonadUnliftIO io
  => io a
  -> io (Either SquealException a)
trySqueal = try

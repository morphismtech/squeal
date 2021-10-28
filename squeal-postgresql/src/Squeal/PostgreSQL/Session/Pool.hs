{-|
Module: Squeal.PostgreSQL.Pool
Description: connection pools
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Connection pools.

Typical use case would be to create your pool using `createConnectionPool`
and run anything that requires the pool connection with `usingConnectionPool`.

Here's a simplified example:

>>> import Squeal.PostgreSQL

>>> :{
do
  let
    qry :: Query_ (Public '[]) () (Only Char)
    qry = values_ (inline 'a' `as` #fromOnly)
  pool <- createConnectionPool "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" 1 0.5 10
  chr <- usingConnectionPool pool $ do
    result <- runQuery qry
    Just (Only a) <- firstRow result
    return a
  destroyConnectionPool pool
  putChar chr
:}
a
-}

{-# LANGUAGE
    DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , TypeInType
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Session.Pool
  ( -- * Pool
    Pool
  , createConnectionPool
  , usingConnectionPool
  , destroyConnectionPool
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString
import Data.Time
import Data.Pool

import Squeal.PostgreSQL.Type.Schema
import Squeal.PostgreSQL.Session (PQ (..))
import Squeal.PostgreSQL.Session.Connection

-- | Create a striped pool of connections.
-- Although the garbage collector will destroy all idle connections when the pool is garbage collected it's recommended to manually `destroyConnectionPool` when you're done with the pool so that the connections are freed up as soon as possible.
createConnectionPool
  :: forall (db :: SchemasType) io. MonadIO io
  => ByteString
  -- ^ The passed string can be empty to use all default parameters, or it can
  -- contain one or more parameter settings separated by whitespace.
  -- Each parameter setting is in the form keyword = value. Spaces around the equal
  -- sign are optional. To write an empty value or a value containing spaces,
  -- surround it with single quotes, e.g., keyword = 'a value'. Single quotes and
  -- backslashes within the value must be escaped with a backslash, i.e., ' and \.
  -> Int
  -- ^ The number of stripes (distinct sub-pools) to maintain. The smallest acceptable value is 1.
  -> NominalDiffTime
  -- ^ Amount of time for which an unused connection is kept open. The smallest acceptable value is 0.5 seconds.
  -- The elapsed time before destroying a connection may be a little longer than requested, as the reaper thread wakes at 1-second intervals.
  -> Int
  -- ^ Maximum number of connections to keep open per stripe. The smallest acceptable value is 1.
  -- Requests for connections will block if this limit is reached on a single stripe, even if other stripes have idle connections available.
  -> io (Pool (K Connection db))
createConnectionPool conninfo stripes idle maxResrc =
  liftIO $ createPool (connectdb conninfo) finish stripes idle maxResrc

{-|
Temporarily take a connection from a `Pool`, perform an action with it,
and return it to the pool afterwards.

If the pool has an idle connection available, it is used immediately.
Otherwise, if the maximum number of connections has not yet been reached,
a new connection is created and used.
If the maximum number of connections has been reached, this function blocks
until a connection becomes available.
-}
usingConnectionPool
  :: (MonadIO io, MonadMask io)
  => Pool (K Connection db) -- ^ pool
  -> PQ db db io x -- ^ session
  -> io x
usingConnectionPool pool (PQ session) = mask $ \restore -> do
  (conn, local) <- liftIO $ takeResource pool
  ret <- restore (session conn) `onException`
            liftIO (destroyResource pool local conn)
  liftIO $ putResource local conn
  return $ unK ret

{- |
Destroy all connections in all stripes in the pool.
Note that this will ignore any exceptions in the destroy function.

This function is useful when you detect that all connections
in the pool are broken. For example after a database has been
restarted all connections opened before the restart will be broken.
In that case it's better to close those connections so that
`usingConnectionPool` won't take a broken connection from the pool
but will open a new connection instead.

Another use-case for this function is that when you know you are done
with the pool you can destroy all idle connections immediately
instead of waiting on the garbage collector to destroy them,
thus freeing up those connections sooner.
-}
destroyConnectionPool
  :: MonadIO io
  => Pool (K Connection db) -- ^ pool
  -> io ()
destroyConnectionPool = liftIO . destroyAllResources

{-|
Module: Squeal.PostgreSQL.Pool
Description: Connection pools
Copyright: (c) Eitan Chatav, 2017
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
    query :: Query_ (Public '[]) () (Only Char)
    query = values_ (literal 'a' `as` #fromOnly)
  pool <- createConnectionPool "host=localhost port=5432 dbname=exampledb" 1 0.5 10
  chr <- usingConnectionPool pool $ do
    result <- runQuery query
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
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , TypeInType
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Pool
  ( -- * Pools
    Pool
  , createConnectionPool
  , usingConnectionPool
  , destroyConnectionPool
  ) where

import Data.ByteString
import Data.Time
import Generics.SOP (K(..), unK)
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Pool (Pool, createPool, destroyAllResources, withResource)

import Squeal.PostgreSQL.PQ

-- | Create a striped pool of connections.
-- Although the garbage collector will destroy all idle connections when the pool is garbage collected it's recommended to manually `destroyAllResources` when you're done with the pool so that the connections are freed up as soon as possible.
createConnectionPool
  :: MonadUnliftIO io
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
  -> io (Pool (K Connection schemas))
createConnectionPool conninfo stripes idle maxResrc =
  createPool (connectdb conninfo) finish stripes idle maxResrc

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
  :: MonadUnliftIO io
  => Pool (K Connection schemas) -- ^ pool
  -> PQ schemas schemas io x -- ^ session
  -> io x
usingConnectionPool pool (PQ session) = unK <$> withResource pool session

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
  :: MonadUnliftIO io
  => Pool (K Connection schemas) -- ^ pool
  -> io ()
destroyConnectionPool = destroyAllResources

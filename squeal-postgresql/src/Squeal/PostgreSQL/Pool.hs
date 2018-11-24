{-|
Module: Squeal.PostgreSQL.Definition
Description: Pooled connections
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

A `MonadPQ` for pooled connections.
-}

{-# LANGUAGE
    DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , TypeInType
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Pool
  ( -- * Pools
    PoolPQ (..)
  , createConnectionPool
  , Pool
  , destroyAllResources
  , PoolPQRun
  , poolpqliftWith
  ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.ByteString
import Data.Pool
import Data.Time
import Generics.SOP (K(..))

import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Schema

{- | `PoolPQ` @db@ should be a drop-in replacement for `PQ` @db db@.

Typical use case would be to create your pool using `createConnectionPool` and run anything that requires the pool connection with it.

Here's a simplified example:

> type Schema = '[ "tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint2])]
>
> someQuery :: Manipulation Schema '[ 'NotNull 'PGint2] '[]
> someQuery = insertRow #tab
>  (Set (param @1) `As` #col :* Nil)
> OnConflictDoNothing (Returning Nil)
>
> insertOne :: (MonadBaseControl IO m, MonadPQ Schema m) => m ()
> insertOne = void $ manipulateParams someQuery . Only $ (1 :: Int16)
>
> insertOneInPool :: ByteString -> IO ()
> insertOneInPool connectionString = do
>   pool <- createConnectionPool connectionString 1 0.5 10
>   liftIO $ runPoolPQ (insertOne) pool

-}
newtype PoolPQ (db :: DBType) m x =
  PoolPQ { runPoolPQ :: Pool (K Connection db) -> m x }
  deriving Functor

-- | Create a striped pool of connections.
-- Although the garbage collector will destroy all idle connections when the pool is garbage collected it's recommended to manually `destroyAllResources` when you're done with the pool so that the connections are freed up as soon as possible.
createConnectionPool
  :: MonadBase IO io
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
createConnectionPool conninfo stripes idle maxResrc = liftBase $
  createPool (connectdb conninfo) finish stripes idle maxResrc

-- | `Applicative` instance for `PoolPQ`.
instance Monad m => Applicative (PoolPQ db m) where
  pure x = PoolPQ $ \ _ -> pure x
  PoolPQ f <*> PoolPQ x = PoolPQ $ \ pool -> do
    f' <- f pool
    x' <- x pool
    return $ f' x'

-- | `Monad` instance for `PoolPQ`.
instance Monad m => Monad (PoolPQ db m) where
  return = pure
  PoolPQ x >>= f = PoolPQ $ \ pool -> do
    x' <- x pool
    runPoolPQ (f x') pool

-- | `MonadTrans` instance for `PoolPQ`.
instance MonadTrans (PoolPQ db) where
  lift m = PoolPQ $ \ _pool -> m

-- | `MonadBase` instance for `PoolPQ`.
instance MonadBase b m => MonadBase b (PoolPQ db m) where
  liftBase = lift . liftBase

-- | `MonadPQ` instance for `PoolPQ`.
instance MonadBaseControl IO io => MonadPQ db (PoolPQ db io) where
  manipulateParams manipulation params = PoolPQ $ \ pool -> do
    withResource pool $ \ conn -> do
      (K result :: K (K Result ys) db) <- flip unPQ conn $
        manipulateParams manipulation params
      return result
  traversePrepared manipulation params = PoolPQ $ \ pool ->
    withResource pool $ \ conn -> do
      (K result :: K (list (K Result ys)) db) <- flip unPQ conn $
        traversePrepared manipulation params
      return result
  traversePrepared_ manipulation params = PoolPQ $ \ pool -> do
    withResource pool $ \ conn -> do
      (_ :: K () db) <- flip unPQ conn $
        traversePrepared_ manipulation params
      return ()
  liftPQ m = PoolPQ $ \ pool ->
    withResource pool $ \ conn -> do
      (K result :: K result db) <- flip unPQ conn $
        liftPQ m
      return result

-- | A snapshot of the state of a `PoolPQ` computation.
type PoolPQRun db =
  forall m x. Monad m => PoolPQ db m x -> m x

-- | Helper function in defining `MonadBaseControl` instance for `PoolPQ`.
poolpqliftWith :: Functor m => (PoolPQRun db -> m a) -> PoolPQ db m a
poolpqliftWith f = PoolPQ $ \ pool ->
  (f $ \ pq -> runPoolPQ pq pool)

-- | `MonadBaseControl` instance for `PoolPQ`.
instance MonadBaseControl b m => MonadBaseControl b (PoolPQ db m) where
  type StM (PoolPQ db m) x = StM m x
  liftBaseWith f =
    poolpqliftWith $ \ run -> liftBaseWith $ \ runInBase -> f $ runInBase . run
  restoreM = PoolPQ . const . restoreM

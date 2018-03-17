{-|
Module: Squeal.PostgreSQL.Definition
Description: Pooled connections
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

A `MonadPQ` for pooled connections.
-}

{-# LANGUAGE
    DataKinds
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , TypeInType
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Pool where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Pool
import Generics.SOP (K(..))

import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Schema

-- | `PoolPQ` @schema@ should be a drop-in replacement for
-- `PQ schema schema`.
newtype PoolPQ (schema :: TablesType) m x =
  PoolPQ { runPoolPQ :: Pool (K Connection schema) -> m x }
  deriving Functor

instance Monad m => Applicative (PoolPQ schema m) where
  pure x = PoolPQ $ \ _ -> pure x
  PoolPQ f <*> PoolPQ x = PoolPQ $ \ pool -> do
    f' <- f pool
    x' <- x pool
    return $ f' x'

instance Monad m => Monad (PoolPQ schema m) where
  return = pure
  PoolPQ x >>= f = PoolPQ $ \ pool -> do
    x' <- x pool
    runPoolPQ (f x') pool

instance MonadTrans (PoolPQ schema) where
  lift m = PoolPQ $ \ _pool -> m

instance MonadBase b m => MonadBase b (PoolPQ schema m) where
  liftBase = lift . liftBase

instance MonadBaseControl IO io => MonadPQ schema (PoolPQ schema io) where
  manipulateParams manipulation params = PoolPQ $ \ pool -> do
    withResource pool $ \ conn -> do
      (K result :: K (K Result ys) schema) <- flip runPQ conn $
        manipulateParams manipulation params
      return result
  traversePrepared manipulation params = PoolPQ $ \ pool ->
    withResource pool $ \ conn -> do
      (K result :: K (list (K Result ys)) schema) <- flip runPQ conn $
        traversePrepared manipulation params
      return result
  traversePrepared_ manipulation params = PoolPQ $ \ pool -> do
    withResource pool $ \ conn -> do
      (_ :: K () schema) <- flip runPQ conn $
        traversePrepared_ manipulation params
      return ()
  liftPQ m = PoolPQ $ \ pool -> 
    withResource pool $ \ conn -> do
      (K result :: K result schema) <- flip runPQ conn $
        liftPQ m
      return result

-- | A snapshot of the state of a `PoolPQ` computation.
type PoolPQRun schema =
  forall m x. Monad m => PoolPQ schema m x -> m x

-- | Helper function in defining `MonadBaseControl` instance for `PoolPQ`.
poolpqliftWith :: Functor m => (PoolPQRun schema -> m a) -> PoolPQ schema m a
poolpqliftWith f = PoolPQ $ \ pool ->
  (f $ \ pq -> runPoolPQ pq pool)

instance MonadBaseControl b m => MonadBaseControl b (PoolPQ schema m) where
  type StM (PoolPQ schema m) x = StM m x
  liftBaseWith f =
    poolpqliftWith $ \ run -> liftBaseWith $ \ runInBase -> f $ runInBase . run
  restoreM = PoolPQ . const . restoreM

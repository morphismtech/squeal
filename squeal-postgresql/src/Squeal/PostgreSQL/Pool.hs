{-# LANGUAGE
    DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Pool where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Pool

import Squeal.PostgreSQL.PQ

newtype PoolPQ schema m x = PoolPQ { runPoolPQ :: Pool (Connection schema) -> m x }
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
      (result, _ :: Connection schema) <- flip runPQ conn $
        manipulateParams manipulation params
      return result
  traversePrepared manipulation params = PoolPQ $ \ pool ->
    withResource pool $ \ conn -> do
      (result, _ :: Connection schema) <- flip runPQ conn $
        traversePrepared manipulation params
      return result
  traversePrepared_ manipulation params = PoolPQ $ \ pool -> do
    withResource pool $ \ conn -> do
      (_, _ :: Connection schema) <- flip runPQ conn $
        traversePrepared_ manipulation params
      return ()
  liftPQ m = PoolPQ $ \ pool -> 
    withResource pool $ \ conn -> do
      (result, _ :: Connection schema) <- flip runPQ conn $
        liftPQ m
      return result

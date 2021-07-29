{-|
Module: Squeal.PostgreSQL.Session.Transaction
Description: transaction control language
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

transaction control language
-}

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , LambdaCase
  , MonoLocalBinds
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  , TypeInType
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Session.Transaction
  ( -- * Transaction
    Transaction
  , MonadTransaction (..)
  , transactionally_
  , ephemerally_
    -- * Transaction Mode
  , TransactionMode (..)
  , defaultMode
  , longRunningMode
  , IsolationLevel (..)
  , AccessMode (..)
  , DeferrableMode (..)
  ) where

import UnliftIO

import Squeal.PostgreSQL.Session.Monad
import Squeal.PostgreSQL.Session.Result
import Squeal.PostgreSQL.Session.Transaction.Unsafe
  ( TransactionMode (..)
  , defaultMode
  , longRunningMode
  , IsolationLevel (..)
  , AccessMode (..)
  , DeferrableMode (..)
  )
import qualified Squeal.PostgreSQL.Session.Transaction.Unsafe as Unsafe

type Transaction db x = forall m. (MonadPQ db m, MonadResult m) => m x

class Monad tx => MonadTransaction db tx | tx -> db where
  transactionally
    :: TransactionMode
    -> Transaction db x
    -> tx x
  transactionallyRetry
    :: TransactionMode
    -> Transaction db x
    -> tx x
  ephemerally
    :: TransactionMode
    -> Transaction db x
    -> tx x

instance (MonadUnliftIO tx, MonadPQ db tx, MonadResult tx)
  => MonadTransaction db tx where
    transactionally = Unsafe.transactionally
    transactionallyRetry = Unsafe.transactionallyRetry
    ephemerally = Unsafe.ephemerally

transactionally_
  :: MonadTransaction db tx
  => Transaction db x
  -> tx x
transactionally_ = transactionally defaultMode

ephemerally_
  :: MonadTransaction db tx
  => Transaction db x
  -> tx x
ephemerally_ = ephemerally defaultMode

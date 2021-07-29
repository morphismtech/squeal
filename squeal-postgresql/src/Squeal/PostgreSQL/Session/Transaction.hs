{-|
Module: Squeal.PostgreSQL.Session.Transaction
Description: transaction control language
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

transaction control language
-}

{-# LANGUAGE
    MonoLocalBinds
  , RankNTypes
#-}

module Squeal.PostgreSQL.Session.Transaction
  ( -- * Transaction
    Transaction
  , transactionally
  , transactionally_
  , transactionallyRetry
  , ephemerally
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

{- | Run a computation `transactionally`;
first `begin`,
then run the computation,
`onException` `rollback` and rethrow the exception,
otherwise `commit` and `return` the result.
-}
transactionally
  :: (MonadUnliftIO tx, MonadPQ db tx)
  => TransactionMode
  -> Transaction db x -- ^ run inside a transaction
  -> tx x
transactionally = Unsafe.transactionally

-- | Run a computation `transactionally_`, in `defaultMode`.
transactionally_
  :: (MonadUnliftIO tx, MonadPQ db tx)
  => Transaction db x -- ^ run inside a transaction
  -> tx x
transactionally_ = Unsafe.transactionally_

{- |
`transactionallyRetry` a computation;

* first `begin`,
* then `try` the computation,
  - if it raises a serialization failure then `rollback` and restart the transaction,
  - if it raises any other exception then `rollback` and rethrow the exception,
  - otherwise `commit` and `return` the result.
-}
transactionallyRetry
  :: (MonadUnliftIO tx, MonadPQ db tx)
  => TransactionMode
  -> Transaction db x -- ^ run inside a transaction
  -> tx x
transactionallyRetry = Unsafe.transactionallyRetry

{- | Run a computation `ephemerally`;
Like `transactionally` but always `rollback`, useful in testing.
-}
ephemerally
  :: (MonadUnliftIO tx, MonadPQ db tx)
  => TransactionMode
  -> Transaction db x -- ^ run inside an ephemeral transaction
  -> tx x
ephemerally = Unsafe.ephemerally

{- | Run a computation `ephemerally` in `defaultMode`. -}
ephemerally_
  :: (MonadUnliftIO tx, MonadPQ db tx)
  => Transaction db x -- ^ run inside an ephemeral transaction
  -> tx x
ephemerally_ = Unsafe.ephemerally_

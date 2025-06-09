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
  , transactionallyRetry
  , ephemerally
  , withSavepoint
    -- * Transaction Mode
  , TransactionType (..)
  , TransactionMode (..)
  , defaultMode
  , longRunningMode
  , retryMode
  , IsolationLevel (..)
  , AccessMode (..)
  , DeferrableMode (..)
  ) where

import Control.Monad.Catch
import Data.ByteString

import Squeal.PostgreSQL.Session.Monad
import Squeal.PostgreSQL.Session.Transaction.Unsafe
  ( TransactionMode (..)
  , TransactionType (..)
  , defaultMode
  , longRunningMode
  , retryMode
  , IsolationLevel (..)
  , AccessMode (..)
  , DeferrableMode (..)
  )
import qualified Squeal.PostgreSQL.Session.Transaction.Unsafe as Unsafe

{- | A type of "safe" `Transaction`s,
do-blocks that permit only
database operations, pure functions, and synchronous exception handling
forbidding arbitrary `IO` operations.

To permit arbitrary `IO`,

>>> import qualified Squeal.PostgreSQL.Session.Transaction.Unsafe as Unsafe

Then use the @Unsafe@ qualified form of the functions below.

A safe `Transaction` can be run in two ways,

1) it can be run directly in `IO` because as a
   universally quantified type,
   @Transaction db x@ permits interpretation in "subtypes" like
   @(MonadPQ db m, MonadIO m) => m x@
   or
   @PQ db db IO x@

2) it can be run in a transaction block, using
   `transactionally`, `ephemerally`,
   or `transactionallyRetry`
-} 
type Transaction db x = forall m. MonadPQ db m => m x

{- | Run a computation `transactionally`;
first `Unsafe.begin`,
then run the computation,
`onException` `Unsafe.rollback` and rethrow the exception,
otherwise `Unsafe.commit` and `return` the result.
-}
transactionally
  :: (MonadMask tx, MonadPQ db tx)
  => Transaction db x -- ^ run inside a transaction
  -> tx x
transactionally =
  Unsafe.runTransaction (Unsafe.Transactional defaultMode)

{- |
`transactionallyRetry` a computation;

* first `Unsafe.begin`,
* then `try` the computation,
  - if it raises a serialization failure or deadloack detection,
    then `Unsafe.rollback` and restart the transaction,
  - if it raises any other exception then `Unsafe.rollback` and rethrow the exception,
  - otherwise `Unsafe.commit` and `return` the result.
-}
transactionallyRetry
  :: (MonadMask tx, MonadPQ db tx)
  => Transaction db x -- ^ run inside a transaction
  -> tx x
transactionallyRetry =
  Unsafe.runTransaction (Unsafe.TransactionalRetry retryMode)

{- | Run a computation `ephemerally`;
Like `transactionally` but always `Unsafe.rollback`, useful in testing.
-}
ephemerally
  :: (MonadMask tx, MonadPQ db tx)
  => Transaction db x -- ^ run inside an ephemeral transaction
  -> tx x
ephemerally =
  Unsafe.runTransaction (Unsafe.Ephemeral defaultMode)
{- | `withSavepoint`, used in a transaction block,
allows a form of nested transactions,
creating a savepoint, then running a transaction,
rolling back to the savepoint if it returned `Left`,
then releasing the savepoint and returning transaction's result.

Make sure to run `withSavepoint` in a transaction block,
not directly or you will provoke a SQL exception.
-}
withSavepoint
  :: ByteString -- ^ savepoint name
  -> Transaction db (Either e x)
  -> Transaction db (Either e x)
withSavepoint = Unsafe.withSavepoint

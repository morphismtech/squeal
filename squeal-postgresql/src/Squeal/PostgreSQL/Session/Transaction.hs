{-|
Module: Squeal.PostgreSQL.Session.Transaction
Description: transaction control language
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

transaction control language
-}

{-# LANGUAGE
    LambdaCase
  , MonoLocalBinds
  , OverloadedStrings
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
  , withSavepoint
    -- * Transaction Mode
  , TransactionMode (..)
  , defaultMode
  , longRunningMode
  , IsolationLevel (..)
  , AccessMode (..)
  , DeferrableMode (..)
  ) where

import Control.Monad.Catch
import Data.ByteString

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Render
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

{- | A type of "safe" `Transaction`s,
do-blocks that permit only database operations and pure
functions, forbidding arbitrary `IO` operations.

To permit arbitrary `IO`,

>>> import qualified Squeal.PostgreSQL.Session.Transaction.Unsafe as Unsafe

Then use the @Unsafe@ qualified form of the functions below.
-} 
type Transaction db x = forall m.
  ( MonadPQ db m
  , MonadResult m
  , MonadCatch m
  ) => m x

{- | Run a computation `transactionally`;
first `Unsafe.begin`,
then run the computation,
`onException` `Unsafe.rollback` and rethrow the exception,
otherwise `Unsafe.commit` and `return` the result.
-}
transactionally
  :: (MonadMask tx, MonadResult tx, MonadPQ db tx)
  => TransactionMode
  -> Transaction db x -- ^ run inside a transaction
  -> tx x
transactionally = Unsafe.transactionally

-- | Run a computation `transactionally_`, in `defaultMode`.
transactionally_
  :: (MonadMask tx, MonadResult tx, MonadPQ db tx)
  => Transaction db x -- ^ run inside a transaction
  -> tx x
transactionally_ = Unsafe.transactionally_

{- |
`transactionallyRetry` a computation;

* first `Unsafe.begin`,
* then `try` the computation,
  - if it raises a serialization failure then `Unsafe.rollback` and restart the transaction,
  - if it raises any other exception then `Unsafe.rollback` and rethrow the exception,
  - otherwise `Unsafe.commit` and `return` the result.
-}
transactionallyRetry
  :: (MonadMask tx, MonadResult tx, MonadPQ db tx)
  => TransactionMode
  -> Transaction db x -- ^ run inside a transaction
  -> tx x
transactionallyRetry = Unsafe.transactionallyRetry

{- | Run a computation `ephemerally`;
Like `transactionally` but always `Unsafe.rollback`, useful in testing.
-}
ephemerally
  :: (MonadMask tx, MonadResult tx, MonadPQ db tx)
  => TransactionMode
  -> Transaction db x -- ^ run inside an ephemeral transaction
  -> tx x
ephemerally = Unsafe.ephemerally

{- | Run a computation `ephemerally` in `defaultMode`. -}
ephemerally_
  :: (MonadMask tx, MonadResult tx, MonadPQ db tx)
  => Transaction db x -- ^ run inside an ephemeral transaction
  -> tx x
ephemerally_ = Unsafe.ephemerally_

{- | `withSavepoint`, used in a transaction block,
allows a form of nested transactions,
running a `Transaction` and returning its result,
rolling back to the savepoint if it returned `Left`,
or releasing the savepoint if it returned `Right`.
-}
withSavepoint
  :: ByteString -- ^ savepoint name
  -> Transaction db (Either e x)
  -> Transaction db (Either e x)
withSavepoint savepoint tx = do
  let svpt = "SAVEPOINT" <+> savepoint
  manipulate_ $ UnsafeManipulation $ svpt
  tx >>= \case
    Left err -> do
      manipulate_ $ UnsafeManipulation $ "ROLLBACK TO" <+> svpt
      return (Left err)
    Right x -> do
      manipulate_ $ UnsafeManipulation $ "RELEASE" <+> svpt
      return (Right x)

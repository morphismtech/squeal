{-|
Module: Squeal.PostgreSQL.Transaction
Description: Squeal transaction control language
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal transaction control language.
-}

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , LambdaCase
  , OverloadedStrings
  , TypeInType
#-}

module Squeal.PostgreSQL.Transaction
  ( -- * Transaction
    transactionally
  , transactionally_
  , transactionallyRetry
  , begin
  , commit
  , rollback
    -- * Transaction Mode
  , TransactionMode (..)
  , defaultMode
  , longRunningMode
  , IsolationLevel (..)
  , AccessMode (..)
  , DeferrableMode (..)
  ) where

import Control.Monad
import UnliftIO

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.PQ

{- | Run a computation `transactionally`;
first `begin`,
then run the computation,
`onException` `rollback` and rethrow the exception,
otherwise `commit` and `return` the result.
-}
transactionally
  :: (MonadUnliftIO tx, MonadPQ schemas tx)
  => TransactionMode
  -> tx x -- ^ run inside a transaction
  -> tx x
transactionally mode tx = mask $ \restore -> do
  begin mode
  result <- restore tx `onException` rollback
  commit
  return result

-- | Run a computation `transactionally_`, in `defaultMode`.
transactionally_
  :: (MonadUnliftIO tx, MonadPQ schemas tx)
  => tx x -- ^ run inside a transaction
  -> tx x
transactionally_ = transactionally defaultMode

{- |
`transactionallyRetry` a computation;

* first `begin`,
* then `try` the computation,
  - if it raises a serialization failure then `rollback` and restart the transaction,
  - if it raises any other exception then `rollback` and rethrow the exception,
  - otherwise `commit` and `return` the result.
-}
transactionallyRetry
  :: (MonadUnliftIO tx, MonadPQ schemas tx)
  => TransactionMode
  -> tx x -- ^ run inside a transaction
  -> tx x
transactionallyRetry mode tx = mask $ \restore ->
  loop . try $ do
    x <- restore tx
    commit
    return x
  where
    loop attempt = do
      begin mode
      attempt >>= \case
        Left (PQException _ (Just "40001") _) -> rollback >> loop attempt
        Left err -> rollback >> throwIO err
        Right x -> return x

-- | @BEGIN@ a transaction.
begin :: MonadPQ schemas tx => TransactionMode -> tx ()
begin mode = void . manipulate . UnsafeManipulation $
  "BEGIN" <+> renderSQL mode <> ";"

-- | @COMMIT@ a transaction.
commit :: MonadPQ schemas tx => tx ()
commit = void . manipulate $ UnsafeManipulation "COMMIT;"

-- | @ROLLBACK@ a transaction.
rollback :: MonadPQ schemas tx => tx ()
rollback = void . manipulate $ UnsafeManipulation "ROLLBACK;"

-- | The available transaction characteristics are the transaction `IsolationLevel`,
-- the transaction `AccessMode` (`ReadWrite` or `ReadOnly`), and the `DeferrableMode`.
data TransactionMode = TransactionMode
  { isolationLevel :: IsolationLevel
  , accessMode  :: AccessMode
  , deferrableMode :: DeferrableMode
  } deriving (Show, Eq)

-- | `TransactionMode` with a `Serializable` `IsolationLevel`,
-- `ReadWrite` `AccessMode` and `NotDeferrable` `DeferrableMode`.
defaultMode :: TransactionMode
defaultMode = TransactionMode ReadCommitted ReadWrite NotDeferrable

-- | `TransactionMode` with a `Serializable` `IsolationLevel`,
-- `ReadOnly` `AccessMode` and `Deferrable` `DeferrableMode`.
-- This mode is well suited for long-running reports or backups.
longRunningMode :: TransactionMode
longRunningMode = TransactionMode Serializable ReadOnly Deferrable

-- | Render a `TransactionMode`.
instance RenderSQL TransactionMode where
  renderSQL mode =
    "ISOLATION LEVEL"
      <+> renderSQL (isolationLevel mode)
      <+> renderSQL (accessMode mode)
      <+> renderSQL (deferrableMode mode)

-- | The SQL standard defines four levels of transaction isolation.
-- The most strict is `Serializable`, which is defined by the standard in a paragraph
-- which says that any concurrent execution of a set of `Serializable` transactions is
-- guaranteed to produce the same effect as running them one at a time in some order.
-- The other three levels are defined in terms of phenomena, resulting from interaction
-- between concurrent transactions, which must not occur at each level.
-- The phenomena which are prohibited at various levels are:
--
-- __Dirty read__: A transaction reads data written by a concurrent uncommitted transaction.
--
-- __Nonrepeatable read__: A transaction re-reads data it has previously read and finds that data
-- has been modified by another transaction (that committed since the initial read).
--
-- __Phantom read__: A transaction re-executes a query returning a set of rows that satisfy
-- a search condition and finds that the set of rows satisfying the condition
-- has changed due to another recently-committed transaction.
--
-- __Serialization anomaly__: The result of successfully committing a group of transactions is inconsistent
-- with all possible orderings of running those transactions one at a time.
--
-- In PostgreSQL, you can request any of the four standard transaction
-- isolation levels, but internally only three distinct isolation levels are implemented,
-- i.e. PostgreSQL's `ReadUncommitted` mode behaves like `ReadCommitted`.
-- This is because it is the only sensible way to map the standard isolation levels to
-- PostgreSQL's multiversion concurrency control architecture.
data IsolationLevel
  = Serializable
  -- ^ Dirty read is not possible.
  -- Nonrepeatable read is not possible.
  -- Phantom read is not possible.
  -- Serialization anomaly is not possible.
  | RepeatableRead
  -- ^ Dirty read is not possible.
  -- Nonrepeatable read is not possible.
  -- Phantom read is not possible.
  -- Serialization anomaly is possible.
  | ReadCommitted
  -- ^ Dirty read is not possible.
  -- Nonrepeatable read is possible.
  -- Phantom read is possible.
  -- Serialization anomaly is possible.
  | ReadUncommitted
  -- ^ Dirty read is not possible.
  -- Nonrepeatable read is possible.
  -- Phantom read is possible.
  -- Serialization anomaly is possible.
  deriving (Show, Eq)

-- | Render an `IsolationLevel`.
instance RenderSQL IsolationLevel where
  renderSQL = \case
    Serializable -> "SERIALIZABLE"
    ReadCommitted -> "READ COMMITTED"
    ReadUncommitted -> "READ UNCOMMITTED"
    RepeatableRead -> "REPEATABLE READ"

-- | The transaction access mode determines whether the transaction is `ReadWrite` or `ReadOnly`.
-- `ReadWrite` is the default. When a transaction is `ReadOnly`,
-- the following SQL commands are disallowed:
-- @INSERT@, @UPDATE@, @DELETE@, and @COPY FROM@
-- if the table they would write to is not a temporary table;
-- all @CREATE@, @ALTER@, and @DROP@ commands;
-- @COMMENT@, @GRANT@, @REVOKE@, @TRUNCATE@;
-- and @EXPLAIN ANALYZE@ and @EXECUTE@ if the command they would execute is among those listed.
-- This is a high-level notion of `ReadOnly` that does not prevent all writes to disk.
data AccessMode
  = ReadWrite
  | ReadOnly
  deriving (Show, Eq)

-- | Render an `AccessMode`.
instance RenderSQL AccessMode where
  renderSQL = \case
    ReadWrite -> "READ WRITE"
    ReadOnly -> "READ ONLY"

-- | The `Deferrable` transaction property has no effect
-- unless the transaction is also `Serializable` and `ReadOnly`.
-- When all three of these properties are selected for a transaction,
-- the transaction may block when first acquiring its snapshot,
-- after which it is able to run without the normal overhead of a
-- `Serializable` transaction and without any risk of contributing
-- to or being canceled by a serialization failure.
-- This `longRunningMode` is well suited for long-running reports or backups.
data DeferrableMode
  = Deferrable
  | NotDeferrable
  deriving (Show, Eq)

-- | Render a `DeferrableMode`.
instance RenderSQL DeferrableMode where
  renderSQL = \case
    Deferrable -> "DEFERRABLE"
    NotDeferrable -> "NOT DEFERRABLE"

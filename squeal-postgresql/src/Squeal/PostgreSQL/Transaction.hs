{-|
Module: Squeal.PostgreSQL.Transaction
Description: Squeal transaction control language
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal transaction control language.
-}

{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , OverloadedStrings
#-}

module Squeal.PostgreSQL.Transaction
  ( -- * Transaction
    transactionally
  , transactionally_
  , begin
  , commit
  , rollback
  , transactionallySchema
  , transactionallySchema_
    -- * Transaction Mode
  , TransactionMode (..)
  , defaultMode
  , longRunningMode
  , IsolationLevel (..)
  , renderIsolationLevel
  , AccessMode (..)
  , renderAccessMode
  , DeferrableMode (..)
  , renderDeferrableMode
  ) where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.ByteString
import Data.Monoid
import Generics.SOP

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Schema

-- | Run a schema invariant computation `transactionally`.
transactionally
  :: (MonadBaseControl IO tx, MonadPQ schema tx)
  => TransactionMode
  -> tx x -- ^ run inside a transaction
  -> tx x
transactionally mode tx = mask $ \restore -> do
  _ <- begin mode
  result <- restore tx `onException` rollback
  _ <- commit
  return result

-- | Run a schema invariant computation `transactionally_` in `defaultMode`.
transactionally_
  :: (MonadBaseControl IO tx, MonadPQ schema tx)
  => tx x -- ^ run inside a transaction
  -> tx x
transactionally_ = transactionally defaultMode

-- | @BEGIN@ a transaction.
begin :: MonadPQ schema tx => TransactionMode -> tx (K Result NilRelation)
begin mode = manipulate . UnsafeManipulation $
  "BEGIN" <+> renderTransactionMode mode <> ";"

-- | @COMMIT@ a schema invariant transaction.
commit :: MonadPQ schema tx => tx (K Result NilRelation)
commit = manipulate $ UnsafeManipulation "COMMIT;"

-- | @ROLLBACK@ a schema invariant transaction.
rollback :: MonadPQ schema tx => tx (K Result NilRelation)
rollback = manipulate $ UnsafeManipulation "ROLLBACK;"

-- | Run a schema changing computation `transactionallySchema`.
transactionallySchema
  :: MonadBaseControl IO io
  => TransactionMode
  -> PQ schema0 schema1 io x
  -> PQ schema0 schema1 io x
transactionallySchema mode u = PQ $ \ conn -> mask $ \ restore -> do
  _ <- liftBase . LibPQ.exec (unK conn) $
    "BEGIN" <+> renderTransactionMode mode <> ";"
  x <- restore (unPQ u conn)
    `onException` (liftBase (LibPQ.exec (unK conn) "ROLLBACK"))
  _ <- liftBase $ LibPQ.exec (unK conn) "COMMIT"
  return x

-- | Run a schema changing computation `transactionallySchema_` in `DefaultMode`.
transactionallySchema_
  :: MonadBaseControl IO io
  => PQ schema0 schema1 io x
  -> PQ schema0 schema1 io x
transactionallySchema_ = transactionallySchema defaultMode

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
defaultMode = TransactionMode Serializable ReadWrite NotDeferrable

-- | `TransactionMode` with a `Serializable` `IsolationLevel`,
-- `ReadOnly` `AccessMode` and `Deferrable` `DeferrableMode`.
-- This mode is well suited for long-running reports or backups.
longRunningMode :: TransactionMode
longRunningMode = TransactionMode Serializable ReadOnly Deferrable

-- | Render a `TransactionMode`.
renderTransactionMode :: TransactionMode -> ByteString
renderTransactionMode mode =
  "ISOLATION LEVEL"
    <+> renderIsolationLevel (isolationLevel mode)
    <+> renderAccessMode (accessMode mode)
    <+> renderDeferrableMode (deferrableMode mode)

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
renderIsolationLevel :: IsolationLevel -> ByteString
renderIsolationLevel = \case
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
renderAccessMode :: AccessMode -> ByteString
renderAccessMode = \case
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
renderDeferrableMode :: DeferrableMode -> ByteString
renderDeferrableMode = \case
  Deferrable -> "DEFERRABLE"
  NotDeferrable -> "NOT DEFERRABLE"

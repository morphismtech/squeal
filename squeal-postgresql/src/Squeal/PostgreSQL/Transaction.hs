{-# LANGUAGE
    DataKinds
  , EmptyCase
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , OverloadedStrings
  , MultiParamTypeClasses
  , TypeFamilies
  , TypeInType
#-}

module Squeal.PostgreSQL.Transaction
  ( transactionally
  , transactionally_
  , begin
  , commit
  , rollback
  , TransactionMode (..)
  , IsolationLevel (..)
  , ReadWriteMode (..)
  ) where

import Control.Exception.Lifted
import Control.Monad.Trans.Control
import Data.ByteString
import Data.Monoid
import Generics.SOP

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Schema

transactionally
  :: (MonadBaseControl IO tx, MonadPQ schema tx)
  => TransactionMode
  -> tx x -> tx x
transactionally mode tx = mask $ \restore -> do
  _ <- begin mode
  result <- restore tx `onException` rollback
  _ <- commit
  return result

transactionally_
  :: (MonadBaseControl IO tx, MonadPQ schema tx)
  => tx x -> tx x
transactionally_ = transactionally (TransactionMode ReadCommitted ReadWrite)

type family NilRelation :: RelationType where NilRelation = '[]

begin :: MonadPQ schema tx => TransactionMode -> tx (K Result NilRelation)
begin mode = manipulate . UnsafeManipulation $
  "BEGIN" <+> renderTransactionMode mode <> ";"

commit :: MonadPQ schema tx => tx (K Result NilRelation)
commit = manipulate $ UnsafeManipulation "COMMIT;"

rollback :: MonadPQ schema tx => tx (K Result NilRelation)
rollback = manipulate $ UnsafeManipulation "ROLLBACK;"

data TransactionMode = TransactionMode
  { isolationLevel :: IsolationLevel
  , readWriteMode  :: ReadWriteMode
  } deriving (Show, Eq)

renderTransactionMode :: TransactionMode -> ByteString
renderTransactionMode mode =
  "ISOLATION LEVEL"
    <+> renderIsolationLevel (isolationLevel mode)
    <+> renderReadWriteMode (readWriteMode mode)

data IsolationLevel
  = Serializable
  | RepeatableRead
  | ReadCommitted
  deriving (Show, Eq)

renderIsolationLevel :: IsolationLevel -> ByteString
renderIsolationLevel = \case
  Serializable -> "SERIALIZABLE"
  ReadCommitted -> "READ COMMITTED"
  RepeatableRead -> "REPEATABLE READ"

data ReadWriteMode
  = ReadWrite
  | ReadOnly
  deriving (Show, Eq)

renderReadWriteMode :: ReadWriteMode -> ByteString
renderReadWriteMode = \case
  ReadWrite -> "READ WRITE"
  ReadOnly -> "READ ONLY"

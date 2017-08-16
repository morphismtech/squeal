{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , FunctionalDependencies
  , PolyKinds
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Function ((&))
import Data.Monoid
import Data.Traversable
import Generics.SOP
import GHC.Exts hiding (fromList)
import GHC.TypeLits

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeal.PostgreSQL.Binary
import Squeal.PostgreSQL.Statement
import Squeal.PostgreSQL.Schema

newtype Connection (schema :: [(Symbol,[(Symbol,ColumnType)])]) =
  Connection { unConnection :: LibPQ.Connection }

newtype PQ
  (schema0 :: [(Symbol,[(Symbol,ColumnType)])])
  (schema1 :: [(Symbol,[(Symbol,ColumnType)])])
  (m :: * -> *)
  (x :: *) =
    PQ { runPQ :: Connection schema0 -> m (x, Connection schema1) }
    deriving Functor

evalPQ :: Functor m => PQ schema0 schema1 m x -> Connection schema0 -> m x
evalPQ (PQ pq) = fmap fst . pq

execPQ
  :: Functor m
  => PQ schema0 schema1 m x
  -> Connection schema0
  -> m (Connection schema1)
execPQ (PQ pq) = fmap snd . pq

pqAp
  :: Monad m
  => PQ schema0 schema1 m (x -> y)
  -> PQ schema1 schema2 m x
  -> PQ schema0 schema2 m y
pqAp (PQ f) (PQ x) = PQ $ \ conn -> do
  (f', conn') <- f conn
  (x', conn'') <- x conn'
  return (f' x', conn'')

pqBind
  :: Monad m
  => (x -> PQ schema1 schema2 m y)
  -> PQ schema0 schema1 m x
  -> PQ schema0 schema2 m y
pqBind f (PQ x) = PQ $ \ conn -> do
  (x', conn') <- x conn
  runPQ (f x') conn'

pqThen
  :: Monad m
  => PQ schema1 schema2 m y
  -> PQ schema0 schema1 m x
  -> PQ schema0 schema2 m y
pqThen pq2 pq1 = pq1 & pqBind (\ _ -> pq2)

define
  :: MonadBase IO io
  => Definition schema0 schema1
  -> PQ schema0 schema1 io (Maybe (Result '[]))
define (UnsafeDefinition q) = PQ $ \ (Connection conn) -> do
  result <- liftBase $ LibPQ.exec conn q
  return (Result <$> result, Connection conn)

pqThenDefine
  :: MonadBase IO io
  => Definition schema1 schema2
  -> PQ schema0 schema1 io x
  -> PQ schema0 schema2 io (Maybe (Result '[]))
pqThenDefine = pqThen . define

class Monad pq => MonadPQ schema pq | pq -> schema where

  manipulateParams
    :: ToParams x params
    => Manipulation schema params ys -> x -> pq (Maybe (Result ys))
  default manipulateParams
    :: (MonadTrans t, MonadPQ schema pq1, pq ~ t pq1)
    => ToParams x params
    => Manipulation schema params ys -> x -> pq (Maybe (Result ys))
  manipulateParams manipulation params = lift $
    manipulateParams manipulation params

  manipulate :: Manipulation schema '[] ys -> pq (Maybe (Result ys))
  manipulate statement = manipulateParams statement ()

  queryParams
    :: ToParams x params
    => Query schema params ys -> x -> pq (Maybe (Result ys))
  queryParams = manipulateParams . queryStatement

  query :: Query schema '[] ys -> pq (Maybe (Result ys))
  query q = queryParams q ()

  traversePrepared
    :: (ToParams x params, Traversable list)
    => Manipulation schema params ys -> list x -> pq (list (Maybe (Result ys)))
  default traversePrepared
    :: (MonadTrans t, MonadPQ schema pq1, pq ~ t pq1)
    => (ToParams x params, Traversable list)
    => Manipulation schema params ys -> list x -> pq (list (Maybe (Result ys)))
  traversePrepared manipulation params = lift $
    traversePrepared manipulation params

  forPrepared
    :: (ToParams x params, Traversable list)
    => list x -> Manipulation schema params ys -> pq (list (Maybe (Result ys)))
  forPrepared = flip traversePrepared

  traversePrepared_
    :: (ToParams x params, Foldable list)
    => Manipulation schema params ys -> list x -> pq ()
  default traversePrepared_
    :: (MonadTrans t, MonadPQ schema pq1, pq ~ t pq1)
    => (ToParams x params, Foldable list)
    => Manipulation schema params ys -> list x -> pq ()
  traversePrepared_ manipulation params = lift $
    traversePrepared_ manipulation params

  forPrepared_
    :: (ToParams x params, Traversable list)
    => list x -> Manipulation schema params ys -> pq ()
  forPrepared_ = flip traversePrepared_

  connectPoll :: pq LibPQ.PollingStatus

instance MonadBase IO io => MonadPQ schema (PQ schema schema io) where

  manipulateParams
    (UnsafeManipulation q :: Manipulation schema ps ys) (params :: x) =
      PQ $ \ (Connection conn) -> do
        let
          toParam' bytes = (LibPQ.invalidOid,bytes,LibPQ.Binary)
          params' = fmap (fmap toParam') (hcollapse (toParams @x @ps params))
        result <- liftBase $ LibPQ.execParams conn q params' LibPQ.Binary
        return (Result <$> result, Connection conn)

  traversePrepared
    (UnsafeManipulation q :: Manipulation schema xs ys) (list :: list x) =
      PQ $ \ (Connection conn) -> do
        let temp = "temporary_statement"
        _prepResult <- liftBase $ LibPQ.prepare conn temp q Nothing
        results <- for list $ \ params -> do
          let
            toParam' bytes = (bytes,LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (toParams @x @xs params))
          result <- liftBase $
            LibPQ.execPrepared conn temp params' LibPQ.Binary
          return $ Result <$> result
        _deallocResult <- liftBase $
          LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
        return (results, Connection conn)

  traversePrepared_
    (UnsafeManipulation q :: Manipulation schema xs ys) (list :: list x) =
      PQ $ \ (Connection conn) -> do
        let temp = "temporary_statement"
        _prepResult <- liftBase $ LibPQ.prepare conn temp q Nothing
        for_ list $ \ params -> do
          let
            toParam' bytes = (bytes,LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (toParams @x @xs params))
          _result <- liftBase $
            LibPQ.execPrepared conn temp params' LibPQ.Binary
          return ()
        _deallocResult <- liftBase $
          LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
        return ((), Connection conn)

  connectPoll = PQ $ \ (Connection conn) -> do
    pollingStatus <- liftBase $ LibPQ.connectPoll conn
    return (pollingStatus, Connection conn)

instance Monad m => Applicative (PQ schema schema m) where
  pure x = PQ $ \ conn -> pure (x, conn)
  (<*>) = pqAp

instance Monad m => Monad (PQ schema schema m) where
  return = pure
  (>>=) = flip pqBind

instance MonadTrans (PQ schema schema) where
  lift m = PQ $ \ conn -> do
    x <- m
    return (x, conn)

instance MonadBase b m => MonadBase b (PQ schema schema m) where
  liftBase = lift . liftBase

type PQRun schema =
  forall m x. Monad m => PQ schema schema m x -> m (x, Connection schema)

pqliftWith :: Functor m => (PQRun schema -> m a) -> PQ schema schema m a
pqliftWith f = PQ $ \ conn ->
  fmap (\ x -> (x, conn)) (f $ \ pq -> runPQ pq conn)

instance MonadBaseControl b m => MonadBaseControl b (PQ schema schema m) where
  type StM (PQ schema schema m) x = StM m (x, Connection schema)
  liftBaseWith f =
    pqliftWith $ \ run -> liftBaseWith $ \ runInBase -> f $ runInBase . run
  restoreM = PQ . const . restoreM

connectdb :: MonadBase IO io => ByteString -> io (Connection schema)
connectdb = fmap Connection . liftBase . LibPQ.connectdb

finish :: MonadBase IO io => Connection schema -> io ()
finish = liftBase . LibPQ.finish . unConnection

withConnection
  :: forall schema io x
   . MonadBaseControl IO io
  => ByteString
  -> (Connection schema -> io x)
  -> io x
withConnection connString = bracket (connectdb connString) finish

newtype Result (xs :: [(Symbol,ColumnType)])
  = Result { unResult :: LibPQ.Result }

newtype RowNumber = RowNumber { unRowNumber :: LibPQ.Row }

newtype ColumnNumber n cs c =
  UnsafeColumnNumber { getColumnNumber :: LibPQ.Column }

class KnownNat n => HasColumnNumber n columns column
  | n columns -> column where
  columnNumber :: ColumnNumber n columns column
  columnNumber =
    UnsafeColumnNumber . fromIntegral $ natVal' (proxy# :: Proxy# n)
instance {-# OVERLAPPING #-} HasColumnNumber 0 (column1:columns) column1
instance {-# OVERLAPPABLE #-}
  (KnownNat n, HasColumnNumber (n-1) columns column)
    => HasColumnNumber n (column' : columns) column

getValue
  :: (FromColumnValue colty y, MonadBase IO io)
  => RowNumber
  -> ColumnNumber n columns colty
  -> Result columns
  -> io y
getValue
  (RowNumber r)
  (UnsafeColumnNumber c :: ColumnNumber n columns colty)
  (Result result)
   = fromColumnValue @colty . K <$>
    liftBase (LibPQ.getvalue result r c)

getRow
  :: (FromRow columns y, MonadBase IO io)
  => RowNumber -> Result columns -> io y
getRow (RowNumber r) (Result result :: Result columns) = do
  let len = fromIntegral (lengthSList (Proxy @columns))
  row' <- traverse (liftBase . LibPQ.getvalue result r) [0 .. len - 1]
  case fromList row' of
    Nothing -> error "getRow: found unexpected length"
    Just row -> return $ fromRow @columns row

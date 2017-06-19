{-# LANGUAGE
    DataKinds
  , PolyKinds
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , MagicHash
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeel.PostgreSQL.PQ where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vinyl
import Data.Vinyl.Functor
import GHC.Exts
import GHC.TypeLits

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeel.PostgreSQL.Binary
import Squeel.PostgreSQL.Statement
import Squeel.PostgreSQL.Schema

newtype Connection db = Connection { unConnection :: LibPQ.Connection }

newtype PQ
  (db0 :: [(Symbol,[(Symbol,NullityType)])])
  (db1 :: [(Symbol,[(Symbol,NullityType)])])
  (m :: * -> *)
  (x :: *) =
    PQ { runPQ :: Connection db0 -> m (x, Connection db1) }
    deriving Functor

evalPQ :: Functor m => PQ db0 db1 m x -> Connection db0 -> m x
evalPQ (PQ pq) = fmap fst . pq

execPQ :: Functor m => PQ db0 db1 m x -> Connection db0 -> m (Connection db1)
execPQ (PQ pq) = fmap snd . pq

class MonadPQ pq where

  pqAp
    :: Monad m
    => pq db0 db1 m (x -> y)
    -> pq db1 db2 m x
    -> pq db0 db2 m y

  pqBind
    :: Monad m
    => (x -> pq db1 db2 m y)
    -> pq db0 db1 m x
    -> pq db0 db2 m y

  pqExec
    :: MonadBase IO io
    => Statement '[] db0 db1 xs
    -> pq db0 db1 io (Maybe (Result xs))

  pqExecParams
    :: (MonadBase IO io, ToOids ps, ToValues xs ps)
    => Statement ps db0 db1 ys
    -> Rec Identity xs
    -> pq db0 db1 io (Maybe (Result ys))

  pqPrepare
    :: (MonadBase IO io, ToOids ps)
    => ByteString
    -> Statement ps db0 db1 xs
    -> pq db0 db1 io (Maybe (Result []), PreparedStatement ps db0 db1 xs)

  pqExecPrepared
    :: (MonadBase IO io, ToValues xs ps)
    => PreparedStatement ps db0 db1 ys
    -> Rec Identity xs
    -> pq db0 db1 io (Maybe (Result ys))

instance MonadPQ PQ where

  pqAp (PQ f) (PQ x) = PQ $ \ conn -> do
    (f', conn') <- f conn
    (x', conn'') <- x conn'
    return (f' x', conn'')

  pqBind f (PQ x) = PQ $ \ conn -> do
    (x', conn') <- x conn
    runPQ (f x') conn'

  pqExec (UnsafeStatement q) = PQ $ \ (Connection conn) -> do
    result <- liftBase $ LibPQ.exec conn q
    return (Result <$> result, Connection conn)

  pqExecParams (UnsafeStatement q :: Statement ps db0 db1 ys) params =
    PQ $ \ (Connection conn) -> do
      let
        params' =
          [ Just (oid, param', LibPQ.Binary)
          | (oid, param') <-
              zip
                (toOids (proxy# :: Proxy# ps))
                (toValues (proxy# :: Proxy# ps) params)
          ]
      result <- liftBase $ LibPQ.execParams conn q params' LibPQ.Binary
      return (Result <$> result, Connection conn)

  pqPrepare statementName (UnsafeStatement q :: Statement ps db0 db1 ys) =
    PQ $ \ (Connection conn) -> do
      result <- liftBase $
        LibPQ.prepare conn statementName q (Just (toOids (proxy# :: Proxy# ps)))
      return
        ( ( Result <$> result
          , UnsafePreparedStatement statementName
        ) , Connection conn )

  pqExecPrepared (q :: PreparedStatement ps db0 db1 ys) params =
    PQ $ \ (Connection conn) -> do
      let
        params' =
          [ Just (param', LibPQ.Binary)
          | param' <- toValues (proxy# :: Proxy# ps) params
          ]
      result <- liftBase $
        LibPQ.execPrepared conn (renderPreparedStatement q) params' LibPQ.Binary
      return (Result <$> result, Connection conn)

instance Monad m => Applicative (PQ db db m) where
  pure x = PQ $ \ conn -> pure (x, conn)
  (<*>) = pqAp

instance Monad m => Monad (PQ db db m) where
  return = pure
  (>>=) = flip pqBind

instance MonadTrans (PQ db db) where
  lift m = PQ $ \ conn -> do
    x <- m
    return (x, conn)

instance MonadBase b m => MonadBase b (PQ db db m) where
  liftBase = lift . liftBase

type PQRun db = forall m x. Monad m => PQ db db m x -> m (x, Connection db)

pqliftWith :: Functor m => (PQRun db -> m a) -> PQ db db m a
pqliftWith f = PQ $ \ conn ->
  fmap (\ x -> (x, conn)) (f $ \ pq -> runPQ pq conn)

instance MonadBaseControl b m => MonadBaseControl b (PQ db db m) where
  type StM (PQ db db m) x = StM m (x, Connection db)
  liftBaseWith f =
    pqliftWith $ \ run -> liftBaseWith $ \ runInBase -> f $ runInBase . run
  restoreM = PQ . const . restoreM

connectdb :: MonadBase IO io => ByteString -> io (Connection db)
connectdb = fmap Connection . liftBase . LibPQ.connectdb

finish :: MonadBase IO io => Connection db -> io ()
finish = liftBase . LibPQ.finish . unConnection

withConnection
  :: MonadBaseControl IO io
  => ByteString
  -> (Connection db -> io x)
  -> io x
withConnection connString action =
  bracket (connectdb connString) finish action

newtype Result xs = Result { unResult :: LibPQ.Result }

newtype RowNumber = RowNumber { unRowNumber :: LibPQ.Row }

newtype ColumnNumber cs c = ColumnNumber { unColumnNumber :: LibPQ.Column }

colNum0 :: ColumnNumber (c0:cs) c0
colNum0 = ColumnNumber 0

colNum1 :: ColumnNumber (c0:c1:cs) c1
colNum1 = ColumnNumber 1

colNum2 :: ColumnNumber (c0:c1:c2:cs) c2
colNum2 = ColumnNumber 2

colNum3 :: ColumnNumber (c0:c1:c2:c3:cs) c3
colNum3 = ColumnNumber 3

colNum4 :: ColumnNumber (c0:c1:c2:c3:c4:cs) c4
colNum4 = ColumnNumber 4

getvalue
  :: (FromValue x y, MonadBase IO io)
  => Proxy# x
  -> Result xs
  -> RowNumber
  -> ColumnNumber xs x
  -> io (Maybe (Either Text y))
getvalue proxy (Result result) (RowNumber r) (ColumnNumber c) = liftBase $
  fmap (fmap (decodeValue proxy)) (LibPQ.getvalue result r c)

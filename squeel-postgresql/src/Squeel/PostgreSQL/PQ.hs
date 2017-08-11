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
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeel.PostgreSQL.PQ where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Maybe
import Data.Proxy (Proxy)
import Data.Text (Text)
import Generics.SOP
import GHC.Exts hiding (fromList)
import GHC.TypeLits

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeel.PostgreSQL.Binary
import Squeel.PostgreSQL.Statement
import Squeel.PostgreSQL.Schema

newtype Connection (db :: [(Symbol,[(Symbol,ColumnType)])]) =
  Connection { unConnection :: LibPQ.Connection }

newtype PQ
  (db0 :: [(Symbol,[(Symbol,ColumnType)])])
  (db1 :: [(Symbol,[(Symbol,ColumnType)])])
  (m :: * -> *)
  (x :: *) =
    PQ { runPQ :: Connection db0 -> m (x, Connection db1) }
    deriving Functor

evalPQ :: Functor m => PQ db0 db1 m x -> Connection db0 -> m x
evalPQ (PQ pq) = fmap fst . pq

execPQ :: Functor m => PQ db0 db1 m x -> Connection db0 -> m (Connection db1)
execPQ (PQ pq) = fmap snd . pq

pqAp
  :: Monad m
  => PQ db0 db1 m (x -> y)
  -> PQ db1 db2 m x
  -> PQ db0 db2 m y
pqAp (PQ f) (PQ x) = PQ $ \ conn -> do
  (f', conn') <- f conn
  (x', conn'') <- x conn'
  return (f' x', conn'')

pqBind
  :: Monad m
  => (x -> PQ db1 db2 m y)
  -> PQ db0 db1 m x
  -> PQ db0 db2 m y
pqBind f (PQ x) = PQ $ \ conn -> do
  (x', conn') <- x conn
  runPQ (f x') conn'

pqThen
  :: Monad m
  => PQ db1 db2 m y
  -> PQ db0 db1 m x
  -> PQ db0 db2 m y
pqThen pq2 pq1 = pq1 & pqBind (\ _ -> pq2)

pqExec
  :: MonadBase IO io
  => Definition db0 db1
  -> PQ db0 db1 io (Maybe (Result '[]))
pqExec (UnsafeDefinition q) = PQ $ \ (Connection conn) -> do
  result <- liftBase $ LibPQ.exec conn q
  return (Result <$> result, Connection conn)

pqThenExec
  :: MonadBase IO io
  => Definition db1 db2
  -> PQ db0 db1 io x
  -> PQ db0 db2 io (Maybe (Result '[]))
pqThenExec = pqThen . pqExec

class Monad m => MonadPQ db m | m -> db where

  pqExecParams
    :: (ToOids ps, AllZip HasEncoding ps xs)
    => Manipulation db ps ys
    -> NP I xs
    -> m (Maybe (Result ys))
  default pqExecParams
    :: ( MonadTrans t
       , MonadPQ db m1
       , m ~ t m1
       , ToOids ps
       , AllZip HasEncoding ps xs )
    => Manipulation db ps ys
    -> NP I xs
    -> m (Maybe (Result ys))
  pqExecParams manipulation params = lift $ pqExecParams manipulation params

  pqExecNil
    :: Manipulation db '[] ys
    -> m (Maybe (Result ys))
  pqExecNil statement = pqExecParams statement Nil

instance MonadBase IO io => MonadPQ db (PQ db db io) where

  pqExecParams (UnsafeManipulation q :: Manipulation db ps ys) params =
    PQ $ \ (Connection conn) -> do
      let
        paramValues = encodings (Proxy :: Proxy ps) params
        oids = toOids (proxy# :: Proxy# ps)
        params' =
          [ Just (oid, param', LibPQ.Binary)
          | (oid, param') <- zip oids paramValues
          ]
      result <- liftBase $ LibPQ.execParams conn q params' LibPQ.Binary
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

newtype Value pgs m x = Value { runValue :: Result pgs -> m x }
  deriving (Functor)
instance Applicative m => Applicative (Value pgs m) where
  pure x = Value $ \ _result -> pure x
  mf <*> mx = Value $ \ result -> runValue mf result <*> runValue mx result
instance Monad m => Monad (Value pgs m) where
  return = pure
  mx >>= f = Value $ \ result -> do
    x <- runValue mx result
    runValue (f x) result
instance MonadTrans (Value pgs) where
  lift m = Value $ \ _result -> m
instance MonadBase b m => MonadBase b (Value pgs m) where
  liftBase = lift . liftBase

getValue
  :: (HasDecoding pg x, MonadBase IO io)
  => RowNumber
  -> ColumnNumber n pgs pg
  -> Value pgs (ExceptT Text (MaybeT io)) x
getValue (RowNumber r) (UnsafeColumnNumber c :: ColumnNumber n pgs pg) =
  Value $ \ (Result result) -> do
    maybeBytestring <- liftBase $ LibPQ.getvalue result r c
    case maybeBytestring of
      Nothing -> mzero
      Just bytestring -> case decodeValue (Proxy :: Proxy pg) bytestring of
        Left err -> throwError err
        Right val -> return val

getRow
  :: (AllZip HasDecoding pgs xs, MonadBase IO io)
  => RowNumber -> Value pgs (ExceptT Text (MaybeT io)) (NP I xs)
getRow (RowNumber r) = Value $ \ (Result result :: Result pgs) -> do
  maybeBytestrings <- traverse (liftBase . LibPQ.getvalue result r)
    [0 .. fromIntegral (lengthSList (Proxy :: Proxy pgs)) - 1]
  case fromList (catMaybes maybeBytestrings) of
    Nothing -> mzero
    Just bytestrings -> case decodings (Proxy :: Proxy pgs) bytestrings of
      Left err -> throwError err
      Right row -> return row

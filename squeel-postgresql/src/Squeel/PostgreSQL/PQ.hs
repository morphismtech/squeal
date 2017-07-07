{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DataKinds
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

newtype Connection db = Connection { unConnection :: LibPQ.Connection }

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

class AtkeyPQ pq where

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

  pqThen
    :: Monad m
    => pq db1 db2 m y
    -> pq db0 db1 m x
    -> pq db0 db2 m y
  pqThen pq2 pq1 = pq1 & pqBind (\ _ -> pq2)

class AtkeyPQ pq => MonadPQ pq where

  pqExec
    :: MonadBase IO io
    => Statement '[] '[] db0 db1
    -> pq db0 db1 io (Maybe (Result '[]))

  pqPrepare
    :: (MonadBase IO io, ToOids ps)
    => ByteString
    -> Statement ps xs db0 db1
    -> pq db0 db1 io (Maybe (Result '[]), PreparedStatement ps xs db0 db1)

class MonadQuery db m | m -> db where

  pqExecParams
    :: (ToOids ps, AllZip HasEncoding ps xs)
    => Statement ps ys db db
    -> NP I xs
    -> m (Maybe (Result ys))

  pqExecNil
    :: Statement '[] ys db db
    -> m (Maybe (Result ys))
  pqExecNil statement = pqExecParams statement Nil

  pqExecPrepared
    :: AllZip HasEncoding ps xs
    => PreparedStatement ps ys db db
    -> NP I xs
    -> m (Maybe (Result ys))

instance AtkeyPQ PQ where

  pqAp (PQ f) (PQ x) = PQ $ \ conn -> do
    (f', conn') <- f conn
    (x', conn'') <- x conn'
    return (f' x', conn'')

  pqBind f (PQ x) = PQ $ \ conn -> do
    (x', conn') <- x conn
    runPQ (f x') conn'

instance MonadPQ PQ where

  pqExec (UnsafeStatement q) = PQ $ \ (Connection conn) -> do
    result <- liftBase $ LibPQ.exec conn q
    return (Result <$> result, Connection conn)

  pqPrepare statementName (UnsafeStatement q :: Statement ps ys db0 db1) =
    PQ $ \ (Connection conn) -> do
      result <- liftBase $
        LibPQ.prepare conn statementName q (Just (toOids (proxy# :: Proxy# ps)))
      return
        ( ( Result <$> result
          , UnsafePreparedStatement statementName
        ) , Connection conn )

instance MonadBase IO io => MonadQuery db (PQ db db io) where

  pqExecParams (UnsafeStatement q :: Statement ps ys db0 db1) params =
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

  pqExecPrepared (q :: PreparedStatement ps ys db0 db1) params =
    PQ $ \ (Connection conn) -> do
      let
        paramValues = encodings (Proxy :: Proxy ps) params
        params' = [ Just (param', LibPQ.Binary) | param' <- paramValues ]
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

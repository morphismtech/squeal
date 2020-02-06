{-|
Module: Squeal.PostgreSQL.PQ
Description: PQ monad
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Using Squeal in your application will come down to defining
the @db@ of your database and including @PQ db db@ in your
application's monad transformer stack, giving it an instance of `MonadPQ`.
-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DefaultSignatures
  , FunctionalDependencies
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , OverloadedStrings
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ
  ( PQ (PQ, unPQ)
  , runPQ
  , execPQ
  , evalPQ
  , withConnection
  ) where

import Control.Category
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import UnliftIO (MonadUnliftIO (..), bracket,  throwIO)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Functor ((<&>))
import Data.Kind
import Data.Traversable
import Generics.SOP
import PostgreSQL.Binary.Encoding (encodingBytes)
import Prelude hiding (id, (.))

import qualified Control.Monad.Fail as Fail
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Encoding as Encoding

import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.PQ.Connection
import Squeal.PostgreSQL.PQ.Encode
import Squeal.PostgreSQL.PQ.Exception
import Squeal.PostgreSQL.PQ.Indexed
import Squeal.PostgreSQL.PQ.Oid
import Squeal.PostgreSQL.PQ.Monad
import Squeal.PostgreSQL.PQ.Result
import Squeal.PostgreSQL.PQ.Statement
import Squeal.PostgreSQL.Schema

-- | We keep track of the schema via an Atkey indexed state monad transformer,
-- `PQ`.
newtype PQ
  (db0 :: SchemasType)
  (db1 :: SchemasType)
  (m :: Type -> Type)
  (x :: Type) =
    PQ { unPQ :: K LibPQ.Connection db0 -> m (K x db1) }

instance Monad m => Functor (PQ db0 db1 m) where
  fmap f (PQ pq) = PQ $ \ conn -> do
    K x <- pq conn
    return $ K (f x)

-- | Run a `PQ` and keep the result and the `LibPQ.Connection`.
runPQ
  :: Functor m
  => PQ db0 db1 m x
  -> K LibPQ.Connection db0
  -> m (x, K LibPQ.Connection db1)
runPQ (PQ pq) conn = (\ x -> (unK x, K (unK conn))) <$> pq conn
  -- K x <- pq conn
  -- return (x, K (unK conn))

-- | Execute a `PQ` and discard the result but keep the `LibPQ.Connection`.
execPQ
  :: Functor m
  => PQ db0 db1 m x
  -> K LibPQ.Connection db0
  -> m (K LibPQ.Connection db1)
execPQ (PQ pq) conn = mapKK (\ _ -> unK conn) <$> pq conn

-- | Evaluate a `PQ` and discard the `LibPQ.Connection` but keep the result.
evalPQ
  :: Functor m
  => PQ db0 db1 m x
  -> K LibPQ.Connection db0
  -> m x
evalPQ (PQ pq) conn = unK <$> pq conn

instance IndexedMonadTrans PQ where

  pqAp (PQ f) (PQ x) = PQ $ \ conn -> do
    K f' <- f conn
    K x' <- x (K (unK conn))
    return $ K (f' x')

  pqBind f (PQ x) = PQ $ \ conn -> do
    K x' <- x conn
    unPQ (f x') (K (unK conn))

instance IndexedMonadTransPQ PQ where

  define (UnsafeDefinition q) = PQ $ \ (K conn) -> do
    resultMaybe <- liftIO $ LibPQ.exec conn q
    case resultMaybe of
      Nothing -> throwIO ConnectionException
      Just result -> K <$> okResult_ result

instance (MonadIO io, db0 ~ db, db1 ~ db) => MonadPQ db (PQ db0 db1 io) where

  executeParams (Manipulation encode decode (UnsafeManipulation q)) x =
    PQ $ \ kconn@(K conn) -> do
      let
        formatParam
          :: forall param. OidOfNull db param
          => K (Maybe Encoding.Encoding) param
          -> io (K (Maybe (LibPQ.Oid, ByteString, LibPQ.Format)) param)
        formatParam (K maybeEncoding) = do
          oid <- liftIO $ runReaderT (oidOfNull @db @param) kconn
          return . K $ maybeEncoding <&> \encoding ->
            (oid, encodingBytes encoding, LibPQ.Binary)
      encodedParams <- liftIO $ runReaderT (runEncodeParams encode x) kconn
      formattedParams <- hcollapse <$>
        hctraverse' (Proxy @(OidOfNull db)) formatParam encodedParams
      resultMaybe <- liftIO $
        LibPQ.execParams conn (q <> ";") formattedParams LibPQ.Binary
      case resultMaybe of
        Nothing -> throwIO ConnectionException
        Just result -> do
          okResult_ result
          return $ K (Result decode result)
  executeParams (Query encode decode q) x =
    executeParams (Manipulation encode decode (queryStatement q)) x

  executePrepared (Manipulation encode decode (UnsafeManipulation q :: Manipulation '[] db params row)) list =
    PQ $ \ kconn@(K conn) -> liftIO $ do
      let
        temp = "temporary_statement"
        oidOfParam :: forall p. OidOfNull db p => (IO :.: K LibPQ.Oid) p
        oidOfParam = Comp $ K <$> runReaderT (oidOfNull @db @p) kconn
        oidsOfParams :: NP (IO :.: K LibPQ.Oid) params
        oidsOfParams = hcpure (Proxy @(OidOfNull db)) oidOfParam
      oids <- hcollapse <$> hsequence' oidsOfParams
      prepResultMaybe <- LibPQ.prepare conn temp (q <> ";") (Just oids)
      case prepResultMaybe of
        Nothing -> throwIO ConnectionException
        Just prepResult -> okResult_ prepResult
      results <- for list $ \ params -> do
        encodedParams <- runReaderT (runEncodeParams encode params) kconn
        let
          formatParam encoding = (encodingBytes encoding, LibPQ.Binary)
          formattedParams =
            [ formatParam <$> maybeParam
            | maybeParam <- hcollapse encodedParams
            ]
        resultMaybe <-
          LibPQ.execPrepared conn temp formattedParams LibPQ.Binary
        case resultMaybe of
          Nothing -> throwIO ConnectionException
          Just result -> do
            okResult_ result
            return $ Result decode result
      deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
      case deallocResultMaybe of
        Nothing -> throwIO ConnectionException
        Just deallocResult -> okResult_ deallocResult
      return (K results)
  executePrepared (Query encode decode q) list =
    executePrepared (Manipulation encode decode (queryStatement q)) list

  executePrepared_ (Manipulation encode _ (UnsafeManipulation q :: Manipulation '[] db params row)) list =
    PQ $ \ kconn@(K conn) -> liftIO $ do
      let
        temp = "temporary_statement"
        oidOfParam :: forall p. OidOfNull db p => (IO :.: K LibPQ.Oid) p
        oidOfParam = Comp $ K <$> runReaderT (oidOfNull @db @p) kconn
        oidsOfParams :: NP (IO :.: K LibPQ.Oid) params
        oidsOfParams = hcpure (Proxy @(OidOfNull db)) oidOfParam
      oids <- hcollapse <$> hsequence' oidsOfParams
      prepResultMaybe <- LibPQ.prepare conn temp (q <> ";") (Just oids)
      case prepResultMaybe of
        Nothing -> throwIO ConnectionException
        Just prepResult -> okResult_ prepResult
      for_ list $ \ params -> do
        encodedParams <- runReaderT (runEncodeParams encode params) kconn
        let
          formatParam encoding = (encodingBytes encoding, LibPQ.Binary)
          formattedParams =
            [ formatParam <$> maybeParam
            | maybeParam <- hcollapse encodedParams
            ]
        resultMaybe <-
          LibPQ.execPrepared conn temp formattedParams LibPQ.Binary
        case resultMaybe of
          Nothing -> throwIO ConnectionException
          Just result -> okResult_ result
      deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
      case deallocResultMaybe of
        Nothing -> throwIO ConnectionException
        Just deallocResult -> okResult_ deallocResult
      return (K ())
  executePrepared_ (Query encode decode q) list =
    executePrepared_ (Manipulation encode decode (queryStatement q)) list

instance (Monad m, db0 ~ db1)
  => Applicative (PQ db0 db1 m) where
  pure x = PQ $ \ _conn -> pure (K x)
  (<*>) = pqAp

instance (Monad m, db0 ~ db1)
  => Monad (PQ db0 db1 m) where
  return = pure
  (>>=) = flip pqBind

instance (Monad m, db0 ~ db1)
  => Fail.MonadFail (PQ db0 db1 m) where
  fail = Fail.fail

instance db0 ~ db1 => MFunctor (PQ db0 db1) where
  hoist f (PQ pq) = PQ (f . pq)

instance db0 ~ db1 => MonadTrans (PQ db0 db1) where
  lift m = PQ $ \ _conn -> do
    x <- m
    return (K x)

instance db0 ~ db1 => MMonad (PQ db0 db1) where
  embed f (PQ pq) = PQ $ \ conn -> do
    evalPQ (f (pq conn)) conn

instance (MonadIO m, schema0 ~ schema1)
  => MonadIO (PQ schema0 schema1 m) where
  liftIO = lift . liftIO

instance (MonadUnliftIO m, db0 ~ db1)
  => MonadUnliftIO (PQ db0 db1 m) where
  withRunInIO
      :: ((forall a . PQ db0 schema1 m a -> IO a) -> IO b)
      -> PQ db0 schema1 m b
  withRunInIO inner = PQ $ \conn ->
    withRunInIO $ \(run :: (forall x . m x -> IO x)) ->
      K <$> inner (\pq -> run $ unK <$> unPQ pq conn)

instance (Monad m, Semigroup r, db0 ~ db1) => Semigroup (PQ db0 db1 m r) where
  f <> g = pqAp (fmap (<>) f) g

instance (Monad m, Monoid r, db0 ~ db1) => Monoid (PQ db0 db1 m r) where
  mempty = pure mempty

-- | Do `connectdb` and `finish` before and after a computation.
withConnection
  :: forall db0 db1 io x
   . MonadUnliftIO io
  => ByteString
  -> PQ db0 db1 io x
  -> io x
withConnection connString action = do
  K x <- bracket (connectdb connString) finish (unPQ action)
  return x

okResult_ :: MonadIO io => LibPQ.Result -> io ()
okResult_ result = liftIO $ do
  status <- LibPQ.resultStatus result
  case status of
    LibPQ.CommandOk -> return ()
    LibPQ.TuplesOk -> return ()
    _ -> do
      stateCode <- LibPQ.resultErrorField result LibPQ.DiagSqlstate
      msg <- LibPQ.resultErrorMessage result
      throwIO . PQException $ PQState status stateCode msg

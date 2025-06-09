{-|
Module: Squeal.PostgreSQL.Session
Description: sessions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Using Squeal in your application will come down to defining
the @DB :: @`SchemasType` of your database and including @PQ DB DB@ in your
application's monad transformer stack, giving it an instance of `MonadPQ` @DB@.
-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DefaultSignatures
  , DerivingVia
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
  , DataKinds
  , PolyKinds
  , StandaloneDeriving
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Session
  ( PQ (PQ, unPQ)
  , withConnection
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (MonadPlus(..))
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Catch
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..))
import Control.Monad.Trans.Indexed
import UnliftIO (MonadUnliftIO(..))
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Hashable
import Data.Kind
import Data.String
import Generics.SOP
import PostgreSQL.Binary.Encoding (encodingBytes)
import Prelude hiding (id, (.))

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Encoding as Encoding

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Session.Encode
import Squeal.PostgreSQL.Session.Exception
import Squeal.PostgreSQL.Session.Oid
import Squeal.PostgreSQL.Session.Monad
import Squeal.PostgreSQL.Session.Result
import Squeal.PostgreSQL.Session.Statement
import Squeal.PostgreSQL.Type.Schema

-- | We keep track of the schema via an Atkey indexed state monad transformer,
-- `PQ`.
newtype PQ
  (db0 :: SchemasType)
  (db1 :: SchemasType)
  (m :: Type -> Type)
  (x :: Type) =
    PQ { unPQ :: LibPQ.Connection -> m x }

deriving via ReaderT LibPQ.Connection m
  instance Functor m => Functor (PQ db0 db1 m)

deriving via ReaderT LibPQ.Connection m
  instance (Applicative m, db0 ~ db1)
    => Applicative (PQ db0 db1 m)

deriving via ReaderT LibPQ.Connection m
  instance (Monad m, db0 ~ db1)
    => Monad (PQ db0 db1 m)

deriving via ReaderT LibPQ.Connection m
  instance (MonadFail m, db0 ~ db1)
    => MonadFail (PQ db0 db1 m)

deriving via ReaderT LibPQ.Connection
  instance MFunctor (PQ db0 db1)

deriving via ReaderT LibPQ.Connection
  instance db0 ~ db1 => MonadTrans (PQ db0 db1)

deriving via ReaderT LibPQ.Connection
  instance db0 ~ db1 => MMonad (PQ db0 db1)

deriving via ReaderT LibPQ.Connection m
  instance (MonadIO m, db0 ~ db1)
    => MonadIO (PQ db0 db1 m)

instance (MonadResult m, db0 ~ db1) => MonadResult (PQ db0 db1 m)

instance IxMonadTrans PQ where

  apIx (PQ f) (PQ x) = PQ $ \ conn -> do
    f' <- f conn
    x' <- x conn
    return $ f' x'

  bindIx f (PQ x) = PQ $ \ conn -> do
    x' <- x conn
    unPQ (f x') conn

instance IxMonadTransPQ PQ where

  define (UnsafeDefinition q) = PQ $ \ conn -> liftIO $ do
    resultMaybe <-  LibPQ.exec conn q
    case resultMaybe of
      Nothing -> throwM $ ConnectionException "LibPQ.exec"
      Just result -> okResult_ result

instance (MonadIO io, MonadResult io, db0 ~ db, db1 ~ db)
  => MonadPQ db (PQ db0 db1 io) where

  executeParams (Manipulation encode decode (UnsafeManipulation q)) x =
    PQ $ \ conn -> liftIO $ do
      let
        formatParam
          :: forall param. OidOfNull db param
          => K (Maybe Encoding.Encoding) param
          -> IO (K (Maybe (LibPQ.Oid, ByteString, LibPQ.Format)) param)
        formatParam (K maybeEncoding) = do
          oid <- runReaderT (oidOfNull @db @param) (K conn)
          return . K $ maybeEncoding <&> \encoding ->
            (oid, encodingBytes encoding, LibPQ.Binary)
      encodedParams <- runReaderT (runEncodeParams encode x) (K conn)
      formattedParams <- hcollapse <$>
        hctraverse' (Proxy @(OidOfNull db)) formatParam encodedParams
      resultMaybe <-
        LibPQ.execParams conn (q <> ";") formattedParams LibPQ.Binary
      case resultMaybe of
        Nothing -> throwM $ ConnectionException "LibPQ.execParams"
        Just result -> do
          okResult_ result
          return $ Result decode result
  executeParams (Query encode decode q) x =
    executeParams (Manipulation encode decode (queryManip q)) x

  prepare (Manipulation encode decode (UnsafeManipulation q :: Manipulation '[] db params row)) = do
    let
      statementNum = fromString $ case show (hash q) of
        '-':num -> "negative_" <> num
        num -> num

      prepName = "prepared_statement_" <> statementNum

      prepare' = PQ $ \ conn -> liftIO $ do
        let
          oidOfParam :: forall p. OidOfNull db p => (IO :.: K LibPQ.Oid) p
          oidOfParam = Comp $ K <$> runReaderT (oidOfNull @db @p) (K conn)
          oidsOfParams :: NP (IO :.: K LibPQ.Oid) params
          oidsOfParams = hcpure (Proxy @(OidOfNull db)) oidOfParam
        oids <- hcollapse <$> hsequence' oidsOfParams
        prepResultMaybe <- LibPQ.prepare conn prepName (q <> ";") (Just oids)
        case prepResultMaybe of
          Nothing -> throwM $ ConnectionException "LibPQ.prepare"
          Just prepResult -> K <$> okResult_ prepResult

      deallocate' = execute_ . manipulation . UnsafeManipulation $
        "DEALLOCATE" <+> prepName

      runPrepared' params = PQ $ \ conn -> liftIO $ do
        encodedParams <- runReaderT (runEncodeParams encode params) (K conn)
        let
          formatParam encoding = (encodingBytes encoding, LibPQ.Binary)
          formattedParams =
            [ formatParam <$> maybeParam
            | maybeParam <- hcollapse encodedParams
            ]
        resultMaybe <-
          LibPQ.execPrepared conn prepName formattedParams LibPQ.Binary
        case resultMaybe of
          Nothing -> throwM $ ConnectionException "LibPQ.runPrepared"
          Just result -> do
            okResult_ result
            return $ Result decode result

    prepare'
    return $ Prepared runPrepared' deallocate'

  prepare (Query encode decode q) = prepare $
    Manipulation encode decode (queryManip q)

instance (MonadUnliftIO m, db0 ~ db1)
  => MonadUnliftIO (PQ db0 db1 m) where
  withRunInIO
      :: ((forall a . PQ db0 schema1 m a -> IO a) -> IO b)
      -> PQ db0 schema1 m b
  withRunInIO inner = PQ $ \conn ->
    withRunInIO $ \(run :: (forall x . m x -> IO x)) ->
      inner (\pq -> run $ unPQ pq conn)

instance (MonadBase b m)
  => MonadBase b (PQ schema schema m) where
  liftBase = lift . liftBase

instance db0 ~ db1 => MonadTransControl (PQ db0 db1) where
  type StT (PQ db0 db1) a = a
  liftWith f = PQ $ \conn -> (f $ \pq -> unPQ pq conn)
  restoreT ma = PQ . const $ ma

-- | A snapshot of the state of a `PQ` computation, used in MonadBaseControl Instance
type PQRun db =
  forall m x. Monad m => PQ db db m x -> m x

instance (MonadBaseControl b m, db0 ~ db1)
  => MonadBaseControl b (PQ db0 db1 m) where
  type StM (PQ db0 db1 m) x = StM m x
  restoreM = PQ . const . restoreM
  liftBaseWith f =
    pqliftWith $ \ run -> liftBaseWith $ \ runInBase -> f $ runInBase . run
    where
      pqliftWith :: Functor m => (PQRun db -> m a) -> PQ db db m a
      pqliftWith g = PQ $ \ conn -> g $ \ pq -> unPQ pq conn

instance (MonadThrow m, db0 ~ db1)
  => MonadThrow (PQ db0 db1 m) where
  throwM = lift . throwM

instance (MonadCatch m, db0 ~ db1)
  => MonadCatch (PQ db0 db1 m) where
  catch (PQ m) f = PQ $ \k -> m k `catch` \e -> unPQ (f e) k

instance (MonadMask m, db0 ~ db1)
  => MonadMask (PQ db0 db1 m) where
  mask a = PQ $ \e -> mask $ \u -> unPQ (a $ q u) e
    where q u (PQ b) = PQ (u . b)

  uninterruptibleMask a =
    PQ $ \k -> uninterruptibleMask $ \u -> unPQ (a $ q u) k
      where q u (PQ b) = PQ (u . b)

  generalBracket acquire release use = PQ $ \k ->
    generalBracket
      (unPQ acquire k)
      (\resource exitCase -> unPQ (release resource exitCase) k)
      (\resource -> unPQ (use resource) k)

instance (Monad m, Semigroup r, db0 ~ db1) => Semigroup (PQ db0 db1 m r) where
  f <> g = apIx (fmap (<>) f) g

instance (Monad m, Monoid r, db0 ~ db1) => Monoid (PQ db0 db1 m r) where
  mempty = pure mempty

instance MonadFix m => MonadFix (PQ db db m) where
  mfix f = PQ $ \conn -> mfix $ \ a -> unPQ (f a) conn

instance (Monad m, Alternative m, db0 ~ db1)
  => Alternative (PQ db0 db1 m) where
    empty = lift empty
    altL <|> altR = PQ $ \ conn ->
      unPQ altL conn <|> unPQ altR conn

instance (MonadPlus m, db0 ~ db1) => MonadPlus (PQ db0 db1 m)

-- | Do `connectdb` and `finish` before and after a computation.
withConnection
  :: forall db0 db1 io x
   . (MonadIO io, MonadMask io)
  => ByteString
  -> PQ db0 db1 io x
  -> io x
withConnection connString action = bracket
  (liftIO (LibPQ.connectdb connString))
  (liftIO . LibPQ.finish)
  (unPQ action)

okResult_ :: MonadIO io => LibPQ.Result -> io ()
okResult_ result = liftIO $ do
  status <- LibPQ.resultStatus result
  case status of
    LibPQ.CommandOk -> return ()
    LibPQ.TuplesOk -> return ()
    _ -> do
      stateCodeMaybe <- LibPQ.resultErrorField result LibPQ.DiagSqlstate
      case stateCodeMaybe of
        Nothing -> throwM $ ConnectionException "LibPQ.resultErrorField"
        Just stateCode -> do
          msgMaybe <- LibPQ.resultErrorMessage result
          case msgMaybe of
            Nothing -> throwM $ ConnectionException "LibPQ.resultErrorMessage"
            Just msg -> throwM . SQLException $ SQLState status stateCode msg

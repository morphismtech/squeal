{-|
Module: Squeal.PostgreSQL.PQ
Description: PQ monad
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

This module is where Squeal commands actually get executed by
`LibPQ`. It containts two typeclasses, `IndexedMonadTransPQ` for executing
a `Definition` and `MonadPQ` for executing a `Manipulation` or `Query`,
and a `PQ` type with instances for them.

Using Squeal in your application will come down to defining
the @schema@ of your database and including @PQ schema schema@ in your
application's monad transformer stack, giving it an instance of `MonadPQ`.

This module also provides functions for retrieving rows from the `LibPQ.Result`
of executing Squeal commands.
-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE
    DefaultSignatures
  , FunctionalDependencies
  , FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ
  ( -- * Connection
    LibPQ.Connection
  , connectdb
  , finish
  , withConnection
  , lowerConnection
    -- * PQ
  , PQ (PQ, unPQ)
  , runPQ
  , execPQ
  , evalPQ
  , IndexedMonadTransPQ (..)
  , MonadPQ (..)
  , PQRun
  , pqliftWith
    -- * Result
  , LibPQ.Result
  , LibPQ.Row
  , ntuples
  , getRow
  , getRows
  , nextRow
  , firstRow
  , liftResult
  , LibPQ.ExecStatus (..)
  , resultStatus
  , resultErrorMessage
  ) where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (unpack)
import Data.Foldable
import Data.Function ((&))
import Data.Kind
import Data.Monoid
import Data.Traversable
import Generics.SOP

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeal.PostgreSQL.Binary
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

-- For `MonadPQ` transformer instances
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Trans.List

import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict

{- | Makes a new connection to the database server.

This function opens a new database connection using the parameters taken
from the string conninfo.

The passed string can be empty to use all default parameters, or it can
contain one or more parameter settings separated by whitespace.
Each parameter setting is in the form keyword = value. Spaces around the equal
sign are optional. To write an empty value or a value containing spaces,
surround it with single quotes, e.g., keyword = 'a value'. Single quotes and
backslashes within the value must be escaped with a backslash, i.e., ' and \.

To specify the schema you wish to connect with, use type application.

>>> :set -XDataKinds
>>> :set -XPolyKinds
>>> :set -XTypeOperators
>>> type Schema = '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint2]]
>>> :set -XTypeApplications
>>> :set -XOverloadedStrings
>>> conn <- connectdb @Schema "host=localhost port=5432 dbname=exampledb"

Note that, for now, squeal doesn't offer any protection from connecting
with the wrong schema!
-}
connectdb
  :: forall schema io
   . MonadBase IO io
  => ByteString -- ^ conninfo
  -> io (K LibPQ.Connection schema)
connectdb = fmap K . liftBase . LibPQ.connectdb

-- | Closes the connection to the server.
finish :: MonadBase IO io => K LibPQ.Connection schema -> io ()
finish = liftBase . LibPQ.finish . unK

-- | Do `connectdb` and `finish` before and after a computation.
withConnection
  :: forall schema0 schema1 io x
   . MonadBaseControl IO io
  => ByteString
  -> PQ schema0 schema1 io x
  -> io x
withConnection connString action = do
  K x <- bracket (connectdb connString) finish (unPQ action)
  return x

-- | Safely `lowerConnection` to a smaller schema.
lowerConnection
  :: K LibPQ.Connection (table ': schema)
  -> K LibPQ.Connection schema
lowerConnection (K conn) = K conn

-- | We keep track of the schema via an Atkey indexed state monad transformer,
-- `PQ`.
newtype PQ
  (schema0 :: SchemaType)
  (schema1 :: SchemaType)
  (m :: Type -> Type)
  (x :: Type) =
    PQ { unPQ :: K LibPQ.Connection schema0 -> m (K x schema1) }

instance Monad m => Functor (PQ schema0 schema1 m) where
  fmap f (PQ pq) = PQ $ \ conn -> do
    K x <- pq conn
    return $ K (f x)

-- | Run a `PQ` and keep the result and the `Connection`.
runPQ
  :: Functor m
  => PQ schema0 schema1 m x
  -> K LibPQ.Connection schema0
  -> m (x, K LibPQ.Connection schema1)
runPQ (PQ pq) conn = (\ x -> (unK x, K (unK conn))) <$> pq conn
  -- K x <- pq conn
  -- return (x, K (unK conn))

-- | Execute a `PQ` and discard the result but keep the `Connection`.
execPQ
  :: Functor m
  => PQ schema0 schema1 m x
  -> K LibPQ.Connection schema0
  -> m (K LibPQ.Connection schema1)
execPQ (PQ pq) conn = mapKK (\ _ -> unK conn) <$> pq conn

-- | Evaluate a `PQ` and discard the `Connection` but keep the result.
evalPQ
  :: Functor m
  => PQ schema0 schema1 m x
  -> K LibPQ.Connection schema0
  -> m x
evalPQ (PQ pq) conn = unK <$> pq conn

-- | An [Atkey indexed monad](https://bentnib.org/paramnotions-jfp.pdf) is a `Functor`
-- [enriched category](https://ncatlab.org/nlab/show/enriched+category).
-- An indexed monad transformer transforms a `Monad` into an indexed monad.
-- And, `IndexedMonadTransPQ` is a class for indexed monad transformers that
-- support running `Definition`s using `define` and embedding a computation
-- in a larger schema using `pqEmbed`.
class IndexedMonadTransPQ pq where

  -- | indexed analog of `<*>`
  pqAp
    :: Monad m
    => pq schema0 schema1 m (x -> y)
    -> pq schema1 schema2 m x
    -> pq schema0 schema2 m y

  -- | indexed analog of `join`
  pqJoin
    :: Monad m
    => pq schema0 schema1 m (pq schema1 schema2 m y)
    -> pq schema0 schema2 m y
  pqJoin pq = pq & pqBind id

  -- | indexed analog of `=<<`
  pqBind
    :: Monad m
    => (x -> pq schema1 schema2 m y)
    -> pq schema0 schema1 m x
    -> pq schema0 schema2 m y

  -- | indexed analog of flipped `>>`
  pqThen
    :: Monad m
    => pq schema1 schema2 m y
    -> pq schema0 schema1 m x
    -> pq schema0 schema2 m y
  pqThen pq2 pq1 = pq1 & pqBind (\ _ -> pq2)

  -- | indexed analog of `<=<`
  pqAndThen
    :: Monad m
    => (y -> pq schema1 schema2 m z)
    -> (x -> pq schema0 schema1 m y)
    -> x -> pq schema0 schema2 m z
  pqAndThen g f x = pqBind g (f x)

  -- | Safely embed a computation in a larger schema.
  pqEmbed
    :: Monad m
    => pq schema0 schema1 m x
    -> pq (table ': schema0) (table : schema1) m x

  -- | Run a `Definition` with `LibPQ.exec`, we expect that libpq obeys the law
  --
  -- @define statement1 & pqThen (define statement2) = define (statement1 >>> statement2)@
  define
    :: MonadBase IO io
    => Definition schema0 schema1
    -> pq schema0 schema1 io (K LibPQ.Result '[])

instance IndexedMonadTransPQ PQ where

  pqAp (PQ f) (PQ x) = PQ $ \ conn -> do
    K f' <- f conn
    K x' <- x (K (unK conn))
    return $ K (f' x')

  pqBind f (PQ x) = PQ $ \ conn -> do
    K x' <- x conn
    unPQ (f x') (K (unK conn))

  pqEmbed (PQ pq) = PQ $ \ (K conn) -> do
    K x <- pq (K conn)
    return $ K x

  define (UnsafeDefinition q) = PQ $ \ (K conn) -> do
    resultMaybe <- liftBase $ LibPQ.exec conn q
    case resultMaybe of
      Nothing -> error
        "define: LibPQ.exec returned no results"
      Just result -> return $ K (K result)

{- | `MonadPQ` is an `mtl` style constraint, similar to
`Control.Monad.State.Class.MonadState`, for using `LibPQ` to

* `manipulateParams` runs a `Manipulation` with params from a type
   with a `ToParams` constraint. It calls `LibPQ.execParams` and
   doesn't afraid of anything.

* `manipulate` is like `manipulateParams` for a parameter-free statement.

* `runQueryParams` is like `manipulateParams` for query statements.

* `runQuery` is like `runQueryParams` for a parameter-free statement.

* `traversePrepared` has the same type signature as a composition of
  `traverse` and `manipulateParams` but provides an optimization by
  preparing the statement with `LibPQ.prepare` and then traversing a
  `Traversable` container with `LibPQ.execPrepared`. The temporary prepared
  statement is then deallocated.

* `forPrepared` is a flipped `traversePrepared`

* `traversePrepared_` is like `traversePrepared` but works on `Foldable`
  containers and returns unit.

* `forPrepared_` is a flipped `traversePrepared_`.

* `liftPQ` lets you lift actions from `LibPQ` that require a connection
  into your monad.

To define an instance, you can minimally define only `manipulateParams`,
`traversePrepared`, `traversePrepared_` and `liftPQ`. Monad transformers get
a default instance.

-}
class Monad pq => MonadPQ schema pq | pq -> schema where
  manipulateParams
    :: ToParams x params
    => Manipulation schema params ys
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> x -> pq (K LibPQ.Result ys)
  default manipulateParams
    :: (MonadTrans t, MonadPQ schema pq1, pq ~ t pq1)
    => ToParams x params
    => Manipulation schema params ys
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> x -> pq (K LibPQ.Result ys)
  manipulateParams manipulation params = lift $
    manipulateParams manipulation params

  manipulate :: Manipulation schema '[] ys -> pq (K LibPQ.Result ys)
  manipulate statement = manipulateParams statement ()

  runQueryParams
    :: ToParams x params
    => Query schema params ys
    -- ^ `select` and friends
    -> x -> pq (K LibPQ.Result ys)
  runQueryParams = manipulateParams . queryStatement

  runQuery
    :: Query schema '[] ys
    -- ^ `select` and friends
    -> pq (K LibPQ.Result ys)
  runQuery q = runQueryParams q ()

  traversePrepared
    :: (ToParams x params, Traversable list)
    => Manipulation schema params ys
    -- ^ `insertRows`, `update`, or `deleteFrom`, and friends
    -> list x -> pq (list (K LibPQ.Result ys))
  default traversePrepared
    :: (MonadTrans t, MonadPQ schema pq1, pq ~ t pq1)
    => (ToParams x params, Traversable list)
    => Manipulation schema params ys -> list x -> pq (list (K LibPQ.Result ys))
  traversePrepared manipulation params = lift $
    traversePrepared manipulation params

  forPrepared
    :: (ToParams x params, Traversable list)
    => list x
    -> Manipulation schema params ys
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> pq (list (K LibPQ.Result ys))
  forPrepared = flip traversePrepared

  traversePrepared_
    :: (ToParams x params, Foldable list)
    => Manipulation schema params '[]
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> list x -> pq ()
  default traversePrepared_
    :: (MonadTrans t, MonadPQ schema pq1, pq ~ t pq1)
    => (ToParams x params, Foldable list)
    => Manipulation schema params '[]
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> list x -> pq ()
  traversePrepared_ manipulation params = lift $
    traversePrepared_ manipulation params

  forPrepared_
    :: (ToParams x params, Foldable list)
    => list x
    -> Manipulation schema params '[]
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> pq ()
  forPrepared_ = flip traversePrepared_

  liftPQ :: (LibPQ.Connection -> IO a) -> pq a
  default liftPQ
    :: (MonadTrans t, MonadPQ schema pq1, pq ~ t pq1)
    => (LibPQ.Connection -> IO a) -> pq a
  liftPQ = lift . liftPQ

errorOnPrepareNotOk :: String -> LibPQ.Result -> IO ()
errorOnPrepareNotOk pfix r = do
  status <- LibPQ.resultStatus r
  case status of
    LibPQ.CommandOk -> return ()
    _               -> do
      msg <- LibPQ.resultErrorMessage r
      error $ pfix <>
        "status: " <> show status <> "\n" <>
        maybe "(no message)" C8.unpack msg

instance (MonadBase IO io, schema0 ~ schema, schema1 ~ schema)
  => MonadPQ schema (PQ schema0 schema1 io) where

  manipulateParams
    (UnsafeManipulation q :: Manipulation schema ps ys) (params :: x) =
      PQ $ \ (K conn) -> do
        let
          toParam' bytes = (LibPQ.invalidOid,bytes,LibPQ.Binary)
          params' = fmap (fmap toParam') (hcollapse (toParams @x @ps params))
          q' = q <> ";"
        resultMaybe <- liftBase $ LibPQ.execParams conn q' params' LibPQ.Binary
        case resultMaybe of
          Nothing -> error
            "manipulateParams: LibPQ.execParams returned no results"
          Just result -> return $ K (K result)

  traversePrepared
    (UnsafeManipulation q :: Manipulation schema xs ys) (list :: list x) =
      PQ $ \ (K conn) -> liftBase $ do
        let temp = "temporary_statement"
        prepResultMaybe <- LibPQ.prepare conn temp q Nothing
        case prepResultMaybe of
          Nothing -> error
            "traversePrepared: LibPQ.prepare returned no results"
          Just prepResult ->
            errorOnPrepareNotOk "traversePrepared: LibPQ.prepare\n" prepResult
        results <- for list $ \ params -> do
          let
            toParam' bytes = (bytes,LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (toParams @x @xs params))
          resultMaybe <- LibPQ.execPrepared conn temp params' LibPQ.Binary
          case resultMaybe of
            Nothing -> error
              "traversePrepared: LibPQ.execParams returned no results"
            Just result -> return $ K result
        deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
        case deallocResultMaybe of
          Nothing -> error
            "traversePrepared: LibPQ.exec DEALLOCATE returned no results"
          Just deallocResult -> do
            status <- LibPQ.resultStatus deallocResult
            unless (status == LibPQ.CommandOk) . error $
              "traversePrepared: DEALLOCATE status " <> show status
        return (K results)

  traversePrepared_
    (UnsafeManipulation q :: Manipulation schema xs '[]) (list :: list x) =
      PQ $ \ (K conn) -> liftBase $ do
        let temp = "temporary_statement"
        prepResultMaybe <- LibPQ.prepare conn temp q Nothing
        case prepResultMaybe of
          Nothing -> error
            "traversePrepared_: LibPQ.prepare returned no results"
          Just prepResult ->
            errorOnPrepareNotOk "traversePrepared_: LibPQ.prepare\n" prepResult
        for_ list $ \ params -> do
          let
            toParam' bytes = (bytes,LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (toParams @x @xs params))
          resultMaybe <- LibPQ.execPrepared conn temp params' LibPQ.Binary
          case resultMaybe of
            Nothing -> error
              "traversePrepared_: LibPQ.execParams returned no results"
            Just _result -> return ()
        deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
        case deallocResultMaybe of
          Nothing -> error
            "traversePrepared: LibPQ.exec DEALLOCATE returned no results"
          Just deallocResult -> do
            status <- LibPQ.resultStatus deallocResult
            unless (status == LibPQ.CommandOk) . error $
              "traversePrepared: DEALLOCATE status " <> show status
        return (K ())

  liftPQ pq = PQ $ \ (K conn) -> do
    y <- liftBase $ pq conn
    return (K y)

instance MonadPQ schema m => MonadPQ schema (IdentityT m)
instance MonadPQ schema m => MonadPQ schema (ReaderT r m)
instance MonadPQ schema m => MonadPQ schema (Strict.StateT s m)
instance MonadPQ schema m => MonadPQ schema (Lazy.StateT s m)
instance (Monoid w, MonadPQ schema m) => MonadPQ schema (Strict.WriterT w m)
instance (Monoid w, MonadPQ schema m) => MonadPQ schema (Lazy.WriterT w m)
instance MonadPQ schema m => MonadPQ schema (MaybeT m)
instance MonadPQ schema m => MonadPQ schema (ExceptT e m)
instance (Monoid w, MonadPQ schema m) => MonadPQ schema (Strict.RWST r w s m)
instance (Monoid w, MonadPQ schema m) => MonadPQ schema (Lazy.RWST r w s m)
instance MonadPQ schema m => MonadPQ schema (ContT r m)
instance MonadPQ schema m => MonadPQ schema (ListT m)

instance (Monad m, schema0 ~ schema1)
  => Applicative (PQ schema0 schema1 m) where
  pure x = PQ $ \ _conn -> pure (K x)
  (<*>) = pqAp

instance (Monad m, schema0 ~ schema1)
  => Monad (PQ schema0 schema1 m) where
  return = pure
  (>>=) = flip pqBind

instance schema0 ~ schema1 => MFunctor (PQ schema0 schema1) where
  hoist f (PQ pq) = PQ (f . pq)

instance schema0 ~ schema1 => MonadTrans (PQ schema0 schema1) where
  lift m = PQ $ \ _conn -> do
    x <- m
    return (K x)

instance schema0 ~ schema1 => MMonad (PQ schema0 schema1) where
  embed f (PQ pq) = PQ $ \ conn -> do
    evalPQ (f (pq conn)) conn

instance (MonadBase b m, schema0 ~ schema1)
  => MonadBase b (PQ schema0 schema1 m) where
  liftBase = lift . liftBase

-- | A snapshot of the state of a `PQ` computation.
type PQRun schema =
  forall m x. Monad m => PQ schema schema m x -> m (K x schema)

-- | Helper function in defining `MonadBaseControl` instance for `PQ`.
pqliftWith :: Functor m => (PQRun schema -> m a) -> PQ schema schema m a
pqliftWith f = PQ $ \ conn ->
  fmap K (f $ \ pq -> unPQ pq conn)

instance (MonadBaseControl b m, schema0 ~ schema1)
  => MonadBaseControl b (PQ schema0 schema1 m) where
  type StM (PQ schema0 schema1 m) x = StM m (K x schema0)
  liftBaseWith f =
    pqliftWith $ \ run -> liftBaseWith $ \ runInBase -> f $ runInBase . run
  restoreM = PQ . const . restoreM

-- | Get a row corresponding to a given row number from a `LibPQ.Result`,
-- throwing an exception if the row number is out of bounds.
getRow
  :: (FromRow columns y, MonadBase IO io)
  => LibPQ.Row
  -- ^ row number
  -> K LibPQ.Result columns
  -- ^ result
  -> io y
getRow r (K result :: K LibPQ.Result columns) = liftBase $ do
  numRows <- LibPQ.ntuples result
  when (numRows < r) $ error $
    "getRow: expected at least " <> show r <> "rows but only saw "
    <> show numRows
  let len = fromIntegral (lengthSList (Proxy @columns))
  row' <- traverse (LibPQ.getvalue result r) [0 .. len - 1]
  case fromList row' of
    Nothing -> error "getRow: found unexpected length"
    Just row -> return $ fromRow @columns row

-- | Intended to be used for unfolding in streaming libraries, `nextRow`
-- takes a total number of rows (which can be found with `ntuples`)
-- and a `LibPQ.Result` and given a row number if it's too large returns `Nothing`,
-- otherwise returning the row along with the next row number.
nextRow
  :: (FromRow columns y, MonadBase IO io)
  => LibPQ.Row -- ^ total number of rows
  -> K LibPQ.Result columns -- ^ result
  -> LibPQ.Row -- ^ row number
  -> io (Maybe (LibPQ.Row,y))
nextRow total (K result :: K LibPQ.Result columns) r
  = liftBase $ if r >= total then return Nothing else do
    let len = fromIntegral (lengthSList (Proxy @columns))
    row' <- traverse (LibPQ.getvalue result r) [0 .. len - 1]
    case fromList row' of
      Nothing -> error "nextRow: found unexpected length"
      Just row -> return $ Just (r+1, fromRow @columns row)

-- | Get all rows from a `LibPQ.Result`.
getRows
  :: (FromRow columns y, MonadBase IO io)
  => K LibPQ.Result columns -- ^ result
  -> io [y]
getRows (K result :: K LibPQ.Result columns) = liftBase $ do
  let len = fromIntegral (lengthSList (Proxy @columns))
  numRows <- LibPQ.ntuples result
  for [0 .. numRows - 1] $ \ r -> do
    row' <- traverse (LibPQ.getvalue result r) [0 .. len - 1]
    case fromList row' of
      Nothing -> error "getRows: found unexpected length"
      Just row -> return $ fromRow @columns row

-- | Get the first row if possible from a `LibPQ.Result`.
firstRow
  :: (FromRow columns y, MonadBase IO io)
  => K LibPQ.Result columns -- ^ result
  -> io (Maybe y)
firstRow (K result :: K LibPQ.Result columns) = liftBase $ do
  numRows <- LibPQ.ntuples result
  if numRows <= 0 then return Nothing else do
    let len = fromIntegral (lengthSList (Proxy @columns))
    row' <- traverse (LibPQ.getvalue result 0) [0 .. len - 1]
    case fromList row' of
      Nothing -> error "firstRow: found unexpected length"
      Just row -> return . Just $ fromRow @columns row

-- | Lifts actions on results from @LibPQ@.
liftResult
  :: MonadBase IO io
  => (LibPQ.Result -> IO x)
  -> K LibPQ.Result results -> io x
liftResult f (K result) = liftBase $ f result

-- | Returns the number of rows (tuples) in the query result.
ntuples :: MonadBase IO io => K LibPQ.Result columns -> io LibPQ.Row
ntuples = liftResult LibPQ.ntuples

-- | Returns the result status of the command.
resultStatus :: MonadBase IO io => K LibPQ.Result results -> io LibPQ.ExecStatus
resultStatus = liftResult LibPQ.resultStatus

-- | Returns the error message most recently generated by an operation
-- on the connection.
resultErrorMessage
  :: MonadBase IO io => K LibPQ.Result results -> io (Maybe ByteString)
resultErrorMessage = liftResult LibPQ.resultErrorMessage

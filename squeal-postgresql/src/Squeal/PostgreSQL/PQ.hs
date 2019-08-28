{-|
Module: Squeal.PostgreSQL.PQ
Description: PQ monad
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

This module is where Squeal commands actually get executed by
`Database.PostgreSQL.LibPQ`. It containts two typeclasses, `IndexedMonadTransPQ` for executing
a `Definition` and `MonadPQ` for executing a `Manipulation` or `Query`,
and a `PQ` type with instances for them.

Using Squeal in your application will come down to defining
the @schemas@ of your database and including @PQ schemas schemas@ in your
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
  , InstanceSigs
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
    -- * Results
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
  , resultErrorCode
    -- * Exceptions
  , SquealException (..)
  , PQState (..)
  , okResult
  , catchSqueal
  , handleSqueal
  , trySqueal
  ) where

import Control.Exception (Exception, throw)
import Control.Monad.Except
import Control.Monad.Morph
import UnliftIO (MonadUnliftIO (..), bracket, catch, handle, try)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Function ((&))
import Data.Kind
import Data.Text (pack, Text)
import Data.Traversable
import Generics.SOP
import PostgreSQL.Binary.Encoding (encodingBytes)

import qualified Control.Monad.Fail as Fail
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

import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict

-- $setup
-- >>> import Squeal.PostgreSQL

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
  :: forall schemas io
   . MonadIO io
  => ByteString -- ^ conninfo
  -> io (K LibPQ.Connection schemas)
connectdb = fmap K . liftIO . LibPQ.connectdb

-- | Closes the connection to the server.
finish :: MonadIO io => K LibPQ.Connection schemas -> io ()
finish = liftIO . LibPQ.finish . unK

-- | Do `connectdb` and `finish` before and after a computation.
withConnection
  :: forall schemas0 schemas1 io x
   . MonadUnliftIO io
  => ByteString
  -> PQ schemas0 schemas1 io x
  -> io x
withConnection connString action = do
  K x <- bracket (connectdb connString) finish (unPQ action)
  return x

-- | Safely `lowerConnection` to a smaller schema.
lowerConnection
  :: K LibPQ.Connection (schema ': schemas)
  -> K LibPQ.Connection schemas
lowerConnection (K conn) = K conn

-- | We keep track of the schema via an Atkey indexed state monad transformer,
-- `PQ`.
newtype PQ
  (schemas0 :: SchemasType)
  (schemas1 :: SchemasType)
  (m :: Type -> Type)
  (x :: Type) =
    PQ { unPQ :: K LibPQ.Connection schemas0 -> m (K x schemas1) }

instance Monad m => Functor (PQ schemas0 schemas1 m) where
  fmap f (PQ pq) = PQ $ \ conn -> do
    K x <- pq conn
    return $ K (f x)

-- | Run a `PQ` and keep the result and the `LibPQ.Connection`.
runPQ
  :: Functor m
  => PQ schemas0 schemas1 m x
  -> K LibPQ.Connection schemas0
  -> m (x, K LibPQ.Connection schemas1)
runPQ (PQ pq) conn = (\ x -> (unK x, K (unK conn))) <$> pq conn
  -- K x <- pq conn
  -- return (x, K (unK conn))

-- | Execute a `PQ` and discard the result but keep the `LibPQ.Connection`.
execPQ
  :: Functor m
  => PQ schemas0 schemas1 m x
  -> K LibPQ.Connection schemas0
  -> m (K LibPQ.Connection schemas1)
execPQ (PQ pq) conn = mapKK (\ _ -> unK conn) <$> pq conn

-- | Evaluate a `PQ` and discard the `LibPQ.Connection` but keep the result.
evalPQ
  :: Functor m
  => PQ schemas0 schemas1 m x
  -> K LibPQ.Connection schemas0
  -> m x
evalPQ (PQ pq) conn = unK <$> pq conn

-- | An [Atkey indexed monad](https://bentnib.org/paramnotions-jfp.pdf) is a `Functor`
-- [enriched category](https://ncatlab.org/nlab/show/enriched+category).
-- An indexed monad transformer transforms a `Monad` into an indexed monad.
-- And, `IndexedMonadTransPQ` is a class for indexed monad transformers that
-- support running `Definition`s using `define`.
class IndexedMonadTransPQ pq where

  -- | indexed analog of `<*>`
  pqAp
    :: Monad m
    => pq schemas0 schemas1 m (x -> y)
    -> pq schemas1 schemas2 m x
    -> pq schemas0 schemas2 m y

  -- | indexed analog of `join`
  pqJoin
    :: Monad m
    => pq schemas0 schemas1 m (pq schemas1 schemas2 m y)
    -> pq schemas0 schemas2 m y
  pqJoin pq = pq & pqBind id

  -- | indexed analog of `=<<`
  pqBind
    :: Monad m
    => (x -> pq schemas1 schemas2 m y)
    -> pq schemas0 schemas1 m x
    -> pq schemas0 schemas2 m y

  -- | indexed analog of flipped `>>`
  pqThen
    :: Monad m
    => pq schemas1 schemas2 m y
    -> pq schemas0 schemas1 m x
    -> pq schemas0 schemas2 m y
  pqThen pq2 pq1 = pq1 & pqBind (\ _ -> pq2)

  -- | indexed analog of `<=<`
  pqAndThen
    :: Monad m
    => (y -> pq schemas1 schemas2 m z)
    -> (x -> pq schemas0 schemas1 m y)
    -> x -> pq schemas0 schemas2 m z
  pqAndThen g f x = pqBind g (f x)

  -- | Run a `Definition` with `LibPQ.exec`.
  --
  -- It should be functorial in effect.
  --
  -- @define id = return ()@
  -- @define (statement1 >>> statement2) = define statement1 & pqThen (define statement2)@
  define
    :: MonadIO io
    => Definition schemas0 schemas1
    -> pq schemas0 schemas1 io ()

instance IndexedMonadTransPQ PQ where

  pqAp (PQ f) (PQ x) = PQ $ \ conn -> do
    K f' <- f conn
    K x' <- x (K (unK conn))
    return $ K (f' x')

  pqBind f (PQ x) = PQ $ \ conn -> do
    K x' <- x conn
    unPQ (f x') (K (unK conn))

  define (UnsafeDefinition q) = PQ $ \ (K conn) -> do
    resultMaybe <- liftIO $ LibPQ.exec conn q
    case resultMaybe of
      Nothing -> throw $ ResultException
        "define: LibPQ.exec returned no results"
      Just result -> K <$> okResult_ result

{- | `MonadPQ` is an @mtl@ style constraint, similar to
`Control.Monad.State.Class.MonadState`, for using `Database.PostgreSQL.LibPQ` to

* `manipulateParams` runs a `Manipulation` with params from a type
   with a `ToParams` constraint. It calls `LibPQ.execParams` and
   doesn't afraid of anything.

* `manipulateParams_` is like `manipulateParams` for a returning-free statement.

* `manipulate` is like `manipulateParams` for a parameter-free statement.

* `manipulate_` is like `manipulate` for a returning-free statement.

* `runQueryParams` is like `manipulateParams` for query statements.

* `runQuery` is like `runQueryParams` for a parameter-free statement.

* `traversePrepared` has the same type signature as a composition of
  `traverse` and `manipulateParams` but provides an optimization by
  preparing the statement with `LibPQ.prepare` and then traversing a
  `Traversable` container with `LibPQ.execPrepared`. The temporary prepared
  statement is then deallocated.

* `forPrepared` is a flipped `traversePrepared`

* `traversePrepared_` is like `traversePrepared` but works on `Foldable`
  containers for a returning-free statement.

* `forPrepared_` is a flipped `traversePrepared_`.

* `liftPQ` lets you lift actions from `Database.PostgreSQL.LibPQ` that require a connection
  into your monad.

To define an instance, you can minimally define only `manipulateParams`,
`traversePrepared`, `traversePrepared_` and `liftPQ`. Monad transformers get
a default instance.

-}
class Monad pq => MonadPQ schemas pq | pq -> schemas where
  manipulateParams
    :: ToParams x params
    => Manipulation '[] schemas params ys
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> x -> pq (K LibPQ.Result ys)
  default manipulateParams
    :: (MonadTrans t, MonadPQ schemas pq1, pq ~ t pq1)
    => ToParams x params
    => Manipulation '[] schemas params ys
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> x -> pq (K LibPQ.Result ys)
  manipulateParams manipulation params = lift $
    manipulateParams manipulation params

  manipulateParams_
    :: ToParams x params
    => Manipulation '[] schemas params '[]
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> x -> pq ()
  manipulateParams_ q x = void $ manipulateParams q x

  manipulate :: Manipulation '[] schemas '[] ys -> pq (K LibPQ.Result ys)
  manipulate statement = manipulateParams statement ()

  manipulate_ :: Manipulation '[] schemas '[] '[] -> pq ()
  manipulate_ = void . manipulate

  runQueryParams
    :: ToParams x params
    => Query '[] '[] schemas params ys
    -- ^ `select` and friends
    -> x -> pq (K LibPQ.Result ys)
  runQueryParams = manipulateParams . queryStatement

  runQuery
    :: Query '[] '[] schemas '[] ys
    -- ^ `select` and friends
    -> pq (K LibPQ.Result ys)
  runQuery q = runQueryParams q ()

  traversePrepared
    :: (ToParams x params, Traversable list)
    => Manipulation '[] schemas params ys
    -- ^ `insertInto`, `update`, or `deleteFrom`, and friends
    -> list x -> pq (list (K LibPQ.Result ys))
  default traversePrepared
    :: (MonadTrans t, MonadPQ schemas pq1, pq ~ t pq1)
    => (ToParams x params, Traversable list)
    => Manipulation '[] schemas params ys -> list x -> pq (list (K LibPQ.Result ys))
  traversePrepared manipulation params = lift $
    traversePrepared manipulation params

  forPrepared
    :: (ToParams x params, Traversable list)
    => list x
    -> Manipulation '[] schemas params ys
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> pq (list (K LibPQ.Result ys))
  forPrepared = flip traversePrepared

  traversePrepared_
    :: (ToParams x params, Foldable list)
    => Manipulation '[] schemas params '[]
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> list x -> pq ()
  default traversePrepared_
    :: (MonadTrans t, MonadPQ schemas pq1, pq ~ t pq1)
    => (ToParams x params, Foldable list)
    => Manipulation '[] schemas params '[]
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> list x -> pq ()
  traversePrepared_ manipulation params = lift $
    traversePrepared_ manipulation params

  forPrepared_
    :: (ToParams x params, Foldable list)
    => list x
    -> Manipulation '[] schemas params '[]
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> pq ()
  forPrepared_ = flip traversePrepared_

  liftPQ :: (LibPQ.Connection -> IO a) -> pq a
  default liftPQ
    :: (MonadTrans t, MonadPQ schemas pq1, pq ~ t pq1)
    => (LibPQ.Connection -> IO a) -> pq a
  liftPQ = lift . liftPQ

instance (MonadIO io, schemas0 ~ schemas, schemas1 ~ schemas)
  => MonadPQ schemas (PQ schemas0 schemas1 io) where

  manipulateParams
    (UnsafeManipulation q :: Manipulation '[] schemas ps ys) (params :: x) =
      PQ $ \ (K conn) -> do
        let
          toParam' encoding =
            (LibPQ.invalidOid, encodingBytes encoding, LibPQ.Binary)
          params' = fmap (fmap toParam') (hcollapse (toParams @x @ps params))
          q' = q <> ";"
        resultMaybe <- liftIO $ LibPQ.execParams conn q' params' LibPQ.Binary
        case resultMaybe of
          Nothing -> throw $ ResultException
            "manipulateParams: LibPQ.execParams returned no results"
          Just result -> do
            okResult_ result
            return $ K (K result)

  traversePrepared
    (UnsafeManipulation q :: Manipulation '[] schemas xs ys) (list :: list x) =
      PQ $ \ (K conn) -> liftIO $ do
        let temp = "temporary_statement"
        prepResultMaybe <- LibPQ.prepare conn temp q Nothing
        case prepResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared: LibPQ.prepare returned no results"
          Just prepResult -> okResult_ prepResult
        results <- for list $ \ params -> do
          let
            toParam' encoding = (encodingBytes encoding, LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (toParams @x @xs params))
          resultMaybe <- LibPQ.execPrepared conn temp params' LibPQ.Binary
          case resultMaybe of
            Nothing -> throw $ ResultException
              "traversePrepared: LibPQ.execParams returned no results"
            Just result -> do
              okResult_ result
              return $ K result
        deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
        case deallocResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared: LibPQ.exec DEALLOCATE returned no results"
          Just deallocResult -> okResult_ deallocResult
        return (K results)

  traversePrepared_
    (UnsafeManipulation q :: Manipulation '[] schemas xs '[]) (list :: list x) =
      PQ $ \ (K conn) -> liftIO $ do
        let temp = "temporary_statement"
        prepResultMaybe <- LibPQ.prepare conn temp q Nothing
        case prepResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared_: LibPQ.prepare returned no results"
          Just prepResult -> okResult_ prepResult
        for_ list $ \ params -> do
          let
            toParam' encoding = (encodingBytes encoding, LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (toParams @x @xs params))
          resultMaybe <- LibPQ.execPrepared conn temp params' LibPQ.Binary
          case resultMaybe of
            Nothing -> throw $ ResultException
              "traversePrepared_: LibPQ.execParams returned no results"
            Just result -> okResult_ result
        deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
        case deallocResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared: LibPQ.exec DEALLOCATE returned no results"
          Just deallocResult -> okResult_ deallocResult
        return (K ())

  liftPQ pq = PQ $ \ (K conn) -> do
    y <- liftIO $ pq conn
    return (K y)

instance MonadPQ schemas m => MonadPQ schemas (IdentityT m)
instance MonadPQ schemas m => MonadPQ schemas (ReaderT r m)
instance MonadPQ schemas m => MonadPQ schemas (Strict.StateT s m)
instance MonadPQ schemas m => MonadPQ schemas (Lazy.StateT s m)
instance (Monoid w, MonadPQ schemas m) => MonadPQ schemas (Strict.WriterT w m)
instance (Monoid w, MonadPQ schemas m) => MonadPQ schemas (Lazy.WriterT w m)
instance MonadPQ schemas m => MonadPQ schemas (MaybeT m)
instance MonadPQ schemas m => MonadPQ schemas (ExceptT e m)
instance (Monoid w, MonadPQ schemas m) => MonadPQ schemas (Strict.RWST r w s m)
instance (Monoid w, MonadPQ schemas m) => MonadPQ schemas (Lazy.RWST r w s m)
instance MonadPQ schemas m => MonadPQ schemas (ContT r m)

instance (Monad m, schemas0 ~ schemas1)
  => Applicative (PQ schemas0 schemas1 m) where
  pure x = PQ $ \ _conn -> pure (K x)
  (<*>) = pqAp

instance (Monad m, schemas0 ~ schemas1)
  => Monad (PQ schemas0 schemas1 m) where
  return = pure
  (>>=) = flip pqBind

instance (Monad m, schemas0 ~ schemas1)
  => Fail.MonadFail (PQ schemas0 schemas1 m) where
  fail = Fail.fail

instance schemas0 ~ schemas1 => MFunctor (PQ schemas0 schemas1) where
  hoist f (PQ pq) = PQ (f . pq)

instance schemas0 ~ schemas1 => MonadTrans (PQ schemas0 schemas1) where
  lift m = PQ $ \ _conn -> do
    x <- m
    return (K x)

instance schemas0 ~ schemas1 => MMonad (PQ schemas0 schemas1) where
  embed f (PQ pq) = PQ $ \ conn -> do
    evalPQ (f (pq conn)) conn

instance (MonadIO m, schema0 ~ schema1)
  => MonadIO (PQ schema0 schema1 m) where
  liftIO = lift . liftIO

instance (MonadUnliftIO m, schemas0 ~ schemas1)
  => MonadUnliftIO (PQ schemas0 schemas1 m) where
  withRunInIO
      :: ((forall a . PQ schemas0 schema1 m a -> IO a) -> IO b)
      -> PQ schemas0 schema1 m b
  withRunInIO inner = PQ $ \conn ->
    withRunInIO $ \(run :: (forall x . m x -> IO x)) ->
      K <$> inner (\pq -> run $ unK <$> unPQ pq conn)

-- | Get a row corresponding to a given row number from a `LibPQ.Result`,
-- throwing an exception if the row number is out of bounds.
getRow
  :: (FromRow columns y, MonadIO io)
  => LibPQ.Row
  -- ^ row number
  -> K LibPQ.Result columns
  -- ^ result
  -> io y
getRow r (K result :: K LibPQ.Result columns) = liftIO $ do
  numRows <- LibPQ.ntuples result
  when (numRows < r) $ throw $ ResultException $
    "getRow: expected at least " <> pack (show r) <> "rows but only saw "
    <> pack (show numRows)
  let len = fromIntegral (lengthSList (Proxy @columns))
  row' <- traverse (LibPQ.getvalue result r) [0 .. len - 1]
  case fromList row' of
    Nothing -> throw $ ResultException "getRow: found unexpected length"
    Just row -> case fromRow @columns row of
      Left parseError -> throw $ ParseException $ "getRow: " <> parseError
      Right y -> return y

-- | Intended to be used for unfolding in streaming libraries, `nextRow`
-- takes a total number of rows (which can be found with `ntuples`)
-- and a `LibPQ.Result` and given a row number if it's too large returns `Nothing`,
-- otherwise returning the row along with the next row number.
nextRow
  :: (FromRow columns y, MonadIO io)
  => LibPQ.Row -- ^ total number of rows
  -> K LibPQ.Result columns -- ^ result
  -> LibPQ.Row -- ^ row number
  -> io (Maybe (LibPQ.Row,y))
nextRow total (K result :: K LibPQ.Result columns) r
  = liftIO $ if r >= total then return Nothing else do
    let len = fromIntegral (lengthSList (Proxy @columns))
    row' <- traverse (LibPQ.getvalue result r) [0 .. len - 1]
    case fromList row' of
      Nothing -> throw $ ResultException "nextRow: found unexpected length"
      Just row -> case fromRow @columns row of
        Left parseError -> throw $ ParseException $ "nextRow: " <> parseError
        Right y -> return $ Just (r+1, y)

-- | Get all rows from a `LibPQ.Result`.
getRows
  :: (FromRow columns y, MonadIO io)
  => K LibPQ.Result columns -- ^ result
  -> io [y]
getRows (K result :: K LibPQ.Result columns) = liftIO $ do
  let len = fromIntegral (lengthSList (Proxy @columns))
  numRows <- LibPQ.ntuples result
  for [0 .. numRows - 1] $ \ r -> do
    row' <- traverse (LibPQ.getvalue result r) [0 .. len - 1]
    case fromList row' of
      Nothing -> throw $ ResultException "getRows: found unexpected length"
      Just row -> case fromRow @columns row of
        Left parseError -> throw $ ParseException $ "getRows: " <> parseError
        Right y -> return y

-- | Get the first row if possible from a `LibPQ.Result`.
firstRow
  :: (FromRow columns y, MonadIO io)
  => K LibPQ.Result columns -- ^ result
  -> io (Maybe y)
firstRow (K result :: K LibPQ.Result columns) = liftIO $ do
  numRows <- LibPQ.ntuples result
  if numRows <= 0 then return Nothing else do
    let len = fromIntegral (lengthSList (Proxy @columns))
    row' <- traverse (LibPQ.getvalue result 0) [0 .. len - 1]
    case fromList row' of
      Nothing -> throw $ ResultException "firstRow: found unexpected length"
      Just row -> case fromRow @columns row of
        Left parseError -> throw $ ParseException $ "firstRow: " <> parseError
        Right y -> return $ Just y

-- | Lifts actions on results from @LibPQ@.
liftResult
  :: MonadIO io
  => (LibPQ.Result -> IO x)
  -> K LibPQ.Result results -> io x
liftResult f (K result) = liftIO $ f result

-- | Returns the number of rows (tuples) in the query result.
ntuples :: MonadIO io => K LibPQ.Result columns -> io LibPQ.Row
ntuples = liftResult LibPQ.ntuples

-- | Returns the result status of the command.
resultStatus :: MonadIO io => K LibPQ.Result results -> io LibPQ.ExecStatus
resultStatus = liftResult LibPQ.resultStatus

-- | Returns the error message most recently generated by an operation
-- on the connection.
resultErrorMessage
  :: MonadIO io => K LibPQ.Result results -> io (Maybe ByteString)
resultErrorMessage = liftResult LibPQ.resultErrorMessage

-- | Returns the error code most recently generated by an operation
-- on the connection.
--
-- https://www.postgresql.org/docs/current/static/errcodes-appendix.html
resultErrorCode
  :: MonadIO io
  => K LibPQ.Result results
  -> io (Maybe ByteString)
resultErrorCode = liftResult (flip LibPQ.resultErrorField LibPQ.DiagSqlstate)

-- | the state of LibPQ
data PQState = PQState
  { sqlExecStatus :: LibPQ.ExecStatus
  , sqlStateCode :: Maybe ByteString
    -- ^ https://www.postgresql.org/docs/current/static/errcodes-appendix.html
  , sqlErrorMessage :: Maybe ByteString
  } deriving (Eq, Show)

-- | `Exception`s that can be thrown by Squeal.
data SquealException
  = PQException PQState
  | ResultException Text
  | ParseException Text
  deriving (Eq, Show)
instance Exception SquealException

okResult_ :: MonadIO io => LibPQ.Result -> io ()
okResult_ result = liftIO $ do
  status <- LibPQ.resultStatus result
  case status of
    LibPQ.CommandOk -> return ()
    LibPQ.TuplesOk -> return ()
    _ -> do
      stateCode <- LibPQ.resultErrorField result LibPQ.DiagSqlstate
      msg <- LibPQ.resultErrorMessage result
      throw . PQException $ PQState status stateCode msg

-- | Check if a `LibPQ.Result`'s status is either `LibPQ.CommandOk`
-- or `LibPQ.TuplesOk` otherwise `throw` a `PQException`.
okResult :: MonadIO io => K LibPQ.Result row -> io ()
okResult = okResult_ . unK

-- | Catch `SquealException`s.
catchSqueal
  :: MonadUnliftIO io
  => io a
  -> (SquealException -> io a) -- ^ handler
  -> io a
catchSqueal = catch

-- | Handle `SquealException`s.
handleSqueal
  :: MonadUnliftIO io
  => (SquealException -> io a) -- ^ handler
  -> io a -> io a
handleSqueal = handle

-- | `Either` return a `SquealException` or a result.
trySqueal
  :: MonadUnliftIO io
  => io a
  -> io (Either SquealException a)
trySqueal = try

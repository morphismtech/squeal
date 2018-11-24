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
the @db@ of your database and including @PQ db db@ in your
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
  , catchSqueal
  , handleSqueal
  ) where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Function ((&))
import Data.Kind
import Data.Text (pack, Text)
import Data.Traversable
import Generics.SOP
import PostgreSQL.Binary.Encoding (encodingBytes)

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
  :: forall db io
   . MonadBase IO io
  => ByteString -- ^ conninfo
  -> io (K LibPQ.Connection db)
connectdb = fmap K . liftBase . LibPQ.connectdb

-- | Closes the connection to the server.
finish :: MonadBase IO io => K LibPQ.Connection db -> io ()
finish = liftBase . LibPQ.finish . unK

-- | Do `connectdb` and `finish` before and after a computation.
withConnection
  :: forall db0 db1 io x
   . MonadBaseControl IO io
  => ByteString
  -> PQ db0 db1 io x
  -> io x
withConnection connString action = do
  K x <- bracket (connectdb connString) finish (unPQ action)
  return x

-- | Safely `lowerConnection` to a smaller schema.
lowerConnection
  :: K LibPQ.Connection (schema ': db)
  -> K LibPQ.Connection db
lowerConnection (K conn) = K conn

-- | We keep track of the schema via an Atkey indexed state monad transformer,
-- `PQ`.
newtype PQ
  (db0 :: DBType)
  (db1 :: DBType)
  (m :: Type -> Type)
  (x :: Type) =
    PQ { unPQ :: K LibPQ.Connection db0 -> m (K x db1) }

instance Monad m => Functor (PQ db0 db1 m) where
  fmap f (PQ pq) = PQ $ \ conn -> do
    K x <- pq conn
    return $ K (f x)

-- | Run a `PQ` and keep the result and the `Connection`.
runPQ
  :: Functor m
  => PQ db0 db1 m x
  -> K LibPQ.Connection db0
  -> m (x, K LibPQ.Connection db1)
runPQ (PQ pq) conn = (\ x -> (unK x, K (unK conn))) <$> pq conn
  -- K x <- pq conn
  -- return (x, K (unK conn))

-- | Execute a `PQ` and discard the result but keep the `Connection`.
execPQ
  :: Functor m
  => PQ db0 db1 m x
  -> K LibPQ.Connection db0
  -> m (K LibPQ.Connection db1)
execPQ (PQ pq) conn = mapKK (\ _ -> unK conn) <$> pq conn

-- | Evaluate a `PQ` and discard the `Connection` but keep the result.
evalPQ
  :: Functor m
  => PQ db0 db1 m x
  -> K LibPQ.Connection db0
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
    => pq db0 db1 m (x -> y)
    -> pq db1 db2 m x
    -> pq db0 db2 m y

  -- | indexed analog of `join`
  pqJoin
    :: Monad m
    => pq db0 db1 m (pq db1 db2 m y)
    -> pq db0 db2 m y
  pqJoin pq = pq & pqBind id

  -- | indexed analog of `=<<`
  pqBind
    :: Monad m
    => (x -> pq db1 db2 m y)
    -> pq db0 db1 m x
    -> pq db0 db2 m y

  -- | indexed analog of flipped `>>`
  pqThen
    :: Monad m
    => pq db1 db2 m y
    -> pq db0 db1 m x
    -> pq db0 db2 m y
  pqThen pq2 pq1 = pq1 & pqBind (\ _ -> pq2)

  -- | indexed analog of `<=<`
  pqAndThen
    :: Monad m
    => (y -> pq db1 db2 m z)
    -> (x -> pq db0 db1 m y)
    -> x -> pq db0 db2 m z
  pqAndThen g f x = pqBind g (f x)

  -- | Safely embed a computation in a larger schema.
  pqEmbed
    :: Monad m
    => pq db0 db1 m x
    -> pq (table ': db0) (table : db1) m x

  -- | Run a `Definition` with `LibPQ.exec`, we expect that libpq obeys the law
  --
  -- @define statement1 & pqThen (define statement2) = define (statement1 >>> statement2)@
  define
    :: MonadBase IO io
    => Definition db0 db1
    -> pq db0 db1 io (K LibPQ.Result '[])

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
      Nothing -> throw $ ResultException
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
class Monad pq => MonadPQ db pq | pq -> db where
  manipulateParams
    :: ToParams x params
    => Manipulation db params ys
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> x -> pq (K LibPQ.Result ys)
  default manipulateParams
    :: (MonadTrans t, MonadPQ db pq1, pq ~ t pq1)
    => ToParams x params
    => Manipulation db params ys
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> x -> pq (K LibPQ.Result ys)
  manipulateParams manipulation params = lift $
    manipulateParams manipulation params

  manipulate :: Manipulation db '[] ys -> pq (K LibPQ.Result ys)
  manipulate statement = manipulateParams statement ()

  runQueryParams
    :: ToParams x params
    => Query db params ys
    -- ^ `select` and friends
    -> x -> pq (K LibPQ.Result ys)
  runQueryParams = manipulateParams . queryStatement

  runQuery
    :: Query db '[] ys
    -- ^ `select` and friends
    -> pq (K LibPQ.Result ys)
  runQuery q = runQueryParams q ()

  traversePrepared
    :: (ToParams x params, Traversable list)
    => Manipulation db params ys
    -- ^ `insertRows`, `update`, or `deleteFrom`, and friends
    -> list x -> pq (list (K LibPQ.Result ys))
  default traversePrepared
    :: (MonadTrans t, MonadPQ db pq1, pq ~ t pq1)
    => (ToParams x params, Traversable list)
    => Manipulation db params ys -> list x -> pq (list (K LibPQ.Result ys))
  traversePrepared manipulation params = lift $
    traversePrepared manipulation params

  forPrepared
    :: (ToParams x params, Traversable list)
    => list x
    -> Manipulation db params ys
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> pq (list (K LibPQ.Result ys))
  forPrepared = flip traversePrepared

  traversePrepared_
    :: (ToParams x params, Foldable list)
    => Manipulation db params '[]
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> list x -> pq ()
  default traversePrepared_
    :: (MonadTrans t, MonadPQ db pq1, pq ~ t pq1)
    => (ToParams x params, Foldable list)
    => Manipulation db params '[]
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> list x -> pq ()
  traversePrepared_ manipulation params = lift $
    traversePrepared_ manipulation params

  forPrepared_
    :: (ToParams x params, Foldable list)
    => list x
    -> Manipulation db params '[]
    -- ^ `insertRows`, `update` or `deleteFrom`
    -> pq ()
  forPrepared_ = flip traversePrepared_

  liftPQ :: (LibPQ.Connection -> IO a) -> pq a
  default liftPQ
    :: (MonadTrans t, MonadPQ db pq1, pq ~ t pq1)
    => (LibPQ.Connection -> IO a) -> pq a
  liftPQ = lift . liftPQ

instance (MonadBase IO io, db0 ~ db, db1 ~ db)
  => MonadPQ db (PQ db0 db1 io) where

  manipulateParams
    (UnsafeManipulation q :: Manipulation db ps ys) (params :: x) =
      PQ $ \ (K conn) -> do
        let
          toParam' encoding =
            (LibPQ.invalidOid, encodingBytes encoding, LibPQ.Binary)
          params' = fmap (fmap toParam') (hcollapse (toParams @x @ps params))
          q' = q <> ";"
        resultMaybe <- liftBase $ LibPQ.execParams conn q' params' LibPQ.Binary
        case resultMaybe of
          Nothing -> throw $ ResultException
            "manipulateParams: LibPQ.execParams returned no results"
          Just result -> do
            tryResult result
            return $ K (K result)

  traversePrepared
    (UnsafeManipulation q :: Manipulation db xs ys) (list :: list x) =
      PQ $ \ (K conn) -> liftBase $ do
        let temp = "temporary_statement"
        prepResultMaybe <- LibPQ.prepare conn temp q Nothing
        case prepResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared: LibPQ.prepare returned no results"
          Just prepResult -> tryResult prepResult
        results <- for list $ \ params -> do
          let
            toParam' encoding = (encodingBytes encoding,LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (toParams @x @xs params))
          resultMaybe <- LibPQ.execPrepared conn temp params' LibPQ.Binary
          case resultMaybe of
            Nothing -> throw $ ResultException
              "traversePrepared: LibPQ.execParams returned no results"
            Just result -> do
              tryResult result
              return $ K result
        deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
        case deallocResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared: LibPQ.exec DEALLOCATE returned no results"
          Just deallocResult -> tryResult deallocResult
        return (K results)

  traversePrepared_
    (UnsafeManipulation q :: Manipulation db xs '[]) (list :: list x) =
      PQ $ \ (K conn) -> liftBase $ do
        let temp = "temporary_statement"
        prepResultMaybe <- LibPQ.prepare conn temp q Nothing
        case prepResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared_: LibPQ.prepare returned no results"
          Just prepResult -> tryResult prepResult
        for_ list $ \ params -> do
          let
            toParam' encoding = (encodingBytes encoding, LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (toParams @x @xs params))
          resultMaybe <- LibPQ.execPrepared conn temp params' LibPQ.Binary
          case resultMaybe of
            Nothing -> throw $ ResultException
              "traversePrepared_: LibPQ.execParams returned no results"
            Just result -> tryResult result
        deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
        case deallocResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared: LibPQ.exec DEALLOCATE returned no results"
          Just deallocResult -> tryResult deallocResult
        return (K ())

  liftPQ pq = PQ $ \ (K conn) -> do
    y <- liftBase $ pq conn
    return (K y)

instance MonadPQ db m => MonadPQ db (IdentityT m)
instance MonadPQ db m => MonadPQ db (ReaderT r m)
instance MonadPQ db m => MonadPQ db (Strict.StateT s m)
instance MonadPQ db m => MonadPQ db (Lazy.StateT s m)
instance (Monoid w, MonadPQ db m) => MonadPQ db (Strict.WriterT w m)
instance (Monoid w, MonadPQ db m) => MonadPQ db (Lazy.WriterT w m)
instance MonadPQ db m => MonadPQ db (MaybeT m)
instance MonadPQ db m => MonadPQ db (ExceptT e m)
instance (Monoid w, MonadPQ db m) => MonadPQ db (Strict.RWST r w s m)
instance (Monoid w, MonadPQ db m) => MonadPQ db (Lazy.RWST r w s m)
instance MonadPQ db m => MonadPQ db (ContT r m)

instance (Monad m, db0 ~ db1)
  => Applicative (PQ db0 db1 m) where
  pure x = PQ $ \ _conn -> pure (K x)
  (<*>) = pqAp

instance (Monad m, db0 ~ db1)
  => Monad (PQ db0 db1 m) where
  return = pure
  (>>=) = flip pqBind

instance db0 ~ db1 => MFunctor (PQ db0 db1) where
  hoist f (PQ pq) = PQ (f . pq)

instance db0 ~ db1 => MonadTrans (PQ db0 db1) where
  lift m = PQ $ \ _conn -> do
    x <- m
    return (K x)

instance db0 ~ db1 => MMonad (PQ db0 db1) where
  embed f (PQ pq) = PQ $ \ conn -> do
    evalPQ (f (pq conn)) conn

instance (MonadBase b m, db0 ~ db1)
  => MonadBase b (PQ db0 db1 m) where
  liftBase = lift . liftBase

-- | A snapshot of the state of a `PQ` computation.
type PQRun db =
  forall m x. Monad m => PQ db db m x -> m (K x db)

-- | Helper function in defining `MonadBaseControl` instance for `PQ`.
pqliftWith :: Functor m => (PQRun db -> m a) -> PQ db db m a
pqliftWith f = PQ $ \ conn ->
  fmap K (f $ \ pq -> unPQ pq conn)

instance (MonadBaseControl b m, db0 ~ db1)
  => MonadBaseControl b (PQ db0 db1 m) where
  type StM (PQ db0 db1 m) x = StM m (K x db0)
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
      Nothing -> throw $ ResultException "nextRow: found unexpected length"
      Just row -> case fromRow @columns row of
        Left parseError -> throw $ ParseException $ "nextRow: " <> parseError
        Right y -> return $ Just (r+1, y)

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
      Nothing -> throw $ ResultException "getRows: found unexpected length"
      Just row -> case fromRow @columns row of
        Left parseError -> throw $ ParseException $ "getRows: " <> parseError
        Right y -> return y

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
      Nothing -> throw $ ResultException "firstRow: found unexpected length"
      Just row -> case fromRow @columns row of
        Left parseError -> throw $ ParseException $ "firstRow: " <> parseError
        Right y -> return $ Just y

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

-- | Returns the error code most recently generated by an operation
-- on the connection.
--
-- https://www.postgresql.org/docs/current/static/errcodes-appendix.html
resultErrorCode
  :: MonadBase IO io
  => K LibPQ.Result results
  -> io (Maybe ByteString)
resultErrorCode = liftResult (flip LibPQ.resultErrorField LibPQ.DiagSqlstate)

-- | `Exception`s that can be thrown by Squeal.
data SquealException
  = PQException
  { sqlExecStatus :: LibPQ.ExecStatus
  , sqlStateCode :: Maybe ByteString
    -- ^ https://www.postgresql.org/docs/current/static/errcodes-appendix.html
  , sqlErrorMessage :: Maybe ByteString
  }
  | ResultException Text
  | ParseException Text
  deriving (Eq, Show)
instance Exception SquealException

tryResult
  :: MonadBase IO io
  => LibPQ.Result
  -> io ()
tryResult result = liftBase $ do
  status <- LibPQ.resultStatus result
  case status of
    LibPQ.CommandOk -> return ()
    LibPQ.TuplesOk -> return ()
    _ -> do
      stateCode <- LibPQ.resultErrorField result LibPQ.DiagSqlstate
      msg <- LibPQ.resultErrorMessage result
      throw $ PQException status stateCode msg

-- | Catch `SquealException`s.
catchSqueal
  :: MonadBaseControl IO io
  => io a
  -> (SquealException -> io a) -- ^ handler
  -> io a
catchSqueal = catch

-- | Handle `SquealException`s.
handleSqueal
  :: MonadBaseControl IO io
  => (SquealException -> io a) -- ^ handler
  -> io a -> io a
handleSqueal = handle

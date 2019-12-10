{-|
Module: Squeal.PostgreSQL.PQ
Description: PQ monad
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

This module is where Squeal commands actually get executed by
`Database.PostgreSQL.LibPQ`. It containts two typeclasses,
`IndexedMonadTransPQ` for executing
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
  , manipulateParams
  , manipulateParams_
  , manipulate
  , manipulate_
  , runQueryParams
  , runQuery
  , traversePrepared
  , forPrepared
  , traversePrepared_
  , forPrepared_
    -- * Indexed Monad
  , IndexedMonadTrans (..)
  , Indexed (..)
    -- * Result
  , LibPQ.Row
  , LibPQ.Column
  , ntuples
  , nfields
  , getRow
  , getRows
  , nextRow
  , firstRow
  , liftResult
  , LibPQ.ExecStatus (..)
  , resultStatus
  , resultErrorMessage
  , resultErrorCode
    -- * Exception
  , SquealException (..)
  , PQState (..)
  , okResult
  , catchSqueal
  , handleSqueal
  , trySqueal
    -- * Re-export
  , K (..)
  ) where

import Control.Category
import Control.Exception (Exception, throw)
import Control.Monad.Except
import Control.Monad.Morph
import UnliftIO (MonadUnliftIO (..), bracket, catch, handle, try)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Kind
import Data.Text (pack, Text)
import Data.Traversable
import Generics.SOP
import PostgreSQL.Binary.Encoding (encodingBytes)
import Prelude hiding (id, (.))

import qualified Control.Monad.Fail as Fail
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Encoding as Encoding

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
  :: forall db io
   . MonadIO io
  => ByteString -- ^ conninfo
  -> io (K LibPQ.Connection db)
connectdb = fmap K . liftIO . LibPQ.connectdb

-- | Closes the connection to the server.
finish :: MonadIO io => K LibPQ.Connection db -> io ()
finish = liftIO . LibPQ.finish . unK

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

-- | Safely `lowerConnection` to a smaller schema.
lowerConnection
  :: K LibPQ.Connection (schema ': db)
  -> K LibPQ.Connection db
lowerConnection (K conn) = K conn

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

{- | An [Atkey indexed monad]
(https://bentnib.org/paramnotions-jfp.pdf)
is a `Functor` [enriched category]
(https://ncatlab.org/nlab/show/enriched+category).
An indexed monad transformer transforms a `Monad` into an indexed monad,
and is a monad transformer when its source and target are the same,
enabling use of standard @do@ notation for endo-index operations.
-}
class
  ( forall i j m. Monad m => Functor (t i j m)
  , forall i j m. (i ~ j, Monad m) => Monad (t i j m)
  , forall i j. i ~ j => MonadTrans (t i j)
  ) => IndexedMonadTrans t where

  {-# MINIMAL pqJoin | pqBind #-}

  -- | indexed analog of `<*>`
  pqAp
    :: Monad m
    => t i j m (x -> y)
    -> t j k m x
    -> t i k m y
  pqAp tf tx = pqBind (<$> tx) tf

  -- | indexed analog of `join`
  pqJoin
    :: Monad m
    => t i j m (t j k m y)
    -> t i k m y
  pqJoin t = t & pqBind id

  -- | indexed analog of `=<<`
  pqBind
    :: Monad m
    => (x -> t j k m y)
    -> t i j m x
    -> t i k m y
  pqBind f t = pqJoin (f <$> t)

  -- | indexed analog of flipped `>>`
  pqThen
    :: Monad m
    => t j k m y
    -> t i j m x
    -> t i k m y
  pqThen pq2 pq1 = pq1 & pqBind (\ _ -> pq2)

  -- | indexed analog of `<=<`
  pqAndThen
    :: Monad m
    => (y -> t j k m z)
    -> (x -> t i j m y)
    -> x -> t i k m z
  pqAndThen g f x = pqBind g (f x)

{- | `Indexed` reshuffles the arguments of an `IndexedMonadTrans`,
exposing its `Category` instance.-}
newtype Indexed t m r i j = Indexed {runIndexed :: t i j m r}
instance
  ( IndexedMonadTrans t
  , Monad m
  , Monoid r
  ) => Category (Indexed t m r) where
    id = Indexed (pure mempty)
    Indexed g . Indexed f = Indexed $ pqAp (fmap (<>) f) g

{- | `IndexedMonadTransPQ` is a class for indexed monad transformers
that support running `Definition`s using `define` which acts functorially in effect.

* @define id = return ()@
* @define (statement1 >> statement2) = define statement1 & pqThen (define statement2)@
-}
class IndexedMonadTrans pq => IndexedMonadTransPQ pq where
  define :: MonadIO io => Definition db0 db1 -> pq db0 db1 io ()

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
class Monad pq => MonadPQ db pq | pq -> db where

  executeParams :: Statement db x y -> x -> pq (Result y)
  default executeParams
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Statement db x y -> x -> pq (Result y)
  executeParams statement params = lift $ executeParams statement params

  executeParams_ :: Statement db x () -> x -> pq ()
  executeParams_ statement params = void $ executeParams statement params

  execute :: Statement db () y -> pq (Result y)
  execute statement = executeParams statement ()

  execute_ :: Statement db () () -> pq ()
  execute_ = void . execute

  executePrepared
    :: Traversable list
    => Statement db x y -> list x -> pq (list (Result y))
  default executePrepared
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Traversable list
    => Statement db x y -> list x -> pq (list (Result y))
  executePrepared statement x = lift $ executePrepared statement x

  executePrepared_
    :: Foldable list
    => Statement db x () -> list x -> pq ()
  default executePrepared_
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Foldable list
    => Statement db x () -> list x -> pq ()
  executePrepared_ statement x = lift $ executePrepared_ statement x

manipulateParams
  :: (MonadPQ db pq, ToParams x params, All OidOfParam params, FromRow row y)
  => Manipulation '[] db params row
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> x -> pq (Result y)
manipulateParams = executeParams . manipulation

manipulateParams_
  :: (MonadPQ db pq, ToParams x params, All OidOfParam params)
  => Manipulation '[] db params '[]
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> x -> pq ()
manipulateParams_ = executeParams_ . manipulation

manipulate
  :: (MonadPQ db pq, FromRow row y)
  => Manipulation '[] db '[] row
  -> pq (Result y)
manipulate = execute . manipulation

manipulate_
  :: MonadPQ db pq
  => Manipulation '[] db '[] '[]
  -> pq ()
manipulate_ = execute_ . manipulation

runQueryParams
  :: (MonadPQ db pq, ToParams x params, All OidOfParam params, FromRow row y)
  => Query '[] '[] db params row
  -- ^ `select` and friends
  -> x -> pq (Result y)
runQueryParams = executeParams . query

runQuery
  :: (MonadPQ db pq, FromRow row y)
  => Query '[] '[] db '[] row
  -- ^ `select` and friends
  -> pq (Result y)
runQuery = execute . query

traversePrepared
  :: ( MonadPQ db pq
     , ToParams x params
     , Traversable list
     , All OidOfParam params
     , FromRow row y )
  => Manipulation '[] db params row
  -- ^ `insertInto`, `update`, or `deleteFrom`, and friends
  -> list x -> pq (list (Result y))
traversePrepared = executePrepared . manipulation

forPrepared
  :: ( MonadPQ db pq
     , ToParams x params
     , Traversable list
     , All OidOfParam params
     , FromRow row y )
  => list x
  -> Manipulation '[] db params row
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> pq (list (Result y))
forPrepared = flip traversePrepared

traversePrepared_
  :: ( MonadPQ db pq
     , ToParams x params
     , Foldable list
     , All OidOfParam params )
  => Manipulation '[] db params '[]
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> list x -> pq ()
traversePrepared_ = executePrepared_ . manipulation

forPrepared_
  :: ( MonadPQ db pq
     , ToParams x params
     , Foldable list
     , All OidOfParam params )
  => list x
  -> Manipulation '[] db params '[]
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> pq ()
forPrepared_ = flip traversePrepared_

instance (MonadIO io, db0 ~ db, db1 ~ db) => MonadPQ db (PQ db0 db1 io) where

  executeParams (Statement encode decode (UnsafeManipulation q)) x =
    PQ $ \ (K conn) -> do
      let
        paramSet
          :: forall param. OidOfParam param
          => K (Maybe Encoding.Encoding) param
          -> K (Maybe (LibPQ.Oid, ByteString, LibPQ.Format)) param
        paramSet (K maybeEncoding) = K $
          maybeEncoding <&> \encoding ->
            (oidOfParam @param, encodingBytes encoding, LibPQ.Binary)
        params'
          = hcollapse
          . hcmap (Proxy @OidOfParam) paramSet
          $ encode x
        q' = q <> ";"
      resultMaybe <- liftIO $ LibPQ.execParams conn q' params' LibPQ.Binary
      case resultMaybe of
        Nothing -> throw $ ResultException
          "executeParams: LibPQ.execParams returned no results"
        Just result -> do
          okResult_ result
          return $ K (Result decode result)

  executePrepared (Statement encode decode (UnsafeManipulation q :: Manipulation '[] db params row)) list =
    PQ $ \ (K conn) -> liftIO $ do
      let
        temp = "temporary_statement"
        paramOid :: forall p. OidOfParam p => K LibPQ.Oid p
        paramOid = K (oidOfParam @p)
        paramOids :: NP (K LibPQ.Oid) params
        paramOids = hcpure (Proxy @OidOfParam) paramOid
        paramOids' :: [LibPQ.Oid]
        paramOids' = hcollapse paramOids
      prepResultMaybe <- LibPQ.prepare conn temp q (Just paramOids')
      case prepResultMaybe of
        Nothing -> throw $ ResultException
          "traversePrepared: LibPQ.prepare returned no results"
        Just prepResult -> okResult_ prepResult
      results <- for list $ \ params -> do
        let
          toParam' encoding = (encodingBytes encoding, LibPQ.Binary)
          params' = fmap (fmap toParam') (hcollapse (encode params))
        resultMaybe <- LibPQ.execPrepared conn temp params' LibPQ.Binary
        case resultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared: LibPQ.execParams returned no results"
          Just result -> do
            okResult_ result
            return $ Result decode result
      deallocResultMaybe <- LibPQ.exec conn ("DEALLOCATE " <> temp <> ";")
      case deallocResultMaybe of
        Nothing -> throw $ ResultException
          "traversePrepared: LibPQ.exec DEALLOCATE returned no results"
        Just deallocResult -> okResult_ deallocResult
      return (K results)

  executePrepared_ (Statement encode _ (UnsafeManipulation q :: Manipulation '[] db params row)) list =
      PQ $ \ (K conn) -> liftIO $ do
        let
          temp = "temporary_statement"
          paramOid :: forall p. OidOfParam p => K LibPQ.Oid p
          paramOid = K (oidOfParam @p)
          paramOids :: NP (K LibPQ.Oid) params
          paramOids = hcpure (Proxy @OidOfParam) paramOid
          paramOids' :: [LibPQ.Oid]
          paramOids' = hcollapse paramOids
        prepResultMaybe <- LibPQ.prepare conn temp q (Just paramOids')
        case prepResultMaybe of
          Nothing -> throw $ ResultException
            "traversePrepared_: LibPQ.prepare returned no results"
          Just prepResult -> okResult_ prepResult
        for_ list $ \ params -> do
          let
            toParam' encoding = (encodingBytes encoding, LibPQ.Binary)
            params' = fmap (fmap toParam') (hcollapse (encode params))
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

-- | Get a row corresponding to a given row number from a `LibPQ.Result`,
-- throwing an exception if the row number is out of bounds.
getRow :: MonadIO io => LibPQ.Row -> Result y -> io y
getRow r (Result decode result) = liftIO $ do
  numRows <- LibPQ.ntuples result
  numCols <- LibPQ.nfields result
  when (numRows < r) $ throw $ ResultException $
    "getRow: expected at least " <> pack (show r) <> "rows but only saw "
    <> pack (show numRows)
  row' <- traverse (LibPQ.getvalue result r) [0 .. numCols - 1]
  case fromList row' of
    Nothing -> throw $ ResultException "getRow: found unexpected length"
    Just row -> case decode row of
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
getRows :: MonadIO io => Result y -> io [y]
getRows (Result decode result) = liftIO $ do
  numCols <- LibPQ.nfields result
  numRows <- LibPQ.ntuples result
  for [0 .. numRows - 1] $ \ r -> do
    row' <- traverse (LibPQ.getvalue result r) [0 .. numCols - 1]
    case fromList row' of
      Nothing -> throw $ ResultException "getRows: found unexpected length"
      Just row -> case decode row of
        Left parseError -> throw $ ParseException $ "getRows: " <> parseError
        Right y -> return y

-- | Get the first row if possible from a `LibPQ.Result`.
firstRow :: MonadIO io => Result y -> io (Maybe y)
firstRow (Result decode result) = liftIO $ do
  numRows <- LibPQ.ntuples result
  numCols <- LibPQ.nfields result
  if numRows <= 0 then return Nothing else do
    row' <- traverse (LibPQ.getvalue result 0) [0 .. numCols - 1]
    case fromList row' of
      Nothing -> throw $ ResultException "firstRow: found unexpected length"
      Just row -> case decode row of
        Left parseError -> throw $ ParseException $ "firstRow: " <> parseError
        Right y -> return $ Just y

-- | Lifts actions on results from @LibPQ@.
liftResult
  :: MonadIO io
  => (LibPQ.Result -> IO x)
  -> Result y -> io x
liftResult f (Result _ result) = liftIO $ f result

-- | Returns the number of rows (tuples) in the query result.
ntuples :: MonadIO io => Result y -> io LibPQ.Row
ntuples = liftResult LibPQ.ntuples

-- | Returns the number of columns (fields) in the query result.
nfields :: MonadIO io => Result y -> io LibPQ.Column
nfields = liftResult LibPQ.nfields

-- | Returns the result status of the command.
resultStatus :: MonadIO io => Result y -> io LibPQ.ExecStatus
resultStatus = liftResult LibPQ.resultStatus

-- | Returns the error message most recently generated by an operation
-- on the connection.
resultErrorMessage
  :: MonadIO io => Result y -> io (Maybe ByteString)
resultErrorMessage = liftResult LibPQ.resultErrorMessage

-- | Returns the error code most recently generated by an operation
-- on the connection.
--
-- https://www.postgresql.org/docs/current/static/errcodes-appendix.html
resultErrorCode
  :: MonadIO io
  => Result y
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

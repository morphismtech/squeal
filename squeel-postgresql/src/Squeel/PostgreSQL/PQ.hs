{-# LANGUAGE
    DataKinds
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
#-}

module Squeel.PostgreSQL.PQ where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text)

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Squeel.PostgreSQL.Query
import Squeel.PostgreSQL.Value

newtype Connection db = Connection { unConnection :: LibPQ.Connection }

newtype PQ m db0 db1 x = PQ
  { runPQ :: Connection db0 -> m (x, Connection db1) }

connectdb :: MonadIO io => ByteString -> io (Connection db)
connectdb = fmap Connection . liftIO . LibPQ.connectdb

connectStart :: MonadIO io => ByteString -> io (Connection db)
connectStart = fmap Connection . liftIO . LibPQ.connectStart

newNullConnection :: MonadIO io => io (Connection db)
newNullConnection = Connection <$> liftIO LibPQ.newNullConnection

finish :: MonadIO io => Connection db -> io ()
finish = liftIO . LibPQ.finish . unConnection

newtype Result xs = Result { unResult :: LibPQ.Result }

exec :: MonadIO io => Query db0 db1 '[] xs -> PQ io db0 db1 (Result xs)
exec = undefined

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
  :: (FromValue x y, MonadIO io)
  => Proxy x
  -> Result xs
  -> RowNumber
  -> ColumnNumber xs x
  -> io (Maybe (Either Text y))
getvalue proxy (Result result) (RowNumber r) (ColumnNumber c) = liftIO $
  fmap (fmap (decodeValue proxy)) (LibPQ.getvalue result r c)

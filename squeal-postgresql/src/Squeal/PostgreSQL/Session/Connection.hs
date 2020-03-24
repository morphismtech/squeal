{-|
Module: Squeal.PostgreSQL.Session.Connection
Description: Database connections
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Database connections
-}

{-# LANGUAGE
    DataKinds
  , PolyKinds
  , RankNTypes
  , TypeOperators
#-}

module Squeal.PostgreSQL.Session.Connection
  ( LibPQ.Connection
  , connectdb
  , finish
  , lowerConnection
  , SOP.K (..)
  , SOP.unK
  ) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)

import Squeal.PostgreSQL.Type.Schema

import qualified Generics.SOP as SOP
import qualified Database.PostgreSQL.LibPQ as LibPQ

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
>>> type DB = '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint2])]]
>>> :set -XTypeApplications
>>> :set -XOverloadedStrings
>>> conn <- connectdb @DB "host=localhost port=5432 dbname=exampledb"

Note that, for now, squeal doesn't offer any protection from connecting
with the wrong schema!
-}
connectdb
  :: forall (db :: SchemasType) io
   . MonadIO io
  => ByteString -- ^ conninfo
  -> io (SOP.K LibPQ.Connection db)
connectdb = fmap SOP.K . liftIO . LibPQ.connectdb

-- | Closes the connection to the server.
finish :: MonadIO io => SOP.K LibPQ.Connection db -> io ()
finish = liftIO . LibPQ.finish . SOP.unK

-- | Safely `lowerConnection` to a smaller schema.
lowerConnection
  :: SOP.K LibPQ.Connection (schema ': db)
  -> SOP.K LibPQ.Connection db
lowerConnection (SOP.K conn) = SOP.K conn

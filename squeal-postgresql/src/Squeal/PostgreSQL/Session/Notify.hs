{-|
Module: Squeal.PostgreSQL.Session.Notify
Description: database notification
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Support for receiving asynchronous notifications
via PostgreSQL's Listen/Notify mechanism.
See https://www.postgresql.org/docs/current/sql-notify.html
for more information.
-}

{-# LANGUAGE
    DataKinds
  , PolyKinds
  , RankNTypes
  , TypeOperators
#-}

module Squeal.PostgreSQL.Session.Notify
  ( getNotification
  , getNotificationWithConfig
  ) where

import Data.Function ((&))
import UnliftIO

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Database.PostgreSQL.LibPQ.Notify as LibPQ
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Session.Exception
import Squeal.PostgreSQL.Session

{-|
Returns a single notification.  If no notifications are
available, 'getNotificationWithConfig' blocks until one arrives.
Unlike 'getNotification', 'getNotificationWithConfig' takes in an
additional 'Config' parameter which provides custom 'interrupt' and
various event hooks for operational insight.

Using a custom 'interrupt' is necessary if one would like to call
'getNotificationWithConfig' on one thread and @NOTIFY@ on another
thread using the same connection.

To support this behavior one must cause 'interrupt' to return after the
call to @NOTIFY@ checks it's result from the server.

See the test file of this package for an example of how to use a custom
'interrupt'.

Note that PostgreSQL does not
deliver notifications while a connection is inside a transaction.
-}
getNotificationWithConfig
  :: MonadUnliftIO io
  => LibPQ.Config
  -> PQ db db io LibPQ.Notify
getNotificationWithConfig config = PQ $ \ (SOP.K conn) -> do
  noteOrErr <- liftIO $ LibPQ.getNotificationWithConfig config (&) conn
  case noteOrErr of
    Left err -> throwSqueal (NotificationException err)
    Right note -> return (SOP.K note)
{-|
Returns a single notification.  If no notifications are
available, 'getNotification' blocks until one arrives.

If 'getNotification' is called and afterwards on a different thread
@NOTIFY@ is called using the same connection, 'getNotification' can
block even if a notification is sent.

To support this behavior one must use 'getNotificationWithConfig' instead.

Note that PostgreSQL does not
deliver notifications while a connection is inside a transaction.
-}
getNotification
  :: MonadUnliftIO io
  => PQ db db io LibPQ.Notify
getNotification = getNotificationWithConfig LibPQ.defaultConfig

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DerivingStrategies                #-}
{-# LANGUAGE ScopedTypeVariables                #-}
{-# LANGUAGE StandaloneDeriving              #-}

module Gauge.DBHelpers where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C
import qualified Data.Text                     as T
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( MonadIO
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Loops            ( iterateWhile )
import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           Test.QuickCheck
import           Squeal.PostgreSQL
import qualified Data.ByteString.Char8         as C
import           Control.DeepSeq
-- Project imports
import           Gauge.Schema                   ( Schemas )
import           Gauge.Queries                  ( InsertUser(..) )
import           Gauge.DBSetup

newtype SquealPool = SquealPool {getSquealPool :: Pool (K Connection Schemas)} deriving (Generic)
-- Below may be wrong - it may screw up the whole connection pool using in tests
instance NFData SquealPool where
  rnf = rwhnf

runDbErr
  :: SquealPool -> PQ Schemas Schemas IO b -> IO (Either SquealException b)
runDbErr pool session = do
  liftIO . runUsingConnPool pool $ trySqueal (transactionally_ session)

runDbWithPool :: SquealPool -> PQ Schemas Schemas IO b -> IO b
runDbWithPool pool session = do
  errOrResult <- runDbErr pool session
  case errOrResult of
    Left  err    -> throwSqueal err
    Right result -> return result

-- | Helper
runUsingConnPool :: SquealPool -> PQ Schemas Schemas IO x -> IO x
runUsingConnPool (SquealPool pool) = usingConnectionPool pool

makePool :: C.ByteString -> IO SquealPool
makePool connStr = do
  pool <- createConnectionPool connStr 1 0.5 10
  return $ SquealPool pool

initDBWithPool :: IO SquealPool
initDBWithPool = do
  void initDB
  pool <- makePool connectionString
  return pool

getRandomUser :: IO InsertUser
getRandomUser = iterateWhile noEmptyEmail $ generate arbitrary
 where
  noEmptyEmail InsertUser { userEmail = userEmail } = T.length userEmail < 5

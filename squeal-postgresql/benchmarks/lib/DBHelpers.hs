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

module DBHelpers where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C
import           Control.Monad                  ( void )
import           Data.Pool                      ( withResource )
import           Control.Monad.Except           ( MonadIO
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           GHC.Generics
import           Test.QuickCheck
import           Squeal.PostgreSQL
import qualified Squeal.PostgreSQL.PQ.Pool     as SPG
import qualified Data.ByteString.Char8         as C
-- Project imports
import           Schema                         ( Schemas )
import           Queries                        ( InsertUser )
import           DBSetup

newtype SquealPool = SquealPool {getSquealPool :: SPG.Pool (K Connection Schemas)}

runDbErr
  :: SquealPool -> PQ Schemas Schemas IO b -> IO (Either SquealException b)
runDbErr pool session = do
  liftIO . runUsingConnPool pool $ trySqueal (transactionally_ session)

runDbWithPool :: SquealPool -> PQ Schemas Schemas IO b -> IO b
runDbWithPool pool session = do
  errOrResult <- runDbErr pool session
  case errOrResult of
    Left err -> throwSqueal
      $ ConnectionException "Running query with connection pool failed"
    Right result -> return result

-- | Helper
runUsingConnPool :: SquealPool -> PQ Schemas Schemas IO x -> IO x
runUsingConnPool (SquealPool pool) (PQ session) =
  unK <$> withResource pool session

makePool :: C.ByteString -> IO SquealPool
makePool connStr = do
  pool <- SPG.createConnectionPool connStr 1 0.5 10
  return $ SquealPool pool

initDBWithPool :: IO SquealPool
initDBWithPool = do
  void initDB
  pool <- makePool connectionString
  return pool

getRandomUser :: IO InsertUser
getRandomUser = generate arbitrary

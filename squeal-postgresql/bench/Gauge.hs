{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Squeal.PostgreSQL       hiding ( defaultMain )
import           Gauge.Main
import           Gauge.Main.Options             ( defaultConfig
                                                , Config(..)
                                                , Verbosity(..)
                                                , DisplayMode(..)
                                                , Mode(..)
                                                )
import           GHC.Generics
import qualified Generics.SOP                  as SOP
import           Test.QuickCheck
-- For CI
import           Main.Utf8                      ( withUtf8 )
-- For keeping a track of which question ID to query
import           Data.Int                       ( Int64 )
import           Data.IORef
-- Project imports
import           Gauge.Schema
import           Gauge.Queries
import           Gauge.DBSetup                  ( teardownDB )
import           Gauge.DBHelpers                ( initDBWithPool
                                                , getRandomUser
                                                , runDbWithPool
                                                , SquealPool
                                                )

main :: IO ()
main = do
  -- A mutable hack here to keep track of
  -- pulling a new user by ID from the db instead of the same id
  currentId <- newIORef (1 :: UserId)

  -- Define benchmarks
  let
    queryRenderGroup :: Benchmark
    queryRenderGroup = bgroup
      "Render Queries"
      [ bench "createUser: weak head normal form" $ whnf renderSQL createUser
      , bench "createUser: normal form" $ nf renderSQL createUser
      , bench "userDetails: weak head normal form" $ whnf renderSQL userDetails
      , bench "userDetails: normal form" $ nf renderSQL userDetails
      , bench "insertDeviceDetails: weak head normal form"
        $ whnf renderSQL insertDeviceDetails
      , bench "insertDeviceDetails: normal form"
        $ nf renderSQL insertDeviceDetails
      ]

    -- Queries against an actual DB

    -- 1. Initialize Schema to DB
    -- 2. Make connection pool and pass it to tests
    -- 3. Generate users on the fly and add them to DB
    -- 4. Tear the Schema down from the DB

    dbInsertsGroup :: Benchmark
    dbInsertsGroup =
      envWithCleanup initDBWithPool (const teardownDB) $ \pool -> bgroup
        "Run individual INSERTs against DB using a connection pool"
        [ bgroup
            "INSERT: add users to the table users"
            [ bench "Run individual INSERT statement" $ makeRunOnce $ perRunEnv
                getRandomUser
                -- The actual action to benchmark
                (\(user :: InsertUser) ->
                  runDbWithPool pool $ createUserSession user
                )
            ]
        ]

    dbSelectsGroup :: Benchmark
    dbSelectsGroup =
      envWithCleanup initDBWithPool (const teardownDB) $ \pool -> bgroup
        "Run individual SELECTs against DB using a connection pool"
        [ bgroup
            "SELECT: fetch users from the table users individually"
            [ bench "Fetch a single user" $ makeRunOnce $ perRunEnv
                (insertAndIncrement pool currentId)
                (\(id_ :: UserId) -> runDbWithPool pool $ userDetailsSession id_
                )
            ]
        ]

  withUtf8 $ defaultMain [queryRenderGroup, dbInsertsGroup, dbSelectsGroup]


-- | Configure the benchmark to run only once (per IO action)
makeRunOnce :: Benchmarkable -> Benchmarkable
makeRunOnce current = current { perRun = True }

getAndIncrementId :: (IORef UserId) -> IO UserId
getAndIncrementId currentId = do
  current <- readIORef currentId
  writeIORef currentId (current + 1)
  return current

-- | This INSERTs a row in the db so that there's always a row to query.
-- Otherwise 'getRow 0' throws an exception.
-- NOTE: will make benchmark time slower but does not affect results.
insertAndIncrement :: SquealPool -> (IORef UserId) -> IO UserId
insertAndIncrement pool currentId = do
  user <- getRandomUser
  _    <- runDbWithPool pool $ createUserSession user
  getAndIncrementId currentId

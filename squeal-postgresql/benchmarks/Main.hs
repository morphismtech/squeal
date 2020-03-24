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
-- Don't define 'module Main where' here
-- or otherwise 'stack bench' won't work.

import           Squeal.PostgreSQL       hiding ( defaultMain )
import           Gauge.Main
import           Gauge.Main.Options             ( defaultConfig
                                                , Config(..)
                                                )
import           GHC.Generics
import qualified Generics.SOP                  as SOP
import           Test.QuickCheck
-- For keeping a track of which question ID to query
import           Data.Int                       ( Int64 )
import           Data.IORef
-- Project imports
import           Schema
import           Queries
import           DBSetup                        ( teardownDB )
import           DBHelpers                      ( initDBWithPool
                                                , getRandomUser
                                                , runDbWithPool
                                                )

-- Configure the number of iterations to 6000
-- If we don't set it fixed, the benchmark will try to query more
-- SELECTs than there are rows, causing exceptions. This is probably
-- because SELECTs are faster, and the default config prioritizes homogenous bench time
-- over iteration count
config :: Config
config = defaultConfig { iters = Just (6000 :: Int64) }

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
    -- 1. Initialize Schema to DB
    -- 2. Make connection pool and pass it to tests
    -- 3. Generate users on the fly and add them to DB
    -- 4. Tear the Schema down from the DB
    dbManipulationsGroup :: Benchmark
    dbManipulationsGroup =
      envWithCleanup initDBWithPool (const teardownDB) $ \pool -> bgroup
        "Run individual queries and manipulations against DB using a connection pool"
        [ bgroup
          "INSERT: add thousands of users to the table users"
          [ bench "Run individual INSERT statement" $ makeRunOnce $ perRunEnv
              getRandomUser
                -- The actual action to benchmark
              (\(user :: InsertUser) ->
                runDbWithPool pool $ createUserSession user
              )
          ]
        , bgroup
          "SELECT: fetch all users from the table users individually"
          [ bench "Run individual SELECT statement" $ makeRunOnce $ perRunEnv
              (getAndIncrementId currentId)
              (\(id_ :: UserId) -> runDbWithPool pool $ userDetailsSession id_)
          ]
        ]

    getAndIncrementId :: (IORef UserId) -> IO UserId
    getAndIncrementId currentId = do
      current <- readIORef currentId
      writeIORef currentId (current + 1)
      return current
  -- run all benchmarks with modified config
  defaultMainWith config [queryRenderGroup, dbManipulationsGroup]


-- | Configure the benchmark to run only once (per IO action)
makeRunOnce :: Benchmarkable -> Benchmarkable
makeRunOnce current = current { perRun = True }

{- 
To benchmark actual IO actions like supplying parameters to a query,
we would generate samples via QuickCheck like this:

```
d :: [InsertUser] <- sample' arbitrary
```

Then start testing those generated values with `manipulateParams`.
-}

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
-- module Main where

import           Squeal.PostgreSQL       hiding ( defaultMain )
import           Gauge.Main
import           GHC.Generics
import qualified Generics.SOP                  as SOP
import           Test.QuickCheck
-- Project imports
import           Schema
import           Queries
import           DBSetup                        ( teardownDB )
import           DBHelpers                      ( initDBWithPool
                                                , getRandomUser
                                                , runDbWithPool
                                                )


main :: IO ()
main = defaultMain
  [ bgroup
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
  , envWithCleanup initDBWithPool (const teardownDB) $ \pool -> bgroup
    "Run individual queries and manipulations against DB using a connection pool"
    [ bgroup
      "INSERT: add 100 users to the table users"
      [ bench "Weak head normal form" $ perRunEnv
          getRandomUser
          (\(user :: InsertUser) -> runDbWithPool pool $ createUserSession user)
      ]
    , bgroup "SELECT: fetch 100 users from the table"
             [bench "PLACEHOLDER" $ nf renderSQL createUser]
    ]
  ]

{- 
To benchmark actual IO actions like supplying parameters to a query,
we would generate samples via QuickCheck like this:

```
d :: [InsertUser] <- sample' arbitrary
```

Then start testing those generated values with `manipulateParams`.
-}

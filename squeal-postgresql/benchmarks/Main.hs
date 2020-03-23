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
import           Criterion.Main
import           GHC.Generics
import qualified Generics.SOP                  as SOP
import           Test.QuickCheck
-- Project imports
import           Schema
import           Queries

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
  ]

{- 
To benchmark actual IO actions like supplying parameters to a query,
we would generate samples via QuickCheck like this:

```
d :: [InsertUser] <- sample' arbitrary
```

Then start testing those generated values with `manipulateParams`.
-}

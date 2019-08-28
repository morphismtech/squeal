module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Squeal/PostgreSQL.hs"
  , "src/Squeal/PostgreSQL/Binary.hs"
  , "src/Squeal/PostgreSQL/Definition.hs"
  , "src/Squeal/PostgreSQL/Manipulation.hs"
  , "src/Squeal/PostgreSQL/Query.hs"
  , "src/Squeal/PostgreSQL/Expression.hs"
  , "src/Squeal/PostgreSQL/PQ.hs"
  , "src/Squeal/PostgreSQL/Migration.hs"
  , "src/Squeal/PostgreSQL/Transaction.hs"
  , "src/Squeal/PostgreSQL/Pool.hs"
  ]

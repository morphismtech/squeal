module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Squeal/PostgreSQL/Binary.hs"
  , "src/Squeal/PostgreSQL/Definition.hs"
  , "src/Squeal/PostgreSQL/Manipulation.hs"
  , "src/Squeal/PostgreSQL/Query.hs"
  ]

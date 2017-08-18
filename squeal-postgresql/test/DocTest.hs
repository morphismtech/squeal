module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Squeal/PostgreSQL/Binary.hs"
  , "src/Squeal/PostgreSQL/Statement.hs"
  ]

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Main (main) where

import Squeal.PostgreSQL

main :: IO ()
main = putStrLn "plugin test"

example1 :: Has "foo" '["bar" ::: Int, "f0o" ::: Double, "baz" ::: Char] Double => ()
example1 = ()

example2 :: ()
example2 = example1

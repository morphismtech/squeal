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

import Data.Proxy
import Squeal.PostgreSQL hiding (Has)
import Squeal.PostgreSQL.Has

main :: IO ()
main = putStrLn "plugin test"

example1 :: Has "foo" '["bar" ::: Int, "f0o" ::: Double, "baz" ::: Char] x => Proxy x -> ()
example1 _ = ()

example2 :: ()
example2 = example1 Proxy

module Main where

import           Test.Hspec
import qualified ExceptionHandling
import qualified Binary

main :: IO ()
main = do
  hspec ExceptionHandling.specs
  Binary.roundtrips

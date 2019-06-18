module Main where

import           Test.Hspec
import qualified ExceptionHandling

main :: IO ()
main = hspec ExceptionHandling.specs

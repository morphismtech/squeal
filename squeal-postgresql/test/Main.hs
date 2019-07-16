module Main (main) where

import Doc (docTests)
import Spec (specTests)
import Property (propertyTests)

main :: IO ()
main = do
  putStrLn "\n\nRUN DOC TESTS\n\n"
  docTests
  putStrLn "\n\nRUN SPEC TESTS\n\n"
  specTests
  putStrLn "\n\nRUN PROP TESTS\n\n"
  propertyTests

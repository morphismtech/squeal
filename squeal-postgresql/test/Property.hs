{-# LANGUAGE
    DataKinds
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Property (propertyTests) where

import Control.Monad.Trans
import Data.Int (Int32)
import Data.ByteString (ByteString)
import Data.Scientific (fromFloatDigits)
import Squeal.PostgreSQL hiding (check, defaultMain)
import Hedgehog hiding (Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Main
import qualified Hedgehog.Range as Range

propertyTests :: IO ()
propertyTests = Main.defaultMain
  [ roundtrip int2 (Gen.int16 Range.exponentialBounded)
  , roundtrip int4 (Gen.int32 Range.exponentialBounded)
  , roundtrip int8 (Gen.int64 Range.exponentialBounded)
  , roundtrip bool Gen.bool
  , roundtrip numeric (scientific (Range.exponentialFloatFrom 0 (-1E9) 1E9))
  , roundtrip float4 (Gen.float (Range.exponentialFloatFrom 0 (-1E9) 1E9))
  , roundtrip float8 (Gen.double (Range.exponentialFloatFrom 0 (-1E308) 1E308))
  , roundtrip int4range gen_int4range
  ]
  where
    scientific = fmap fromFloatDigits . Gen.float
    gen_int4range = Gen.constant (Empty :: Range Int32)

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=exampledb"

roundtrip
  :: (ToParam x ty, FromValue ty x, Show x, Eq x)
  => TypeExpression schemas ('NotNull ty)
  -> Gen x
  -> IO Bool
roundtrip ty gen = check . property $ do
  x <- forAll gen
  Just (Only y) <- lift . withConnection connectionString $
    firstRow =<< runQueryParams
      (values_ (parameter @1 ty `as` #fromOnly)) (Only x)
  x === y

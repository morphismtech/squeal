{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Property (propertyTests) where

import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.Scientific (fromFloatDigits)
import Data.Fixed (Fixed(MkFixed), Micro, Pico)
import Data.Time
import Squeal.PostgreSQL hiding (check, defaultMain)
import Hedgehog hiding (Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Main
import qualified Hedgehog.Range as Range

propertyTests :: IO ()
propertyTests = Main.defaultMain
  [ roundtrip int2 genInt16 id
  , roundtrip int4 genInt32 id
  , roundtrip int8 genInt64 id
  , roundtrip bool Gen.bool id
  , roundtrip numeric genScientific id
  , roundtrip float4 genFloat id
  , roundtrip float8 genDouble id
  , roundtrip time genTimeOfDay normalizeTimeOfDay
  -- , roundtrip timetz genTimeWithZone normalizeTimeWithZone
  , roundtrip timestamp genLocalTime normalizeLocalTime
  , roundtrip timestamptz genUTCTime id
  , roundtrip date genDay id
  , roundtrip interval genDiffTime id
  , roundtrip int4range (genRange genInt32) normalizeIntRange
  , roundtrip int8range (genRange genInt64) normalizeIntRange
  , roundtrip numrange (genRange genScientific) id
  , roundtrip tsrange (genRange genLocalTime) (fmap normalizeLocalTime)
  , roundtrip tstzrange (genRange genUTCTime) id
  , roundtrip daterange (genRange genDay) normalizeIntRange
  ]
  where
    genInt16 = Gen.int16 Range.exponentialBounded
    genInt32 = Gen.int32 Range.exponentialBounded
    genInt64 = Gen.int64 Range.exponentialBounded
    genScientific = fromFloatDigits <$>
      Gen.float (Range.exponentialFloatFrom 0 (-1E9) 1E9)
    genFloat = Gen.float (Range.exponentialFloatFrom 0 (-1E9) 1E9)
    genDouble = Gen.double (Range.exponentialFloatFrom 0 (-1E308) 1E308)
    genRange gen = do
      lb <- gen
      ub <- Gen.filter (lb <=) gen
      Gen.element
        [ Empty
        , NonEmpty (Closed lb) (Closed ub)
        , NonEmpty (Closed lb) (Open ub)
        , NonEmpty (Closed lb) Infinite
        , NonEmpty (Open lb) (Closed ub)
        , NonEmpty (Open lb) (Open ub)
        , NonEmpty (Open lb) Infinite
        , NonEmpty Infinite (Closed ub)
        , NonEmpty Infinite (Open ub)
        , NonEmpty Infinite Infinite ]
    genDay = do
      y <- toInteger <$> Gen.int (Range.constant 2000 2019)
      m <- Gen.int (Range.constant 1 12)
      d <- Gen.int (Range.constant 1 28)
      return $ fromGregorian y m d
    genDiffTime = secondsToDiffTime . toInteger <$>
      Gen.int (Range.constant 0 86401)
    genUTCTime = UTCTime <$> genDay <*> genDiffTime
    genTimeOfDay = do
      h <- Gen.int (Range.constant 0 23)
      m <- Gen.int (Range.constant 0 59)
      s <- MkFixed . toInteger <$> Gen.int (Range.constant 0 59)
      return $ TimeOfDay h m s
    genLocalTime = LocalTime <$> genDay <*> genTimeOfDay
    genTimeZone = Gen.element $ map (read @TimeZone)
      [ "UTC", "UT", "GMT", "EST", "EDT", "CST"
      , "CDT", "MST", "MDT", "PST", "PDT" ]
    genTimeWithZone = (,) <$> genTimeOfDay <*> genTimeZone

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=exampledb"

normalizeIntRange :: (Enum int, Ord int) => Range int -> Range int
normalizeIntRange = \case
  Empty -> Empty
  NonEmpty l u ->
    let
      l' = normalizeL l
      u' = normalizeU u
    in if emptyNormalized l' u' then Empty else NonEmpty l' u'
  where
    normalizeL = \case
      Open l -> Closed (succ l)
      normalized -> normalized
    normalizeU = \case
      Closed u -> Open (succ u)
      normalized -> normalized
    emptyNormalized (Closed l) (Open u) = l >= u
    emptyNormalized _ _ = False

normalizeTimeOfDay :: TimeOfDay -> TimeOfDay
normalizeTimeOfDay (TimeOfDay h m s) = TimeOfDay h m
  . fromRational @Pico
  . toRational @Micro
  . fromRational @Micro
  . toRational @Pico
  $ s

normalizeLocalTime :: LocalTime -> LocalTime
normalizeLocalTime (LocalTime d t) = LocalTime d (normalizeTimeOfDay t)

normalizeTimeWithZone :: (TimeOfDay, TimeZone) -> (TimeOfDay, TimeZone)
normalizeTimeWithZone (t, z) = (normalizeTimeOfDay t, z)

roundtrip
  :: (ToParam x ty, FromValue ty x, Show x, Eq x)
  => TypeExpression schemas ('NotNull ty)
  -> Gen x
  -> (x -> x)
  -> IO Bool
roundtrip ty gen norm = check . property $ do
  x <- forAll gen
  Just (Only y) <- lift . withConnection connectionString $
    firstRow =<< runQueryParams
      (values_ (parameter @1 ty `as` #fromOnly)) (Only x)
  norm x === y

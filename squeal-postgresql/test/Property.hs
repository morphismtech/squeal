{-# LANGUAGE
    DataKinds
  , GADTs
  , LambdaCase
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Main (main) where

import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Scientific (fromFloatDigits)
import Data.Fixed (Fixed(MkFixed), Micro, Pico)
import Data.String (IsString(fromString))
import Data.Time
import Squeal.PostgreSQL hiding (check, defaultMain, Group)
import Hedgehog hiding (Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Main
import qualified Hedgehog.Range as Range

main :: IO ()
main = Main.defaultMain [checkSequential roundtrips]

roundtrips :: Group
roundtrips = Group "roundtrips"
  [ roundtrip int2 genInt16
  , roundtrip int4 genInt32
  , roundtrip int8 genInt64
  , roundtrip bool Gen.bool
  , roundtrip numeric genScientific
  , roundtrip float4 genFloat
  , roundtrip float8 genDouble
  , roundtripOn normalizeTimeOfDay time genTimeOfDay
  -- , roundtrip timetz genTimeWithZone normalizeTimeWithZone
  , roundtripOn normalizeLocalTime timestamp genLocalTime
  , roundtrip timestamptz genUTCTime
  , roundtrip date genDay
  , roundtrip interval genDiffTime
  , roundtripOn normalizeIntRange int4range (genRange genInt32)
  , roundtripOn normalizeIntRange int8range (genRange genInt64)
  , roundtrip numrange (genRange genScientific)
  , roundtripOn (fmap normalizeLocalTime) tsrange (genRange genLocalTime)
  , roundtrip tstzrange (genRange genUTCTime)
  , roundtripOn normalizeIntRange daterange (genRange genDay)
  ]
  where
    genInt16 = Gen.int16 Range.exponentialBounded
    genInt32 = Gen.int32 Range.exponentialBounded
    genInt64 = Gen.int64 Range.exponentialBounded
    genScientific = fromFloatDigits <$> genFloat
    genPosFloat = Gen.float
      (Range.exponentialFloatFrom 1 minPosFloat maxPosFloat)
    genFloat = Gen.prune $ Gen.choice
      [ genPosFloat
      , negate <$> genPosFloat
      , Gen.element [0,1/0] ]
    genPosDouble = Gen.double
      (Range.exponentialFloatFrom 1 minPosFloat maxPosFloat)
    genDouble = Gen.prune $ Gen.choice
      [ genPosDouble
      , negate <$> genPosDouble
      , Gen.element [0,1/0] ]
    genRange gen = do
      lb <- gen
      ub <- Gen.filter (lb <) gen
      Gen.element
        [ Empty, singleton lb, whole
        , lb <=..<= ub , lb <=..< ub, lb <..<= ub, lb <..< ub
        , atLeast lb, moreThan lb, atMost ub, lessThan ub ]
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
    -- genTimeZone = Gen.element $ map (read @TimeZone)
    --   [ "UTC", "UT", "GMT", "EST", "EDT", "CST"
    --   , "CDT", "MST", "MDT", "PST", "PDT" ]
    -- genTimeWithZone = (,) <$> genTimeOfDay <*> genTimeZone

roundtrip
  :: (ToParam x ty, FromValue ty x, Show x, Eq x)
  => TypeExpression schemas ('NotNull ty)
  -> Gen x
  -> (PropertyName, Property)
roundtrip = roundtripOn id

roundtripOn
  :: (ToParam x ty, FromValue ty x, Show x, Eq x)
  => (x -> x)
  -> TypeExpression schemas ('NotNull ty)
  -> Gen x
  -> (PropertyName, Property)
roundtripOn norm ty gen = propertyWithName $ do
  x <- forAll gen
  Just (Only y) <- lift . withConnection connectionString $
    firstRow =<< runQueryParams
      (values_ (parameter @1 ty `as` #fromOnly)) (Only x)
  norm x === y
  where
    propertyWithName prop =
      (fromString (unpack (renderSQL ty)), property prop)

maxPosFloat :: RealFloat a => a
maxPosFloat = x
  where
    n = floatDigits x
    b = floatRadix x
    (_, u) = floatRange x
    x = encodeFloat (b^n - 1) (u - n)

minPosFloat :: RealFloat a => a
minPosFloat = x
  where
    n = floatDigits x
    b = floatRadix x
    (l, _) = floatRange x
    x = encodeFloat (b^n - 1) (l - n - 1)

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

-- normalizeTimeWithZone :: (TimeOfDay, TimeZone) -> (TimeOfDay, TimeZone)
-- normalizeTimeWithZone (t, z) = (normalizeTimeOfDay t, z)

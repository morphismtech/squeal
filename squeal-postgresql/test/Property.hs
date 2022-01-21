{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DerivingVia
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeOperators
  , UndecidableInstances
#-}

module Main (main) where

import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Int (Int16)
import Data.Scientific (fromFloatDigits)
import Data.Fixed (Fixed(MkFixed), Micro, Pico)
import Data.String (IsString(fromString))
import Data.Time
import Hedgehog hiding (Range)
import Main.Utf8
import Squeal.PostgreSQL hiding (check)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Main
import qualified Hedgehog.Range as Range

main :: IO ()
main = withUtf8 $ do
  withConnection connectionString $ define createDB
  Main.defaultMain [checkSequential roundtrips]
  withConnection connectionString $ define dropDB

roundtrips :: Group
roundtrips = Group "roundtrips"
  [ roundtrip int2 genInt16
  , roundtrip int4 genInt32
  , roundtrip int8 genInt64
  , roundtrip bool Gen.bool
  , roundtrip numeric genScientific
  , roundtrip float4 genFloat
  , roundtrip float8 genDouble
  , roundtripOn normalizeAscii text genStringAscii
  , roundtripOn normalizeUtf8 text genStringUnicode
  -- , roundtripOn normalizeUtf8 text genStringAll
  , roundtripOn normalizeTimeOfDay time genTimeOfDay
  -- , roundtrip timetz genTimeWithZone
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
  , roundtrip (typedef #schwarma) genSchwarma
  , roundtrip (vararray (typedef #schwarma)) genSchwarmaArray
  , roundtrip (typerow #tab) genRow
  , roundtrip (vararray (typetable #tab)) genRowArray
  , ("table insert", roundtripTable)
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
      , Gen.element [0,1/0,-1/0]
      ]
    genPosDouble = Gen.double
      (Range.exponentialFloatFrom 1 minPosFloat maxPosFloat)
    genDouble = Gen.prune $ Gen.choice
      [ genPosDouble
      , negate <$> genPosDouble
      , Gen.element [0,1/0,-1/0]
      ]
    genStringAscii = Gen.string (Range.linear 0 100) Gen.ascii
    -- genStringLatin1 = Gen.string (Range.linear 0 100) Gen.latin1
    genStringUnicode = Gen.string (Range.linear 0 100) Gen.unicode
    -- genStringAll = Gen.string (Range.linear 0 100) Gen.unicodeAll
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
    genDiffTime = do
      secs <- secondsToDiffTime . toInteger <$>
        Gen.int (Range.constant 0 86401)
      picos <- picosecondsToDiffTime . (* 1000000) . toInteger <$>
        Gen.int (Range.constant 0 (1000000 - 1))
      return $ secs + picos
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
    genSchwarma = Gen.enumBounded @_ @Schwarma
    genSchwarmaArray = VarArray <$> Gen.list (Range.constant 1 10) genSchwarma
    genRow = HaskRow
      <$> genInt16
      <*> Gen.enumBounded
      <*> Gen.bool
    genRowArray = VarArray <$> Gen.list (Range.constant 1 10) genRow

roundtrip
  :: forall x
   . ( ToPG DB x, FromPG x, Inline x
     , OidOf DB (PG x), PGTyped DB (PG x)
     , Show x, Eq x, NullPG x ~ 'NotNull (PG x) )
  => TypeExpression DB ('NotNull (PG x))
  -> Gen x
  -> (PropertyName, Property)
roundtrip = roundtripOn id

roundtripOn
  :: forall x
   . ( ToPG DB x, FromPG x, Inline x
     , OidOf DB (PG x), PGTyped DB (PG x)
     , Show x, Eq x, NullPG x ~ 'NotNull (PG x) )
  => (x -> x)
  -> TypeExpression DB ('NotNull (PG x))
  -> Gen x
  -> (PropertyName, Property)
roundtripOn norm ty gen = propertyWithName $ do
  x <- forAll gen
  Just (Only y) <- lift . withConnection connectionString $
    firstRow =<< runQueryParams
      (values_ (parameter @1 ty `as` #fromOnly)) (Only x)
  Just (Only z) <- lift . withConnection connectionString $
    firstRow =<< runQuery
      (values_ (inline @x @'NotNull x `as` #fromOnly))
  y === z
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
connectionString = "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"

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

normalizeAscii :: String -> String
normalizeAscii = (stripped =<<)
  where
    stripped = \case
      '\NUL' -> ""
      ch -> [ch]

normalizeUtf8 :: String -> String
normalizeUtf8 = (stripped =<<)
  where
    stripped = \case
      '\NUL' -> ""
      ch -> [ch]

data Schwarma = Chicken | Lamb | Beef
  deriving stock (Eq, Show, Bounded, Enum, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsPG, FromPG, ToPG db, Inline) via Enumerated Schwarma


type Schema = '[
  "schwarma" ::: 'Typedef (PG Schwarma),
  "tab" ::: 'Table ('[] :=> PGRow)]

type DB0 = Public '[]

type DB = Public Schema

createDB :: Definition DB0 DB
createDB =
  createTypeEnumFrom @Schwarma #schwarma >>>
  createTable #tab
    ( notNullable int2 `as` #foo :*
      notNullable (typedef #schwarma) `as` #bar :*
      notNullable bool `as` #baz
    )
    Nil

dropDB :: Definition DB DB0
dropDB = dropTable #tab >>> dropType #schwarma

type PGRow = '[
  "foo" ::: 'NoDef :=> 'NotNull 'PGint2,
  "bar" ::: 'NoDef :=> 'NotNull (PG Schwarma),
  "baz" ::: 'NoDef :=> 'NotNull 'PGbool]

data HaskRow = HaskRow {foo :: Int16, bar :: Schwarma, baz :: Bool}
  deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsPG, FromPG, Inline) via Composite HaskRow
deriving via Composite HaskRow
  instance db ~ DB => ToPG db HaskRow

insertTabParams :: Statement DB (VarArray [HaskRow]) ()
insertTabParams = Manipulation enc dec sql
  where
    enc = aParam
    dec = return ()
    sql = insertInto_ #tab unnestedParam
    unnestedParam = Select
      ( Set (#unnest & field #tab #foo) `as` #foo :*
        Set (#unnest & field #tab #bar) `as` #bar :*
        Set (#unnest & field #tab #baz) `as` #baz
      )
      ( from (unnest (param @1)) )

insertTabInlines :: [HaskRow] -> Statement DB () ()
insertTabInlines = \case
  [] -> error "needs at least 1 row"
  rw:rows -> manipulation $ insertInto_ #tab (inlineValues rw rows)

selectTab :: Statement DB () HaskRow
selectTab = query $ select Star (from (table #tab))

roundtripTable :: Property
roundtripTable = property $ do
  let
    genInt16 = Gen.int16 Range.exponentialBounded
    genRow = HaskRow
      <$> genInt16
      <*> Gen.enumBounded
      <*> Gen.bool
    genRowArray = Gen.list (Range.constant 1 100) genRow
  params <- forAll genRowArray
  inlines <- forAll genRowArray
  tabRows <- lift . withConnection connectionString $ ephemerally_ $ do
    executeParams_ insertTabParams (VarArray params)
    execute_ (insertTabInlines inlines)
    getRows =<< execute selectTab
  tabRows === params ++ inlines

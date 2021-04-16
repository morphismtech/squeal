{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DerivingVia
  , FlexibleContexts
  , GADTs
  , LambdaCase
  , OverloadedLabels
  , OverloadedStrings
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
  , UndecidableInstances
#-}

module Main (main) where

-- import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
-- import Data.List (intercalate)
import Data.String (IsString(fromString))
import Hedgehog hiding (Range)
import Main.Utf8
import Squeal.PostgreSQL hiding (check)
import Squeal.PostgreSQL.LTree
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Main
import qualified Hedgehog.Range as Range

main :: IO ()
main = withUtf8 $ do
  withConnection connectionString $ define createLTree
  Main.defaultMain [checkSequential roundtrips]

roundtrips :: Group
roundtrips = Group "roundtrips"
  [ roundtrip int2 genInt16
  , roundtrip ltree genLTree
  ]
  where
    genLTree = return $ UnsafeLTree "foo"
    -- genLTree = do
    --   n <- Gen.integral (Range.constant 0 10)
    --   labels <- replicateM n $
    --     Gen.string (Range.constant 1 50) Gen.alphaNum
    --   return $ UnsafeLTree $ fromString $ intercalate "." labels
    genInt16 = Gen.int16 Range.exponentialBounded
    -- genStringAscii = Gen.string (Range.linear 0 100) Gen.ascii
    -- genStringLatin1 = Gen.string (Range.linear 0 100) Gen.latin1
    -- genStringUnicode = Gen.string (Range.linear 0 100) Gen.unicode
    -- genStringAll = Gen.string (Range.linear 0 100) Gen.unicodeAll

roundtrip
  :: forall x
   . ( ToPG DB x, FromPG x, Inline x
     , OidOf DB (PG x), PGTyped DB (PG x)
     , Show x, Eq x )
  => TypeExpression DB ('NotNull (PG x))
  -> Gen x
  -> (PropertyName, Property)
roundtrip = roundtripOn id

roundtripOn
  :: forall x
   . ( ToPG DB x, FromPG x, Inline x
     , OidOf DB (PG x), PGTyped DB (PG x)
     , Show x, Eq x )
  => (x -> x)
  -> TypeExpression DB ('NotNull (PG x))
  -> Gen x
  -> (PropertyName, Property)
roundtripOn norm ty gen = propertyWithName $ do
  x <- forAll gen
  -- Just (Only y) <- lift . withConnection connectionString $
  --   firstRow =<< runQueryParams
  --     (values_ (parameter @1 ty `as` #fromOnly)) (Only x)
  Just (Only z) <- lift . withConnection connectionString $
    firstRow =<< runQuery
      (values_ (inline @x @'NotNull x `as` #fromOnly))
  -- y === z
  -- norm x === y
  norm x === z
  where
    propertyWithName prop =
      (fromString (unpack (renderSQL ty)), property prop)

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"

type DB = '["public" ::: '[]]

-- normalizeAscii :: String -> String
-- normalizeAscii = (stripped =<<)
--   where
--     stripped = \case
--       '\NUL' -> ""
--       ch -> [ch]

-- normalizeUtf8 :: String -> String
-- normalizeUtf8 = (stripped =<<)
--   where
--     stripped = \case
--       '\NUL' -> ""
--       ch -> [ch]

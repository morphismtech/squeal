{-# LANGUAGE
    DataKinds
  , OverloadedLabels
  , OverloadedStrings
#-}

module Squeel.PostgreSQL.QuerySpec where

import Data.Boolean
import Data.Vinyl
import Test.Hspec

import Squeel.PostgreSQL.Query
import Squeel.PostgreSQL.Type

spec :: Spec
spec = do
  it "correctly renders a simple SELECT query" $ do
    renderQuery (select (sumAndCol1 `from` (table1 `where_` true)))
    `shouldBe`
    "SELECT col1+col2 AS sum, col1 AS col1 FROM table1 WHERE TRUE"
  it "combines WHEREs using AND" $ do
    renderQuery (select (sumAndCol1 `from` (table1 `where_` true `where_` false)))
    `shouldBe`
    "SELECT col1+col2 AS sum, col1 AS col1 FROM table1 WHERE TRUE AND FALSE"

type Columns =
  '[ '("col1", 'PGInt4)
   , '("col2", 'PGInt4)
   ]

sumAndCol1 :: Projection '[] Columns
  '[ '("sum", 'PGInt4)
   , '("col1", 'PGInt4)
   ]
sumAndCol1 = project ((#col1 + #col2) `As` #sum :& #col1 :& RNil)

table1 :: Relation '[] '[ '("table1", Columns)] Columns
table1 = #table1

{-# LANGUAGE
    DataKinds
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
#-}

module Squeel.PostgreSQL.QuerySpec where

import Data.Boolean
import Data.Function
import Data.Proxy
import Data.Vinyl
import Test.Hspec

import Squeel.PostgreSQL.Query
import Squeel.PostgreSQL.Type

spec :: Spec
spec = do
  it "correctly renders a simple SELECT query" $ do
    renderQuery (select (sumAndCol1 `from` (table1 & where_ true)))
    `shouldBe`
    "SELECT col1+col2 AS sum, col1 AS col1 FROM table1 WHERE TRUE;"
  it "combines WHEREs using AND" $ do
    renderQuery (select (sumAndCol1 `from` (table1 & where_ true & where_ false)))
    `shouldBe`
    "SELECT col1+col2 AS sum, col1 AS col1 FROM table1 WHERE TRUE AND FALSE;"
  it "performs subSELECTs" $ do
    renderQuery (select (star `from` subselect (sumAndCol1 `from` table1)))
    `shouldBe`
    "SELECT * FROM SELECT col1+col2 AS sum, col1 AS col1 FROM table1;"
  it "does LIMIT clauses" $ do
    renderQuery (select (star `from` (table1 & limit 1)))
    `shouldBe`
    "SELECT * FROM table1 LIMIT 1;"
  it "should use the minimum of given LIMITs" $ do
    renderQuery (select (star `from` (table1 & limit 1 & limit 2)))
    `shouldBe`
    "SELECT * FROM table1 LIMIT CASE WHEN 1<=2 THEN 1 ELSE 2 END;"
  it "should render parameters using $ signs" $ do
    renderQuery (select (star `from` (parameterizedTable1 & limit param1)))
    `shouldBe`
    "SELECT * FROM table1 LIMIT $1;"
  it "does OFFSET clauses" $ do
    renderQuery (select (star `from` (table1 & offset 1)))
    `shouldBe`
    "SELECT * FROM table1 OFFSET 1;"
  it "should use the sum of given OFFSETs" $ do
    renderQuery (select (star `from` (table1 & offset 1 & offset 2)))
    `shouldBe`
    "SELECT * FROM table1 OFFSET 1+2;"
  it "correctly render simple INSERTs" $ do
    renderQuery (insertInto @Tables @Columns (Proxy @"table1")
      ((2 `As` #col1 :& 4 `As` #col2 :& RNil)))
    `shouldBe`
    "INSERT INTO table1 (col1, col2) VALUES (2, 4);"
  it "should be safe against SQL injection in literal text" $ do
    renderQuery (select (literalText `from` table1))
    `shouldBe`
    "SELECT E''';DROP TABLE table1;' AS literal FROM table1;"

type Columns = '[ '("col1", 'PGInt4), '("col2", 'PGInt4)]
type Tables = '[ '("table1", Columns)]

sumAndCol1 :: Projection '[] Columns '[ '("sum", 'PGInt4), '("col1", 'PGInt4)]
sumAndCol1 = project ((#col1 + #col2) `As` #sum :& #col1 :& RNil)

literalText :: Projection '[] Columns '[ '("literal", 'PGText)]
literalText = project ("\';DROP TABLE table1;" `As` #literal :& RNil)

table1 :: Relation '[] Tables Columns
table1 = #table1

parameterizedTable1 :: Relation '[ 'PGInt8] Tables Columns
parameterizedTable1 = #table1

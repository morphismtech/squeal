{-# LANGUAGE
    DataKinds
  , OverloadedLabels
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeOperators
#-}

module Squeel.PostgreSQL.QuerySpec where

import Data.Boolean
import Data.ByteString
import Data.Function
import Data.Vinyl
import Test.Hspec

import Squeel.PostgreSQL.Query
import Squeel.PostgreSQL.Schema

spec :: Spec
spec = do
  it "correctly renders a simple SELECT query" $ do
    select (sumAndCol1 `from` (table1 & where_ true))
    `shouldRenderAs`
    "SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 WHERE TRUE;"
  it "combines WHEREs using AND" $ do
    select (sumAndCol1 `from` (table1 & where_ true & where_ false))
    `shouldRenderAs`
    "SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 WHERE (TRUE AND FALSE);"
  it "performs sub SELECTs" $ do
    select (star `from` subselect ((sumAndCol1 `from` table1) `As` (#sub :: Alias "sub")))
    `shouldRenderAs`
    "SELECT * FROM SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 AS sub;"
  it "does LIMIT clauses" $ do
    select (star `from` (table1 & limit 1))
    `shouldRenderAs`
    "SELECT * FROM table1 LIMIT 1;"
  it "should use the minimum of given LIMITs" $ do
    select (star `from` (table1 & limit 1 & limit 2))
    `shouldRenderAs`
    "SELECT * FROM table1 LIMIT CASE WHEN (1 <= 2) THEN 1 ELSE 2 END;"
  it "should render parameters using $ signs" $ do
    select (star `from` (parameterizedTable1 & limit param1))
    `shouldRenderAs`
    "SELECT * FROM table1 LIMIT $1;"
  it "does OFFSET clauses" $ do
    select (star `from` (table1 & offset 1))
    `shouldRenderAs`
    "SELECT * FROM table1 OFFSET 1;"
  it "should use the sum of given OFFSETs" $ do
    select (star `from` (table1 & offset 1 & offset 2))
    `shouldRenderAs`
    "SELECT * FROM table1 OFFSET (1 + 2);"
  it "correctly render simple INSERTs" $ do
    insertInto @Tables @Columns
      (#table1 :: Alias "table1")
      (2 `As` #col1 :& 4 `As` #col2 :& RNil)
    `shouldRenderAs`
    "INSERT INTO table1 (col1, col2) VALUES (2, 4);"
  it "should be safe against SQL injection in literal text" $ do
    insertInto @StudentsTable @StudentsColumns
      (#students :: Alias "students")
      ("Robert'); DROP TABLE students;" `As` #name :& RNil)
    `shouldRenderAs`
    "INSERT INTO students (name) VALUES (E'Robert''); DROP TABLE students;');"
  -- describe "JOINs" $ do
  --   it "should render CROSS JOINs" $
  --     pending
  --   it "should render INNER JOINs" $ do
  --     select (vals `from` joinTable) `shouldRenderAs`
  --       "((orders\
  --       \ INNER JOIN customers ON orders.customerID = customers.customerID)\
  --       \ INNER JOIN shippers ON orders.shipperID = shippers.shipperID)"
-- #orders
--   & innerJoin #customers
--     (#orders .&. #customerID ==* #customers .&. #customerID)
--   & innerJoin #shippers
--     (#orders .&. #shipperID ==* #shippers .&. #shipperID)
--   `shouldRenderAs`
--   "((orders\
--   \ INNER JOIN customers ON orders.customerID = customers.customerID)\
--   \ INNER JOIN shippers ON orders.shipperID = shippers.shipperID)"

type Columns = '[ "col1" ::: 'PGInt4, "col2" ::: 'PGInt4]
type Tables = '[ "table1" ::: Columns ]

sumAndCol1 :: Projection '[] Tables
  '["sum" ::: 'PGInt4, "col1" ::: 'PGInt4]
sumAndCol1 = project $
  (#col1 + #col2) `As` #sum
  :& #col1 :& RNil

literalText :: Projection '[] Tables '["literal" ::: 'PGText]
literalText = project $ "\';DROP TABLE table1;" `As` #literal :& RNil

table1 :: Tabulation '[] Tables Tables
table1 = #table1

parameterizedTable1 :: Tabulation '[ 'PGInt8] Tables Tables
parameterizedTable1 = #table1

shouldRenderAs
  :: Query params schema1 schema2 columns
  -> ByteString
  -> Expectation
shouldRenderAs query str = renderQuery query `shouldBe` str

type StudentsColumns = '["name" ::: 'PGText]
type StudentsTable = '["students" ::: StudentsColumns]

type OrderColumns =
  [ "orderID" ::: 'PGInt4, "orderVal" ::: 'PGText ]
type CustomerColumns =
  [ "customerID" ::: 'PGInt4, "customerVal" ::: 'PGFloat4 ]
type ShipperColumns =
  [ "shipperID" ::: 'PGInt4, "shipperVal" ::: 'PGBool ]
type JoinTables =
  '[ "order" ::: OrderColumns
   , "customer" ::: CustomerColumns
   , "shipper" ::: ShipperColumns
   ]
type ValueColumns =
  [ "orderVal" ::: 'PGText
  , "customerVal" ::: 'PGFloat4
  , "shipperVal" ::: 'PGBool
  ]
-- vals :: Projection '[] JoinTables ValueColumns
-- vals = project $
--   #orders .&. #orderVal `As` #orderVal
--   :& #customers .&. #customerVal `As` #customerVal
--   :& #shippers .&. #shippersVal `As` #shippersVal :& RNil
-- joinTable :: Tabulation '[] JoinTables JoinTables
-- joinTable = tabulation $
--   #orders
--   & innerJoin #customers
--     (#orders .&. #customerID ==* #customers .&. #customerID)
--   & innerJoin #shippers
--     (#orders .&. #shipperID ==* #shippers .&. #shipperID)

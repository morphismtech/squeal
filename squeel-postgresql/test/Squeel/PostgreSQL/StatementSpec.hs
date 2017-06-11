{-# LANGUAGE
    DataKinds
  , MagicHash
  , OverloadedLabels
  , OverloadedStrings
  , TypeOperators
#-}

module Squeel.PostgreSQL.StatementSpec where

import Data.Boolean
import Data.Function
import Data.Vinyl
import GHC.Exts
import Test.Hspec

import Squeel.PostgreSQL.Statement
import Squeel.PostgreSQL.Schema

spec :: Spec
spec = do
  let
    query `shouldRenderAs` str = renderStatement query `shouldBe` str
  it "correctly renders a simple SELECT query" $ do
    let
      statement :: Statement '[] Tables Tables SumAndCol1
      statement = select $
        ((#col1 + #col2) `As` #sum :& #col1 :& RNil)
          `from` (#table1 & where_ true)
    statement `shouldRenderAs`
      "SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 WHERE TRUE;"
  it "combines WHEREs using AND" $ do
    let
      statement :: Statement '[] Tables Tables SumAndCol1
      statement = select $
        ((#col1 + #col2) `As` #sum :& #col1 :& RNil)
          `from` (#table1 & where_ true & where_ false)
    statement `shouldRenderAs`
      "SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 WHERE (TRUE AND FALSE);"
  it "performs sub SELECTs" $ do
    let
      selection = ((#col1 + #col2) `As` #sum :& #col1 :& RNil) `from` #table1
      statement :: Statement '[] Tables Tables SumAndCol1
      statement = select $ starFrom (subselect (selection `As` #sub))
    statement `shouldRenderAs`
      "SELECT * FROM SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 AS sub;"
  it "does LIMIT clauses" $ do
    let
      statement :: Statement '[] Tables Tables Columns
      statement = select $ starFrom (#table1 & limit 1)
    statement `shouldRenderAs` "SELECT * FROM table1 LIMIT 1;"
  it "should use the minimum of given LIMITs" $ do
    let
      statement :: Statement '[] Tables Tables Columns
      statement = select $ starFrom (#table1 & limit 1 & limit 2)
    statement `shouldRenderAs`
      "SELECT * FROM table1 LIMIT CASE WHEN (1 <= 2) THEN 1 ELSE 2 END;"
  it "should render parameters using $ signs" $ do
    let
      statement :: Statement '[ 'PGInt8] Tables Tables Columns
      statement = select $ starFrom (#table1 & limit param1)
    statement `shouldRenderAs` "SELECT * FROM table1 LIMIT $1;"
  it "does OFFSET clauses" $ do
    let
      statement :: Statement '[] Tables Tables Columns
      statement = select $ starFrom (#table1 & offset 1)
    statement `shouldRenderAs` "SELECT * FROM table1 OFFSET 1;"
  it "should use the sum of given OFFSETs" $ do
    let
      statement :: Statement '[] Tables Tables Columns
      statement =  select $ starFrom (#table1 & offset 1 & offset 2)
    statement `shouldRenderAs` "SELECT * FROM table1 OFFSET (1 + 2);"
  it "correctly render simple INSERTs" $ do
    let
      statement :: Statement '[] Tables Tables '[]
      statement = insertInto #table1 $ 2 `As` #col1 :& 4 `As` #col2 :& RNil
    statement `shouldRenderAs`
      "INSERT INTO table1 (col1, col2) VALUES (2, 4);"
  it "should be safe against SQL injection in literal text" $ do
    let
      statement :: Statement '[] StudentsTable StudentsTable '[]
      statement = insertInto #students $
        "Robert'); DROP TABLE students;" `As` #name :& RNil
    statement `shouldRenderAs`
      "INSERT INTO students (name) VALUES (E'Robert''); DROP TABLE students;');"
  describe "JOINs" $ do
    let
      vals =
        #orders .&. #orderVal `As` #orderVal
        :& #customers .&. #customerVal `As` #customerVal
        :& #shippers .&. #shipperVal `As` #shipperVal :& RNil
    it "should render CROSS JOINs" $ do
      let
        statement :: Statement '[] JoinTables JoinTables ValueColumns
        statement = select $ vals `from`
          (tables (#orders & crossJoin #customers & crossJoin #shippers))
      statement `shouldRenderAs`
        "SELECT\
        \ orders.orderVal AS orderVal,\
        \ customers.customerVal AS customerVal,\
        \ shippers.shipperVal AS shipperVal\
        \ FROM orders\
        \ CROSS JOIN customers\
        \ CROSS JOIN shippers;"
    it "should render INNER JOINs" $ do
      let
        innerJoins :: TableExpression '[] JoinTables JoinTables
        innerJoins = tables $
          #orders
          & innerJoin #customers
            (#orders .&. #customerID ==* #customers .&. #customerID)
          & innerJoin #shippers
            (#orders .&. #shipperID ==* #shippers .&. #shipperID)
        selection :: Statement '[] JoinTables JoinTables ValueColumns
        selection =  select $ vals `from` innerJoins
      selection `shouldRenderAs`
        "SELECT\
        \ orders.orderVal AS orderVal,\
        \ customers.customerVal AS customerVal,\
        \ shippers.shipperVal AS shipperVal\
        \ FROM orders\
        \ INNER JOIN customers\
        \ ON (orders.customerID = customers.customerID)\
        \ INNER JOIN shippers\
        \ ON (orders.shipperID = shippers.shipperID);"
  it "should render simple CREATE TABLE statements" $ do
    let
      statement :: Statement '[] '[] Tables '[]
      statement = createTable #table1 (proxy# :: Proxy# Columns)
    statement `shouldRenderAs`
      "CREATE TABLE table1 (col1 int4, col2 int4);"
  it "should render DROP TABLE statements" $ do
    let
      statement :: Statement '[] Tables '[] '[]
      statement = dropTable #table1
    statement `shouldRenderAs`
      "DROP TABLE table1;"

type Columns = '[ "col1" ::: 'PGInt4, "col2" ::: 'PGInt4]
type Tables = '[ "table1" ::: Columns ]
type SumAndCol1 = '[ "sum" ::: 'PGInt4, "col1" ::: 'PGInt4]
type StudentsColumns = '["name" ::: 'PGText]
type StudentsTable = '["students" ::: StudentsColumns]
type OrderColumns =
  [ "orderID"    ::: 'PGInt4
  , "orderVal"   ::: 'PGText
  , "customerID" ::: 'PGInt4
  , "shipperID"  ::: 'PGInt4
  ]
type CustomerColumns =
  [ "customerID" ::: 'PGInt4, "customerVal" ::: 'PGFloat4 ]
type ShipperColumns =
  [ "shipperID" ::: 'PGInt4, "shipperVal" ::: 'PGBool ]
type JoinTables =
  [ "shippers"  ::: ShipperColumns
  , "customers" ::: CustomerColumns
  , "orders"    ::: OrderColumns
  ]
type ValueColumns =
  [ "orderVal"    ::: 'PGText
  , "customerVal" ::: 'PGFloat4
  , "shipperVal"  ::: 'PGBool
  ]

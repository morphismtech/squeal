{-# LANGUAGE
    DataKinds
  , MagicHash
  , OverloadedLabels
  , OverloadedStrings
  , TypeOperators
#-}

module Squeel.PostgreSQL.StatementSpec where

-- import Data.Boolean
import Data.Function
import Generics.SOP hiding (from)
import Test.Hspec

import Squeel.PostgreSQL.Statement
import Squeel.PostgreSQL.Schema

spec :: Spec
spec = do
  let
    query `shouldRenderAs` str = renderStatement query `shouldBe` str
  it "correctly renders a simple SELECT query" $ do
    let
      statement :: Statement '[] SumAndCol1 Tables Tables
      statement = select $
        ((#col1 + #col2) `As` #sum :* #col1 :* Nil)
          `from` (#table1 & where_ (#col1 >* #col2))
    statement `shouldRenderAs`
      "SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 AS table1 WHERE (col1 > col2);"
  it "combines WHEREs using AND" $ do
    let
      statement :: Statement '[] SumAndCol1 Tables Tables
      statement = select $
        ((#col1 + #col2) `As` #sum :* #col1 :* Nil)
          `from` (#table1 & where_ true & where_ false)
    statement `shouldRenderAs`
      "SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 AS table1 WHERE (TRUE AND FALSE);"
  it "performs sub SELECTs" $ do
    let
      selection = ((#col1 + #col2) `As` #sum :* #col1 :* Nil) `from` #table1
      statement :: Statement '[] SumAndCol1 Tables Tables
      statement = select $ starFrom (subselect (selection `As` #sub))
    statement `shouldRenderAs`
      "SELECT * FROM SELECT (col1 + col2) AS sum, col1 AS col1 FROM table1 AS table1 AS sub;"
  it "does LIMIT clauses" $ do
    let
      statement :: Statement '[] Columns Tables Tables
      statement = select $ starFrom (#table1 & limit 1)
    statement `shouldRenderAs` "SELECT * FROM table1 AS table1 LIMIT 1;"
  it "should use the minimum of given LIMITs" $ do
    let
      statement :: Statement '[] Columns Tables Tables
      statement = select $ starFrom (#table1 & limit 1 & limit 2)
    statement `shouldRenderAs`
      "SELECT * FROM table1 AS table1 LIMIT LEAST(1, 2);"
  it "should render parameters using $ signs" $ do
    let
      statement :: Statement '[ 'Required ('NotNull 'PGInt8)] Columns Tables Tables
      statement = select $ starFrom (#table1 & limit param1)
    statement `shouldRenderAs` "SELECT * FROM table1 AS table1 LIMIT $1;"
  it "does OFFSET clauses" $ do
    let
      statement :: Statement '[] Columns Tables Tables
      statement = select $ starFrom (#table1 & offset 1)
    statement `shouldRenderAs` "SELECT * FROM table1 AS table1 OFFSET 1;"
  it "should use the sum of given OFFSETs" $ do
    let
      statement :: Statement '[] Columns Tables Tables
      statement =  select $ starFrom (#table1 & offset 1 & offset 2)
    statement `shouldRenderAs` "SELECT * FROM table1 AS table1 OFFSET (1 + 2);"
  it "correctly render simple INSERTs" $ do
    let
      statement :: Statement '[] '[] Tables Tables
      statement = insertInto #table1 $ 2 `As` #col1 :* 4 `As` #col2 :* Nil
    statement `shouldRenderAs`
      "INSERT INTO table1 (col1, col2) VALUES (2, 4);"
  it "correctly render simple UPDATEs" $ do
    let
      statement :: Statement '[] '[] Tables Tables
      statement = update #table1 (set 2 `As` #col1 :* same `As` #col2 :* Nil) (#col1 /=* #col2)
    statement `shouldRenderAs`
      "UPDATE table1 SET col1 = 2 WHERE (col1 <> col2);"
  it "correctly render upsert INSERTs" $ do
    let
      statement :: Statement '[] '[] Tables Tables
      statement = upsertInto #table1
        (2 `As` #col1 :* 4 `As` #col2 :* Nil)
        (set 2 `As` #col1 :* same `As` #col2 :* Nil)
        (#col1 /=* #col2)
    statement `shouldRenderAs`
      "INSERT INTO table1 (col1, col2) VALUES (2, 4) ON CONFLICT UPDATE table1 SET col1 = 2 WHERE (col1 <> col2);"
  it "correctly renders DELETEs" $ do
    let
      statement :: Statement '[] '[] Tables Tables
      statement = deleteFrom #table1 (#col1 ==* #col2)
    statement `shouldRenderAs`
      "DELETE FROM table1 WHERE (col1 = col2);"
  it "should be safe against SQL injection in literal text" $ do
    let
      statement :: Statement '[] '[] StudentsTable StudentsTable
      statement = insertInto #students $
        "Robert'); DROP TABLE students;" `As` #name :* Nil
    statement `shouldRenderAs`
      "INSERT INTO students (name) VALUES (E'Robert''); DROP TABLE students;');"
  describe "JOINs" $ do
    let
      vals =
        (#orders &. #orderVal) `As` #orderVal
        :* (#customers &. #customerVal) `As` #customerVal
        :* (#shippers &. #shipperVal) `As` #shipperVal :* Nil
    it "should render CROSS JOINs" $ do
      let
        statement :: Statement '[] ValueColumns JoinTables JoinTables
        statement = select $ vals `from`
          (join (#orders & Cross #customers & Cross #shippers))
      statement `shouldRenderAs`
        "SELECT\
        \ orders.orderVal AS orderVal,\
        \ customers.customerVal AS customerVal,\
        \ shippers.shipperVal AS shipperVal\
        \ FROM orders AS orders\
        \ CROSS JOIN customers AS customers\
        \ CROSS JOIN shippers AS shippers;"
    it "should render INNER JOINs" $ do
      let
        innerJoins :: TableExpression '[] JoinTables JoinTables
        innerJoins = join $
          #orders
          & Inner #customers
            ((#orders &. #customerID) ==* (#customers &. #customerID))
          & Inner #shippers
            ((#orders &. #shipperID) ==* (#shippers &. #shipperID))
        selection :: Statement '[] ValueColumns JoinTables JoinTables
        selection =  select $ vals `from` innerJoins
      selection `shouldRenderAs`
        "SELECT\
        \ orders.orderVal AS orderVal,\
        \ customers.customerVal AS customerVal,\
        \ shippers.shipperVal AS shipperVal\
        \ FROM orders AS orders\
        \ INNER JOIN customers AS customers\
        \ ON (orders.customerID = customers.customerID)\
        \ INNER JOIN shippers AS shippers\
        \ ON (orders.shipperID = shippers.shipperID);"
    it "should render self JOINs" $ do
      let
        statement :: Statement '[] OrderColumns JoinTables JoinTables
        statement = select $ #orders1 `dotStarFrom`
          (join (Table (#orders `As` #orders1)
            & Cross (#orders `As` #orders2)))
      statement `shouldRenderAs`
        "SELECT orders1.*\
        \ FROM orders AS orders1\
        \ CROSS JOIN orders AS orders2;"
  it "should render CREATE TABLE statements" $ do
    createTable #table1
      (  (int4 & notNull) `As` #col1
      :* (int4 & notNull) `As` #col2
      :* Nil)
      `shouldRenderAs`
      "CREATE TABLE table1 (col1 int4 NOT NULL, col2 int4 NOT NULL);"
    createTable #table2
      (  serial `As` #col1
      :* text `As` #col2
      :* (int8 & notNull & default_ 8) `As` #col3
      :* Nil)
      `shouldRenderAs`
      "CREATE TABLE table2 (col1 serial, col2 text, col3 int8 NOT NULL DEFAULT 8);"
  it "should render DROP TABLE statements" $ do
    let
      statement :: Statement '[] '[] Tables '[]
      statement = dropTable #table1
    statement `shouldRenderAs` "DROP TABLE table1;"

type Columns =
  '[ "col1" ::: 'Required ('NotNull 'PGInt4)
   , "col2" ::: 'Required ('NotNull 'PGInt4)
   ]
type Tables = '[ "table1" ::: Columns ]
type SumAndCol1 = '[ "sum" ::: 'Required ('NotNull 'PGInt4), "col1" ::: 'Required ('NotNull 'PGInt4)]
type StudentsColumns = '["name" ::: 'Required ('NotNull 'PGText)]
type StudentsTable = '["students" ::: StudentsColumns]
type OrderColumns =
  '[ "orderID"    ::: 'Required ('NotNull 'PGInt4)
   , "orderVal"   ::: 'Required ('NotNull 'PGText)
   , "customerID" ::: 'Required ('NotNull 'PGInt4)
   , "shipperID"  ::: 'Required ('NotNull 'PGInt4)
   ]
type CustomerColumns =
  '[ "customerID" ::: 'Required ('NotNull 'PGInt4)
   , "customerVal" ::: 'Required ('NotNull 'PGFloat4)
   ]
type ShipperColumns =
  '[ "shipperID" ::: 'Required ('NotNull 'PGInt4)
   , "shipperVal" ::: 'Required ('NotNull 'PGBool)
   ]
type JoinTables =
  '[ "shippers"  ::: ShipperColumns
   , "customers" ::: CustomerColumns
   , "orders"    ::: OrderColumns
   ]
type ValueColumns =
  '[ "orderVal"    ::: 'Required ('NotNull 'PGText)
   , "customerVal" ::: 'Required ('NotNull 'PGFloat4)
   , "shipperVal"  ::: 'Required ('NotNull 'PGBool)
   ]

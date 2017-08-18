{-# LANGUAGE
    DataKinds
  , MagicHash
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.StatementSpec where

import Control.Category ((>>>))
import Data.Function
import Generics.SOP hiding (from)
import Test.Hspec

import Squeal.PostgreSQL.Statement
import Squeal.PostgreSQL.Schema

spec :: Spec
spec = do
  let
    qry `queryRenders` str = queryStatement qry `manipulationRenders` str
    definition `definitionRenders` str =
      renderDefinition definition `shouldBe` str
    manipulation `manipulationRenders` str =
      renderManipulation manipulation `shouldBe` str
  it "correctly renders a simple SELECT query" $ do
    let
      statement :: Query Tables '[] SumAndCol1
      statement =
        select ((#col1 + #col2) `As` #sum :* #col1 `As` #col1 :* Nil)
        (from (Table (#table1 `As` #table1)) & where_ (#col1 >* #col2))
    statement `queryRenders`
      "SELECT (col1 + col2) AS sum, col1 AS col1\
      \ FROM table1 AS table1 WHERE (col1 > col2);"
  it "combines WHEREs using AND" $ do
    let
      statement :: Query Tables '[] SumAndCol1
      statement =
        select ((#col1 + #col2) `As` #sum :* #col1 `As` #col1 :* Nil)
        (from (Table (#table1 `As` #table1)) & where_ true & where_ false)
    statement `queryRenders`
      "SELECT (col1 + col2) AS sum, col1 AS col1\
      \ FROM table1 AS table1 WHERE (TRUE AND FALSE);"
  it "performs sub SELECTs" $ do
    let
      statement :: Query Tables '[] SumAndCol1
      statement =
        selectStar (from (Subquery
          (select ((#col1 + #col2) `As` #sum :* #col1 `As` #col1 :* Nil)
            (from (Table (#table1 `As` #table1))) `As` #sub)))
    statement `queryRenders`
      "SELECT * FROM\
      \ (SELECT (col1 + col2) AS sum, col1 AS col1\
      \ FROM table1 AS table1) AS sub;"
  it "does LIMIT clauses" $ do
    let
      statement :: Query Tables '[] Columns
      statement = selectStar (from (Table (#table1 `As` #table1)) & limit 1)
    statement `queryRenders` "SELECT * FROM table1 AS table1 LIMIT 1;"
  it "should use the minimum of given LIMITs" $ do
    let
      statement :: Query Tables '[] Columns
      statement =
        selectStar (from (Table (#table1 `As` #table1)) & limit 1 & limit 2)
    statement `queryRenders` "SELECT * FROM table1 AS table1 LIMIT 1;"
  it "should render parameters using $ signs" $ do
    let
      statement :: Query Tables '[ 'Required ('NotNull 'PGbool)] Columns
      statement = selectStar
        (from (Table (#table1 `As` #table1)) & where_ (param (Proxy @1)))
    statement `queryRenders`
      "SELECT * FROM table1 AS table1 WHERE ($1 :: bool);"
  it "does OFFSET clauses" $ do
    let
      statement :: Query Tables '[] Columns
      statement =
        selectStar (from (Table (#table1 `As` #table1)) & offset 1)
    statement `queryRenders` "SELECT * FROM table1 AS table1 OFFSET 1;"
  it "should use the sum of given OFFSETs" $ do
    let
      statement :: Query Tables '[] Columns
      statement =  selectStar
        (from (Table (#table1 `As` #table1)) & offset 1 & offset 2)
    statement `queryRenders` "SELECT * FROM table1 AS table1 OFFSET 3;"
  it "should render GROUP BY and HAVING clauses" $ do
    let
      statement :: Query Tables '[] SumAndCol1
      statement =
        select (sum_ #col2 `As` #sum :* #col1 `As` #col1 :* Nil)
        ( from (Table (#table1 `As` #table1))
          & group (By #col1 :* Nil) 
          & having (#col1 + sum_ #col2 >* 1) )
    statement `queryRenders`
      "SELECT sum(col2) AS sum, col1 AS col1\
      \ FROM table1 AS table1\
      \ GROUP BY col1\
      \ HAVING ((col1 + sum(col2)) > 1);"
  it "correctly renders simple INSERTs" $ do
    let
      statement :: Manipulation Tables '[] '[]
      statement =
        insertInto #table1 (Values (2 `As` #col1 :* 4 `As` #col2 :* Nil) [])
          Conflict (Returning Nil)
    statement `manipulationRenders`
      "INSERT INTO table1 (col1, col2) VALUES (2, 4);"
  it "correctly renders returning INSERTs" $ do
    let
      statement :: Manipulation Tables '[] SumAndCol1
      statement =
        insertInto #table1 (Values (2 `As` #col1 :* 4 `As` #col2 :* Nil) [])
          Conflict
          (Returning $ (#col1 + #col2) `As` #sum :* #col1 `As` #col1 :* Nil)
    statement `manipulationRenders`
      "INSERT INTO table1 (col1, col2) VALUES (2, 4)\
      \ RETURNING (col1 + col2) AS sum, col1 AS col1;"
  it "correctly renders simple UPDATEs" $ do
    let
      statement :: Manipulation Tables '[] '[]
      statement =
        update #table1 (Set 2 `As` #col1 :* Same `As` #col2 :* Nil)
          (#col1 /=* #col2) (Returning Nil)
    statement `manipulationRenders`
      "UPDATE table1 SET col1 = 2\
      \ WHERE (col1 <> col2);"
  it "correctly renders returning UPDATEs" $ do
    let
      statement :: Manipulation Tables '[] SumAndCol1
      statement =
        update #table1 (Set 2 `As` #col1 :* Same `As` #col2 :* Nil)
          (#col1 /=* #col2)
          (Returning $ (#col1 + #col2) `As` #sum :* #col1 `As` #col1 :* Nil)
    statement `manipulationRenders`
      "UPDATE table1 SET col1 = 2\
      \ WHERE (col1 <> col2)\
      \ RETURNING (col1 + col2) AS sum, col1 AS col1;"
  it "correctly renders upsert INSERTs" $ do
    let
      statement :: Manipulation Tables '[] '[]
      statement =
        insertInto #table1 (Values (2 `As` #col1 :* 4 `As` #col2 :* Nil) [])
          (OnConflictDoUpdate
            (Set 2 `As` #col1 :* Same `As` #col2 :* Nil) Nothing)
          (Returning Nil)
    statement `manipulationRenders`
      "INSERT INTO table1 (col1, col2) VALUES (2, 4)\
      \ ON CONFLICT DO UPDATE\
      \ SET col1 = 2;"
  it "correctly renders returning upsert INSERTs" $ do
    let
      statement :: Manipulation Tables '[] SumAndCol1
      statement =
        insertInto #table1 (Values (2 `As` #col1 :* 4 `As` #col2 :* Nil) [])
          (OnConflictDoUpdate
            (Set 2 `As` #col1 :* Same `As` #col2 :* Nil)
            (Just (#col1 /=* #col2)))
          (Returning $ (#col1 + #col2) `As` #sum :* #col1 `As` #col1 :* Nil)
    statement `manipulationRenders`
      "INSERT INTO table1 (col1, col2) VALUES (2, 4)\
      \ ON CONFLICT DO UPDATE\
      \ SET col1 = 2\
      \ WHERE (col1 <> col2)\
      \ RETURNING (col1 + col2) AS sum, col1 AS col1;"
  it "correctly renders DELETEs" $ do
    let
      statement :: Manipulation Tables '[] '[]
      statement = deleteFrom #table1 (#col1 ==* #col2)
    statement `manipulationRenders`
      "DELETE FROM table1 WHERE (col1 = col2);"
  it "should be safe against SQL injection in literal text" $ do
    let
      statement :: Manipulation StudentsTable '[] '[]
      statement = insertInto #students
        (Values ("Robert'); DROP TABLE students;" `As` #name :* Nil) [])
        Conflict (Returning Nil)
    statement `manipulationRenders`
      "INSERT INTO students (name) VALUES (E'Robert''); DROP TABLE students;');"
  describe "JOINs" $ do
    it "should render CROSS JOINs" $ do
      let
        statement :: Query JoinTables '[] ValueColumns
        statement = select
          ( #orders ! #orderVal `As` #orderVal :*
            #customers ! #customerVal `As` #customerVal :*
            #shippers ! #shipperVal `As` #shipperVal :* Nil )
          ( from (Table (#orders `As` #orders)
            & CrossJoin (Table (#customers `As` #customers))
            & CrossJoin (Table (#shippers `As` #shippers))) )
      statement `queryRenders`
        "SELECT\
        \ orders.orderVal AS orderVal,\
        \ customers.customerVal AS customerVal,\
        \ shippers.shipperVal AS shipperVal\
        \ FROM orders AS orders\
        \ CROSS JOIN customers AS customers\
        \ CROSS JOIN shippers AS shippers;"
    it "should render INNER JOINs" $ do
      let
        statement :: Query JoinTables '[] ValueColumns
        statement = select
          ( (#orders ! #orderVal) `As` #orderVal
            :* (#customers ! #customerVal) `As` #customerVal
            :* (#shippers ! #shipperVal) `As` #shipperVal :* Nil)
          ( from (Table (#orders `As` #orders)
            & InnerJoin (Table (#customers `As` #customers))
              ((#orders ! #customerID) ==* (#customers ! #customerID))
            & InnerJoin (Table (#shippers `As` #shippers))
              ((#orders ! #shipperID) ==* (#shippers ! #shipperID))))
      statement `queryRenders`
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
        statement :: Query JoinTables '[] OrderColumns
        statement = selectDotStar #orders1
          (from (Table (#orders `As` #orders1)
            & CrossJoin (Table (#orders `As` #orders2))))
      statement `queryRenders`
        "SELECT orders1.*\
        \ FROM orders AS orders1\
        \ CROSS JOIN orders AS orders2;"
  it "should render CREATE TABLE statements" $ do
    createTable #table1
      ((int4 & notNull) `As` #col1 :* (int4 & notNull) `As` #col2 :* Nil)
      [primaryKey (Column #col1 :* Column #col2 :* Nil)]
      `definitionRenders`
      "CREATE TABLE table1\
      \ (col1 int4 NOT NULL, col2 int4 NOT NULL,\
      \ PRIMARY KEY (col1, col2));"
    createTable #table2
      ( serial `As` #col1 :*
        text `As` #col2 :*
        (int8 & notNull & default_ 8) `As` #col3 :* Nil )
        [check (#col3 >* 0)]
      `definitionRenders`
      "CREATE TABLE table2\
      \ (col1 serial,\
      \ col2 text,\
      \ col3 int8 NOT NULL DEFAULT 8,\
      \ CHECK ((col3 > 0)));"
    let
      statement :: Definition '[]
        '[ "users" :::
           '[ "id" ::: 'Optional ('NotNull 'PGint4)
            , "username" ::: 'Required ('NotNull 'PGtext)
            ]
         , "emails" :::
           '[ "id" ::: 'Optional ('NotNull 'PGint4)
            , "userid" ::: 'Required ('NotNull 'PGint4)
            , "email" ::: 'Required ('NotNull 'PGtext)
            ]
         ]
      statement =
        createTable #users
          (serial `As` #id :* (text & notNull) `As` #username :* Nil)
          [primaryKey (Column #id :* Nil)]
        >>>
        createTable #emails
          ( serial `As` #id :*
            (integer & notNull) `As` #userid :*
            (text & notNull) `As` #email :* Nil )
          [ primaryKey (Column #id :* Nil)
          , foreignKey (Column #userid :* Nil) #users (Column #id :* Nil)
            OnDeleteCascade OnUpdateRestrict
          , unique (Column #email :* Nil)
          , check (#email /=* "")
          ]
    statement `definitionRenders`
      "CREATE TABLE users\
      \ (id serial, username text NOT NULL,\
      \ PRIMARY KEY (id));\
      \ \
      \CREATE TABLE emails\
      \ (id serial,\
      \ userid integer NOT NULL,\
      \ email text NOT NULL,\
      \ PRIMARY KEY (id),\
      \ FOREIGN KEY (userid) REFERENCES users (id)\
      \ ON DELETE CASCADE ON UPDATE RESTRICT,\
      \ UNIQUE (email),\
      \ CHECK ((email <> E'')));"
  it "should render DROP TABLE statements" $ do
    let
      statement :: Definition Tables '[]
      statement = dropTable #table1
    statement `definitionRenders` "DROP TABLE table1;"

type Columns =
  '[ "col1" ::: 'Required ('NotNull 'PGint4)
   , "col2" ::: 'Required ('NotNull 'PGint4)
   ]
type Tables = '[ "table1" ::: Columns ]
type SumAndCol1 =
  '[ "sum" ::: 'Required ('NotNull 'PGint4)
   , "col1" ::: 'Required ('NotNull 'PGint4)
   ]
type StudentsColumns = '["name" ::: 'Required ('NotNull 'PGtext)]
type StudentsTable = '["students" ::: StudentsColumns]
type OrderColumns =
  '[ "orderID"    ::: 'Required ('NotNull 'PGint4)
   , "orderVal"   ::: 'Required ('NotNull 'PGtext)
   , "customerID" ::: 'Required ('NotNull 'PGint4)
   , "shipperID"  ::: 'Required ('NotNull 'PGint4)
   ]
type CustomerColumns =
  '[ "customerID" ::: 'Required ('NotNull 'PGint4)
   , "customerVal" ::: 'Required ('NotNull 'PGfloat4)
   ]
type ShipperColumns =
  '[ "shipperID" ::: 'Required ('NotNull 'PGint4)
   , "shipperVal" ::: 'Required ('NotNull 'PGbool)
   ]
type JoinTables =
  '[ "orders"    ::: OrderColumns
   , "customers" ::: CustomerColumns
   , "shippers"  ::: ShipperColumns
   ]
type ValueColumns =
  '[ "orderVal"    ::: 'Required ('NotNull 'PGtext)
   , "customerVal" ::: 'Required ('NotNull 'PGfloat4)
   , "shipperVal"  ::: 'Required ('NotNull 'PGbool)
   ]

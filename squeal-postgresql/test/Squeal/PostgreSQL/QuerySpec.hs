{-# LANGUAGE
    DataKinds
  , MagicHash
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.QuerySpec where

import Data.Function
import Generics.SOP hiding (from)
import Test.Hspec

import Squeal.PostgreSQL

spec :: Spec
spec = do
  let
    qry `queryRenders` str =
      renderManipulation (queryStatement qry) `shouldBe` str
  it "correctly renders a simple SELECT query" $ do
    let
      statement :: Query Tables '[] SumAndCol1
      statement =
        select ((#col1 + #col2) `As` #sum :* #col1 `As` #col1 :* Nil)
        (from (Table (#table1 `As` #table1)) & where_ (#col1 .> #col2))
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
        (from (Table (#table1 `As` #table1)) & where_ (param @1))
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
          & having (#col1 + sum_ #col2 .> 1) )
    statement `queryRenders`
      "SELECT sum(col2) AS sum, col1 AS col1\
      \ FROM table1 AS table1\
      \ GROUP BY col1\
      \ HAVING ((col1 + sum(col2)) > 1);"
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
              ((#orders ! #customerID) .== (#customers ! #customerID))
            & InnerJoin (Table (#shippers `As` #shippers))
              ((#orders ! #shipperID) .== (#shippers ! #shipperID))))
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

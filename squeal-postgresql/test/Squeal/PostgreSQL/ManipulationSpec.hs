{-# LANGUAGE
    DataKinds
  , MagicHash
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.ManipulationSpec where

import Generics.SOP hiding (from)
import Test.Hspec

import Squeal.PostgreSQL

spec :: Spec
spec = do
  let
    manipulation `manipulationRenders` str =
      renderManipulation manipulation `shouldBe` str
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
          (#col1 ./= #col2) (Returning Nil)
    statement `manipulationRenders`
      "UPDATE table1 SET col1 = 2\
      \ WHERE (col1 <> col2);"
  it "correctly renders returning UPDATEs" $ do
    let
      statement :: Manipulation Tables '[] SumAndCol1
      statement =
        update #table1 (Set 2 `As` #col1 :* Same `As` #col2 :* Nil)
          (#col1 ./= #col2)
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
            (Just (#col1 ./= #col2)))
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
      statement = deleteFrom #table1 (#col1 .== #col2)
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

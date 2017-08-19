{-# LANGUAGE
    DataKinds
  , MagicHash
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}

module Squeal.PostgreSQL.DefinitionSpec where

import Control.Category ((>>>))
import Data.Function
import Generics.SOP hiding (from)
import Test.Hspec

import Squeal.PostgreSQL

spec :: Spec
spec = do
  let
    definition `definitionRenders` str =
      renderDefinition definition `shouldBe` str
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

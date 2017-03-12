{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , TypeOperators
#-}

module Squeel.PostgreSQL.Type where

import GHC.TypeLits

newtype PGType = PGType Symbol
data Column = Column Symbol PGType
data Table = Table Symbol [Column]
data Database = Database Symbol [Table]
data Schema = Schema Symbol [Database]

type family HasColumn table column where
  HasColumn ('Table table '[]) column = 'False
  HasColumn ('Table table (column ': columns)) column = 'True
  HasColumn ('Table table (column ': columns)) column'
    = HasColumn ('Table table columns) column'

type family HasTable database table where
  HasTable ('Database database '[]) table = 'False
  HasTable ('Database database (table ': tables)) table = 'True
  HasTable ('Database database (table ': tables)) table'
    = HasTable ('Database database tables) table'

type family HasDatabase schema database where
  HasDatabase ('Schema schema '[]) database = 'False
  HasDatabase ('Schema schema (database ': databases)) database = 'True
  HasDatabase ('Schema schema (database ': databases)) database'
    = HasDatabase ('Schema schema databases) database'

type family TableColumns table where
  TableColumns ('Table table columns) = columns

type family DatabaseTables database where
  DatabaseTables ('Database database tables) = tables

type family SchemaDatabases schema where
  SchemaDatabases ('Schema schema databases) = databases

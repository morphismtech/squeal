{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , DeriveGeneric
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , StandaloneDeriving
  , TypeInType
  , TypeOperators
#-}

module Squeal.PostgreSQL.Definition
  (  -- * Definition
    Definition (UnsafeDefinition, renderDefinition)
    -- * Create
  , createTable
  , TableConstraint (UnsafeTableConstraint, renderTableConstraint)
  , check
  , unique
  , primaryKey
  , foreignKey
  , OnDelete (OnDeleteNoAction, OnDeleteRestrict, OnDeleteCascade)
  , renderOnDelete
  , OnUpdate (OnUpdateNoAction, OnUpdateRestrict, OnUpdateCascade)
  , renderOnUpdate
    -- * Drop
  , dropTable
    -- * Alter
  , alterTable
  , alterTableRename
  , AlterTable (UnsafeAlterTable, renderAlterTable)
  , addColumnDefault
  , addColumnNull
  , dropColumn
  , renameColumn
  , alterColumn
  , AlterColumn (UnsafeAlterColumn, renderAlterColumn)
  , setDefault
  , dropDefault
  , setNotNull
  , dropNotNull
  , alterType
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString
import Data.Monoid
import Generics.SOP
import GHC.TypeLits
import Prelude hiding ((.), id)

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Schema

{-----------------------------------------
statements
-----------------------------------------}

-- | A `Definition` is a statement that changes the schema of the
-- database, like a `createTable`, `dropTable`, or `alterTable` command.
-- `Definition`s may be composed using the `>>>` operator from the
-- `Category` typeclass.
newtype Definition
  (schema0 :: TablesType)
  (schema1 :: TablesType)
  = UnsafeDefinition { renderDefinition :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance Category Definition where
  id = UnsafeDefinition ";"
  ddl1 . ddl0 = UnsafeDefinition $
    renderDefinition ddl0 <+> renderDefinition ddl1

{-----------------------------------------
CREATE statements
-----------------------------------------}

-- | `createTable` adds a table to the schema.
--
-- >>> :set -XOverloadedLabels
-- >>> :{
-- renderDefinition $
--   createTable #tab (int `As` #a :* real `As` #b :* Nil) []
-- :}
-- "CREATE TABLE tab (a int, b real);"
createTable
  :: (KnownSymbol table, SListI columns)
  => Alias table -- ^ the name of the table to add
  -> NP (Aliased TypeExpression) (column ': columns)
    -- ^ the names and datatype of each column
  -> [TableConstraint schema (column ': columns)]
    -- ^ constraints that must hold for the table
  -> Definition schema (Create table (column ': columns) schema)
createTable table columns constraints = UnsafeDefinition $
  "CREATE TABLE" <+> renderAlias table
  <+> parenthesized
    ( renderCommaSeparated renderColumnDef columns
      <> renderConstraints constraints )
  <> ";"
  where
    renderColumnDef :: Aliased TypeExpression x -> ByteString
    renderColumnDef (ty `As` column) =
      renderAlias column <+> renderTypeExpression ty
    renderConstraints :: [TableConstraint schema columns] -> ByteString
    renderConstraints = \case
      [] -> ""
      _ -> ", " <> commaSeparated (renderTableConstraint <$> constraints)

-- | Data types are a way to limit the kind of data that can be stored in a
-- table. For many applications, however, the constraint they provide is
-- too coarse. For example, a column containing a product price should
-- probably only accept positive values. But there is no standard data type
-- that accepts only positive numbers. Another issue is that you might want
-- to constrain column data with respect to other columns or rows.
-- For example, in a table containing product information,
-- there should be only one row for each product number.
-- `TableConstraint`s give you as much control over the data in your tables
-- as you wish. If a user attempts to store data in a column that would
-- violate a constraint, an error is raised. This applies
-- even if the value came from the default value definition.
newtype TableConstraint
  (schema :: TablesType)
  (columns :: ColumnsType)
    = UnsafeTableConstraint { renderTableConstraint :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | A `check` constraint is the most generic `TableConstraint` type.
-- It allows you to specify that the value in a certain column must satisfy
-- a Boolean (truth-value) expression.
--
-- >>> :{
-- renderDefinition $
--   createTable #tab
--     ( (int & notNull) `As` #a :*
--       (int & notNull) `As` #b :* Nil )
--     [ check (#a .> #b) ]
-- :}
-- "CREATE TABLE tab (a int NOT NULL, b int NOT NULL, CHECK ((a > b)));"
check
  :: Condition '[table ::: columns] 'Ungrouped '[]
  -> TableConstraint schema columns
check condition = UnsafeTableConstraint $
  "CHECK" <+> parenthesized (renderExpression condition)

-- | `unique` constraints ensure that the data contained in a column,
-- or a group of columns, is unique among all the rows in the table.
--
-- >>> :{
-- renderDefinition $
--   createTable #tab
--     ( int `As` #a :*
--       int `As` #b :* Nil )
--     [ unique (Column #a :* Column #b :* Nil) ]
-- :}
-- "CREATE TABLE tab (a int, b int, UNIQUE (a, b));"
unique
  :: SListI subcolumns
  => NP (Column columns) subcolumns
  -> TableConstraint schema columns
unique columns = UnsafeTableConstraint $
  "UNIQUE" <+> parenthesized (renderCommaSeparated renderColumn columns)

-- | A `primaryKey` constraint indicates that a column, or group of columns,
-- can be used as a unique identifier for rows in the table.
-- This requires that the values be both unique and not null.
--
-- >>> :{
-- renderDefinition $
--   createTable #tab
--     ( serial `As` #id :*
--       (text & notNull) `As` #name :* Nil )
--     [ primaryKey (Column #id :* Nil) ]
-- :}
-- "CREATE TABLE tab (id serial, name text NOT NULL, PRIMARY KEY (id));"
primaryKey
  :: (SListI subcolumns, AllNotNull subcolumns)
  => NP (Column columns) subcolumns
  -- ^ identifying column or group of columns
  -> TableConstraint schema columns
primaryKey columns = UnsafeTableConstraint $
  "PRIMARY KEY" <+> parenthesized (renderCommaSeparated renderColumn columns)

-- | A `foreignKey` specifies that the values in a column
-- (or a group of columns) must match the values appearing in some row of
-- another table. We say this maintains the referential integrity
-- between two related tables.
--
-- >>> :{
-- let
--   definition :: Definition '[]
--     '[ "users" :::
--        '[ "id" ::: 'Optional ('NotNull 'PGint4)
--         , "username" ::: 'Required ('NotNull 'PGtext)
--         ]
--      , "emails" :::
--        '[ "id" ::: 'Optional ('NotNull 'PGint4)
--         , "userid" ::: 'Required ('NotNull 'PGint4)
--         , "email" ::: 'Required ('NotNull 'PGtext)
--         ]
--      ]
--   definition =
--     createTable #users
--       (serial `As` #id :* (text & notNull) `As` #username :* Nil)
--       [primaryKey (Column #id :* Nil)] >>>
--     createTable #emails
--       ( serial `As` #id :*
--         (integer & notNull) `As` #userid :*
--         (text & notNull) `As` #email :* Nil )
--       [ primaryKey (Column #id :* Nil)
--       , foreignKey (Column #userid :* Nil) #users (Column #id :* Nil)
--         OnDeleteCascade OnUpdateRestrict
--       ]
-- in renderDefinition definition
-- :}
-- "CREATE TABLE users (id serial, username text NOT NULL, PRIMARY KEY (id)); CREATE TABLE emails (id serial, userid integer NOT NULL, email text NOT NULL, PRIMARY KEY (id), FOREIGN KEY (userid) REFERENCES users (id) ON DELETE CASCADE ON UPDATE RESTRICT);"
foreignKey
  :: ( HasTable reftable schema refcolumns
     , SameTypes subcolumns refsubcolumns
     , NotAllNull subcolumns
     , SListI subcolumns
     , SListI refsubcolumns)
  => NP (Column columns) subcolumns
  -- ^ column or columns in the table
  -> Alias reftable
  -- ^ reference table
  -> NP (Column refcolumns) refsubcolumns
  -- ^ reference column or columns in the reference table
  -> OnDelete
  -- ^ what to do when reference is deleted
  -> OnUpdate
  -- ^ what to do when reference is updated
  -> TableConstraint schema columns
foreignKey columns reftable refcolumns onDelete onUpdate =
  UnsafeTableConstraint $
    "FOREIGN KEY" <+> parenthesized (renderCommaSeparated renderColumn columns)
    <+> "REFERENCES" <+> renderAlias reftable
    <+> parenthesized (renderCommaSeparated renderColumn refcolumns)
    <+> renderOnDelete onDelete
    <+> renderOnUpdate onUpdate

-- | `OnDelete` indicates what to do with rows that reference a deleted row.
data OnDelete
  = OnDeleteNoAction
    -- ^ if any referencing rows still exist when the constraint is checked,
    -- an error is raised
  | OnDeleteRestrict -- ^ prevents deletion of a referenced row
  | OnDeleteCascade
    -- ^ specifies that when a referenced row is deleted,
    -- row(s) referencing it should be automatically deleted as well
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnDelete

renderOnDelete :: OnDelete -> ByteString
renderOnDelete = \case
  OnDeleteNoAction -> "ON DELETE NO ACTION"
  OnDeleteRestrict -> "ON DELETE RESTRICT"
  OnDeleteCascade -> "ON DELETE CASCADE"

-- | Analagous to `OnDelete` there is also `OnUpdate` which is invoked
-- when a referenced column is changed (updated).
data OnUpdate
  = OnUpdateNoAction
  -- ^ if any referencing rows has not changed when the constraint is checked,
  -- an error is raised
  | OnUpdateRestrict -- ^ prevents update of a referenced row
  | OnUpdateCascade
    -- ^ the updated values of the referenced column(s) should be copied
    -- into the referencing row(s)
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnUpdate

renderOnUpdate :: OnUpdate -> ByteString
renderOnUpdate = \case
  OnUpdateNoAction -> "ON UPDATE NO ACTION"
  OnUpdateRestrict -> "ON UPDATE RESTRICT"
  OnUpdateCascade -> "ON UPDATE CASCADE"

{-----------------------------------------
DROP statements
-----------------------------------------}

-- | `dropTable` removes a table from the schema.
--
-- >>> renderDefinition $ dropTable #muh_table
-- "DROP TABLE muh_table;"
dropTable
  :: KnownSymbol table
  => Alias table
  -> Definition schema (Drop table schema)
dropTable table = UnsafeDefinition $ "DROP TABLE" <+> renderAlias table <> ";"

{-----------------------------------------
ALTER statements
-----------------------------------------}

-- | `alterTable` changes the definition of a table from the schema.
alterTable
  :: HasTable table schema columns0
  => Alias table
  -> AlterTable columns0 columns1
  -> Definition schema (Alter table schema columns1)
alterTable table alteration = UnsafeDefinition $
  "ALTER TABLE"
  <+> renderAlias table
  <+> renderAlterTable alteration
  <> ";"

-- | `alterTable` changes the name of a table from the schema.
--
-- >>> renderDefinition $ alterTableRename #foo #bar
-- "ALTER TABLE foo RENAME TO bar;"
alterTableRename
  :: (KnownSymbol table0, KnownSymbol table1)
  => Alias table0
  -> Alias table1
  -> Definition schema (Rename table0 table1 schema)
alterTableRename table0 table1 = UnsafeDefinition $
  "ALTER TABLE" <+> renderAlias table0
  <+> "RENAME TO" <+> renderAlias table1 <> ";"

newtype AlterTable
  (columns0 :: ColumnsType)
  (columns1 :: ColumnsType) =
    UnsafeAlterTable {renderAlterTable :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

addColumnDefault
  :: KnownSymbol column
  => Alias column
  -> TypeExpression ('Optional ty)
  -> AlterTable columns (Create column ('Optional ty) columns)
addColumnDefault column ty = UnsafeAlterTable $
  "ADD COLUMN" <+> renderAlias column <+> renderTypeExpression ty

addColumnNull
  :: KnownSymbol column
  => Alias column
  -> TypeExpression ('Required ('Null ty))
  -> AlterTable columns (Create column ('Required ('Null ty)) columns)
addColumnNull column ty = UnsafeAlterTable $
  "ADD COLUMN" <+> renderAlias column <+> renderTypeExpression ty

dropColumn
  :: KnownSymbol column
  => Alias column
  -> AlterTable columns (Drop column columns)
dropColumn column = UnsafeAlterTable $
  "DROP COLUMN" <+> renderAlias column

renameColumn
  :: (KnownSymbol column0, KnownSymbol column1)
  => Alias column0
  -> Alias column1
  -> AlterTable columns (Rename column0 column1 columns)
renameColumn column0 column1 = UnsafeAlterTable $
  "RENAME COLUMN" <+> renderAlias column0  <+> "TO" <+> renderAlias column1

alterColumn
  :: (KnownSymbol column, HasColumn column columns ty0)
  => Alias column
  -> AlterColumn ty0 ty1
  -> AlterTable columns (Alter column columns ty1)
alterColumn column alteration = UnsafeAlterTable $
  "ALTER COLUMN" <+> renderAlias column <+> renderAlterColumn alteration

newtype AlterColumn (ty0 :: ColumnType) (ty1 :: ColumnType) =
  UnsafeAlterColumn {renderAlterColumn :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

setDefault
  :: Expression '[] 'Ungrouped '[] ('Required ty)
  -> AlterColumn ('Required ty) ('Optional ty)
setDefault expression = UnsafeAlterColumn $
  "SET DEFAULT" <+> renderExpression expression

dropDefault :: AlterColumn ('Optional ty) ('Required ty)
dropDefault = UnsafeAlterColumn $ "DROP DEFAULT"

setNotNull
  :: AlterColumn (optionality ('Null ty)) (optionality ('NotNull ty))
setNotNull = UnsafeAlterColumn $ "SET NOT NULL"

dropNotNull
  :: AlterColumn (optionality ('NotNull ty)) (optionality ('Null ty))
dropNotNull = UnsafeAlterColumn $ "DROP NOT NULL"

alterType
  :: TypeExpression (optionality (nullity ty1))
  -> AlterColumn (optionality (nullity ty0)) (optionality (nullity ty1))
alterType ty = UnsafeAlterColumn $ "TYPE " <> renderTypeExpression ty

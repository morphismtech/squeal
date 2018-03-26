{-|
Module: Squeal.PostgreSQL.Definition
Description: Squeal data definition language
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal data definition language.
-}

{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , DeriveDataTypeable
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeInType
  , TypeOperators
#-}

module Squeal.PostgreSQL.Definition
  (  -- * Definition
    Definition (UnsafeDefinition, renderDefinition)
  , (>>>)
    -- * Create
  , createTable
  , createTableIfNotExists
  , TableConstraintExpression (..)
  , Column (..)
  , check
  , unique
  , primaryKey
  , foreignKey
  , ForeignKeyed
  , OnDeleteClause (OnDeleteNoAction, OnDeleteRestrict, OnDeleteCascade)
  , renderOnDeleteClause
  , OnUpdateClause (OnUpdateNoAction, OnUpdateRestrict, OnUpdateCascade)
  , renderOnUpdateClause
    -- * Drop
  , dropTable
    -- * Alter
  , alterTable
  , alterTableRename
  , AlterTable (UnsafeAlterTable, renderAlterTable)
  , addConstraint
  , dropConstraint
  , AddColumn (addColumn)
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
import GHC.TypeLits
import Prelude hiding ((.), id)

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Schema

{-----------------------------------------
statements
-----------------------------------------}

-- | A `Definition` is a statement that changes the schema of the
-- database, like a `createTable`, `dropTable`, or `alterTable` command.
-- `Definition`s may be composed using the `>>>` operator.
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
--   createTable #tab (int `As` #a :* real `As` #b :* Nil) Nil
-- :}
-- "CREATE TABLE tab (a int, b real);"
createTable
  :: ( KnownSymbol table
     , columns ~ (col ': cols)
     , SOP.SListI columns
     , SOP.SListI constraints )
  => Alias table -- ^ the name of the table to add
  -> NP (Aliased TypeExpression) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression schema columns)) constraints
    -- ^ constraints that must hold for the table
  -> Definition schema (Create table (constraints :=> columns) schema)
createTable table columns constraints = UnsafeDefinition $
  "CREATE TABLE" <+> renderCreation table columns constraints

-- | `createTableIfNotExists` creates a table if it doesn't exist, but does not add it to the schema.
-- Instead, the schema already has the table so if the table did not yet exist, the schema was wrong.
-- `createTableIfNotExists` fixes this. Interestingly, this property makes it an idempotent in the `Category` `Definition`.
--
-- >>> :set -XOverloadedLabels -XTypeApplications
-- >>> type Table = '[] :=> '["a" ::: 'NoDef :=> 'Null 'PGint4, "b" ::: 'NoDef :=> 'Null 'PGfloat4]
-- >>> type Schema = '["tab" ::: Table]
-- >>> :{
-- renderDefinition
--   (createTableIfNotExists #tab (int `As` #a :* real `As` #b :* Nil) Nil :: Definition Schema Schema)
-- :}
-- "CREATE TABLE IF NOT EXISTS tab (a int, b real);"
createTableIfNotExists
  :: ( Has table schema (constraints :=> columns)
     , SOP.SListI columns
     , SOP.SListI constraints )
  => Alias table -- ^ the name of the table to add
  -> NP (Aliased TypeExpression) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression schema columns)) constraints
    -- ^ constraints that must hold for the table
  -> Definition schema schema
createTableIfNotExists table columns constraints = UnsafeDefinition $
  "CREATE TABLE IF NOT EXISTS"
  <+> renderCreation table columns constraints

-- helper function for `createTable` and `createTableIfNotExists`
renderCreation
  :: ( KnownSymbol table
     , SOP.SListI columns
     , SOP.SListI constraints )
  => Alias table -- ^ the name of the table to add
  -> NP (Aliased TypeExpression) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression schema columns)) constraints
    -- ^ constraints that must hold for the table
  -> ByteString
renderCreation table columns constraints = renderAlias table
  <+> parenthesized
    ( renderCommaSeparated renderColumnDef columns
      <> ( case constraints of
             Nil -> ""
             _ -> ", " <>
               renderCommaSeparated renderConstraint constraints ) )
  <> ";"
  where
    renderColumnDef :: Aliased TypeExpression x -> ByteString
    renderColumnDef (ty `As` column) =
      renderAlias column <+> renderTypeExpression ty
    renderConstraint
      :: Aliased (TableConstraintExpression schema columns) constraint
      -> ByteString
    renderConstraint (constraint `As` alias) =
      "CONSTRAINT" <+> renderAlias alias <+> renderTableConstraintExpression constraint

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
newtype TableConstraintExpression
  (schema :: TablesType)
  (columns :: ColumnsType)
  (tableConstraint :: TableConstraint)
    = UnsafeTableConstraintExpression
    { renderTableConstraintExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | @Column columns column@ is a witness that `column` is in `columns`. 
data Column
  (columns :: ColumnsType)
  (column :: (Symbol,ColumnType))
  where
    Column
      :: Has column columns ty
      => Alias column
      -> Column columns (column ::: ty)

-- | Render a `Column`.
renderColumn :: Column columns column -> ByteString
renderColumn (Column column) = renderAlias column

-- | A `check` constraint is the most generic `TableConstraint` type.
-- It allows you to specify that the value in a certain column must satisfy
-- a Boolean (truth-value) expression.
--
-- >>> :{
-- renderDefinition $
--   createTable #tab
--     ( (int & notNull) `As` #a :*
--       (int & notNull) `As` #b :* Nil )
--     ( check (Column #a :* Column #b :* Nil) (#a .> #b) `As` #inequality :* Nil )
-- :}
-- "CREATE TABLE tab (a int NOT NULL, b int NOT NULL, CONSTRAINT inequality CHECK ((a > b)));"
check
  :: NP (Column columns) subcolumns
  -> Condition '[table ::: ColumnsToRelation subcolumns] 'Ungrouped '[]
  -- ^ condition to check
  -> TableConstraintExpression schema columns ('Check (AliasesOf subcolumns))
check _cols condition = UnsafeTableConstraintExpression $
  "CHECK" <+> parenthesized (renderExpression condition)

-- | A `unique` constraint ensure that the data contained in a column,
-- or a group of columns, is unique among all the rows in the table.
--
-- >>> :{
-- renderDefinition $
--   createTable #tab
--     ( int `As` #a :*
--       int `As` #b :* Nil )
--     ( unique (Column #a :* Column #b :* Nil) `As` #uq_a_b :* Nil )
-- :}
-- "CREATE TABLE tab (a int, b int, CONSTRAINT uq_a_b UNIQUE (a, b));"
unique
  :: SOP.SListI subcolumns
  => NP (Column columns) subcolumns
  -- ^ unique column or group of columns
  -> TableConstraintExpression schema columns ('Unique (AliasesOf subcolumns))
unique columns = UnsafeTableConstraintExpression $
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
--     ( primaryKey (Column #id :* Nil) `As` #pk_id :* Nil )
-- :}
-- "CREATE TABLE tab (id serial, name text NOT NULL, CONSTRAINT pk_id PRIMARY KEY (id));"
primaryKey
  :: (SOP.SListI subcolumns, AllNotNull subcolumns)
  => NP (Column columns) subcolumns
  -- ^ identifying column or group of columns
  -> TableConstraintExpression schema columns ('PrimaryKey (AliasesOf subcolumns))
primaryKey columns = UnsafeTableConstraintExpression $
  "PRIMARY KEY" <+> parenthesized (renderCommaSeparated renderColumn columns)

-- | A `foreignKey` specifies that the values in a column
-- (or a group of columns) must match the values appearing in some row of
-- another table. We say this maintains the referential integrity
-- between two related tables.
--
-- >>> :{
-- type Schema =
--   '[ "users" :::
--        '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
--        '[ "id" ::: 'Def :=> 'NotNull 'PGint4
--         , "name" ::: 'NoDef :=> 'NotNull 'PGtext
--         ]
--    , "emails" :::
--        '[  "pk_emails" ::: 'PrimaryKey '["id"]
--         , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
--         ] :=>
--        '[ "id" ::: 'Def :=> 'NotNull 'PGint4
--         , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
--         , "email" ::: 'NoDef :=> 'Null 'PGtext
--         ]
--    ]
-- :}
--
-- >>> :{
-- let
--   setup :: Definition '[] Schema
--   setup = 
--    createTable #users
--      ( serial `As` #id :*
--        (text & notNull) `As` #name :* Nil )
--      ( primaryKey (Column #id :* Nil) `As` #pk_users :* Nil ) >>>
--    createTable #emails
--      ( serial `As` #id :*
--        (int & notNull) `As` #user_id :*
--        text `As` #email :* Nil )
--      ( primaryKey (Column #id :* Nil) `As` #pk_emails :*
--        foreignKey (Column #user_id :* Nil) #users (Column #id :* Nil)
--          OnDeleteCascade OnUpdateCascade `As` #fk_user_id :* Nil )
-- in renderDefinition setup
-- :}
-- "CREATE TABLE users (id serial, name text NOT NULL, CONSTRAINT pk_users PRIMARY KEY (id)); CREATE TABLE emails (id serial, user_id int NOT NULL, email text, CONSTRAINT pk_emails PRIMARY KEY (id), CONSTRAINT fk_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE ON UPDATE CASCADE);"
foreignKey
  :: ForeignKeyed schema table reftable subcolumns refsubcolumns
  => NP (Column columns) subcolumns
  -- ^ column or columns in the table
  -> Alias table
  -- ^ reference table
  -> NP (Column (TableToColumns reftable)) refsubcolumns
  -- ^ reference column or columns in the reference table
  -> OnDeleteClause
  -- ^ what to do when reference is deleted
  -> OnUpdateClause
  -- ^ what to do when reference is updated
  -> TableConstraintExpression schema columns
      ('ForeignKey (AliasesOf subcolumns) table (AliasesOf refsubcolumns))
foreignKey columns reftable refcolumns onDelete onUpdate =
  UnsafeTableConstraintExpression $
    "FOREIGN KEY" <+> parenthesized (renderCommaSeparated renderColumn columns)
    <+> "REFERENCES" <+> renderAlias reftable
    <+> parenthesized (renderCommaSeparated renderColumn refcolumns)
    <+> renderOnDeleteClause onDelete
    <+> renderOnUpdateClause onUpdate

-- | A type synonym for constraints on a table with a foreign key.
type ForeignKeyed schema table reftable subcolumns refsubcolumns
  = ( Has table schema reftable
    , SameTypes subcolumns refsubcolumns
    , AllNotNull subcolumns
    , SOP.SListI subcolumns
    , SOP.SListI refsubcolumns )

-- | `OnDeleteClause` indicates what to do with rows that reference a deleted row.
data OnDeleteClause
  = OnDeleteNoAction
    -- ^ if any referencing rows still exist when the constraint is checked,
    -- an error is raised
  | OnDeleteRestrict -- ^ prevents deletion of a referenced row
  | OnDeleteCascade
    -- ^ specifies that when a referenced row is deleted,
    -- row(s) referencing it should be automatically deleted as well
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnDeleteClause

-- | Render `OnDeleteClause`.
renderOnDeleteClause :: OnDeleteClause -> ByteString
renderOnDeleteClause = \case
  OnDeleteNoAction -> "ON DELETE NO ACTION"
  OnDeleteRestrict -> "ON DELETE RESTRICT"
  OnDeleteCascade -> "ON DELETE CASCADE"

-- | Analagous to `OnDeleteClause` there is also `OnUpdateClause` which is invoked
-- when a referenced column is changed (updated).
data OnUpdateClause
  = OnUpdateNoAction
  -- ^ if any referencing rows has not changed when the constraint is checked,
  -- an error is raised
  | OnUpdateRestrict -- ^ prevents update of a referenced row
  | OnUpdateCascade
    -- ^ the updated values of the referenced column(s) should be copied
    -- into the referencing row(s)
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnUpdateClause

-- | Render `OnUpdateClause`.
renderOnUpdateClause :: OnUpdateClause -> ByteString
renderOnUpdateClause = \case
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
  => Alias table -- ^ table to remove
  -> Definition schema (Drop table schema)
dropTable table = UnsafeDefinition $ "DROP TABLE" <+> renderAlias table <> ";"

{-----------------------------------------
ALTER statements
-----------------------------------------}

-- | `alterTable` changes the definition of a table from the schema.
alterTable
  :: Has tab schema table0
  => Alias tab -- ^ table to alter
  -> AlterTable schema table0 table1 -- ^ alteration to perform
  -> Definition schema (Alter tab schema table1)
alterTable table alteration = UnsafeDefinition $
  "ALTER TABLE"
  <+> renderAlias table
  <+> renderAlterTable alteration
  <> ";"

-- | `alterTableRename` changes the name of a table from the schema.
--
-- >>> renderDefinition $ alterTableRename #foo #bar
-- "ALTER TABLE foo RENAME TO bar;"
alterTableRename
  :: (KnownSymbol table0, KnownSymbol table1)
  => Alias table0 -- ^ table to rename
  -> Alias table1 -- ^ what to rename it
  -> Definition schema (Rename table0 table1 schema)
alterTableRename table0 table1 = UnsafeDefinition $
  "ALTER TABLE" <+> renderAlias table0
  <+> "RENAME TO" <+> renderAlias table1 <> ";"

-- | An `AlterTable` describes the alteration to perform on the columns
-- of a table.
newtype AlterTable
  (schema :: TablesType)
  (table0 :: TableType)
  (table1 :: TableType) =
    UnsafeAlterTable {renderAlterTable :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | An `addConstraint` adds a table constraint.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4]]
--     '["tab" ::: '["positive" ::: Check '["col"]] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4]]
--   definition = alterTable #tab (addConstraint #positive (check (Column #col :* Nil) (#col .> 0)))
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab ADD CONSTRAINT positive CHECK ((col > 0));"
addConstraint
  :: KnownSymbol alias
  => Alias alias
  -> TableConstraintExpression schema columns constraint
  -- ^ constraint to add
  -> AlterTable schema (constraints :=> columns)
      (Create alias constraint constraints :=> columns)
addConstraint alias constraint = UnsafeAlterTable $
  "ADD" <+> "CONSTRAINT" <+> renderAlias alias
    <+> renderTableConstraintExpression constraint

-- | A `dropConstraint` drops a table constraint.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '["positive" ::: Check '["col"]] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4]]
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4]]
--   definition = alterTable #tab (dropConstraint #positive)
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab DROP CONSTRAINT positive;"
dropConstraint
  :: KnownSymbol constraint
  => Alias constraint
  -- ^ constraint to drop
  -> AlterTable schema
      (constraints :=> columns)
      (Drop constraint constraints :=> columns)
dropConstraint constraint = UnsafeAlterTable $
  "DROP" <+> "CONSTRAINT" <+> renderAlias constraint

-- | An `AddColumn` is either @NULL@ or has @DEFAULT@.
class AddColumn ty where
  -- | `addColumn` adds a new column, initially filled with whatever
  -- default value is given or with @NULL@.
  --
  -- >>> :{
  -- let
  --   definition :: Definition
  --     '["tab" ::: '[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4]]
  --     '["tab" ::: '[] :=>
  --        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
  --         , "col2" ::: 'Def :=> 'Null 'PGtext ]]
  --   definition = alterTable #tab (addColumn #col2 (text & default_ "foo"))
  -- in renderDefinition definition
  -- :}
  -- "ALTER TABLE tab ADD COLUMN col2 text DEFAULT E'foo';"
  --
  -- >>> :{
  -- let
  --   definition :: Definition
  --     '["tab" ::: '[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4]]
  --     '["tab" ::: '[] :=>
  --        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
  --         , "col2" ::: 'NoDef :=> 'Null 'PGtext ]]
  --   definition = alterTable #tab (addColumn #col2 text)
  -- in renderDefinition definition
  -- :}
  -- "ALTER TABLE tab ADD COLUMN col2 text;"
  addColumn
    :: KnownSymbol column
    => Alias column -- ^ column to add
    -> TypeExpression ty -- ^ type of the new column
    -> AlterTable schema (constraints :=> columns)
        (constraints :=> Create column ty columns)
  addColumn column ty = UnsafeAlterTable $
    "ADD COLUMN" <+> renderAlias column <+> renderTypeExpression ty
instance {-# OVERLAPPING #-} AddColumn ('Def :=> ty)
instance {-# OVERLAPPABLE #-} AddColumn ('NoDef :=> 'Null ty)

-- | A `dropColumn` removes a column. Whatever data was in the column
-- disappears. Table constraints involving the column are dropped, too.
-- However, if the column is referenced by a foreign key constraint of
-- another table, PostgreSQL will not silently drop that constraint.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '[] :=>
--        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
--         , "col2" ::: 'NoDef :=> 'Null 'PGtext ]]
--     '["tab" ::: '[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4]]
--   definition = alterTable #tab (dropColumn #col2)
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab DROP COLUMN col2;"
dropColumn
  :: KnownSymbol column
  => Alias column -- ^ column to remove
  -> AlterTable schema
      (constraints :=> columns)
      (DropIfConstraintsInvolve column constraints :=> Drop column columns)
dropColumn column = UnsafeAlterTable $
  "DROP COLUMN" <+> renderAlias column

-- | A `renameColumn` renames a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '[] :=> '["foo" ::: 'NoDef :=> 'Null 'PGint4]]
--     '["tab" ::: '[] :=> '["bar" ::: 'NoDef :=> 'Null 'PGint4]]
--   definition = alterTable #tab (renameColumn #foo #bar)
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab RENAME COLUMN foo TO bar;"
renameColumn
  :: (KnownSymbol column0, KnownSymbol column1)
  => Alias column0 -- ^ column to rename
  -> Alias column1 -- ^ what to rename the column
  -> AlterTable schema (constraints :=> columns)
      (constraints :=> Rename column0 column1 columns)
renameColumn column0 column1 = UnsafeAlterTable $
  "RENAME COLUMN" <+> renderAlias column0  <+> "TO" <+> renderAlias column1

-- | An `alterColumn` alters a single column.
alterColumn
  :: (KnownSymbol column, Has column columns ty0)
  => Alias column -- ^ column to alter
  -> AlterColumn ty0 ty1 -- ^ alteration to perform
  -> AlterTable schema (constraints :=> columns)
      (constraints :=> Alter column columns ty1)
alterColumn column alteration = UnsafeAlterTable $
  "ALTER COLUMN" <+> renderAlias column <+> renderAlterColumn alteration

-- | An `AlterColumn` describes the alteration to perform on a single column.
newtype AlterColumn (ty0 :: ColumnType) (ty1 :: ColumnType) =
  UnsafeAlterColumn {renderAlterColumn :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | A `setDefault` sets a new default for a column. Note that this doesn't
-- affect any existing rows in the table, it just changes the default for
-- future `insertTable` and `updateTable` commands.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4]]
--     '["tab" ::: '[] :=> '["col" ::: 'Def :=> 'Null 'PGint4]]
--   definition = alterTable #tab (alterColumn #col (setDefault 5))
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab ALTER COLUMN col SET DEFAULT 5;"
setDefault
  :: Expression '[] 'Ungrouped '[] ty -- ^ default value to set
  -> AlterColumn (constraint :=> ty) ('Def :=> ty)
setDefault expression = UnsafeAlterColumn $
  "SET DEFAULT" <+> renderExpression expression

-- | A `dropDefault` removes any default value for a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '[] :=> '["col" ::: 'Def :=> 'Null 'PGint4]]
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4]]
--   definition = alterTable #tab (alterColumn #col dropDefault)
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab ALTER COLUMN col DROP DEFAULT;"
dropDefault :: AlterColumn ('Def :=> ty) ('NoDef :=> ty)
dropDefault = UnsafeAlterColumn $ "DROP DEFAULT"

-- | A `setNotNull` adds a @NOT NULL@ constraint to a column.
-- The constraint will be checked immediately, so the table data must satisfy
-- the constraint before it can be added.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4]]
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4]]
--   definition = alterTable #tab (alterColumn #col setNotNull)
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab ALTER COLUMN col SET NOT NULL;"
setNotNull
  :: AlterColumn (constraint :=> 'Null ty) (constraint :=> 'NotNull ty)
setNotNull = UnsafeAlterColumn $ "SET NOT NULL"

-- | A `dropNotNull` drops a @NOT NULL@ constraint from a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4]]
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4]]
--   definition = alterTable #tab (alterColumn #col dropNotNull)
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab ALTER COLUMN col DROP NOT NULL;"
dropNotNull
  :: AlterColumn (constraint :=> 'NotNull ty) (constraint :=> 'Null ty)
dropNotNull = UnsafeAlterColumn $ "DROP NOT NULL"

-- | An `alterType` converts a column to a different data type.
-- This will succeed only if each existing entry in the column can be
-- converted to the new type by an implicit cast.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4]]
--     '["tab" ::: '[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGnumeric]]
--   definition =
--     alterTable #tab (alterColumn #col (alterType (numeric & notNull)))
-- in renderDefinition definition
-- :}
-- "ALTER TABLE tab ALTER COLUMN col TYPE numeric NOT NULL;"
alterType :: TypeExpression ty -> AlterColumn ty0 ty
alterType ty = UnsafeAlterColumn $ "TYPE" <+> renderTypeExpression ty

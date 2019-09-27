{-|
Module: Squeal.PostgreSQL.Definition.Table
Description: Create, drop and alter table definitions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Create, drop and alter table definitions.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeOperators
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Definition.Table
  ( createTable
  , createTableIfNotExists
  , dropTable
  , dropTableIfExists
  , alterTable
  , alterTableIfExists
  , alterTableRename
  , alterTableIfExistsRename
  , AlterTable (..)
  , addConstraint
  , dropConstraint
  , AddColumn (..)
  , dropColumn
  , renameColumn
  , alterColumn
  , AlterColumn (..)
  , setDefault
  , dropDefault
  , setNotNull
  , dropNotNull
  , alterType
  ) where

import Control.DeepSeq
import Data.ByteString
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Definition.Table.Column
import Squeal.PostgreSQL.Definition.Table.Constraint
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- | `createTable` adds a table to the schema.

>>> :set -XOverloadedLabels
>>> :{
type Table = '[] :=>
  '[ "a" ::: 'NoDef :=> 'Null 'PGint4
   , "b" ::: 'NoDef :=> 'Null 'PGfloat4 ]
:}

>>> :{
let
  setup :: Definition (Public '[]) (Public '["tab" ::: 'Table Table])
  setup = createTable #tab
    (nullable int `as` #a :* nullable real `as` #b) Nil
in printSQL setup
:}
CREATE TABLE "tab" ("a" int NULL, "b" real NULL);
-}
createTable
  :: ( KnownSymbol sch
     , KnownSymbol tab
     , columns ~ (col ': cols)
     , SOP.SListI columns
     , SOP.SListI constraints
     , Has sch schemas0 schema0
     , schemas1 ~ Alter sch (Create tab ('Table (constraints :=> columns)) schema0) schemas0 )
  => QualifiedAlias sch tab -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression schemas0)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression sch tab schemas1)) constraints
    -- ^ constraints that must hold for the table
  -> Definition schemas0 schemas1
createTable tab columns constraints = UnsafeDefinition $
  "CREATE TABLE" <+> renderCreation tab columns constraints

{-| `createTableIfNotExists` creates a table if it doesn't exist, but does not add it to the schema.
Instead, the schema already has the table so if the table did not yet exist, the schema was wrong.
`createTableIfNotExists` fixes this. Interestingly, this property makes it an idempotent in
the `Category` of `Definition`s.

>>> :set -XOverloadedLabels -XTypeApplications
>>> :{
type Table = '[] :=>
  '[ "a" ::: 'NoDef :=> 'Null 'PGint4
   , "b" ::: 'NoDef :=> 'Null 'PGfloat4 ]
:}

>>> type Schemas = Public '["tab" ::: 'Table Table]

>>> :{
let
  setup :: Definition Schemas Schemas
  setup = createTableIfNotExists #tab
    (nullable int `as` #a :* nullable real `as` #b) Nil
in printSQL setup
:}
CREATE TABLE IF NOT EXISTS "tab" ("a" int NULL, "b" real NULL);
-}
createTableIfNotExists
  :: ( KnownSymbol sch
     , KnownSymbol tab
     , columns ~ (col ': cols)
     , SOP.SListI columns
     , SOP.SListI constraints
     , Has sch schemas0 schema0
     , schemas1 ~ Alter sch (CreateIfNotExists tab ('Table (constraints :=> columns)) schema0) schemas0 )
  => QualifiedAlias sch tab -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression schemas0)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression sch tab schemas1)) constraints
    -- ^ constraints that must hold for the table
  -> Definition schemas0 schemas1
createTableIfNotExists tab columns constraints = UnsafeDefinition $
  "CREATE TABLE IF NOT EXISTS"
  <+> renderCreation tab columns constraints

-- helper function for `createTable` and `createTableIfNotExists`
renderCreation
  :: ( KnownSymbol sch
     , KnownSymbol tab
     , SOP.SListI columns
     , SOP.SListI constraints )
  => QualifiedAlias sch tab -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression schemas0)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression sch tab schemas1)) constraints
    -- ^ constraints that must hold for the table
  -> ByteString
renderCreation tab columns constraints = renderSQL tab
  <+> parenthesized
    ( renderCommaSeparated renderColumnDef columns
      <> ( case constraints of
             Nil -> ""
             _ -> ", " <>
               renderCommaSeparated renderConstraint constraints ) )
  <> ";"
  where
    renderColumnDef :: Aliased (ColumnTypeExpression schemas) x -> ByteString
    renderColumnDef (ty `As` column) =
      renderSQL column <+> renderColumnTypeExpression ty
    renderConstraint
      :: Aliased (TableConstraintExpression sch tab schemas) constraint
      -> ByteString
    renderConstraint (constraint `As` alias) =
      "CONSTRAINT" <+> renderSQL alias <+> renderSQL constraint

-- | `dropTable` removes a table from the schema.
--
-- >>> :{
-- let
--   definition :: Definition '["public" ::: '["muh_table" ::: 'Table t]] (Public '[])
--   definition = dropTable #muh_table
-- :}
--
-- >>> printSQL definition
-- DROP TABLE "muh_table";
dropTable
  :: ( Has sch schemas schema
     , KnownSymbol tab )
  => QualifiedAlias sch tab -- ^ table to remove
  -> Definition schemas (Alter sch (DropSchemum tab 'Table schema) schemas)
dropTable tab = UnsafeDefinition $ "DROP TABLE" <+> renderSQL tab <> ";"

dropTableIfExists
  :: ( Has sch schemas schema
     , KnownSymbol tab)
  => QualifiedAlias sch tab -- ^ table to remove
  -> Definition schemas (Alter sch (DropSchemumIfExists tab 'Table schema) schemas)
dropTableIfExists tab = UnsafeDefinition $ "DROP TABLE IF EXISTS" <+> renderSQL tab <> ";"

-- | `alterTable` changes the definition of a table from the schema.
alterTable
  :: (Has sch schemas schema, KnownSymbol tab)
  => QualifiedAlias sch tab -- ^ table to alter
  -> AlterTable sch tab schemas table -- ^ alteration to perform
  -> Definition schemas (Alter sch (Alter tab ('Table table) schema) schemas)
alterTable tab alteration = UnsafeDefinition $
  "ALTER TABLE"
  <+> renderSQL tab
  <+> renderAlterTable alteration
  <> ";"

-- | `alterTable` changes the definition of a table from the schema.
alterTableIfExists
  :: (Has sch schemas schema, KnownSymbol tab)
  => QualifiedAlias sch tab -- ^ table to alter
  -> AlterTable sch tab schemas table -- ^ alteration to perform
  -> Definition schemas (Alter sch (AlterIfExists tab ('Table table) schema) schemas)
alterTableIfExists tab alteration = UnsafeDefinition $
  "ALTER TABLE IF EXISTS"
  <+> renderSQL tab
  <+> renderAlterTable alteration
  <> ";"

-- | `alterTableRename` changes the name of a table from the schema.
--
-- >>> printSQL $ alterTableRename #foo #bar
-- ALTER TABLE "foo" RENAME TO "bar";
alterTableRename
  :: (KnownSymbol tab0, KnownSymbol tab1)
  => Alias tab0 -- ^ table to rename
  -> Alias tab1 -- ^ what to rename it
  -> Definition schema (Rename tab0 tab1 schema)
alterTableRename tab0 tab1 = UnsafeDefinition $
  "ALTER TABLE" <+> renderSQL tab0
  <+> "RENAME TO" <+> renderSQL tab1 <> ";"

alterTableIfExistsRename
  :: (KnownSymbol tab0, KnownSymbol tab1)
  => Alias tab0 -- ^ table to rename
  -> Alias tab1 -- ^ what to rename it
  -> Definition schema (RenameIfExists tab0 tab1 schema)
alterTableIfExistsRename tab0 tab1 = UnsafeDefinition $
  "ALTER TABLE IF EXISTS" <+> renderSQL tab0
  <+> "RENAME TO" <+> renderSQL tab1 <> ";"

-- | An `AlterTable` describes the alteration to perform on the columns
-- of a table.
newtype AlterTable
  (sch :: Symbol)
  (tab :: Symbol)
  (schemas :: SchemasType)
  (table :: TableType) =
    UnsafeAlterTable {renderAlterTable :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | An `addConstraint` adds a table constraint.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]]
--     '["public" ::: '["tab" ::: 'Table ('["positive" ::: 'Check '["col"]] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]]
--   definition = alterTable #tab (addConstraint #positive (check #col (#col .> 0)))
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ADD CONSTRAINT "positive" CHECK (("col" > 0));
addConstraint
  :: ( KnownSymbol alias
     , Has sch schemas schema
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , table1 ~ (Create alias constraint constraints :=> columns) )
  => Alias alias
  -> TableConstraintExpression sch tab schemas constraint
  -- ^ constraint to add
  -> AlterTable sch tab schemas table1
addConstraint alias constraint = UnsafeAlterTable $
  "ADD" <+> "CONSTRAINT" <+> renderSQL alias
    <+> renderSQL constraint

-- | A `dropConstraint` drops a table constraint.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["public" ::: '["tab" ::: 'Table ('["positive" ::: Check '["col"]] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]]
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]]
--   definition = alterTable #tab (dropConstraint #positive)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" DROP CONSTRAINT "positive";
dropConstraint
  :: ( KnownSymbol constraint
     , Has sch schemas schema
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , table1 ~ (Drop constraint constraints :=> columns) )
  => Alias constraint
  -- ^ constraint to drop
  -> AlterTable sch tab schemas table1
dropConstraint constraint = UnsafeAlterTable $
  "DROP" <+> "CONSTRAINT" <+> renderSQL constraint

-- | An `AddColumn` is either @NULL@ or has @DEFAULT@.
class AddColumn ty where
  -- | `addColumn` adds a new column, initially filled with whatever
  -- default value is given or with @NULL@.
  --
  -- >>> :{
  -- let
  --   definition :: Definition
  --     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4])]]
  --     '["public" ::: '["tab" ::: 'Table ('[] :=>
  --        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
  --         , "col2" ::: 'Def :=> 'Null 'PGtext ])]]
  --   definition = alterTable #tab (addColumn #col2 (text & nullable & default_ "foo"))
  -- in printSQL definition
  -- :}
  -- ALTER TABLE "tab" ADD COLUMN "col2" text NULL DEFAULT E'foo';
  --
  -- >>> :{
  -- let
  --   definition :: Definition
  --     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4])]]
  --     '["public" ::: '["tab" ::: 'Table ('[] :=>
  --        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
  --         , "col2" ::: 'NoDef :=> 'Null 'PGtext ])]]
  --   definition = alterTable #tab (addColumn #col2 (text & nullable))
  -- in printSQL definition
  -- :}
  -- ALTER TABLE "tab" ADD COLUMN "col2" text NULL;
  addColumn
    :: ( KnownSymbol column
       , Has sch schemas schema
       , Has tab schema ('Table table0)
       , table0 ~ (constraints :=> columns) )
    => Alias column -- ^ column to add
    -> ColumnTypeExpression schemas ty -- ^ type of the new column
    -> AlterTable sch tab schemas (constraints :=> Create column ty columns)
  addColumn column ty = UnsafeAlterTable $
    "ADD COLUMN" <+> renderSQL column <+> renderColumnTypeExpression ty
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
--     '["public" ::: '["tab" ::: 'Table ('[] :=>
--        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
--         , "col2" ::: 'NoDef :=> 'Null 'PGtext ])]]
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4])]]
--   definition = alterTable #tab (dropColumn #col2)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" DROP COLUMN "col2";
dropColumn
  :: ( KnownSymbol column
     , Has sch schemas schema
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , table1 ~ (constraints :=> Drop column columns) )
  => Alias column -- ^ column to remove
  -> AlterTable sch tab schemas table1
dropColumn column = UnsafeAlterTable $
  "DROP COLUMN" <+> renderSQL column

-- | A `renameColumn` renames a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["foo" ::: 'NoDef :=> 'Null 'PGint4])]]
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["bar" ::: 'NoDef :=> 'Null 'PGint4])]]
--   definition = alterTable #tab (renameColumn #foo #bar)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" RENAME COLUMN "foo" TO "bar";
renameColumn
  :: ( KnownSymbol column0
     , KnownSymbol column1
     , Has sch schemas schema
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , table1 ~ (constraints :=> Rename column0 column1 columns) )
  => Alias column0 -- ^ column to rename
  -> Alias column1 -- ^ what to rename the column
  -> AlterTable sch tab schemas table1
renameColumn column0 column1 = UnsafeAlterTable $
  "RENAME COLUMN" <+> renderSQL column0  <+> "TO" <+> renderSQL column1

-- | An `alterColumn` alters a single column.
alterColumn
  :: ( KnownSymbol column
     , Has sch schemas schema
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , Has column columns ty0
     , table1 ~ (constraints :=> Alter column ty1 columns))
  => Alias column -- ^ column to alter
  -> AlterColumn schemas ty0 ty1 -- ^ alteration to perform
  -> AlterTable sch tab schemas table1
alterColumn column alteration = UnsafeAlterTable $
  "ALTER COLUMN" <+> renderSQL column <+> renderAlterColumn alteration

-- | An `AlterColumn` describes the alteration to perform on a single column.
newtype AlterColumn (schemas :: SchemasType) (ty0 :: ColumnType) (ty1 :: ColumnType) =
  UnsafeAlterColumn {renderAlterColumn :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | A `setDefault` sets a new default for a column. Note that this doesn't
-- affect any existing rows in the table, it just changes the default for
-- future insert and update commands.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])]]
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'Def :=> 'Null 'PGint4])]]
--   definition = alterTable #tab (alterColumn #col (setDefault 5))
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" SET DEFAULT 5;
setDefault
  :: Expression '[] '[] 'Ungrouped schemas '[] '[] ty -- ^ default value to set
  -> AlterColumn schemas (constraint :=> ty) ('Def :=> ty)
setDefault expression = UnsafeAlterColumn $
  "SET DEFAULT" <+> renderExpression expression

-- | A `dropDefault` removes any default value for a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'Def :=> 'Null 'PGint4])]]
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])]]
--   definition = alterTable #tab (alterColumn #col dropDefault)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" DROP DEFAULT;
dropDefault :: AlterColumn schemas ('Def :=> ty) ('NoDef :=> ty)
dropDefault = UnsafeAlterColumn $ "DROP DEFAULT"

-- | A `setNotNull` adds a @NOT NULL@ constraint to a column.
-- The constraint will be checked immediately, so the table data must satisfy
-- the constraint before it can be added.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])]]
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]]
--   definition = alterTable #tab (alterColumn #col setNotNull)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" SET NOT NULL;
setNotNull
  :: AlterColumn schemas (constraint :=> 'Null ty) (constraint :=> 'NotNull ty)
setNotNull = UnsafeAlterColumn $ "SET NOT NULL"

-- | A `dropNotNull` drops a @NOT NULL@ constraint from a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]]
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])]]
--   definition = alterTable #tab (alterColumn #col dropNotNull)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" DROP NOT NULL;
dropNotNull
  :: AlterColumn schemas (constraint :=> 'NotNull ty) (constraint :=> 'Null ty)
dropNotNull = UnsafeAlterColumn $ "DROP NOT NULL"

-- | An `alterType` converts a column to a different data type.
-- This will succeed only if each existing entry in the column can be
-- converted to the new type by an implicit cast.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]]
--     '["public" ::: '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGnumeric])]]
--   definition =
--     alterTable #tab (alterColumn #col (alterType (numeric & notNullable)))
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" TYPE numeric NOT NULL;
alterType :: ColumnTypeExpression schemas ty -> AlterColumn schemas ty0 ty
alterType ty = UnsafeAlterColumn $ "TYPE" <+> renderColumnTypeExpression ty

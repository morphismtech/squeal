{-|
Module: Squeal.PostgreSQL.Definition.Table
Description: create, drop and alter tables
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

create, drop and alter tables
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
  ( -- * Create
    createTable
  , createTableIfNotExists
    -- * Drop
  , dropTable
  , dropTableIfExists
    -- * Alter
  , alterTable
  , alterTableIfExists
  , alterTableRename
  , alterTableIfExistsRename
  , alterTableSetSchema
  , AlterTable (..)
    -- ** Constraints
  , addConstraint
  , dropConstraint
    -- ** Columns
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

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Definition.Constraint
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

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
     , Has sch db0 schema0
     , db1 ~ Alter sch (Create tab ('Table (constraints :=> columns)) schema0) db0 )
  => QualifiedAlias sch tab -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression db0)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression sch tab db1)) constraints
    -- ^ constraints that must hold for the table
  -> Definition db0 db1
createTable tab columns constraints = UnsafeDefinition $
  "CREATE TABLE" <+> renderCreation tab columns constraints

{-| `createTableIfNotExists` creates a table if it doesn't exist, but does not add it to the schema.
Instead, the schema already has the table so if the table did not yet exist, the schema was wrong.
`createTableIfNotExists` fixes this. Interestingly, this property makes it an idempotent in
the `Control.Category.Category` of `Definition`s.

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
     , Has sch db0 schema0
     , db1 ~ Alter sch (CreateIfNotExists tab ('Table (constraints :=> columns)) schema0) db0 )
  => QualifiedAlias sch tab -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression db0)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression sch tab db1)) constraints
    -- ^ constraints that must hold for the table
  -> Definition db0 db1
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
  -> NP (Aliased (ColumnTypeExpression db0)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression sch tab db1)) constraints
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
    renderColumnDef :: Aliased (ColumnTypeExpression db) x -> ByteString
    renderColumnDef (ty `As` column) =
      renderSQL column <+> renderColumnTypeExpression ty
    renderConstraint
      :: Aliased (TableConstraintExpression sch tab db) constraint
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
  :: ( Has sch db schema
     , KnownSymbol tab )
  => QualifiedAlias sch tab -- ^ table to remove
  -> Definition db (Alter sch (DropSchemum tab 'Table schema) db)
dropTable tab = UnsafeDefinition $ "DROP TABLE" <+> renderSQL tab <> ";"

-- | Drop a table if it exists.
dropTableIfExists
  :: ( Has sch db schema
     , KnownSymbol tab)
  => QualifiedAlias sch tab -- ^ table to remove
  -> Definition db (Alter sch (DropSchemumIfExists tab 'Table schema) db)
dropTableIfExists tab = UnsafeDefinition $
  "DROP TABLE IF EXISTS" <+> renderSQL tab <> ";"

-- | `alterTable` changes the definition of a table from the schema.
alterTable
  :: (Has sch db schema, KnownSymbol tab)
  => QualifiedAlias sch tab -- ^ table to alter
  -> AlterTable sch tab db table -- ^ alteration to perform
  -> Definition db (Alter sch (Alter tab ('Table table) schema) db)
alterTable tab alteration = UnsafeDefinition $
  "ALTER TABLE"
  <+> renderSQL tab
  <+> renderAlterTable alteration
  <> ";"

-- | `alterTable` changes the definition of a table from the schema.
alterTableIfExists
  :: (Has sch db schema, KnownSymbol tab)
  => QualifiedAlias sch tab -- ^ table to alter
  -> AlterTable sch tab db table -- ^ alteration to perform
  -> Definition db (Alter sch (AlterIfExists tab ('Table table) schema) db)
alterTableIfExists tab alteration = UnsafeDefinition $
  "ALTER TABLE IF EXISTS"
  <+> renderSQL tab
  <+> renderAlterTable alteration
  <> ";"

-- | `alterTableRename` changes the name of a table from the schema.
--
-- >>> type Schemas = '[ "public" ::: '[ "foo" ::: 'Table ('[] :=> '[]) ] ]
-- >>> :{
--  let migration :: Definition Schemas '["public" ::: '["bar" ::: 'Table ('[] :=> '[]) ] ]
--      migration = alterTableRename #foo #bar
--  in printSQL migration
-- :}
-- ALTER TABLE "foo" RENAME TO "bar";
alterTableRename
  :: ( Has sch db schema
     , KnownSymbol tab1
     , Has tab0 schema ('Table table))
  => QualifiedAlias sch tab0 -- ^ table to rename
  -> Alias tab1 -- ^ what to rename it
  -> Definition db (Alter sch (Rename tab0 tab1 schema) db )
alterTableRename tab0 tab1 = UnsafeDefinition $
  "ALTER TABLE" <+> renderSQL tab0
  <+> "RENAME TO" <+> renderSQL tab1 <> ";"

-- | `alterTableIfExistsRename` changes the name of a table from the schema if it exists.
--
-- >>> type Schemas = '[ "public" ::: '[ "foo" ::: 'Table ('[] :=> '[]) ] ]
-- >>> :{
--  let migration :: Definition Schemas Schemas
--      migration = alterTableIfExistsRename #goo #gar
--  in printSQL migration
-- :}
-- ALTER TABLE IF EXISTS "goo" RENAME TO "gar";
alterTableIfExistsRename
  :: ( Has sch db schema
     , KnownSymbol tab0
     , KnownSymbol tab1 )
  => QualifiedAlias sch tab0 -- ^ table to rename
  -> Alias tab1 -- ^ what to rename it
  -> Definition db (Alter sch (RenameIfExists tab0 tab1 schema) db )
alterTableIfExistsRename tab0 tab1 = UnsafeDefinition $
  "ALTER TABLE IF EXISTS" <+> renderSQL tab0
  <+> "RENAME TO" <+> renderSQL tab1 <> ";"

{- | This form moves the table into another schema.

>>> type DB0 = '[ "sch0" ::: '[ "tab" ::: 'Table ('[] :=> '[]) ], "sch1" ::: '[] ]
>>> type DB1 = '[ "sch0" ::: '[], "sch1" ::: '[ "tab" ::: 'Table ('[] :=> '[]) ] ]
>>> :{
let def :: Definition DB0 DB1
    def = alterTableSetSchema (#sch0 ! #tab) #sch1
in printSQL def
:}
ALTER TABLE "sch0"."tab" SET SCHEMA "sch1";
-}
alterTableSetSchema
  :: ( Has sch0 db schema0
     , Has tab schema0 ('Table table)
     , Has sch1 db schema1 )
  => QualifiedAlias sch0 tab -- ^ table to move
  -> Alias sch1 -- ^ where to move it
  -> Definition db (SetSchema sch0 sch1 schema0 schema1 tab 'Table table db)
alterTableSetSchema tab sch = UnsafeDefinition $
  "ALTER TABLE" <+> renderSQL tab <+> "SET SCHEMA" <+> renderSQL sch <> ";"

-- | An `AlterTable` describes the alteration to perform on the columns
-- of a table.
newtype AlterTable
  (sch :: Symbol)
  (tab :: Symbol)
  (db :: SchemasType)
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
-- ALTER TABLE "tab" ADD CONSTRAINT "positive" CHECK (("col" > (0 :: int4)));
addConstraint
  :: ( KnownSymbol alias
     , Has sch db schema
     , Has tab schema ('Table (constraints :=> columns)) )
  => Alias alias
  -> TableConstraintExpression sch tab db constraint
  -- ^ constraint to add
  -> AlterTable sch tab db (Create alias constraint constraints :=> columns)
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
     , Has sch db schema
     , Has tab schema ('Table (constraints :=> columns)) )
  => Alias constraint
  -- ^ constraint to drop
  -> AlterTable sch tab db (Drop constraint constraints :=> columns)
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
  -- ALTER TABLE "tab" ADD COLUMN "col2" text NULL DEFAULT (E'foo' :: text);
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
       , Has sch db schema
       , Has tab schema ('Table (constraints :=> columns)) )
    => Alias column -- ^ column to add
    -> ColumnTypeExpression db ty -- ^ type of the new column
    -> AlterTable sch tab db (constraints :=> Create column ty columns)
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
     , Has sch db schema
     , Has tab schema ('Table (constraints :=> columns)) )
  => Alias column -- ^ column to remove
  -> AlterTable sch tab db (constraints :=> Drop column columns)
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
     , Has sch db schema
     , Has tab schema ('Table (constraints :=> columns)) )
  => Alias column0 -- ^ column to rename
  -> Alias column1 -- ^ what to rename the column
  -> AlterTable sch tab db (constraints :=> Rename column0 column1 columns)
renameColumn column0 column1 = UnsafeAlterTable $
  "RENAME COLUMN" <+> renderSQL column0  <+> "TO" <+> renderSQL column1

-- | An `alterColumn` alters a single column.
alterColumn
  :: ( KnownSymbol column
     , Has sch db schema
     , Has tab schema ('Table (constraints :=> columns))
     , Has column columns ty0 )
  => Alias column -- ^ column to alter
  -> AlterColumn db ty0 ty1 -- ^ alteration to perform
  -> AlterTable sch tab db (constraints :=> Alter column ty1 columns)
alterColumn column alteration = UnsafeAlterTable $
  "ALTER COLUMN" <+> renderSQL column <+> renderAlterColumn alteration

-- | An `AlterColumn` describes the alteration to perform on a single column.
newtype AlterColumn (db :: SchemasType) (ty0 :: ColumnType) (ty1 :: ColumnType) =
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
-- ALTER TABLE "tab" ALTER COLUMN "col" SET DEFAULT (5 :: int4);
setDefault
  :: Expression 'Ungrouped '[] '[] db '[] '[] ty -- ^ default value to set
  -> AlterColumn db (constraint :=> ty) ('Def :=> ty)
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
dropDefault :: AlterColumn db ('Def :=> ty) ('NoDef :=> ty)
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
  :: AlterColumn db (constraint :=> 'Null ty) (constraint :=> 'NotNull ty)
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
  :: AlterColumn db (constraint :=> 'NotNull ty) (constraint :=> 'Null ty)
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
alterType :: ColumnTypeExpression db ty -> AlterColumn db ty0 ty
alterType ty = UnsafeAlterColumn $ "TYPE" <+> renderColumnTypeExpression ty

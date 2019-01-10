{-|
Module: Squeal.PostgreSQL.Definition
Description: Squeal data definition language
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal data definition language.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeOperators
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Definition
  ( -- * Definition
    Definition (..)
  , (>>>)
    -- * Tables
    -- ** Create
  , createSchema
  , createSchemaIfNotExists
  , createTable
  , createTableIfNotExists
  , TableConstraintExpression (..)
  , check
  , unique
  , primaryKey
  , foreignKey
  , ForeignKeyed
  , OnDeleteClause (..)
  , OnUpdateClause (..)
    -- ** Drop
  , dropTable
    -- ** Alter
  , alterTable
  , alterTableRename
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
    -- * Views
  , createView
  , dropView
    -- * Types
  , createTypeEnum
  , createTypeEnumFrom
  , createTypeComposite
  , createTypeCompositeFrom
  , dropType
    -- * Columns
  , ColumnTypeExpression (..)
  , nullable
  , notNullable
  , default_
  , serial2
  , smallserial
  , serial4
  , serial
  , serial8
  , bigserial
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
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

{-----------------------------------------
statements
-----------------------------------------}

-- | A `Definition` is a statement that changes the schemas of the
-- database, like a `createTable`, `dropTable`, or `alterTable` command.
-- `Definition`s may be composed using the `>>>` operator.
newtype Definition
  (schemas0 :: SchemasType)
  (schemas1 :: SchemasType)
  = UnsafeDefinition { renderDefinition :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (Definition schemas0 schemas1) where
  renderSQL = renderDefinition

instance Category Definition where
  id = UnsafeDefinition ";"
  ddl1 . ddl0 = UnsafeDefinition $
    renderSQL ddl0 <> "\n" <> renderSQL ddl1

{-----------------------------------------
CREATE statements
-----------------------------------------}

createSchema
  :: KnownSymbol sch
  => Alias sch
  -> Definition schemas (Create sch '[] schemas)
createSchema sch = UnsafeDefinition $
  "CREATE" <+> "SCHEMA" <+> renderSQL sch <> ";"

createSchemaIfNotExists
  :: (KnownSymbol sch, Has sch schemas schema)
  => Alias sch
  -> Definition schemas schemas
createSchemaIfNotExists sch = UnsafeDefinition $
  "CREATE" <+> "SCHEMA" <+> "IF" <+> "NOT" <+> "EXISTS"
  <+> renderSQL sch <> ";"

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
  :: ( Has sch schemas schema
     , Has tab schema ('Table (constraints :=> columns))
     , SOP.SListI columns
     , SOP.SListI constraints )
  => QualifiedAlias sch tab -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression schemas)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression sch tab schemas)) constraints
    -- ^ constraints that must hold for the table
  -> Definition schemas schemas
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
  (sch :: Symbol)
  (tab :: Symbol)
  (schemas :: SchemasType)
  (constraint :: TableConstraint)
    = UnsafeTableConstraintExpression
    { renderTableConstraintExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL
  (TableConstraintExpression sch tab schemas constraint) where
    renderSQL = renderTableConstraintExpression

{-| A `check` constraint is the most generic `TableConstraint` type.
It allows you to specify that the value in a certain column must satisfy
a Boolean (truth-value) expression.

>>> :{
type Schema = '[
  "tab" ::: 'Table ('[ "inequality" ::: 'Check '["a","b"]] :=> '[
    "a" ::: 'NoDef :=> 'NotNull 'PGint4,
    "b" ::: 'NoDef :=> 'NotNull 'PGint4
  ])]
:}

>>> :{
let
  definition :: Definition (Public '[]) (Public Schema)
  definition = createTable #tab
    ( (int & notNullable) `as` #a :*
      (int & notNullable) `as` #b )
    ( check (#a :* #b) (#a .> #b) `as` #inequality )
:}

>>> printSQL definition
CREATE TABLE "tab" ("a" int NOT NULL, "b" int NOT NULL, CONSTRAINT "inequality" CHECK (("a" > "b")));
-}
check
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , HasAll aliases (TableToRow table) subcolumns )
  => NP Alias aliases
  -- ^ specify the subcolumns which are getting checked
  -> (forall t. Condition '[] schemas '[] 'Ungrouped '[t ::: subcolumns])
  -- ^ a closed `Condition` on those subcolumns
  -> TableConstraintExpression sch tab schemas ('Check aliases)
check _cols condition = UnsafeTableConstraintExpression $
  "CHECK" <+> parenthesized (renderSQL condition)

{-| A `unique` constraint ensure that the data contained in a column,
or a group of columns, is unique among all the rows in the table.

>>> :{
type Schema = '[
  "tab" ::: 'Table( '[ "uq_a_b" ::: 'Unique '["a","b"]] :=> '[
    "a" ::: 'NoDef :=> 'Null 'PGint4,
    "b" ::: 'NoDef :=> 'Null 'PGint4
  ])]
:}

>>> :{
let
  definition :: Definition (Public '[]) (Public Schema)
  definition = createTable #tab
    ( (int & nullable) `as` #a :*
      (int & nullable) `as` #b )
    ( unique (#a :* #b) `as` #uq_a_b )
:}

>>> printSQL definition
CREATE TABLE "tab" ("a" int NULL, "b" int NULL, CONSTRAINT "uq_a_b" UNIQUE ("a", "b"));
-}
unique
  :: ( Has sch schemas schema
     , Has tab schema('Table table)
     , HasAll aliases (TableToRow table) subcolumns )
  => NP Alias aliases
  -- ^ specify subcolumns which together are unique for each row
  -> TableConstraintExpression sch tab schemas ('Unique aliases)
unique columns = UnsafeTableConstraintExpression $
  "UNIQUE" <+> parenthesized (renderSQL columns)

{-| A `primaryKey` constraint indicates that a column, or group of columns,
can be used as a unique identifier for rows in the table.
This requires that the values be both unique and not null.

>>> :{
type Schema = '[
  "tab" ::: 'Table ('[ "pk_id" ::: 'PrimaryKey '["id"]] :=> '[
    "id" ::: 'Def :=> 'NotNull 'PGint4,
    "name" ::: 'NoDef :=> 'NotNull 'PGtext
  ])]
:}

>>> :{
let
  definition :: Definition (Public '[]) (Public Schema)
  definition = createTable #tab
    ( serial `as` #id :*
      (text & notNullable) `as` #name )
    ( primaryKey #id `as` #pk_id )
:}

>>> printSQL definition
CREATE TABLE "tab" ("id" serial, "name" text NOT NULL, CONSTRAINT "pk_id" PRIMARY KEY ("id"));
-}
primaryKey
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , HasAll aliases (TableToColumns table) subcolumns
     , AllNotNull subcolumns )
  => NP Alias aliases
  -- ^ specify the subcolumns which together form a primary key.
  -> TableConstraintExpression sch tab schemas ('PrimaryKey aliases)
primaryKey columns = UnsafeTableConstraintExpression $
  "PRIMARY KEY" <+> parenthesized (renderSQL columns)

{-| A `foreignKey` specifies that the values in a column
(or a group of columns) must match the values appearing in some row of
another table. We say this maintains the referential integrity
between two related tables.

>>> :{
type Schema =
  '[ "users" ::: 'Table (
       '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "name" ::: 'NoDef :=> 'NotNull 'PGtext
        ])
   , "emails" ::: 'Table (
       '[  "pk_emails" ::: 'PrimaryKey '["id"]
        , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
        ] :=>
       '[ "id" ::: 'Def :=> 'NotNull 'PGint4
        , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
        , "email" ::: 'NoDef :=> 'Null 'PGtext
        ])
   ]
:}

>>> :{
let
  setup :: Definition (Public '[]) (Public Schema)
  setup =
   createTable #users
     ( serial `as` #id :*
       (text & notNullable) `as` #name )
     ( primaryKey #id `as` #pk_users ) >>>
   createTable #emails
     ( serial `as` #id :*
       (int & notNullable) `as` #user_id :*
       (text & nullable) `as` #email )
     ( primaryKey #id `as` #pk_emails :*
       foreignKey #user_id #users #id
         OnDeleteCascade OnUpdateCascade `as` #fk_user_id )
in printSQL setup
:}
CREATE TABLE "users" ("id" serial, "name" text NOT NULL, CONSTRAINT "pk_users" PRIMARY KEY ("id"));
CREATE TABLE "emails" ("id" serial, "user_id" int NOT NULL, "email" text NULL, CONSTRAINT "pk_emails" PRIMARY KEY ("id"), CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "users" ("id") ON DELETE CASCADE ON UPDATE CASCADE);

A `foreignKey` can even be a table self-reference.

>>> :{
type Schema =
  '[ "employees" ::: 'Table (
       '[ "employees_pk"          ::: 'PrimaryKey '["id"]
        , "employees_employer_fk" ::: 'ForeignKey '["employer_id"] "employees" '["id"]
        ] :=>
       '[ "id"          :::   'Def :=> 'NotNull 'PGint4
        , "name"        ::: 'NoDef :=> 'NotNull 'PGtext
        , "employer_id" ::: 'NoDef :=>    'Null 'PGint4
        ])
   ]
:}

>>> :{
let
  setup :: Definition (Public '[]) (Public Schema)
  setup =
   createTable #employees
     ( serial `as` #id :*
       (text & notNullable) `as` #name :*
       (integer & nullable) `as` #employer_id )
     ( primaryKey #id `as` #employees_pk :*
       foreignKey #employer_id #employees #id
         OnDeleteCascade OnUpdateCascade `as` #employees_employer_fk )
in printSQL setup
:}
CREATE TABLE "employees" ("id" serial, "name" text NOT NULL, "employer_id" integer NULL, CONSTRAINT "employees_pk" PRIMARY KEY ("id"), CONSTRAINT "employees_employer_fk" FOREIGN KEY ("employer_id") REFERENCES "employees" ("id") ON DELETE CASCADE ON UPDATE CASCADE);
-}
foreignKey
  :: (ForeignKeyed schemas sch schema child parent
        table reftable
        columns refcolumns
        constraints cols
        reftys tys )
  => NP Alias columns
  -- ^ column or columns in the table
  -> Alias parent
  -- ^ reference table
  -> NP Alias refcolumns
  -- ^ reference column or columns in the reference table
  -> OnDeleteClause
  -- ^ what to do when reference is deleted
  -> OnUpdateClause
  -- ^ what to do when reference is updated
  -> TableConstraintExpression sch child schemas
      ('ForeignKey columns parent refcolumns)
foreignKey keys parent refs ondel onupd = UnsafeTableConstraintExpression $
  "FOREIGN KEY" <+> parenthesized (renderSQL keys)
  <+> "REFERENCES" <+> renderSQL parent
  <+> parenthesized (renderSQL refs)
  <+> renderSQL ondel
  <+> renderSQL onupd

-- | A constraint synonym between types involved in a foreign key constraint.
type ForeignKeyed schemas
  sch
  schema
  child parent
  table reftable
  columns refcolumns
  constraints cols
  reftys tys =
    ( Has sch schemas schema
    , Has child schema ('Table table)
    , Has parent schema ('Table reftable)
    , HasAll columns (TableToColumns table) tys
    , reftable ~ (constraints :=> cols)
    , HasAll refcolumns cols reftys
    , SOP.AllZip SamePGType tys reftys
    , Uniquely refcolumns constraints )

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
instance RenderSQL OnDeleteClause where
  renderSQL = \case
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
instance RenderSQL OnUpdateClause where
  renderSQL = \case
    OnUpdateNoAction -> "ON UPDATE NO ACTION"
    OnUpdateRestrict -> "ON UPDATE RESTRICT"
    OnUpdateCascade -> "ON UPDATE CASCADE"

{-----------------------------------------
DROP statements
-----------------------------------------}

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
     , Has tab schema ('Table table))
  => QualifiedAlias sch tab -- ^ table to remove
  -> Definition schemas (Alter sch (Drop tab schema) schemas)
dropTable tab = UnsafeDefinition $ "DROP TABLE" <+> renderSQL tab <> ";"

{-----------------------------------------
ALTER statements
-----------------------------------------}

-- | `alterTable` changes the definition of a table from the schema.
alterTable
  :: (Has sch schemas schema, Has tab schema ('Table table0))
  => QualifiedAlias sch tab -- ^ table to alter
  -> AlterTable sch tab schemas table1 -- ^ alteration to perform
  -> Definition schemas (Alter sch (Alter tab ('Table table1) schema) schemas)
alterTable tab alteration = UnsafeDefinition $
  "ALTER TABLE"
  <+> renderSQL tab
  <+> renderAlterTable alteration
  <> ";"

-- | `alterTableRename` changes the name of a table from the schema.
--
-- >>> printSQL $ alterTableRename #foo #bar
-- ALTER TABLE "foo" RENAME TO "bar";
alterTableRename
  :: (KnownSymbol table0, KnownSymbol table1)
  => Alias table0 -- ^ table to rename
  -> Alias table1 -- ^ what to rename it
  -> Definition schema (Rename table0 table1 schema)
alterTableRename table0 table1 = UnsafeDefinition $
  "ALTER TABLE" <+> renderSQL table0
  <+> "RENAME TO" <+> renderSQL table1 <> ";"

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
  :: Expression '[] schemas '[] 'Ungrouped '[] ty -- ^ default value to set
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

{- | Create a view.
>>> type ABC = '["a" ::: 'NoDef :=> 'Null 'PGint4, "b" ::: 'NoDef :=> 'Null 'PGint4, "c" ::: 'NoDef :=> 'Null 'PGint4]
>>> type BC = '["b" ::: 'Null 'PGint4, "c" ::: 'Null 'PGint4]
>>> :{
let
  definition :: Definition
    '[ "public" ::: '["abc" ::: 'Table ('[] :=> ABC)]]
    '[ "public" ::: '["abc" ::: 'Table ('[] :=> ABC), "bc"  ::: 'View BC]]
  definition =
    createView #bc (select_ (#b :* #c) (from (table #abc)))
in printSQL definition
:}
CREATE VIEW "bc" AS SELECT "b" AS "b", "c" AS "c" FROM "abc" AS "abc";
-}
createView
  :: (KnownSymbol sch, KnownSymbol vw, Has sch schemas schema)
  => QualifiedAlias sch vw -- ^ the name of the view to add
  -> Query '[] schemas '[] view -- ^ query
  -> Definition schemas (Alter sch (Create vw ('View view) schema) schemas)
createView alias query = UnsafeDefinition $
  "CREATE" <+> "VIEW" <+> renderSQL alias <+> "AS"
  <+> renderQuery query <> ";"

-- | Drop a view.
--
-- >>> :{
-- let
--   definition :: Definition
--     '[ "public" ::: '["abc" ::: 'Table ('[] :=> '["a" ::: 'NoDef :=> 'Null 'PGint4, "b" ::: 'NoDef :=> 'Null 'PGint4, "c" ::: 'NoDef :=> 'Null 'PGint4])
--      , "bc"  ::: 'View ('["b" ::: 'Null 'PGint4, "c" ::: 'Null 'PGint4])]]
--     '[ "public" ::: '["abc" ::: 'Table ('[] :=> '["a" ::: 'NoDef :=> 'Null 'PGint4, "b" ::: 'NoDef :=> 'Null 'PGint4, "c" ::: 'NoDef :=> 'Null 'PGint4])]]
--   definition = dropView #bc
-- in printSQL definition
-- :}
-- DROP VIEW "bc";
dropView
  :: (Has sch schemas schema, Has vw schema ('View view))
  => QualifiedAlias sch vw -- ^ view to remove
  -> Definition schemas (Alter sch (Drop vw schema) schemas)
dropView vw = UnsafeDefinition $ "DROP VIEW" <+> renderSQL vw <> ";"

-- | Enumerated types are created using the `createTypeEnum` command, for example
--
-- >>> printSQL $ (createTypeEnum #mood (label @"sad" :* label @"ok" :* label @"happy") :: Definition (Public '[]) '["public" ::: '["mood" ::: 'Typedef ('PGenum '["sad","ok","happy"])]])
-- CREATE TYPE "mood" AS ENUM ('sad', 'ok', 'happy');
createTypeEnum
  :: (KnownSymbol enum, Has sch schemas schema, SOP.All KnownSymbol labels)
  => QualifiedAlias sch enum
  -- ^ name of the user defined enumerated type
  -> NP PGlabel labels
  -- ^ labels of the enumerated type
  -> Definition schemas (Alter sch (Create enum ('Typedef ('PGenum labels)) schema) schemas)
createTypeEnum enum labels = UnsafeDefinition $
  "CREATE" <+> "TYPE" <+> renderSQL enum <+> "AS" <+> "ENUM" <+>
  parenthesized (renderSQL labels) <> ";"

-- | Enumerated types can also be generated from a Haskell type, for example
--
-- >>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
-- >>> instance SOP.Generic Schwarma
-- >>> instance SOP.HasDatatypeInfo Schwarma
-- >>> :{
-- let
--   createSchwarma :: Definition (Public '[]) '["public" ::: '["schwarma" ::: 'Typedef (PG (Enumerated Schwarma))]]
--   createSchwarma = createTypeEnumFrom @Schwarma #schwarma
-- in
--   printSQL createSchwarma
-- :}
-- CREATE TYPE "schwarma" AS ENUM ('Beef', 'Lamb', 'Chicken');
createTypeEnumFrom
  :: forall hask sch enum schemas schema.
  ( SOP.Generic hask
  , SOP.All KnownSymbol (LabelsPG hask)
  , KnownSymbol enum
  , Has sch schemas schema )
  => QualifiedAlias sch enum
  -- ^ name of the user defined enumerated type
  -> Definition schemas (Alter sch (Create enum ('Typedef (PG (Enumerated hask))) schema) schemas)
createTypeEnumFrom enum = createTypeEnum enum
  (SOP.hpure label :: NP PGlabel (LabelsPG hask))

{- | `createTypeComposite` creates a composite type. The composite type is
specified by a list of attribute names and data types.

>>> :{
type PGcomplex = 'PGcomposite
  '[ "real"      ::: 'NotNull 'PGfloat8
   , "imaginary" ::: 'NotNull 'PGfloat8 ]
:}

>>> :{
let
  setup :: Definition (Public '[]) '["public" ::: '["complex" ::: 'Typedef PGcomplex]]
  setup = createTypeComposite #complex
    (float8 `as` #real :* float8 `as` #imaginary)
in printSQL setup
:}
CREATE TYPE "complex" AS ("real" float8, "imaginary" float8);
-}
createTypeComposite
  :: (KnownSymbol ty, Has sch schemas schema, SOP.SListI fields)
  => QualifiedAlias sch ty
  -- ^ name of the user defined composite type
  -> NP (Aliased (TypeExpression schemas)) fields
  -- ^ list of attribute names and data types
  -> Definition schemas (Alter sch (Create ty ('Typedef ('PGcomposite fields)) schema) schemas)
createTypeComposite ty fields = UnsafeDefinition $
  "CREATE" <+> "TYPE" <+> renderSQL ty <+> "AS" <+> parenthesized
  (renderCommaSeparated renderField fields) <> ";"
  where
    renderField :: Aliased (TypeExpression schemas) x -> ByteString
    renderField (typ `As` alias) =
      renderSQL alias <+> renderSQL typ

-- | Composite types can also be generated from a Haskell type, for example
--
-- >>> data Complex = Complex {real :: Double, imaginary :: Double} deriving GHC.Generic
-- >>> instance SOP.Generic Complex
-- >>> instance SOP.HasDatatypeInfo Complex
-- >>> type Schema = '["complex" ::: 'Typedef (PG (Composite Complex))]
-- >>> :{
-- let
--   createComplex :: Definition (Public '[]) (Public Schema)
--   createComplex = createTypeCompositeFrom @Complex #complex
-- in
--   printSQL createComplex
-- :}
-- CREATE TYPE "complex" AS ("real" float8, "imaginary" float8);
createTypeCompositeFrom
  :: forall hask sch ty schemas schema.
  ( SOP.All (FieldTyped schemas) (RowPG hask)
  , KnownSymbol ty
  , Has sch schemas schema )
  => QualifiedAlias sch ty
  -- ^ name of the user defined composite type
  -> Definition schemas (Alter sch (Create ty ( 'Typedef (PG (Composite hask))) schema) schemas)
createTypeCompositeFrom ty = createTypeComposite ty
  (SOP.hcpure (SOP.Proxy :: SOP.Proxy (FieldTyped schemas)) fieldtype
    :: NP (Aliased (TypeExpression schemas)) (RowPG hask))

class FieldTyped schemas ty where
  fieldtype :: Aliased (TypeExpression schemas) ty
instance (KnownSymbol alias, PGTyped schemas ty)
  => FieldTyped schemas (alias ::: ty) where
    fieldtype = pgtype `As` Alias

-- | Drop a type.
--
-- >>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
-- >>> instance SOP.Generic Schwarma
-- >>> instance SOP.HasDatatypeInfo Schwarma
-- >>> printSQL (dropType #schwarma :: Definition '["public" ::: '["schwarma" ::: 'Typedef (PG (Enumerated Schwarma))]] (Public '[]))
-- DROP TYPE "schwarma";
dropType
  :: (Has sch schemas schema, Has td schema ('Typedef ty))
  => QualifiedAlias sch td
  -- ^ name of the user defined type
  -> Definition schemas (Alter sch (Drop td schema) schemas)
dropType tydef = UnsafeDefinition $ "DROP" <+> "TYPE" <+> renderSQL tydef <> ";"

-- | `ColumnTypeExpression`s are used in `createTable` commands.
newtype ColumnTypeExpression (schemas :: SchemasType) (ty :: ColumnType)
  = UnsafeColumnTypeExpression { renderColumnTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (ColumnTypeExpression schemas ty) where
  renderSQL = renderColumnTypeExpression

-- | used in `createTable` commands as a column constraint to note that
-- @NULL@ may be present in a column
nullable
  :: TypeExpression schemas (nullity ty)
  -> ColumnTypeExpression schemas ('NoDef :=> 'Null ty)
nullable ty = UnsafeColumnTypeExpression $ renderSQL ty <+> "NULL"

-- | used in `createTable` commands as a column constraint to ensure
-- @NULL@ is not present in a column
notNullable
  :: TypeExpression schemas (nullity ty)
  -> ColumnTypeExpression schemas ('NoDef :=> 'NotNull ty)
notNullable ty = UnsafeColumnTypeExpression $ renderSQL ty <+> "NOT NULL"

-- | used in `createTable` commands as a column constraint to give a default
default_
  :: Expression '[] schemas '[] 'Ungrouped '[] ty
  -> ColumnTypeExpression schemas ('NoDef :=> ty)
  -> ColumnTypeExpression schemas ('Def :=> ty)
default_ x ty = UnsafeColumnTypeExpression $
  renderSQL ty <+> "DEFAULT" <+> renderExpression x

-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint2`
serial2, smallserial
  :: ColumnTypeExpression schemas ('Def :=> 'NotNull 'PGint2)
serial2 = UnsafeColumnTypeExpression "serial2"
smallserial = UnsafeColumnTypeExpression "smallserial"
-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint4`
serial4, serial
  :: ColumnTypeExpression schemas ('Def :=> 'NotNull 'PGint4)
serial4 = UnsafeColumnTypeExpression "serial4"
serial = UnsafeColumnTypeExpression "serial"
-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint8`
serial8, bigserial
  :: ColumnTypeExpression schemas ('Def :=> 'NotNull 'PGint8)
serial8 = UnsafeColumnTypeExpression "serial8"
bigserial = UnsafeColumnTypeExpression "bigserial"

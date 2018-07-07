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
    Definition (UnsafeDefinition, renderDefinition)
  , (>>>)
    -- * Tables
    -- ** Create
  , createTable
  , createTableIfNotExists
  , TableConstraintExpression (..)
  , check
  , unique
  , primaryKey
  , foreignKey
  , ForeignKeyed
  , OnDeleteClause (OnDeleteNoAction, OnDeleteRestrict, OnDeleteCascade)
  , renderOnDeleteClause
  , OnUpdateClause (OnUpdateNoAction, OnUpdateRestrict, OnUpdateCascade)
  , renderOnUpdateClause
    -- ** Drop
  , dropTable
    -- ** Alter
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

-- | A `Definition` is a statement that changes the schema of the
-- database, like a `createTable`, `dropTable`, or `alterTable` command.
-- `Definition`s may be composed using the `>>>` operator.
newtype Definition
  (schema0 :: SchemaType)
  (schema1 :: SchemaType)
  = UnsafeDefinition { renderDefinition :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (Definition schema0 schema1) where
  renderSQL = renderDefinition

instance Category Definition where
  id = UnsafeDefinition ";"
  ddl1 . ddl0 = UnsafeDefinition $
    renderDefinition ddl0 <> "\n" <> renderDefinition ddl1

{-----------------------------------------
CREATE statements
-----------------------------------------}

{- | `createTable` adds a table to the schema.

>>> :set -XOverloadedLabels
>>> :{
type Table = '[] :=>
  '[ "a" ::: 'NoDef :=> 'Null 'PGint4
   , "b" ::: 'NoDef :=> 'Null 'PGfloat4 ]
:}

>>> :{
let
  setup :: Definition '[] '["tab" ::: 'Table Table]
  setup = createTable #tab
    (nullable int `as` #a :* nullable real `as` #b) Nil
in printSQL setup
:}
CREATE TABLE "tab" ("a" int NULL, "b" real NULL);
-}
createTable
  :: ( KnownSymbol table
     , columns ~ (col ': cols)
     , SOP.SListI columns
     , SOP.SListI constraints
     , schema1 ~ Create table ('Table (constraints :=> columns)) schema0 )
  => Alias table -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression schema0)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression schema1 table)) constraints
    -- ^ constraints that must hold for the table
  -> Definition schema0 schema1
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

>>> type Schema = '["tab" ::: 'Table Table]

>>> :{
let
  setup :: Definition Schema Schema
  setup = createTableIfNotExists #tab
    (nullable int `as` #a :* nullable real `as` #b) Nil
in printSQL setup
:}
CREATE TABLE IF NOT EXISTS "tab" ("a" int NULL, "b" real NULL);
-}
createTableIfNotExists
  :: ( Has table schema ('Table (constraints :=> columns))
     , SOP.SListI columns
     , SOP.SListI constraints )
  => Alias table -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression schema)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression schema table)) constraints
    -- ^ constraints that must hold for the table
  -> Definition schema schema
createTableIfNotExists tab columns constraints = UnsafeDefinition $
  "CREATE TABLE IF NOT EXISTS"
  <+> renderCreation tab columns constraints

-- helper function for `createTable` and `createTableIfNotExists`
renderCreation
  :: ( KnownSymbol table
     , SOP.SListI columns
     , SOP.SListI constraints )
  => Alias table -- ^ the name of the table to add
  -> NP (Aliased (ColumnTypeExpression schema0)) columns
    -- ^ the names and datatype of each column
  -> NP (Aliased (TableConstraintExpression schema1 table)) constraints
    -- ^ constraints that must hold for the table
  -> ByteString
renderCreation tab columns constraints = renderAlias tab
  <+> parenthesized
    ( renderCommaSeparated renderColumnDef columns
      <> ( case constraints of
             Nil -> ""
             _ -> ", " <>
               renderCommaSeparated renderConstraint constraints ) )
  <> ";"
  where
    renderColumnDef :: Aliased (ColumnTypeExpression schema) x -> ByteString
    renderColumnDef (ty `As` column) =
      renderAlias column <+> renderColumnTypeExpression ty
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
  (schema :: SchemaType)
  (table :: Symbol)
  (tableConstraint :: TableConstraint)
    = UnsafeTableConstraintExpression
    { renderTableConstraintExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

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
  definition :: Definition '[] Schema
  definition = createTable #tab
    ( (int & notNullable) `as` #a :*
      (int & notNullable) `as` #b )
    ( check (#a :* #b) (#a .> #b) `as` #inequality )
:}

>>> printSQL definition
CREATE TABLE "tab" ("a" int NOT NULL, "b" int NOT NULL, CONSTRAINT "inequality" CHECK (("a" > "b")));
-}
check
  :: ( Has alias schema ('Table table)
     , HasAll aliases (TableToRelation table) subcolumns )
  => NP Alias aliases
  -- ^ specify the subcolumns which are getting checked
  -> (forall tab. Condition schema '[tab ::: subcolumns] 'Ungrouped '[])
  -- ^ a closed `Condition` on those subcolumns
  -> TableConstraintExpression schema alias ('Check aliases)
check _cols condition = UnsafeTableConstraintExpression $
  "CHECK" <+> parenthesized (renderExpression condition)

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
  definition :: Definition '[] Schema
  definition = createTable #tab
    ( (int & nullable) `as` #a :*
      (int & nullable) `as` #b )
    ( unique (#a :* #b) `as` #uq_a_b )
:}

>>> printSQL definition
CREATE TABLE "tab" ("a" int NULL, "b" int NULL, CONSTRAINT "uq_a_b" UNIQUE ("a", "b"));
-}
unique
  :: ( Has alias schema ('Table table)
     , HasAll aliases (TableToRelation table) subcolumns )
  => NP Alias aliases
  -- ^ specify subcolumns which together are unique for each row
  -> TableConstraintExpression schema alias ('Unique aliases)
unique columns = UnsafeTableConstraintExpression $
  "UNIQUE" <+> parenthesized (commaSeparated (renderAliases columns))

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
  definition :: Definition '[] Schema
  definition = createTable #tab
    ( serial `as` #id :*
      (text & notNullable) `as` #name )
    ( primaryKey #id `as` #pk_id )
:}

>>> printSQL definition
CREATE TABLE "tab" ("id" serial, "name" text NOT NULL, CONSTRAINT "pk_id" PRIMARY KEY ("id"));
-}
primaryKey
  :: ( Has alias schema ('Table table)
     , HasAll aliases (TableToColumns table) subcolumns
     , AllNotNull subcolumns )
  => NP Alias aliases
  -- ^ specify the subcolumns which together form a primary key.
  -> TableConstraintExpression schema alias ('PrimaryKey aliases)
primaryKey columns = UnsafeTableConstraintExpression $
  "PRIMARY KEY" <+> parenthesized (commaSeparated (renderAliases columns))

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
  setup :: Definition '[] Schema
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
  setup :: Definition '[] Schema
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
  :: (ForeignKeyed schema child parent
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
  -> TableConstraintExpression schema child
      ('ForeignKey columns parent refcolumns)
foreignKey keys parent refs ondel onupd = UnsafeTableConstraintExpression $
  "FOREIGN KEY" <+> parenthesized (commaSeparated (renderAliases keys))
  <+> "REFERENCES" <+> renderAlias parent
  <+> parenthesized (commaSeparated (renderAliases refs))
  <+> renderOnDeleteClause ondel
  <+> renderOnUpdateClause onupd

-- | A constraint synonym between types involved in a foreign key constraint.
type ForeignKeyed schema
  child parent
  table reftable
  columns refcolumns
  constraints cols
  reftys tys =
    ( Has child schema ('Table table)
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
-- >>> :{
-- let
--   definition :: Definition '["muh_table" ::: 'Table t] '[]
--   definition = dropTable #muh_table
-- :}
--
-- >>> printSQL definition
-- DROP TABLE "muh_table";
dropTable
  :: Has table schema ('Table t)
  => Alias table -- ^ table to remove
  -> Definition schema (Drop table schema)
dropTable tab = UnsafeDefinition $ "DROP TABLE" <+> renderAlias tab <> ";"

{-----------------------------------------
ALTER statements
-----------------------------------------}

-- | `alterTable` changes the definition of a table from the schema.
alterTable
  :: KnownSymbol alias
  => Alias alias -- ^ table to alter
  -> AlterTable alias table schema -- ^ alteration to perform
  -> Definition schema (Alter alias ('Table table) schema)
alterTable tab alteration = UnsafeDefinition $
  "ALTER TABLE"
  <+> renderAlias tab
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
  "ALTER TABLE" <+> renderAlias table0
  <+> "RENAME TO" <+> renderAlias table1 <> ";"

-- | An `AlterTable` describes the alteration to perform on the columns
-- of a table.
newtype AlterTable
  (alias :: Symbol)
  (table :: TableType)
  (schema :: SchemaType) =
    UnsafeAlterTable {renderAlterTable :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | An `addConstraint` adds a table constraint.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]
--     '["tab" ::: 'Table ('["positive" ::: Check '["col"]] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]
--   definition = alterTable #tab (addConstraint #positive (check #col (#col .> 0)))
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ADD CONSTRAINT "positive" CHECK (("col" > 0));
addConstraint
  :: ( KnownSymbol alias
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , table1 ~ (Create alias constraint constraints :=> columns) )
  => Alias alias
  -> TableConstraintExpression schema tab constraint
  -- ^ constraint to add
  -> AlterTable tab table1 schema
addConstraint alias constraint = UnsafeAlterTable $
  "ADD" <+> "CONSTRAINT" <+> renderAlias alias
    <+> renderTableConstraintExpression constraint

-- | A `dropConstraint` drops a table constraint.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: 'Table ('["positive" ::: Check '["col"]] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]
--   definition = alterTable #tab (dropConstraint #positive)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" DROP CONSTRAINT "positive";
dropConstraint
  :: ( KnownSymbol constraint
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , table1 ~ (Drop constraint constraints :=> columns) )
  => Alias constraint
  -- ^ constraint to drop
  -> AlterTable tab table1 schema
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
  --     '["tab" ::: 'Table ('[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4])]
  --     '["tab" ::: 'Table ('[] :=>
  --        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
  --         , "col2" ::: 'Def :=> 'Null 'PGtext ])]
  --   definition = alterTable #tab (addColumn #col2 (text & nullable & default_ "foo"))
  -- in printSQL definition
  -- :}
  -- ALTER TABLE "tab" ADD COLUMN "col2" text NULL DEFAULT E'foo';
  --
  -- >>> :{
  -- let
  --   definition :: Definition
  --     '["tab" ::: 'Table ('[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4])]
  --     '["tab" ::: 'Table ('[] :=>
  --        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
  --         , "col2" ::: 'NoDef :=> 'Null 'PGtext ])]
  --   definition = alterTable #tab (addColumn #col2 (text & nullable))
  -- in printSQL definition
  -- :}
  -- ALTER TABLE "tab" ADD COLUMN "col2" text NULL;
  addColumn
    :: ( KnownSymbol column
       , Has tab schema ('Table table0)
       , table0 ~ (constraints :=> columns)
       , table1 ~ (constraints :=> Create column ty columns) )
    => Alias column -- ^ column to add
    -> ColumnTypeExpression schema ty -- ^ type of the new column
    -> AlterTable tab table1 schema
  addColumn column ty = UnsafeAlterTable $
    "ADD COLUMN" <+> renderAlias column <+> renderColumnTypeExpression ty
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
--     '["tab" ::: 'Table ('[] :=>
--        '[ "col1" ::: 'NoDef :=> 'Null 'PGint4
--         , "col2" ::: 'NoDef :=> 'Null 'PGtext ])]
--     '["tab" ::: 'Table ('[] :=> '["col1" ::: 'NoDef :=> 'Null 'PGint4])]
--   definition = alterTable #tab (dropColumn #col2)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" DROP COLUMN "col2";
dropColumn
  :: ( KnownSymbol column
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , table1 ~ (constraints :=> Drop column columns) )
  => Alias column -- ^ column to remove
  -> AlterTable tab table1 schema
dropColumn column = UnsafeAlterTable $
  "DROP COLUMN" <+> renderAlias column

-- | A `renameColumn` renames a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: 'Table ('[] :=> '["foo" ::: 'NoDef :=> 'Null 'PGint4])]
--     '["tab" ::: 'Table ('[] :=> '["bar" ::: 'NoDef :=> 'Null 'PGint4])]
--   definition = alterTable #tab (renameColumn #foo #bar)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" RENAME COLUMN "foo" TO "bar";
renameColumn
  :: ( KnownSymbol column0
     , KnownSymbol column1
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , table1 ~ (constraints :=> Rename column0 column1 columns) )
  => Alias column0 -- ^ column to rename
  -> Alias column1 -- ^ what to rename the column
  -> AlterTable tab table1 schema
renameColumn column0 column1 = UnsafeAlterTable $
  "RENAME COLUMN" <+> renderAlias column0  <+> "TO" <+> renderAlias column1

-- | An `alterColumn` alters a single column.
alterColumn
  :: ( KnownSymbol column
     , Has tab schema ('Table table0)
     , table0 ~ (constraints :=> columns)
     , Has column columns ty0
     , tables1 ~ (constraints :=> Alter column ty1 columns))
  => Alias column -- ^ column to alter
  -> AlterColumn schema ty0 ty1 -- ^ alteration to perform
  -> AlterTable tab table1 schema
alterColumn column alteration = UnsafeAlterTable $
  "ALTER COLUMN" <+> renderAlias column <+> renderAlterColumn alteration

-- | An `AlterColumn` describes the alteration to perform on a single column.
newtype AlterColumn (schema :: SchemaType) (ty0 :: ColumnType) (ty1 :: ColumnType) =
  UnsafeAlterColumn {renderAlterColumn :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | A `setDefault` sets a new default for a column. Note that this doesn't
-- affect any existing rows in the table, it just changes the default for
-- future insert and update commands.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])]
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'Def :=> 'Null 'PGint4])]
--   definition = alterTable #tab (alterColumn #col (setDefault 5))
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" SET DEFAULT 5;
setDefault
  :: Expression schema '[] 'Ungrouped '[] ty -- ^ default value to set
  -> AlterColumn schema (constraint :=> ty) ('Def :=> ty)
setDefault expression = UnsafeAlterColumn $
  "SET DEFAULT" <+> renderExpression expression

-- | A `dropDefault` removes any default value for a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'Def :=> 'Null 'PGint4])]
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])]
--   definition = alterTable #tab (alterColumn #col dropDefault)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" DROP DEFAULT;
dropDefault :: AlterColumn schema ('Def :=> ty) ('NoDef :=> ty)
dropDefault = UnsafeAlterColumn $ "DROP DEFAULT"

-- | A `setNotNull` adds a @NOT NULL@ constraint to a column.
-- The constraint will be checked immediately, so the table data must satisfy
-- the constraint before it can be added.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])]
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]
--   definition = alterTable #tab (alterColumn #col setNotNull)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" SET NOT NULL;
setNotNull
  :: AlterColumn schema (constraint :=> 'Null ty) (constraint :=> 'NotNull ty)
setNotNull = UnsafeAlterColumn $ "SET NOT NULL"

-- | A `dropNotNull` drops a @NOT NULL@ constraint from a column.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])]
--   definition = alterTable #tab (alterColumn #col dropNotNull)
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" DROP NOT NULL;
dropNotNull
  :: AlterColumn schema (constraint :=> 'NotNull ty) (constraint :=> 'Null ty)
dropNotNull = UnsafeAlterColumn $ "DROP NOT NULL"

-- | An `alterType` converts a column to a different data type.
-- This will succeed only if each existing entry in the column can be
-- converted to the new type by an implicit cast.
--
-- >>> :{
-- let
--   definition :: Definition
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGint4])]
--     '["tab" ::: 'Table ('[] :=> '["col" ::: 'NoDef :=> 'NotNull 'PGnumeric])]
--   definition =
--     alterTable #tab (alterColumn #col (alterType (numeric & notNullable)))
-- in printSQL definition
-- :}
-- ALTER TABLE "tab" ALTER COLUMN "col" TYPE numeric NOT NULL;
alterType :: ColumnTypeExpression schema ty -> AlterColumn schema ty0 ty
alterType ty = UnsafeAlterColumn $ "TYPE" <+> renderColumnTypeExpression ty

-- | Create a view.
--
-- >>> :{
-- let
--   definition :: Definition
--     '[ "abc" ::: 'Table ('[] :=> '["a" ::: 'NoDef :=> 'Null 'PGint4, "b" ::: 'NoDef :=> 'Null 'PGint4, "c" ::: 'NoDef :=> 'Null 'PGint4])]
--     '[ "abc" ::: 'Table ('[] :=> '["a" ::: 'NoDef :=> 'Null 'PGint4, "b" ::: 'NoDef :=> 'Null 'PGint4, "c" ::: 'NoDef :=> 'Null 'PGint4])
--      , "bc"  ::: 'View ('["b" ::: 'Null 'PGint4, "c" ::: 'Null 'PGint4])]
--   definition =
--     createView #bc (select (#b :* #c) (from (table #abc)))
-- in printSQL definition
-- :}
-- CREATE VIEW "bc" AS SELECT "b" AS "b", "c" AS "c" FROM "abc" AS "abc";
createView
  :: KnownSymbol view
  => Alias view -- ^ the name of the view to add
  -> Query schema '[] relation
    -- ^ query
  -> Definition schema (Create view ('View relation) schema)
createView alias query = UnsafeDefinition $
  "CREATE" <+> "VIEW" <+> renderAlias alias <+> "AS"
  <+> renderQuery query <> ";"

-- | Drop a view.
--
-- >>> :{
-- let
--   definition :: Definition
--     '[ "abc" ::: 'Table ('[] :=> '["a" ::: 'NoDef :=> 'Null 'PGint4, "b" ::: 'NoDef :=> 'Null 'PGint4, "c" ::: 'NoDef :=> 'Null 'PGint4])
--      , "bc"  ::: 'View ('["b" ::: 'Null 'PGint4, "c" ::: 'Null 'PGint4])]
--     '[ "abc" ::: 'Table ('[] :=> '["a" ::: 'NoDef :=> 'Null 'PGint4, "b" ::: 'NoDef :=> 'Null 'PGint4, "c" ::: 'NoDef :=> 'Null 'PGint4])]
--   definition = dropView #bc
-- in printSQL definition
-- :}
-- DROP VIEW "bc";
dropView
  :: Has view schema ('View v)
  => Alias view -- ^ view to remove
  -> Definition schema (Drop view schema)
dropView v = UnsafeDefinition $ "DROP VIEW" <+> renderAlias v <> ";"

-- | Enumerated types are created using the `createTypeEnum` command, for example
--
-- >>> printSQL $ createTypeEnum #mood (label @"sad" :* label @"ok" :* label @"happy")
-- CREATE TYPE "mood" AS ENUM ('sad', 'ok', 'happy');
createTypeEnum
  :: (KnownSymbol enum, SOP.All KnownSymbol labels)
  => Alias enum
  -- ^ name of the user defined enumerated type
  -> NP PGlabel labels
  -- ^ labels of the enumerated type
  -> Definition schema (Create enum ('Typedef ('PGenum labels)) schema)
createTypeEnum enum labels = UnsafeDefinition $
  "CREATE" <+> "TYPE" <+> renderAlias enum <+> "AS" <+> "ENUM" <+>
  parenthesized (commaSeparated (renderLabels labels)) <> ";"

-- | Enumerated types can also be generated from a Haskell type, for example
--
-- >>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
-- >>> instance SOP.Generic Schwarma
-- >>> instance SOP.HasDatatypeInfo Schwarma
-- >>> printSQL $ createTypeEnumFrom @Schwarma #schwarma
-- CREATE TYPE "schwarma" AS ENUM ('Beef', 'Lamb', 'Chicken');
createTypeEnumFrom
  :: forall hask enum schema.
  ( SOP.Generic hask
  , SOP.All KnownSymbol (LabelsFrom hask)
  , KnownSymbol enum
  )
  => Alias enum
  -- ^ name of the user defined enumerated type
  -> Definition schema (Create enum ('Typedef (EnumFrom hask)) schema)
createTypeEnumFrom enum = createTypeEnum enum
  (SOP.hpure label :: NP PGlabel (LabelsFrom hask))

{- | `createTypeComposite` creates a composite type. The composite type is
specified by a list of attribute names and data types.

>>> type PGcomplex = 'PGcomposite '["real" ::: 'PGfloat8, "imaginary" ::: 'PGfloat8]

>>> :{
let
  setup :: Definition '[] '["complex" ::: 'Typedef PGcomplex]
  setup = createTypeComposite #complex
    (float8 `as` #real :* float8 `as` #imaginary)
in printSQL setup
:}
CREATE TYPE "complex" AS ("real" float8, "imaginary" float8);
-}
createTypeComposite
  :: (KnownSymbol ty, SOP.SListI fields)
  => Alias ty
  -- ^ name of the user defined composite type
  -> NP (Aliased (TypeExpression schema)) fields
  -- ^ list of attribute names and data types
  -> Definition schema (Create ty ('Typedef ('PGcomposite fields)) schema)
createTypeComposite ty fields = UnsafeDefinition $
  "CREATE" <+> "TYPE" <+> renderAlias ty <+> "AS" <+> parenthesized
  (renderCommaSeparated renderField fields) <> ";"
  where
    renderField :: Aliased (TypeExpression schema) x -> ByteString
    renderField (typ `As` field) =
      renderAlias field <+> renderTypeExpression typ

-- | Composite types can also be generated from a Haskell type, for example
--
-- >>> data Complex = Complex {real :: Maybe Double, imaginary :: Maybe Double} deriving GHC.Generic
-- >>> instance SOP.Generic Complex
-- >>> instance SOP.HasDatatypeInfo Complex
-- >>> printSQL $ createTypeCompositeFrom @Complex #complex
-- CREATE TYPE "complex" AS ("real" float8, "imaginary" float8);
createTypeCompositeFrom
  :: forall hask ty schema.
  ( ZipAliased (FieldNamesFrom hask) (FieldTypesFrom hask)
  , SOP.All (PGTyped schema) (FieldTypesFrom hask)
  , KnownSymbol ty
  )
  => Alias ty
  -- ^ name of the user defined composite type
  -> Definition schema (Create ty ( 'Typedef (CompositeFrom hask)) schema)
createTypeCompositeFrom ty = createTypeComposite ty $ zipAs
  (SOP.hpure Alias :: NP Alias (FieldNamesFrom hask))
  (SOP.hcpure (SOP.Proxy :: SOP.Proxy (PGTyped schema)) pgtype
    :: NP (TypeExpression schema) (FieldTypesFrom hask))

-- | Drop a type.
--
-- >>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
-- >>> instance SOP.Generic Schwarma
-- >>> instance SOP.HasDatatypeInfo Schwarma
-- >>> printSQL (dropType #schwarma :: Definition '["schwarma" ::: 'Typedef (EnumFrom Schwarma)] '[])
-- DROP TYPE "schwarma";
dropType
  :: Has tydef schema ('Typedef ty)
  => Alias tydef
  -- ^ name of the user defined type
  -> Definition schema (Drop tydef schema)
dropType tydef = UnsafeDefinition $ "DROP" <+> "TYPE" <+> renderAlias tydef <> ";"

-- | `ColumnTypeExpression`s are used in `createTable` commands.
newtype ColumnTypeExpression (schema :: SchemaType) (ty :: ColumnType)
  = UnsafeColumnTypeExpression { renderColumnTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

-- | used in `createTable` commands as a column constraint to note that
-- @NULL@ may be present in a column
nullable
  :: TypeExpression schema ty
  -> ColumnTypeExpression schema ('NoDef :=> 'Null ty)
nullable ty = UnsafeColumnTypeExpression $ renderTypeExpression ty <+> "NULL"

-- | used in `createTable` commands as a column constraint to ensure
-- @NULL@ is not present in a column
notNullable
  :: TypeExpression schema ty
  -> ColumnTypeExpression schema (def :=> 'NotNull ty)
notNullable ty = UnsafeColumnTypeExpression $ renderTypeExpression ty <+> "NOT NULL"

-- | used in `createTable` commands as a column constraint to give a default
default_
  :: Expression schema '[] 'Ungrouped '[] ty
  -> ColumnTypeExpression schema ('NoDef :=> ty)
  -> ColumnTypeExpression schema ('Def :=> ty)
default_ x ty = UnsafeColumnTypeExpression $
  renderColumnTypeExpression ty <+> "DEFAULT" <+> renderExpression x

-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint2`
serial2, smallserial
  :: ColumnTypeExpression schema ('Def :=> 'NotNull 'PGint2)
serial2 = UnsafeColumnTypeExpression "serial2"
smallserial = UnsafeColumnTypeExpression "smallserial"
-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint4`
serial4, serial
  :: ColumnTypeExpression schema ('Def :=> 'NotNull 'PGint4)
serial4 = UnsafeColumnTypeExpression "serial4"
serial = UnsafeColumnTypeExpression "serial"
-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint8`
serial8, bigserial
  :: ColumnTypeExpression schema ('Def :=> 'NotNull 'PGint8)
serial8 = UnsafeColumnTypeExpression "serial8"
bigserial = UnsafeColumnTypeExpression "bigserial"

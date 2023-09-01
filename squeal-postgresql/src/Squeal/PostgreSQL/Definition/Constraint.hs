{-|
Module: Squeal.PostgreSQL.Definition.Constraint
Description: constraint expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

constraint expressions
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
  , DataKinds
  , PolyKinds
  , TypeOperators
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Definition.Constraint
  ( -- * Table Constraints
    TableConstraintExpression (..)
  , check
  , unique
  , primaryKey
    -- ** Foreign Keys
  , foreignKey
  , ForeignKeyed
  , OnDeleteClause (..)
  , OnUpdateClause (..)
  , ReferentialAction (..)
  ) where

import Control.DeepSeq
import Data.ByteString
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

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
  (db :: SchemasType)
  (constraint :: TableConstraint)
    = UnsafeTableConstraintExpression
    { renderTableConstraintExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL
  (TableConstraintExpression sch tab db constraint) where
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
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , HasAll aliases (TableToRow table) subcolumns )
  => NP Alias aliases
  -- ^ specify the subcolumns which are getting checked
  -> (forall t. Condition 'Ungrouped '[] '[] db '[] '[t ::: subcolumns])
  -- ^ a closed `Condition` on those subcolumns
  -> TableConstraintExpression sch tab db ('Check aliases)
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
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , HasAll aliases (TableToRow table) subcolumns )
  => NP Alias aliases
  -- ^ specify subcolumns which together are unique for each row
  -> TableConstraintExpression sch tab db ('Unique aliases)
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
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , HasAll aliases (TableToColumns table) subcolumns
     , AllNotNull subcolumns )
  => NP Alias aliases
  -- ^ specify the subcolumns which together form a primary key.
  -> TableConstraintExpression sch tab db ('PrimaryKey aliases)
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
        , "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["id"]
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
         (OnDelete Cascade) (OnUpdate Cascade) `as` #fk_user_id )
in printSQL setup
:}
CREATE TABLE "users" ("id" serial, "name" text NOT NULL, CONSTRAINT "pk_users" PRIMARY KEY ("id"));
CREATE TABLE "emails" ("id" serial, "user_id" int NOT NULL, "email" text NULL, CONSTRAINT "pk_emails" PRIMARY KEY ("id"), CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "users" ("id") ON DELETE CASCADE ON UPDATE CASCADE);

A `foreignKey` can even be a table self-reference.

>>> :{
type Schema =
  '[ "employees" ::: 'Table (
       '[ "employees_pk"          ::: 'PrimaryKey '["id"]
        , "employees_employer_fk" ::: 'ForeignKey '["employer_id"] "public" "employees" '["id"]
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
         (OnDelete Cascade) (OnUpdate Cascade) `as` #employees_employer_fk )
in printSQL setup
:}
CREATE TABLE "employees" ("id" serial, "name" text NOT NULL, "employer_id" integer NULL, CONSTRAINT "employees_pk" PRIMARY KEY ("id"), CONSTRAINT "employees_employer_fk" FOREIGN KEY ("employer_id") REFERENCES "employees" ("id") ON DELETE CASCADE ON UPDATE CASCADE);
-}
foreignKey
  :: (ForeignKeyed db 
        sch0 sch1
        schema0 schema1 
        child parent
        table reftable
        columns refcolumns
        constraints cols
        reftys tys )
  => NP Alias columns
  -- ^ column or columns in the table
  -> QualifiedAlias sch0 parent
  -- ^ reference table
  -> NP Alias refcolumns
  -- ^ reference column or columns in the reference table
  -> OnDeleteClause
  -- ^ what to do when reference is deleted
  -> OnUpdateClause
  -- ^ what to do when reference is updated
  -> TableConstraintExpression sch1 child db
      ('ForeignKey columns sch0 parent refcolumns)
foreignKey keys parent refs ondel onupd = UnsafeTableConstraintExpression $
  "FOREIGN KEY" <+> parenthesized (renderSQL keys)
  <+> "REFERENCES" <+> renderSQL parent
  <+> parenthesized (renderSQL refs)
  <+> renderSQL ondel
  <+> renderSQL onupd

-- | A constraint synonym between types involved in a foreign key constraint.
type ForeignKeyed db
  sch0 sch1
  schema0 schema1
  child parent
  table reftable
  columns refcolumns
  constraints cols
  reftys tys =
    ( Has sch0 db schema0
    , Has sch1 db schema1
    , Has parent schema0 ('Table reftable)
    , Has child schema1 ('Table table)
    , HasAll columns (TableToColumns table) tys
    , reftable ~ (constraints :=> cols)
    , HasAll refcolumns cols reftys
    , SOP.AllZip SamePGType tys reftys
    , Uniquely refcolumns constraints )

-- | `OnDeleteClause` indicates what to do with rows that reference a deleted row.
newtype OnDeleteClause = OnDelete ReferentialAction
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnDeleteClause
instance RenderSQL OnDeleteClause where
  renderSQL (OnDelete action) = "ON DELETE" <+> renderSQL action

-- | Analagous to `OnDeleteClause` there is also `OnUpdateClause` which is invoked
-- when a referenced column is changed (updated).
newtype OnUpdateClause = OnUpdate ReferentialAction
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnUpdateClause
instance RenderSQL OnUpdateClause where
  renderSQL (OnUpdate action) = "ON UPDATE" <+> renderSQL action

{- | When the data in the referenced columns is changed,
certain actions are performed on the data in this table's columns.-}
data ReferentialAction
  = NoAction
  {- ^ Produce an error indicating that the deletion or update
  would create a foreign key constraint violation.
  If the constraint is deferred, this error will be produced
  at constraint check time if there still exist any referencing rows.-}
  | Restrict
  {- ^ Produce an error indicating that the deletion or update
  would create a foreign key constraint violation.
  This is the same as `NoAction` except that the check is not deferrable.-}
  | Cascade
  {- ^ Delete any rows referencing the deleted row,
  or update the value of the referencing column
  to the new value of the referenced column, respectively.-}
  | SetNull {- ^ Set the referencing column(s) to null.-}
  | SetDefault {- ^ Set the referencing column(s) to their default values.-}
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData ReferentialAction
instance RenderSQL ReferentialAction where
  renderSQL = \case
    NoAction -> "NO ACTION"
    Restrict -> "RESTRICT"
    Cascade -> "CASCADE"
    SetNull -> "SET NULL"
    SetDefault -> "SET DEFAULT"

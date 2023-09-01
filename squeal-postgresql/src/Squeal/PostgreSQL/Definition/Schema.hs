{-|
Module: Squeal.PostgreSQL.Definition.Schema
Description: create and drop schemas
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

create and drop schemas
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

module Squeal.PostgreSQL.Definition.Schema
  ( -- * Create
    createSchema
  , createSchemaIfNotExists
    -- * Drop
  , dropSchemaCascade
  , dropSchemaCascadeIfExists
  ) where

import GHC.TypeLits

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- |
`createSchema` enters a new schema into the current database.
The schema name must be distinct from the name of any existing schema
in the current database.

A schema is essentially a namespace: it contains named objects
(tables, data types, functions, and operators) whose names
can duplicate those of other objects existing in other schemas.
Named objects are accessed by `QualifiedAlias`es with the schema
name as a prefix.

>>> :{
let
  definition :: Definition '["public" ::: '[]] '["public" ::: '[], "my_schema" ::: '[]]
  definition = createSchema #my_schema
in printSQL definition
:}
CREATE SCHEMA "my_schema";
-}
createSchema
  :: KnownSymbol sch
  => Alias sch -- ^ schema alias
  -> Definition db (Create sch '[] db)
createSchema sch = UnsafeDefinition $
  "CREATE" <+> "SCHEMA" <+> renderSQL sch <> ";"

{- | Create a schema if it does not yet exist.-}
createSchemaIfNotExists
  :: (KnownSymbol sch, Has sch db schema)
  => Alias sch -- ^ schema alias
  -> Definition db (CreateIfNotExists sch '[] db)
createSchemaIfNotExists sch = UnsafeDefinition $
  "CREATE" <+> "SCHEMA" <+> "IF" <+> "NOT" <+> "EXISTS"
  <+> renderSQL sch <> ";"

-- | Drop a schema.
-- Automatically drop objects (tables, functions, etc.)
-- that are contained in the schema.
--
-- >>> :{
-- let
--   definition :: Definition '["muh_schema" ::: schema, "public" ::: public] '["public" ::: public]
--   definition = dropSchemaCascade #muh_schema
-- :}
--
-- >>> printSQL definition
-- DROP SCHEMA "muh_schema" CASCADE;
dropSchemaCascade
  :: KnownSymbol sch
  => Alias sch -- ^ schema alias
  -> Definition db (Drop sch db)
dropSchemaCascade sch = UnsafeDefinition $
  "DROP SCHEMA" <+> renderSQL sch <+> "CASCADE;"

-- | Drop a schema if it exists.
-- Automatically drop objects (tables, functions, etc.)
-- that are contained in the schema.
dropSchemaCascadeIfExists
  :: KnownSymbol sch
  => Alias sch -- ^ schema alias
  -> Definition db (DropIfExists sch db)
dropSchemaCascadeIfExists sch = UnsafeDefinition $
  "DROP SCHEMA IF EXISTS" <+> renderSQL sch <+> "CASCADE;"

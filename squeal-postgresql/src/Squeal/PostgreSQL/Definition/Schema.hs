{-|
Module: Squeal.PostgreSQL.Definition.Schema
Description: Create and drop schema definitions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Create and drop schema definitions.
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

module Squeal.PostgreSQL.Definition.Schema
  ( createSchema
  , createSchemaIfNotExists
  , dropSchema
  , dropSchemaIfExists
  ) where

import GHC.TypeLits

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

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
-}
createSchema
  :: KnownSymbol sch
  => Alias sch
  -> Definition db (Create sch '[] db)
createSchema sch = UnsafeDefinition $
  "CREATE" <+> "SCHEMA" <+> renderSQL sch <> ";"

{- | Idempotent version of `createSchema`. -}
createSchemaIfNotExists
  :: (KnownSymbol sch, Has sch db schema)
  => Alias sch
  -> Definition db (CreateIfNotExists sch '[] db)
createSchemaIfNotExists sch = UnsafeDefinition $
  "CREATE" <+> "SCHEMA" <+> "IF" <+> "NOT" <+> "EXISTS"
  <+> renderSQL sch <> ";"

-- | >>> :{
-- let
--   definition :: Definition '["muh_schema" ::: schema, "public" ::: public] '["public" ::: public]
--   definition = dropSchema #muh_schema
-- :}
--
-- >>> printSQL definition
-- DROP SCHEMA "muh_schema";
dropSchema
  :: KnownSymbol sch
  => Alias sch
  -- ^ user defined schema
  -> Definition db (Drop sch db)
dropSchema sch = UnsafeDefinition $ "DROP SCHEMA" <+> renderSQL sch <> ";"

dropSchemaIfExists
  :: KnownSymbol sch
  => Alias sch
  -- ^ user defined schema
  -> Definition db (DropIfExists sch db)
dropSchemaIfExists sch = UnsafeDefinition $ "DROP SCHEMA" <+> renderSQL sch <> ";"

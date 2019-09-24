{-|
Module: Squeal.PostgreSQL.Definition.Index
Description: Create and drop index definitions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Create and drop index definitions.
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

module Squeal.PostgreSQL.Definition.Index
  ( createIndex
  , createIndexIfNotExists
  , dropIndex
  , dropIndexIfExists
  , IndexMethod (..)
  , btree
  , hash
  , gist
  , spgist
  , gin
  , brin
  ) where

import Data.ByteString
import GHC.TypeLits

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Expression.Sort
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- |
>>> :{
type Table = '[] :=>
  '[ "a" ::: 'NoDef :=> 'Null 'PGint4
   , "b" ::: 'NoDef :=> 'Null 'PGfloat4 ]
:}

>>> :{
let
  setup :: Definition (Public '[]) (Public '["tab" ::: 'Table Table, "ix" ::: 'Index 'Btree])
  setup =
    createTable #tab (nullable int `as` #a :* nullable real `as` #b) Nil >>>
    createIndex #ix #tab btree [#a & AscNullsFirst, #b & AscNullsLast]
in printSQL setup
:}
CREATE TABLE "tab" ("a" int NULL, "b" real NULL);
CREATE INDEX "ix" ON "tab" USING btree (("a") ASC NULLS FIRST, ("b") ASC NULLS LAST);
-}
createIndex
  :: (Has sch schemas schema, Has tab schema ('Table table), KnownSymbol ix)
  => Alias ix
  -> QualifiedAlias sch tab
  -> IndexMethod method
  -> [SortExpression '[] '[] 'Ungrouped schemas '[] '[tab ::: TableToRow table]]
  -> Definition schemas (Alter sch (Create ix ('Index method) schema) schemas)
createIndex ix tab method cols = UnsafeDefinition $
  "CREATE" <+> "INDEX" <+> renderSQL ix <+> "ON" <+> renderSQL tab
    <+> "USING" <+> renderSQL method
    <+> parenthesized (commaSeparated (renderIndex <$> cols))
    <> ";"
  where
    renderIndex = \case
      Asc expression -> parenthesized (renderSQL expression) <+> "ASC"
      Desc expression -> parenthesized (renderSQL expression) <+> "DESC"
      AscNullsFirst expression -> parenthesized (renderSQL expression)
        <+> "ASC NULLS FIRST"
      DescNullsFirst expression -> parenthesized (renderSQL expression)
        <+> "DESC NULLS FIRST"
      AscNullsLast expression -> parenthesized (renderSQL expression)
        <+> "ASC NULLS LAST"
      DescNullsLast expression -> parenthesized (renderSQL expression)
        <+> "DESC NULLS LAST"

createIndexIfNotExists
  :: (Has sch schemas schema, Has tab schema ('Table table), KnownSymbol ix)
  => Alias ix
  -> QualifiedAlias sch tab
  -> IndexMethod method
  -> [SortExpression '[] '[] 'Ungrouped schemas '[] '[tab ::: TableToRow table]]
  -> Definition schemas (Alter sch (CreateIfNotExists ix ('Index method) schema) schemas)
createIndexIfNotExists ix tab method cols = UnsafeDefinition $
  "CREATE INDEX IF NOT EXISTS" <+> renderSQL ix <+> "ON" <+> renderSQL tab
    <+> "USING" <+> renderSQL method
    <+> parenthesized (commaSeparated (renderIndex <$> cols))
    <> ";"
  where
    renderIndex = \case
      Asc expression -> parenthesized (renderSQL expression) <+> "ASC"
      Desc expression -> parenthesized (renderSQL expression) <+> "DESC"
      AscNullsFirst expression -> parenthesized (renderSQL expression)
        <+> "ASC NULLS FIRST"
      DescNullsFirst expression -> parenthesized (renderSQL expression)
        <+> "DESC NULLS FIRST"
      AscNullsLast expression -> parenthesized (renderSQL expression)
        <+> "ASC NULLS LAST"
      DescNullsLast expression -> parenthesized (renderSQL expression)
        <+> "DESC NULLS LAST"

newtype IndexMethod ty = UnsafeIndexMethod {renderIndexMethod :: ByteString}
  deriving stock (Eq, Ord, Show, GHC.Generic)
instance RenderSQL (IndexMethod ty) where renderSQL = renderIndexMethod
btree :: IndexMethod 'Btree
btree = UnsafeIndexMethod "btree"
hash :: IndexMethod 'Hash
hash = UnsafeIndexMethod "hash"
gist :: IndexMethod 'Gist
gist = UnsafeIndexMethod "gist"
spgist :: IndexMethod 'Spgist
spgist = UnsafeIndexMethod "spgist"
gin :: IndexMethod 'Gin
gin = UnsafeIndexMethod "gin"
brin :: IndexMethod 'Brin
brin = UnsafeIndexMethod "brin"

-- |
-- >>> printSQL (dropIndex #ix :: Definition (Public '["ix" ::: 'Index 'Btree]) (Public '[]))
-- DROP INDEX "ix";
dropIndex
  :: (Has sch schemas schema, KnownSymbol ix)
  => QualifiedAlias sch ix
  -- ^ name of the user defined index
  -> Definition schemas (Alter sch (DropSchemum ix 'Index schema) schemas)
dropIndex ix = UnsafeDefinition $ "DROP" <+> "INDEX" <+> renderSQL ix <> ";"

dropIndexIfExists
  :: (Has sch schemas schema, KnownSymbol ix)
  => QualifiedAlias sch ix
  -- ^ name of the user defined index
  -> Definition schemas (Alter sch (DropSchemumIfExists ix 'Index schema) schemas)
dropIndexIfExists ix = UnsafeDefinition $ "DROP INDEX IF EXISTS" <+> renderSQL ix <> ";"

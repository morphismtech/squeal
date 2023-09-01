{-|
Module: Squeal.PostgreSQL.Definition.Index
Description: create and drop indexes
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

create and drop indexes
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

module Squeal.PostgreSQL.Definition.Index
  ( -- * Create
    createIndex
  , createIndexIfNotExists
    -- * Drop
  , dropIndex
  , dropIndexIfExists
    -- * Index Method
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

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Expression.Sort
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> :set -XPolyKinds

{- | Create an index.

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
  :: (Has sch db schema, Has tab schema ('Table table), KnownSymbol ix)
  => Alias ix -- ^ index alias
  -> QualifiedAlias sch tab -- ^ table alias
  -> IndexMethod method -- ^ index method
  -> [SortExpression 'Ungrouped '[] '[] db '[] '[tab ::: TableToRow table]]
  -- ^ sorted columns
  -> Definition db (Alter sch (Create ix ('Index method) schema) db)
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

-- | Create an index if it doesn't exist.
createIndexIfNotExists
  :: (Has sch db schema, Has tab schema ('Table table), KnownSymbol ix)
  => Alias ix -- ^ index alias
  -> QualifiedAlias sch tab -- ^ table alias
  -> IndexMethod method -- ^ index method
  -> [SortExpression 'Ungrouped '[] '[] db '[] '[tab ::: TableToRow table]]
  -- ^ sorted columns
  -> Definition db (Alter sch (CreateIfNotExists ix ('Index method) schema) db)
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

{- |
PostgreSQL provides several index types:
B-tree, Hash, GiST, SP-GiST, GIN and BRIN.
Each index type uses a different algorithm
that is best suited to different types of queries.
-}
newtype IndexMethod ty = UnsafeIndexMethod {renderIndexMethod :: ByteString}
  deriving stock (Eq, Ord, Show, GHC.Generic)
instance RenderSQL (IndexMethod ty) where renderSQL = renderIndexMethod
-- | B-trees can handle equality and range queries on data
-- that can be sorted into some ordering.
btree :: IndexMethod 'Btree
btree = UnsafeIndexMethod "btree"
-- | Hash indexes can only handle simple equality comparisons.
hash :: IndexMethod 'Hash
hash = UnsafeIndexMethod "hash"
-- | GiST indexes are not a single kind of index,
-- but rather an infrastructure within which many different
-- indexing strategies can be implemented.
gist :: IndexMethod 'Gist
gist = UnsafeIndexMethod "gist"
-- | SP-GiST indexes, like GiST indexes,
-- offer an infrastructure that supports various kinds of searches.
spgist :: IndexMethod 'Spgist
spgist = UnsafeIndexMethod "spgist"
-- | GIN indexes are “inverted indexes” which are appropriate for
-- data values that contain multiple component values, such as arrays.
gin :: IndexMethod 'Gin
gin = UnsafeIndexMethod "gin"
-- | BRIN indexes (a shorthand for Block Range INdexes) store summaries
-- about the values stored in consecutive physical block ranges of a table.
brin :: IndexMethod 'Brin
brin = UnsafeIndexMethod "brin"

-- | Drop an index.
--
-- >>> printSQL (dropIndex #ix :: Definition (Public '["ix" ::: 'Index 'Btree]) (Public '[]))
-- DROP INDEX "ix";
dropIndex
  :: (Has sch db schema, KnownSymbol ix)
  => QualifiedAlias sch ix -- ^ index alias
  -> Definition db (Alter sch (DropSchemum ix 'Index schema) db)
dropIndex ix = UnsafeDefinition $ "DROP INDEX" <+> renderSQL ix <> ";"

-- | Drop an index if it exists.
dropIndexIfExists
  :: (Has sch db schema, KnownSymbol ix)
  => QualifiedAlias sch ix -- ^ index alias
  -> Definition db (Alter sch (DropSchemumIfExists ix 'Index schema) db)
dropIndexIfExists ix = UnsafeDefinition $
  "DROP INDEX IF EXISTS" <+> renderSQL ix <> ";"

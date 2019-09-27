{-|
Module: Squeal.PostgreSQL.Definition.View
Description: Create and drop view definitions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Create and drop view definitions.
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

module Squeal.PostgreSQL.Definition.View
  ( createView
  , createOrReplaceView
  , dropView
  , dropViewIfExists
  ) where

import GHC.TypeLits

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

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
  :: (Has sch schemas schema, KnownSymbol vw)
  => QualifiedAlias sch vw -- ^ the name of the view to add
  -> Query '[] '[] schemas '[] view -- ^ query
  -> Definition schemas (Alter sch (Create vw ('View view) schema) schemas)
createView alias query = UnsafeDefinition $
  "CREATE" <+> "VIEW" <+> renderSQL alias <+> "AS"
  <+> renderQuery query <> ";"

createOrReplaceView
  :: (Has sch schemas schema, KnownSymbol vw)
  => QualifiedAlias sch vw -- ^ the name of the view to add
  -> Query '[] '[] schemas '[] view -- ^ query
  -> Definition schemas (Alter sch (CreateOrReplace vw ('View view) schema) schemas)
createOrReplaceView alias query = UnsafeDefinition $
  "CREATE OR REPLACE VIEW" <+> renderSQL alias <+> "AS"
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
  :: (Has sch schemas schema, KnownSymbol vw)
  => QualifiedAlias sch vw -- ^ view to remove
  -> Definition schemas (Alter sch (DropSchemum vw 'View schema) schemas)
dropView vw = UnsafeDefinition $ "DROP VIEW" <+> renderSQL vw <> ";"

dropViewIfExists
  :: (Has sch schemas schema, KnownSymbol vw)
  => QualifiedAlias sch vw -- ^ view to remove
  -> Definition schemas (Alter sch (DropIfExists vw schema) schemas)
dropViewIfExists vw = UnsafeDefinition $ "DROP VIEW IF EXISTS" <+> renderSQL vw <> ";"

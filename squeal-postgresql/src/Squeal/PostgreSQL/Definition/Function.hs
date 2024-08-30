{-|
Module: Squeal.PostgreSQL.Definition.Function
Description: create and drop functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

create and drop functions
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

module Squeal.PostgreSQL.Definition.Function
  ( -- * Create
    createFunction
  , createOrReplaceFunction
  , createSetFunction
  , createOrReplaceSetFunction
    -- * Drop
  , dropFunction
  , dropFunctionIfExists
    -- * Function Definition
  , FunctionDefinition(..)
  , languageSqlExpr
  , languageSqlQuery
  ) where

import Control.DeepSeq
import Data.ByteString
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Query.Values
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- | Create a function.

>>> type Fn = 'Function ( '[ 'Null 'PGint4, 'Null 'PGint4] :=> 'Returns ( 'Null 'PGint4))
>>> :{
let
  definition :: Definition (Public '[]) (Public '["fn" ::: Fn])
  definition = createFunction #fn (int4 *: int4) int4 $
    languageSqlExpr (param @1 * param @2 + 1)
in printSQL definition
:}
CREATE FUNCTION "fn" (int4, int4) RETURNS int4 language sql as $$ SELECT * FROM (VALUES (((($1 :: int4) * ($2 :: int4)) + (1 :: int4)))) AS t ("ret") $$;
-}
createFunction
  :: ( Has sch db schema
     , KnownSymbol fun
     , SOP.SListI args )
  => QualifiedAlias sch fun -- ^ function alias
  -> NP (TypeExpression db) args -- ^ arguments
  -> TypeExpression db ret -- ^ return type
  -> FunctionDefinition db args ('Returns ret) -- ^ function definition
  -> Definition db (Alter sch (Create fun ('Function (args :=> 'Returns ret)) schema) db)
createFunction fun args ret fundef = UnsafeDefinition $
  "CREATE" <+> "FUNCTION" <+> renderSQL fun
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> "RETURNS" <+> renderSQL ret <+> renderSQL fundef <> ";"

{- | Create or replace a function.
It is not possible to change the name or argument types
or return type of a function this way.

>>> type Fn = 'Function ( '[ 'Null 'PGint4, 'Null 'PGint4] :=> 'Returns ( 'Null 'PGint4))
>>> :{
let
  definition :: Definition (Public '["fn" ::: Fn]) (Public '["fn" ::: Fn])
  definition =
    createOrReplaceFunction #fn
      (int4 *: int4) int4 $
      languageSqlExpr (param @1 @('Null 'PGint4) * param @2 @('Null 'PGint4) + 1)
in printSQL definition
:}
CREATE OR REPLACE FUNCTION "fn" (int4, int4) RETURNS int4 language sql as $$ SELECT * FROM (VALUES (((($1 :: int4) * ($2 :: int4)) + (1 :: int4)))) AS t ("ret") $$;
-}
createOrReplaceFunction
  :: ( Has sch db schema
     , KnownSymbol fun
     , SOP.SListI args )
  => QualifiedAlias sch fun -- ^ function alias
  -> NP (TypeExpression db) args -- ^ arguments
  -> TypeExpression db ret -- ^ return type
  -> FunctionDefinition db args ('Returns ret) -- ^ function definition
  -> Definition db (Alter sch (CreateOrReplace fun ('Function (args :=> 'Returns ret)) schema) db)
createOrReplaceFunction fun args ret fundef = UnsafeDefinition $
  "CREATE" <+> "OR" <+> "REPLACE" <+> "FUNCTION" <+> renderSQL fun
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> "RETURNS" <+> renderSQL ret <+> renderSQL fundef <> ";"

-- | Use a parameterized `Expression` as a function body
languageSqlExpr
  :: Expression 'Ungrouped '[] '[] db args '[] ret
  -- ^ function body
  -> FunctionDefinition db args ('Returns ret)
languageSqlExpr expr = UnsafeFunctionDefinition $
  "language sql as"
    <+> "$$" <+> renderSQL (values_ (expr `as` #ret)) <+> "$$"

-- | Use a parametrized `Query` as a function body
languageSqlQuery
  :: Query '[] '[] db args rets
  -- ^ function body
  -> FunctionDefinition db args ('ReturnsTable rets)
languageSqlQuery qry = UnsafeFunctionDefinition $
  "language sql as" <+> "$$" <+> renderSQL qry <+> "$$"

{- | Create a set function.

>>> type Tab = 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])
>>> type Fn = 'Function ('[ 'Null 'PGint4, 'Null 'PGint4] :=> 'ReturnsTable '["ret" ::: 'Null 'PGint4])
>>> :{
let
  definition :: Definition (Public '["tab" ::: Tab]) (Public '["tab" ::: Tab, "fn" ::: Fn])
  definition = createSetFunction #fn (int4 *: int4) (int4 `as` #ret) $
    languageSqlQuery (select_ ((param @1 * param @2 + #col) `as` #ret) (from (table #tab)))
in printSQL definition
:}
CREATE FUNCTION "fn" (int4, int4) RETURNS TABLE ("ret" int4) language sql as $$ SELECT ((($1 :: int4) * ($2 :: int4)) + "col") AS "ret" FROM "tab" AS "tab" $$;
-}
createSetFunction
  :: ( Has sch db schema
     , KnownSymbol fun
     , SOP.SListI args
     , SOP.SListI rets )
  => QualifiedAlias sch fun -- ^ function alias
  -> NP (TypeExpression db) args -- ^ arguments
  -> NP (Aliased (TypeExpression db)) rets -- ^ return type
  -> FunctionDefinition db args ('ReturnsTable rets) -- ^ function definition
  -> Definition db (Alter sch (Create fun ('Function (args :=> 'ReturnsTable rets)) schema) db)
createSetFunction fun args rets fundef = UnsafeDefinition $
  "CREATE" <+> "FUNCTION" <+> renderSQL fun
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> "RETURNS" <+> "TABLE"
    <+> parenthesized (renderCommaSeparated renderRet rets)
    <+> renderSQL fundef <> ";"
  where
    renderRet :: Aliased (TypeExpression s) r -> ByteString
    renderRet (ty `As` col) = renderSQL col <+> renderSQL ty

{- | Create or replace a set function.

>>> type Tab = 'Table ('[] :=> '["col" ::: 'NoDef :=> 'Null 'PGint4])
>>> type Fn = 'Function ('[ 'Null 'PGint4, 'Null 'PGint4] :=> 'ReturnsTable '["ret" ::: 'Null 'PGint4])
>>> :{
let
  definition :: Definition (Public '["tab" ::: Tab, "fn" ::: Fn]) (Public '["tab" ::: Tab, "fn" ::: Fn])
  definition = createOrReplaceSetFunction #fn (int4 *: int4) (int4 `as` #ret) $
    languageSqlQuery (select_ ((param @1 * param @2 + #col) `as` #ret) (from (table #tab)))
in printSQL definition
:}
CREATE OR REPLACE FUNCTION "fn" (int4, int4) RETURNS TABLE ("ret" int4) language sql as $$ SELECT ((($1 :: int4) * ($2 :: int4)) + "col") AS "ret" FROM "tab" AS "tab" $$;
-}
createOrReplaceSetFunction
  :: ( Has sch db schema
     , KnownSymbol fun
     , SOP.SListI args
     , SOP.SListI rets )
  => QualifiedAlias sch fun -- ^ function alias
  -> NP (TypeExpression db) args -- ^ arguments
  -> NP (Aliased (TypeExpression db)) rets -- ^ return type
  -> FunctionDefinition db args ('ReturnsTable rets) -- ^ function definition
  -> Definition db (Alter sch (CreateOrReplace fun ('Function (args :=> 'ReturnsTable rets)) schema) db)
createOrReplaceSetFunction fun args rets fundef = UnsafeDefinition $
  "CREATE" <+> "OR" <+> "REPLACE" <+> "FUNCTION" <+> renderSQL fun
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> "RETURNS" <+> "TABLE"
    <+> parenthesized (renderCommaSeparated renderRet rets)
    <+> renderSQL fundef <> ";"
  where
    renderRet :: Aliased (TypeExpression s) r -> ByteString
    renderRet (ty `As` col) = renderSQL col <+> renderSQL ty

{- | Drop a function.

>>> type Fn = 'Function ( '[ 'Null 'PGint4, 'Null 'PGint4] :=> 'Returns ( 'Null 'PGint4))
>>> :{
let
  definition :: Definition (Public '["fn" ::: Fn]) (Public '[])
  definition = dropFunction #fn
in printSQL definition
:}
DROP FUNCTION "fn";
-}
dropFunction
  :: (Has sch db schema, KnownSymbol fun)
  => QualifiedAlias sch fun
  -- ^ function alias
  -> Definition db (Alter sch (DropSchemum fun 'Function schema) db)
dropFunction fun = UnsafeDefinition $
  "DROP FUNCTION" <+> renderSQL fun <> ";"

{- | Drop a function.

>>> type Fn = 'Function ( '[ 'Null 'PGint4, 'Null 'PGint4] :=> 'Returns ( 'Null 'PGint4))
>>> :{
let
  definition :: Definition (Public '[]) (Public '[])
  definition = dropFunctionIfExists #fn
in printSQL definition
:}
DROP FUNCTION IF EXISTS "fn";
-}
dropFunctionIfExists
  :: (Has sch db schema, KnownSymbol fun)
  => QualifiedAlias sch fun
  -- ^ function alias
  -> Definition db (Alter sch (DropSchemumIfExists fun 'Function schema) db)
dropFunctionIfExists fun = UnsafeDefinition $
  "DROP FUNCTION IF EXISTS" <+> renderSQL fun <> ";"

{- | Body of a user defined function-}
newtype FunctionDefinition db args ret = UnsafeFunctionDefinition
  { renderFunctionDefinition :: ByteString }
  deriving (Eq,Show,GHC.Generic,NFData)
instance RenderSQL (FunctionDefinition db args ret) where
  renderSQL = renderFunctionDefinition

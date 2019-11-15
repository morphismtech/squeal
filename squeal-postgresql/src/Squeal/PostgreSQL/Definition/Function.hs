{-|
Module: Squeal.PostgreSQL.Definition.Function
Description: Create and drop function and operator definitions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Create and drop function and operator definitions.
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

module Squeal.PostgreSQL.Definition.Function
  ( createFunction
  , createOrReplaceFunction
  , createSetFunction
  , createOrReplaceSetFunction
  , createBinaryOp
  , createLeftOp
  , createRightOp
  , dropFunction
  , dropFunctionIfExists
  , dropOperator
  , dropOperatorIfExists
  , FunctionDefinition(..)
  , languageSqlExpr
  , languageSqlQuery
  ) where

import Control.DeepSeq
import Data.ByteString
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

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
CREATE FUNCTION "fn" (int4, int4) RETURNS int4 language sql as $$ SELECT * FROM (VALUES (((($1 :: int4) * ($2 :: int4)) + 1))) AS t ("ret") $$;
-}
createFunction
  :: ( Has sch db schema
     , KnownSymbol fun
     , SOP.SListI args )
  => QualifiedAlias sch fun
  -> NP (TypeExpression db) args
  -> TypeExpression db ret
  -> FunctionDefinition db args ('Returns ret)
  -> Definition db (Alter sch (Create fun ('Function (args :=> 'Returns ret)) schema) db)
createFunction fun args ret fundef = UnsafeDefinition $
  "CREATE" <+> "FUNCTION" <+> renderSQL fun
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> "RETURNS" <+> renderSQL ret <+> renderSQL fundef <> ";"

-- | Create or replace a function.
-- It is not possible to change the name or argument types
-- or return type of a function this way.
createOrReplaceFunction
  :: ( Has sch db schema
     , KnownSymbol fun
     , SOP.SListI args )
  => QualifiedAlias sch fun
  -> NP (TypeExpression db) args
  -> TypeExpression db ret
  -> FunctionDefinition db args ('Returns ret)
  -> Definition db (Alter sch (CreateOrReplace fun ('Function (args :=> 'Returns ret)) schema) db)
createOrReplaceFunction fun args ret fundef = UnsafeDefinition $
  "CREATE" <+> "OR" <+> "REPLACE" <+> "FUNCTION" <+> renderSQL fun
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> "RETURNS" <+> renderSQL ret <+> renderSQL fundef <> ";"

-- | Use a parameterized `Expression` as a function body
languageSqlExpr
  :: Expression '[] '[] 'Ungrouped db args '[] ret
  -> FunctionDefinition db args ('Returns ret)
languageSqlExpr expr = UnsafeFunctionDefinition $
  "language sql as"
    <+> "$$" <+> renderSQL (values_ (expr `as` #ret)) <+> "$$"

-- | Use a parametrized `Query` as a function body
languageSqlQuery
  :: Query '[] '[] db args rets
  -> FunctionDefinition db args ('ReturnsTable rets)
languageSqlQuery qry = UnsafeFunctionDefinition $
  "language sql as" <+> "$$" <+> renderSQL qry <+> "$$"

createSetFunction
  :: ( Has sch db schema
     , KnownSymbol fun
     , SOP.SListI args
     , SOP.SListI rets )
  => QualifiedAlias sch fun
  -> NP (TypeExpression db) args
  -> NP (Aliased (TypeExpression db)) rets
  -> FunctionDefinition db args ('ReturnsTable rets)
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

createOrReplaceSetFunction
  :: ( Has sch db schema
     , KnownSymbol fun
     , SOP.SListI args
     , SOP.SListI rets )
  => QualifiedAlias sch fun
  -> NP (TypeExpression db) args
  -> NP (Aliased (TypeExpression db)) rets
  -> FunctionDefinition db args ('ReturnsTable rets)
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

createBinaryOp
  :: forall op fun sch db schema x y z.
     ( Has sch db schema
     , Has fun schema ('Function ('[x,y] :=> 'Returns z))
     , KnownSymbol op )
  => QualifiedAlias sch fun
  -> TypeExpression db x
  -> TypeExpression db y
  -> Definition db
      (Alter sch (Create op ('Op ('BinOp x y z)) schema) db)
createBinaryOp fun x y = UnsafeDefinition $
  "CREATE" <+> "OPERATOR" <+> renderSymbol @op
    <+> parenthesized (commaSeparated opdef)
    where
      opdef =
        [ "FUNCTION" <+> "=" <+> renderSQL fun
        , "LEFTARG" <+> "=" <+> renderSQL x
        , "RIGHTARG" <+> "=" <+> renderSQL y ]

createLeftOp
  :: forall op fun sch db schema x y.
     ( Has sch db schema
     , Has fun schema ('Function ('[x] :=> 'Returns y))
     , KnownSymbol op )
  => QualifiedAlias sch fun
  -> TypeExpression db x
  -> Definition db
      (Alter sch (Create op ('Op ('LeftOp x y)) schema) db)
createLeftOp fun x = UnsafeDefinition $
  "CREATE" <+> "OPERATOR" <+> renderSymbol @op
    <+> parenthesized (commaSeparated opdef)
    where
      opdef =
        [ "FUNCTION" <+> "=" <+> renderSQL fun
        , "RIGHTARG" <+> "=" <+> renderSQL x ]

createRightOp
  :: forall op fun sch db schema x y.
     ( Has sch db schema
     , Has fun schema ('Function ('[x] :=> 'Returns y))
     , KnownSymbol op )
  => QualifiedAlias sch fun
  -> TypeExpression db x
  -> Definition db
      (Alter sch (Create op ('Op ('RightOp x y)) schema) db)
createRightOp fun x = UnsafeDefinition $
  "CREATE" <+> "OPERATOR" <+> renderSymbol @op
    <+> parenthesized (commaSeparated opdef)
    where
      opdef =
        [ "FUNCTION" <+> "=" <+> renderSQL fun
        , "LEFTARG" <+> "=" <+> renderSQL x ]

dropFunction
  :: (Has sch db schema, KnownSymbol fun)
  => QualifiedAlias sch fun
  -- ^ name of the user defined function
  -> Definition db (Alter sch (DropSchemum fun 'Function schema) db)
dropFunction fun = UnsafeDefinition $
  "DROP FUNCTION" <+> renderSQL fun <> ";"

dropFunctionIfExists
  :: (Has sch db schema, KnownSymbol fun)
  => QualifiedAlias sch fun
  -- ^ name of the user defined function
  -> Definition db (Alter sch (DropSchemumIfExists fun 'Function schema) db)
dropFunctionIfExists fun = UnsafeDefinition $
  "DROP FUNCTION IF EXISTS" <+> renderSQL fun <> ";"

dropOperator
  :: (Has sch db schema, KnownSymbol op)
  => QualifiedAlias sch op
  -- ^ name of the user defined operator
  -> Definition db (Alter sch (DropSchemum op 'Op schema) db)
dropOperator op = UnsafeDefinition $
  "DROP OPERATOR" <+> renderSQL op <> ";"

dropOperatorIfExists
  :: (Has sch db schema, KnownSymbol op)
  => QualifiedAlias sch op
  -- ^ name of the user defined operator
  -> Definition db (Alter sch (DropSchemumIfExists op 'Op schema) db)
dropOperatorIfExists op = UnsafeDefinition $
  "DROP OPERATOR IF EXISTS" <+> renderSQL op <> ";"

newtype FunctionDefinition db args ret = UnsafeFunctionDefinition
  { renderFunctionDefinition :: ByteString }
  deriving (Eq,Show,GHC.Generic,NFData)
instance RenderSQL (FunctionDefinition db args ret) where
  renderSQL = renderFunctionDefinition

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

createFunction
  :: ( Has sch schemas schema
     , KnownSymbol fun
     , SOP.SListI args )
  => QualifiedAlias sch fun
  -> NP (TypeExpression schemas) args
  -> TypeExpression schemas ret
  -> FunctionDefinition schemas args ('Returns ret)
  -> Definition schemas (Alter sch (Create fun ('Function (args :=> 'Returns ret)) schema) schemas)
createFunction fun args ret fundef = UnsafeDefinition $
  "CREATE" <+> "FUNCTION" <+> renderSQL fun
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> "RETURNS" <+> renderSQL ret <+> renderSQL fundef <> ";"

createOrReplaceFunction
  :: ( Has sch schemas schema
     , KnownSymbol fun
     , SOP.SListI args )
  => QualifiedAlias sch fun
  -> NP (TypeExpression schemas) args
  -> TypeExpression schemas ret
  -> FunctionDefinition schemas args ('Returns ret)
  -> Definition schemas (Alter sch (CreateOrReplace fun ('Function (args :=> 'Returns ret)) schema) schemas)
createOrReplaceFunction fun args ret fundef = UnsafeDefinition $
  "CREATE" <+> "OR" <+> "REPLACE" <+> "FUNCTION" <+> renderSQL fun
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> "RETURNS" <+> renderSQL ret <+> renderSQL fundef <> ";"

languageSqlExpr
  :: Expression '[] '[] 'Ungrouped schemas args '[] ret
  -> FunctionDefinition schemas args ('Returns ret)
languageSqlExpr expr = UnsafeFunctionDefinition $
  "language sql as"
    <+> "$$" <+> renderSQL (values_ (expr `as` #ret)) <+> "$$"

languageSqlQuery
  :: Query '[] '[] schemas args rets
  -> FunctionDefinition schemas args ('ReturnsTable rets)
languageSqlQuery qry = UnsafeFunctionDefinition $
  "language sql as" <+> "$$" <+> renderSQL qry <+> "$$"

createSetFunction
  :: ( Has sch schemas schema
     , KnownSymbol fun
     , SOP.SListI args
     , SOP.SListI rets )
  => QualifiedAlias sch fun
  -> NP (TypeExpression schemas) args
  -> NP (Aliased (TypeExpression schemas)) rets
  -> FunctionDefinition schemas args ('ReturnsTable rets)
  -> Definition schemas (Alter sch (Create fun ('Function (args :=> 'ReturnsTable rets)) schema) schemas)
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
  :: ( Has sch schemas schema
     , KnownSymbol fun
     , SOP.SListI args
     , SOP.SListI rets )
  => QualifiedAlias sch fun
  -> NP (TypeExpression schemas) args
  -> NP (Aliased (TypeExpression schemas)) rets
  -> FunctionDefinition schemas args ('ReturnsTable rets)
  -> Definition schemas (Alter sch (CreateOrReplace fun ('Function (args :=> 'ReturnsTable rets)) schema) schemas)
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
  :: forall op fun sch schemas schema x y z.
     ( Has sch schemas schema
     , Has fun schema ('Function ('[x,y] :=> 'Returns z))
     , KnownSymbol op )
  => QualifiedAlias sch fun
  -> TypeExpression schemas x
  -> TypeExpression schemas y
  -> Definition schemas
      (Alter sch (Create op ('Operator ('BinaryOp x y z)) schema) schemas)
createBinaryOp fun x y = UnsafeDefinition $
  "CREATE" <+> "OPERATOR" <+> renderSymbol @op
    <+> parenthesized (commaSeparated opdef)
    where
      opdef =
        [ "FUNCTION" <+> "=" <+> renderSQL fun
        , "LEFTARG" <+> "=" <+> renderSQL x
        , "RIGHTARG" <+> "=" <+> renderSQL y ]

createLeftOp
  :: forall op fun sch schemas schema x y.
     ( Has sch schemas schema
     , Has fun schema ('Function ('[x] :=> 'Returns y))
     , KnownSymbol op )
  => QualifiedAlias sch fun
  -> TypeExpression schemas x
  -> Definition schemas
      (Alter sch (Create op ('Operator ('LeftOp x y)) schema) schemas)
createLeftOp fun x = UnsafeDefinition $
  "CREATE" <+> "OPERATOR" <+> renderSymbol @op
    <+> parenthesized (commaSeparated opdef)
    where
      opdef =
        [ "FUNCTION" <+> "=" <+> renderSQL fun
        , "RIGHTARG" <+> "=" <+> renderSQL x ]

createRightOp
  :: forall op fun sch schemas schema x y.
     ( Has sch schemas schema
     , Has fun schema ('Function ('[x] :=> 'Returns y))
     , KnownSymbol op )
  => QualifiedAlias sch fun
  -> TypeExpression schemas x
  -> Definition schemas
      (Alter sch (Create op ('Operator ('RightOp x y)) schema) schemas)
createRightOp fun x = UnsafeDefinition $
  "CREATE" <+> "OPERATOR" <+> renderSymbol @op
    <+> parenthesized (commaSeparated opdef)
    where
      opdef =
        [ "FUNCTION" <+> "=" <+> renderSQL fun
        , "LEFTARG" <+> "=" <+> renderSQL x ]

dropFunction
  :: (Has sch schemas schema, KnownSymbol fun)
  => QualifiedAlias sch fun
  -- ^ name of the user defined function
  -> Definition schemas (Alter sch (DropSchemum fun 'Function schema) schemas)
dropFunction fun = UnsafeDefinition $
  "DROP" <+> "FUNCTION" <+> renderSQL fun <> ";"

dropFunctionIfExists
  :: (Has sch schemas schema, KnownSymbol fun)
  => QualifiedAlias sch fun
  -- ^ name of the user defined function
  -> Definition schemas (Alter sch (DropSchemumIfExists fun 'Function schema) schemas)
dropFunctionIfExists fun = UnsafeDefinition $
  "DROP FUNCTION IF EXISTS" <+> renderSQL fun <> ";"

dropOperator
  :: (Has sch schemas schema, KnownSymbol op)
  => QualifiedAlias sch op
  -- ^ name of the user defined operator
  -> Definition schemas (Alter sch (DropSchemum op 'Operator schema) schemas)
dropOperator op = UnsafeDefinition $
  "DROP" <+> "OPERATOR" <+> renderSQL op <> ";"

dropOperatorIfExists
  :: (Has sch schemas schema, KnownSymbol op)
  => QualifiedAlias sch op
  -- ^ name of the user defined operator
  -> Definition schemas (Alter sch (DropSchemumIfExists op 'Operator schema) schemas)
dropOperatorIfExists op = UnsafeDefinition $
  "DROP OPERATOR IF EXISTS" <+> renderSQL op <> ";"

newtype FunctionDefinition schemas args ret = UnsafeFunctionDefinition
  { renderFunctionDefinition :: ByteString }
  deriving (Eq,Show,GHC.Generic,NFData)
instance RenderSQL (FunctionDefinition schemas args ret) where
  renderSQL = renderFunctionDefinition

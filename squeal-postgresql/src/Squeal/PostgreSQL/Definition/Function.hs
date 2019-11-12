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

languageSqlExpr
  :: Expression '[] '[] 'Ungrouped db args '[] ret
  -> FunctionDefinition db args ('Returns ret)
languageSqlExpr expr = UnsafeFunctionDefinition $
  "language sql as"
    <+> "$$" <+> renderSQL (values_ (expr `as` #ret)) <+> "$$"

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
      (Alter sch (Create op ('Operator ('BinaryOp x y z)) schema) db)
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
      (Alter sch (Create op ('Operator ('LeftOp x y)) schema) db)
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
      (Alter sch (Create op ('Operator ('RightOp x y)) schema) db)
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
  "DROP" <+> "FUNCTION" <+> renderSQL fun <> ";"

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
  -> Definition db (Alter sch (DropSchemum op 'Operator schema) db)
dropOperator op = UnsafeDefinition $
  "DROP" <+> "OPERATOR" <+> renderSQL op <> ";"

dropOperatorIfExists
  :: (Has sch db schema, KnownSymbol op)
  => QualifiedAlias sch op
  -- ^ name of the user defined operator
  -> Definition db (Alter sch (DropSchemumIfExists op 'Operator schema) db)
dropOperatorIfExists op = UnsafeDefinition $
  "DROP OPERATOR IF EXISTS" <+> renderSQL op <> ";"

newtype FunctionDefinition db args ret = UnsafeFunctionDefinition
  { renderFunctionDefinition :: ByteString }
  deriving (Eq,Show,GHC.Generic,NFData)
instance RenderSQL (FunctionDefinition db args ret) where
  renderSQL = renderFunctionDefinition

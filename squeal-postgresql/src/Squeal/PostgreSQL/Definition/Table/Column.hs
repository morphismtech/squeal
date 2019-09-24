{-|
Module: Squeal.PostgreSQL.Definition.Table.Column
Description: Column type expressions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Column type expressions.
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

module Squeal.PostgreSQL.Definition.Table.Column
  ( ColumnTypeExpression (..)
  , nullable
  , notNullable
  , default_
  , serial2
  , smallserial
  , serial4
  , serial
  , serial8
  , bigserial
  ) where

import Control.DeepSeq
import Data.ByteString

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | `ColumnTypeExpression`s are used in `createTable` commands.
newtype ColumnTypeExpression (schemas :: SchemasType) (ty :: ColumnType)
  = UnsafeColumnTypeExpression { renderColumnTypeExpression :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (ColumnTypeExpression schemas ty) where
  renderSQL = renderColumnTypeExpression

-- | used in `createTable` commands as a column constraint to note that
-- @NULL@ may be present in a column
nullable
  :: TypeExpression schemas (nullity ty)
  -> ColumnTypeExpression schemas ('NoDef :=> 'Null ty)
nullable ty = UnsafeColumnTypeExpression $ renderSQL ty <+> "NULL"

-- | used in `createTable` commands as a column constraint to ensure
-- @NULL@ is not present in a column
notNullable
  :: TypeExpression schemas (nullity ty)
  -> ColumnTypeExpression schemas ('NoDef :=> 'NotNull ty)
notNullable ty = UnsafeColumnTypeExpression $ renderSQL ty <+> "NOT NULL"

-- | used in `createTable` commands as a column constraint to give a default
default_
  :: Expression '[] '[] 'Ungrouped schemas '[] '[] ty
  -> ColumnTypeExpression schemas ('NoDef :=> ty)
  -> ColumnTypeExpression schemas ('Def :=> ty)
default_ x ty = UnsafeColumnTypeExpression $
  renderSQL ty <+> "DEFAULT" <+> renderExpression x

-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint2`
serial2, smallserial
  :: ColumnTypeExpression schemas ('Def :=> 'NotNull 'PGint2)
serial2 = UnsafeColumnTypeExpression "serial2"
smallserial = UnsafeColumnTypeExpression "smallserial"
-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint4`
serial4, serial
  :: ColumnTypeExpression schemas ('Def :=> 'NotNull 'PGint4)
serial4 = UnsafeColumnTypeExpression "serial4"
serial = UnsafeColumnTypeExpression "serial"
-- | not a true type, but merely a notational convenience for creating
-- unique identifier columns with type `PGint8`
serial8, bigserial
  :: ColumnTypeExpression schemas ('Def :=> 'NotNull 'PGint8)
serial8 = UnsafeColumnTypeExpression "serial8"
bigserial = UnsafeColumnTypeExpression "bigserial"

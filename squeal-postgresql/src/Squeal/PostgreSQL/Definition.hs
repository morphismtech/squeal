{-|
Module: Squeal.PostgreSQL.Definition
Description: Squeal data definition language
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal data definition language.
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

module Squeal.PostgreSQL.Definition
  ( -- * Definition
    Definition (..)
  , (>>>)
  , manipDefinition
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString
import Data.Monoid
import Prelude hiding ((.), id)

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
statements
-----------------------------------------}

-- | A `Definition` is a statement that changes the schemas of the
-- database, like a `createTable`, `dropTable`, or `alterTable` command.
-- `Definition`s may be composed using the `>>>` operator.
newtype Definition
  (schemas0 :: SchemasType)
  (schemas1 :: SchemasType)
  = UnsafeDefinition { renderDefinition :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (Definition schemas0 schemas1) where
  renderSQL = renderDefinition

instance Category Definition where
  id = UnsafeDefinition ";"
  ddl1 . ddl0 = UnsafeDefinition $
    renderSQL ddl0 <> "\n" <> renderSQL ddl1

-- | A `Manipulation` without input or output can be run as a statement
-- along with other `Definition`s, by embedding it using `manipDefinition`.
manipDefinition
  :: Manipulation '[] schemas '[] '[]
  -- ^ no input or output
  -> Definition schemas schemas
manipDefinition = UnsafeDefinition . (<> ";") . renderSQL

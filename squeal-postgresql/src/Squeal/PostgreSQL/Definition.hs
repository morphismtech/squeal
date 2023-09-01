{-|
Module: Squeal.PostgreSQL.Definition
Description: data definition language
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

data definition language
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
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Definition
  ( -- * Definition
    Definition (..)
  , (>>>)
  , manipulation_
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString
import Data.Monoid
import Prelude hiding ((.), id)

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
statements
-----------------------------------------}

-- | A `Definition` is a statement that changes the schemas of the
-- database, like a `Squeal.PostgreSQL.Definition.Table.createTable`,
-- `Squeal.PostgreSQL.Definition.Table.dropTable`,
-- or `Squeal.PostgreSQL.Definition.Table.alterTable` command.
-- `Definition`s may be composed using the `>>>` operator.
newtype Definition
  (db0 :: SchemasType)
  (db1 :: SchemasType)
  = UnsafeDefinition { renderDefinition :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (Definition db0 db1) where
  renderSQL = renderDefinition

instance Category Definition where
  id = UnsafeDefinition ";"
  ddl1 . ddl0 = UnsafeDefinition $
    renderSQL ddl0 <> "\n" <> renderSQL ddl1

instance db0 ~ db1 => Semigroup (Definition db0 db1) where (<>) = (>>>)
instance db0 ~ db1 => Monoid (Definition db0 db1) where mempty = id

-- | A `Manipulation` without input or output can be run as a statement
-- along with other `Definition`s, by embedding it using `manipulation_`.
manipulation_
  :: Manipulation '[] db '[] '[]
  -- ^ no input or output
  -> Definition db db
manipulation_ = UnsafeDefinition . (<> ";") . renderSQL

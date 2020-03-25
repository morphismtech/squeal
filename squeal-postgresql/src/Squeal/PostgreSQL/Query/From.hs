{-|
Module: Squeal.PostgreSQL.Query.From
Description: Squeal from clauses.
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal from clauses.
-}

{-# LANGUAGE
    ConstraintKinds
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , QuantifiedConstraints
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , RankNTypes
  , UndecidableInstances
  #-}

module Squeal.PostgreSQL.Query.From
  ( -- * From Clause
    FromClause (..)
  , table
  , subquery
  , view
  , common
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
FROM clauses
-----------------------------------------}

{- |
A `FromClause` can be a table name, or a derived table such
as a subquery, a @JOIN@ construct, or complex combinations of these.
-}
newtype FromClause
  (lat :: FromType)
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (from :: FromType)
  = UnsafeFromClause { renderFromClause :: ByteString }
  deriving stock (GHC.Generic,Show,Eq,Ord)
  deriving newtype (NFData)
instance RenderSQL (FromClause lat with db params from) where
  renderSQL = renderFromClause

-- | A real `table` is a table from the database.
table
  :: (Has sch db schema, Has tab schema ('Table table))
  => Aliased (QualifiedAlias sch) (alias ::: tab) -- ^ (renamable) table alias
  -> FromClause lat with db params '[alias ::: TableToRow table]
table (tab `As` alias) = UnsafeFromClause $
  renderSQL tab <+> "AS" <+> renderSQL alias

-- | `subquery` derives a table from a `Query`.
subquery
  :: Aliased (Query lat with db params) query
  -- ^ aliased `Query`
  -> FromClause lat with db params '[query]
subquery = UnsafeFromClause . renderAliased (parenthesized . renderSQL)

-- | `view` derives a table from a `View`.
view
  :: (Has sch db schema, Has vw schema ('View view))
  => Aliased (QualifiedAlias sch) (alias ::: vw) -- ^ (renamable) view alias
  -> FromClause lat with db params '[alias ::: view]
view (vw `As` alias) = UnsafeFromClause $
  renderSQL vw <+> "AS" <+> renderSQL alias

-- | `common` derives a table from a common table expression.
common
  :: Has cte with common
  => Aliased Alias (alias ::: cte) -- ^ (renamable) common table expression alias
  -> FromClause lat with db params '[alias ::: common]
common (cte `As` alias) = UnsafeFromClause $
  renderSQL cte <+> "AS" <+> renderSQL alias

instance Additional (FromClause lat with db params) where
  also right left = UnsafeFromClause $
    renderSQL left <> ", " <> renderSQL right

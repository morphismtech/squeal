{-|
Module: Squeal.PostgreSQL.Delete
Description: delete statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

delete statements
-}

{-# LANGUAGE
    DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PatternSynonyms
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Manipulation.Delete
  ( -- * Delete
    deleteFrom
  , deleteFrom_
  ) where

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
DELETE statements
-----------------------------------------}

-- | Delete rows from a table.
deleteFrom
  :: ( SOP.SListI row
     , Has sch db schema
     , Has tab schema ('Table table) )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> UsingClause with db params from
  -> Condition  'Ungrouped '[] with db params (tab ::: TableToRow table ': from)
  -- ^ condition under which to delete a row
  -> ReturningClause with db params '[tab ::: TableToRow table] row
  -- ^ results to return
  -> Manipulation with db params row
deleteFrom tab using wh returning = UnsafeManipulation $
  "DELETE FROM"
  <+> renderSQL tab
  <> case using of
    NoUsing -> ""
    Using tables -> " USING" <+> renderSQL tables
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

-- | Delete rows returning `Nil`.
deleteFrom_
  :: ( Has sch db schema
     , Has tab schema ('Table table) )
  => QualifiedAlias sch tab -- ^ table to delete from
  -> Condition  'Ungrouped '[] with db params '[tab ::: TableToRow table]
  -- ^ condition under which to delete a row
  -> Manipulation with db params '[]
deleteFrom_ tab wh = deleteFrom tab NoUsing wh (Returning_ Nil)

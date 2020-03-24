{-|
Module: Squeal.PostgreSQL.Delete
Description: Squeal delete statements.
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal delete statements.
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
  , UsingClause (..)
  ) where

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
DELETE statements
-----------------------------------------}

-- | Specify additional tables with `Using`
-- an `also` list of table expressions, allowing columns
-- from other tables to appear in the WHERE condition.
-- This is similar to the list of tables that can be specified
-- in the FROM Clause of a SELECT statement;
-- for example, an alias for the table name can be specified.
-- Do not repeat the target table in the `Using` list,
-- unless you wish to set up a self-join.
-- `NoUsing` if no additional tables are to be used.
data UsingClause with db params from where
  NoUsing :: UsingClause with db params '[]
  Using
    :: FromClause '[] with db params from
    -- ^ what to use
    -> UsingClause with db params from

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

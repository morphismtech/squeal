{-|
Module: Squeal.PostgreSQL.Update
Description: Squeal update statements.
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal update statements.
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

module Squeal.PostgreSQL.Manipulation.Update
  ( -- * Update
    update
  , update_
  ) where

import Data.ByteString hiding (foldr)

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Default
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

renderUpdate
  :: (forall x. RenderSQL (expr x))
  => Aliased (Optional expr) ty
  -> ByteString
renderUpdate (expr `As` col) = renderSQL col <+> "=" <+> renderSQL expr

{-----------------------------------------
UPDATE statements
-----------------------------------------}

-- | An `update` command changes the values of the specified columns
-- in all rows that satisfy the condition.
update
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , Updatable table updates
     , SOP.SListI row )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (Aliased (Optional (Expression 'Ungrouped '[] '[] db params '[tab ::: TableToRow table]))) updates
  -- ^ modified values to replace old values
  -> Condition  'Ungrouped '[] with db params '[tab ::: TableToRow table]
  -- ^ condition under which to perform update on a row
  -> ReturningClause with db params '[tab ::: TableToRow table] row -- ^ results to return
  -> Manipulation with db params row
update tab columns wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderSQL tab
  <+> "SET"
  <+> renderCommaSeparated renderUpdate columns
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

-- | Update a row returning `Nil`.
update_
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , Updatable table updates )
  => QualifiedAlias sch tab -- ^ table to update
  -> NP (Aliased (Optional (Expression 'Ungrouped '[] '[] db params '[tab ::: TableToRow table]))) updates
  -- ^ modified values to replace old values
  -> Condition  'Ungrouped '[] with db params '[tab ::: TableToRow table]
  -- ^ condition under which to perform update on a row
  -> Manipulation with db params '[]
update_ tab columns wh = update tab columns wh (Returning_ Nil)

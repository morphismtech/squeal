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

{- | Delete rows from a table.

>>> type Columns = '["col1" ::: 'Def :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab1" ::: 'Table ('[] :=> Columns), "tab2" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manp :: Manipulation with (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  manp = deleteFrom #tab1 (Using (table #tab2)) (#tab1 ! #col1 .== #tab2 ! #col2) (Returning (#tab1 & DotStar))
in printSQL manp
:}
DELETE FROM "tab1" AS "tab1" USING "tab2" AS "tab2" WHERE ("tab1"."col1" = "tab2"."col2") RETURNING "tab1".*
-}
deleteFrom
  :: ( SOP.SListI row
     , Has sch db schema
     , Has tab0 schema ('Table table) )
  => Aliased (QualifiedAlias sch) (tab ::: tab0) -- ^ table to delete from
  -> UsingClause with db params from
  -> Condition  'Ungrouped '[] with db params (tab ::: TableToRow table ': from)
  -- ^ condition under which to delete a row
  -> ReturningClause with db params (tab ::: TableToRow table ': from) row
  -- ^ results to return
  -> Manipulation with db params row
deleteFrom (tab0 `As` tab) using wh returning = UnsafeManipulation $
  "DELETE FROM"
  <+> renderSQL tab0 <+> "AS" <+> renderSQL tab
  <> case using of
    NoUsing -> ""
    Using tables -> " USING" <+> renderSQL tables
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

{- | Delete rows returning `Nil`.

>>> type Columns = '["col1" ::: 'Def :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manp :: Manipulation with (Public Schema) '[ 'NotNull 'PGint4] '[]
  manp = deleteFrom_ (#tab `as` #t) (#t ! #col1 .== param @1)
in printSQL manp
:}
DELETE FROM "tab" AS "t" WHERE ("t"."col1" = ($1 :: int4))
-}
deleteFrom_
  :: ( Has sch db schema
     , Has tab0 schema ('Table table) )
  => Aliased (QualifiedAlias sch) (tab ::: tab0) -- ^ table to delete from
  -> Condition  'Ungrouped '[] with db params '[tab ::: TableToRow table]
  -- ^ condition under which to delete a row
  -> Manipulation with db params '[]
deleteFrom_ tab wh = deleteFrom tab NoUsing wh (Returning_ Nil)

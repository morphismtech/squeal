{-|
Module: Squeal.PostgreSQL.Update
Description: update statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

update statements
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
  , DataKinds
  , PolyKinds
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Manipulation.Update
  ( -- * Update
    update
  , update_
  ) where

import Data.ByteString hiding (foldr)
import GHC.TypeLits

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

{- | An `update` command changes the values of the specified columns
in all rows that satisfy the condition.

>>> type Columns = '["col1" ::: 'Def :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab1" ::: 'Table ('[] :=> Columns), "tab2" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manp :: Manipulation with (Public Schema) '[]
    '["col1" ::: 'NotNull 'PGint4,
      "col2" ::: 'NotNull 'PGint4]
  manp = update
    (#tab1 `as` #t1)
    (Set (2 + #t2 ! #col2) `as` #col1)
    (Using (table (#tab2 `as` #t2)))
    (#t1 ! #col1 ./= #t2 ! #col2)
    (Returning (#t1 & DotStar))
in printSQL manp
:}
UPDATE "tab1" AS "t1" SET "col1" = ((2 :: int4) + "t2"."col2") FROM "tab2" AS "t2" WHERE ("t1"."col1" <> "t2"."col2") RETURNING "t1".*
-}
update
  :: ( Has sch db schema
     , Has tab0 schema ('Table table)
     , Updatable table updates
     , SOP.SListI row )
  => Aliased (QualifiedAlias sch) (tab ::: tab0) -- ^ table to update
  -> NP (Aliased (Optional (Expression 'Ungrouped '[] with db params (tab ::: TableToRow table ': from)))) updates
  -- ^ update expressions, modified values to replace old values
  -> UsingClause with db params from
  -- ^ FROM A table expression allowing columns from other tables to appear
  -- in the WHERE condition and update expressions.
  -> Condition  'Ungrouped '[] with db params (tab ::: TableToRow table ': from)
  -- ^ WHERE condition under which to perform update on a row
  -> ReturningClause with db params (tab ::: TableToRow table ': from) row -- ^ results to return
  -> Manipulation with db params row
update (tab0 `As` tab) columns using wh returning = UnsafeManipulation $
  "UPDATE"
  <+> renderSQL tab0 <+> "AS" <+> renderSQL tab
  <+> "SET"
  <+> renderCommaSeparated renderUpdate columns
  <> case using of
    NoUsing -> ""
    Using tables -> " FROM" <+> renderSQL tables
  <+> "WHERE" <+> renderSQL wh
  <> renderSQL returning

{- | Update a row returning `Nil`.

>>> type Columns = '["col1" ::: 'Def :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manp :: Manipulation with (Public Schema) '[] '[]
  manp = update_ #tab (Set 2 `as` #col1) (#col1 ./= #col2)
in printSQL manp
:}
UPDATE "tab" AS "tab" SET "col1" = (2 :: int4) WHERE ("col1" <> "col2")
-}
update_
  :: ( Has sch db schema
     , Has tab0 schema ('Table table)
     , KnownSymbol tab
     , Updatable table updates )
  => Aliased (QualifiedAlias sch) (tab ::: tab0) -- ^ table to update
  -> NP (Aliased (Optional (Expression 'Ungrouped '[] with db params '[tab ::: TableToRow table]))) updates
  -- ^ modified values to replace old values
  -> Condition  'Ungrouped '[] with db params '[tab ::: TableToRow table]
  -- ^ condition under which to perform update on a row
  -> Manipulation with db params '[]
update_ tab columns wh = update tab columns NoUsing wh (Returning_ Nil)

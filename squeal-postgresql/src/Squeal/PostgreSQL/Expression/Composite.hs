{-|
Module: Squeal.PostgreSQL.Expression.Composite
Description: composite functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

composite functions
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Composite
  ( -- * Composite Functions
    row
  , rowStar
  , field
  ) where

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | A row constructor is an expression that builds a row value
-- (also called a composite value) using values for its member fields.
--
-- >>> :{
-- type Complex = 'PGcomposite
--   '[ "real"      ::: 'NotNull 'PGfloat8
--    , "imaginary" ::: 'NotNull 'PGfloat8 ]
-- :}
--
-- >>> let i = row (0 `as` #real :* 1 `as` #imaginary) :: Expression grp lat with db params from ('NotNull Complex)
-- >>> printSQL i
-- ROW((0.0 :: float8), (1.0 :: float8))
row
  :: SOP.SListI row
  => NP (Aliased (Expression grp lat with db params from)) row
  -- ^ zero or more expressions for the row field values
  -> Expression grp lat with db params from (null ('PGcomposite row))
row exprs = UnsafeExpression $ "ROW" <> parenthesized
  (renderCommaSeparated (\ (expr `As` _) -> renderSQL expr) exprs)

-- | A row constructor on all columns in a table expression.
rowStar
  :: Has tab from row
  => Alias tab -- ^ intermediate table
  -> Expression grp lat with db params from (null ('PGcomposite row))
rowStar tab = UnsafeExpression $ "ROW" <>
  parenthesized (renderSQL tab <> ".*")

-- | >>> :{
-- type Complex = 'PGcomposite
--   '[ "real"      ::: 'NotNull 'PGfloat8
--    , "imaginary" ::: 'NotNull 'PGfloat8 ]
-- type Schema = '["complex" ::: 'Typedef Complex]
-- :}
--
-- >>> let i = row (0 `as` #real :* 1 `as` #imaginary) :: Expression lat '[] grp (Public Schema) from params ('NotNull Complex)
-- >>> printSQL $ i & field #complex #imaginary
-- (ROW((0.0 :: float8), (1.0 :: float8))::"complex")."imaginary"
field
  :: ( relss ~ DbRelations db
     , Has sch relss rels
     , Has rel rels row
     , Has field row ty
     )
  => QualifiedAlias sch rel -- ^ row type
  -> Alias field -- ^ field name
  -> Expression grp lat with db params from ('NotNull ('PGcomposite row))
  -> Expression grp lat with db params from ty
field rel fld expr = UnsafeExpression $
  parenthesized (renderSQL expr <> "::" <> renderSQL rel)
    <> "." <> renderSQL fld

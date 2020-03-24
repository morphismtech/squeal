{-|
Module: Squeal.PostgreSQL.Query.With
Description: Squeal with statements.
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal with statements.
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

module Squeal.PostgreSQL.Query.With
  ( -- ** With
    With (..)
  , CommonTableExpression (..)
  , withRecursive
  ) where

import Data.Quiver.Functor
import GHC.TypeLits

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | `with` provides a way to write auxiliary statements for use in a larger query.
-- These statements, referred to as `CommonTableExpression`s, can be thought of as
-- defining temporary tables that exist just for one query.
class With statement where
  with
    :: Path (CommonTableExpression statement db params) with0 with1
    -- ^ common table expressions
    -> statement with1 db params row
    -- ^ larger query
    -> statement with0 db params row
instance With (Query lat) where
  with Done query = query
  with ctes query = UnsafeQuery $
    "WITH" <+> commaSeparated (qtoList renderSQL ctes) <+> renderSQL query

{- |
>>> import Data.Monoid (Sum (..))
>>> import Data.Int (Int64)
>>> :{
  let
    query :: Query_ schema () (Sum Int64)
    query = withRecursive
      ( values_ ((1 & astype int) `as` #n)
        `unionAll`
        select_ ((#n + 1) `as` #n)
          (from (common #t) & where_ (#n .< 100)) `as` #t )
      ( select_ (fromNull 0 (sum_ (All #n)) `as` #getSum) (from (common #t) & groupBy Nil))
  in printSQL query
:}
WITH RECURSIVE "t" AS ((SELECT * FROM (VALUES (((1 :: int4) :: int))) AS t ("n")) UNION ALL (SELECT ("n" + (1 :: int4)) AS "n" FROM "t" AS "t" WHERE ("n" < (100 :: int4)))) SELECT COALESCE(sum(ALL "n"), (0 :: int8)) AS "getSum" FROM "t" AS "t"
-}
withRecursive
  :: Aliased (Query lat (recursive ': with) db params) recursive
  -- ^ recursive query
  -> Query lat (recursive ': with) db params row
  -- ^ larger query
  -> Query lat with db params row
withRecursive (recursive `As` cte) query = UnsafeQuery $
  "WITH RECURSIVE" <+> renderSQL cte
    <+> "AS" <+> parenthesized (renderSQL recursive)
    <+> renderSQL query

-- | A `CommonTableExpression` is an auxiliary statement in a `with` clause.
data CommonTableExpression statement
  (db :: SchemasType)
  (params :: [NullType])
  (with0 :: FromType)
  (with1 :: FromType) where
  CommonTableExpression
    :: Aliased (statement with db params) (cte ::: common)
    -- ^ aliased statement
    -> CommonTableExpression statement db params with (cte ::: common ': with)
instance
  ( KnownSymbol cte
  , with1 ~ (cte ::: common ': with)
  ) => Aliasable cte
    (statement with db params common)
    (CommonTableExpression statement db params with with1) where
      statement `as` cte = CommonTableExpression (statement `as` cte)
instance
  ( KnownSymbol cte
  , with1 ~ (cte ::: common ': with)
  ) => Aliasable cte
    (statement with db params common)
    (Path (CommonTableExpression statement db params) with with1) where
      statement `as` cte = qsingle (statement `as` cte)

instance (forall c s p r. RenderSQL (statement c s p r)) => RenderSQL
  (CommonTableExpression statement db params with0 with1) where
    renderSQL (CommonTableExpression (statement `As` cte)) =
      renderSQL cte <+> "AS" <+> parenthesized (renderSQL statement)

{-|
Module: Squeal.PostgreSQL.Query.With
Description: with statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

with statements
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
  , Materialization (..)
  , materialized
  , notMaterialized
  ) where

import Data.Quiver.Functor
import GHC.TypeLits

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- | `with` provides a way to write auxiliary statements for use in a larger query.
These statements, referred to as `CommonTableExpression`s, can be thought of as
defining temporary tables that exist just for one query.

`with` can be used for a `Query`. Multiple `CommonTableExpression`s can be
chained together with the `Path` constructor `:>>`, and each `CommonTableExpression`
is constructed via overloaded `as`.

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  qry :: Query lat with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = with (
    select Star (from (table #tab)) `as` #cte1 :>>
    select Star (from (common #cte1)) `as` #cte2
    ) (select Star (from (common #cte2)))
in printSQL qry
:}
WITH "cte1" AS (SELECT * FROM "tab" AS "tab"), "cte2" AS (SELECT * FROM "cte1" AS "cte1") SELECT * FROM "cte2" AS "cte2"

You can use data-modifying statements in `with`. This allows you to perform several
different operations in the same query. An example is:

>>> type ProductsColumns = '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]
>>> type ProductsSchema = '["products" ::: 'Table ('[] :=> ProductsColumns), "products_deleted" ::: 'Table ('[] :=> ProductsColumns)]
>>> :{
let
  manp :: Manipulation with (Public ProductsSchema) '[ 'NotNull 'PGdate] '[]
  manp = with
    (deleteFrom #products NoUsing (#date .< param @1) (Returning Star) `as` #del)
    (insertInto_ #products_deleted (Subquery (select Star (from (common #del)))))
in printSQL manp
:}
WITH "del" AS (DELETE FROM "products" AS "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" AS "products_deleted" SELECT * FROM "del" AS "del"
-}
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

{- | A `withRecursive` `Query` can refer to its own output.
A very simple example is this query to sum the integers from 1 through 100:

>>> import Data.Monoid (Sum (..))
>>> import Data.Int (Int64)
>>> :{
  let
    sum100 :: Statement db () (Sum Int64)
    sum100 = query $
      withRecursive
        ( values_ ((1 & astype int) `as` #n)
          `unionAll`
          select_ ((#n + 1) `as` #n)
            (from (common #t) & where_ (#n .< 100)) `as` #t )
        ( select_
            (fromNull 0 (sum_ (All #n)) `as` #getSum)
            (from (common #t) & groupBy Nil) )
  in printSQL sum100
:}
WITH RECURSIVE "t" AS ((SELECT * FROM (VALUES (((1 :: int4) :: int))) AS t ("n")) UNION ALL (SELECT ("n" + (1 :: int4)) AS "n" FROM "t" AS "t" WHERE ("n" < (100 :: int4)))) SELECT COALESCE(sum(ALL "n"), (0 :: int8)) AS "getSum" FROM "t" AS "t"

The general form of a recursive WITH query is always a non-recursive term,
then `union` (or `unionAll`), then a recursive term, where
only the recursive term can contain a reference to the query's own output.
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

-- | Whether the contents of the WITH clause are materialized.
-- If a WITH query is non-recursive and side-effect-free (that is, it is a SELECT containing no volatile functions) then it can be folded into the parent query, allowing joint optimization of the two query levels.
--
-- Note: Use of `Materialized` or `NotMaterialized` requires PostgreSQL version 12 or higher. For earlier versions, use `DefaultMaterialization` which in those earlier versions of PostgreSQL behaves as `Materialized`. PostgreSQL 12 both changes the default behavior as well as adds options for customizing the materialization behavior.
data Materialization =
  DefaultMaterialization -- ^ By default, folding happens if the parent query references the WITH query just once, but not if it references the WITH query more than once. Note: this is the behavior in PostgreSQL 12+. In PostgreSQL 11 and earlier, all CTEs are materialized.
  | Materialized -- ^ You can override that decision by specifying MATERIALIZED to force separate calculation of the WITH query. Requires PostgreSQL 12+.
  | NotMaterialized -- ^ or by specifying NOT MATERIALIZED to force it to be merged into the parent query. Requires PostgreSQL 12+.
  deriving (Eq, Ord, Show, Read, Enum, GHC.Generic)
instance SOP.Generic Materialization
instance SOP.HasDatatypeInfo Materialization
instance RenderSQL Materialization where
  renderSQL = \case
    DefaultMaterialization -> ""
    Materialized -> "MATERIALIZED"
    NotMaterialized -> "NOT MATERIALIZED"

-- | A `CommonTableExpression` is an auxiliary statement in a `with` clause.
data CommonTableExpression statement
  (db :: SchemasType)
  (params :: [NullType])
  (with0 :: FromType)
  (with1 :: FromType) where
  CommonTableExpression
    :: Aliased (statement with db params) (cte ::: common)
    -- ^ aliased statement
    -> Materialization
    -- ^ materialization of the CTE output
    -> CommonTableExpression statement db params with (cte ::: common ': with)
instance
  ( KnownSymbol cte
  , with1 ~ (cte ::: common ': with)
  ) => Aliasable cte
    (statement with db params common)
    (CommonTableExpression statement db params with with1) where
      statement `as` cte = CommonTableExpression (statement `as` cte) DefaultMaterialization
instance
  ( KnownSymbol cte
  , with1 ~ (cte ::: common ': with)
  ) => Aliasable cte
    (statement with db params common)
    (Path (CommonTableExpression statement db params) with with1) where
      statement `as` cte = qsingle (statement `as` cte)

instance (forall c s p r. RenderSQL (statement c s p r)) => RenderSQL
  (CommonTableExpression statement db params with0 with1) where
    renderSQL (CommonTableExpression (statement `As` cte) materialization) =
      renderSQL cte
        <+> "AS"
        <+> renderSQL materialization
        <> case materialization of
              DefaultMaterialization -> ""
              _ -> " "
        <> parenthesized (renderSQL statement)

{- | Force separate calculation of the WITH query.

>>> :{
let
  qry :: Query lat with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = with (
    materialized (select Star (from (table #tab)) `as` #cte1) :>>
    select Star (from (common #cte1)) `as` #cte2
    ) (select Star (from (common #cte2)))
in printSQL qry
:}
WITH "cte1" AS MATERIALIZED (SELECT * FROM "tab" AS "tab"), "cte2" AS (SELECT * FROM "cte1" AS "cte1") SELECT * FROM "cte2" AS "cte2"

Note: if the last CTE has `materialized` or `notMaterialized` you must add `:>> Done`.

Requires PostgreSQL 12 or higher.
-}
materialized
  :: Aliased (statement with db params) (cte ::: common)
  -> CommonTableExpression statement db params with (cte ::: common ': with)
materialized stmt = CommonTableExpression stmt Materialized

{- | Force the WITH query to be merged into the parent query.

>>> :{
let
  qry :: Query lat with (Public Schema) params '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  qry = with (
    select Star (from (table #tab)) `as` #cte1 :>>
    notMaterialized (select Star (from (common #cte1)) `as` #cte2) :>>
    Done
    ) (select Star (from (common #cte2)))
in printSQL qry
:}
WITH "cte1" AS (SELECT * FROM "tab" AS "tab"), "cte2" AS NOT MATERIALIZED (SELECT * FROM "cte1" AS "cte1") SELECT * FROM "cte2" AS "cte2"

Note: if the last CTE has `materialized` or `notMaterialized` you must add `:>> Done` to finish the `Path`.

Requires PostgreSQL 12 or higher.
-}
notMaterialized
  :: Aliased (statement with db params) (cte ::: common)
  -> CommonTableExpression statement db params with (cte ::: common ': with)
notMaterialized stmt = CommonTableExpression stmt NotMaterialized

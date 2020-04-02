{-|
Module: Squeal.PostgreSQL.Expression.Sort
Description: sort expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

sort expressions
-}

{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , StandaloneDeriving
#-}

module Squeal.PostgreSQL.Expression.Sort
  ( -- * Sort
    SortExpression (..)
  , OrderBy (..)
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- | `SortExpression`s are used by `orderBy` to optionally sort the results
-- of a `Squeal.PostgreSQL.Query.Query`. `Asc` or `Desc`
-- set the sort direction of a `NotNull` result
-- column to ascending or descending. Ascending order puts smaller values
-- first, where "smaller" is defined in terms of the
-- `Squeal.PostgreSQL.Expression.Comparison..<` operator. Similarly,
-- descending order is determined with the
-- `Squeal.PostgreSQL.Expression.Comparison..>` operator. `AscNullsFirst`,
-- `AscNullsLast`, `DescNullsFirst` and `DescNullsLast` options are used to
-- determine whether nulls appear before or after non-null values in the sort
-- ordering of a `Null` result column.
data SortExpression grp lat with db params from where
  Asc
    :: Expression grp lat with db params from ('NotNull ty)
    -- ^ sort by
    -> SortExpression grp lat with db params from
  Desc
    :: Expression grp lat with db params from ('NotNull ty)
    -- ^ sort by
    -> SortExpression grp lat with db params from
  AscNullsFirst
    :: Expression grp lat with db params from  ('Null ty)
    -- ^ sort by
    -> SortExpression grp lat with db params from
  AscNullsLast
    :: Expression grp lat with db params from  ('Null ty)
    -- ^ sort by
    -> SortExpression grp lat with db params from
  DescNullsFirst
    :: Expression grp lat with db params from  ('Null ty)
    -- ^ sort by
    -> SortExpression grp lat with db params from
  DescNullsLast
    :: Expression grp lat with db params from  ('Null ty)
    -- ^ sort by
    -> SortExpression grp lat with db params from
deriving instance Show (SortExpression grp lat with db params from)
instance RenderSQL (SortExpression grp lat with db params from) where
  renderSQL = \case
    Asc expression -> renderSQL expression <+> "ASC"
    Desc expression -> renderSQL expression <+> "DESC"
    AscNullsFirst expression -> renderSQL expression
      <+> "ASC NULLS FIRST"
    DescNullsFirst expression -> renderSQL expression
      <+> "DESC NULLS FIRST"
    AscNullsLast expression -> renderSQL expression <+> "ASC NULLS LAST"
    DescNullsLast expression -> renderSQL expression <+> "DESC NULLS LAST"
instance RenderSQL [SortExpression grp lat with db params from] where
  renderSQL = \case
    [] -> ""
    srts -> " ORDER BY"
      <+> commaSeparated (renderSQL <$> srts)

{- |
The `orderBy` clause causes the result rows of a `Squeal.PostgreSQL.Query.TableExpression`
to be sorted according to the specified `SortExpression`(s).
If two rows are equal according to the leftmost expression,
they are compared according to the next expression and so on.
If they are equal according to all specified expressions,
they are returned in an implementation-dependent order.

You can also control the order in which rows are processed by window functions
using `orderBy` within `Squeal.PostgreSQL.Query.Over`.
-}
class OrderBy expr grp | expr -> grp where
  orderBy
    :: [SortExpression grp lat with db params from]
      -- ^ sorts
    -> expr lat with db params from
    -> expr lat with db params from

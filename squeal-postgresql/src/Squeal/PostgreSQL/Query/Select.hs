{-|
Module: Squeal.PostgreSQL.Query.Select
Description: select statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

select statements
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

module Squeal.PostgreSQL.Query.Select
  ( -- ** Select
    select
  , select_
  , selectDistinct
  , selectDistinct_
  , selectDistinctOn
  , selectDistinctOn_
  , Selection (..)
  ) where

import Data.ByteString (ByteString)
import Data.String
import Generics.SOP hiding (from)
import GHC.TypeLits

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Sort
import Squeal.PostgreSQL.Expression.Window
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Query.Table
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
SELECT queries
-----------------------------------------}

{- | The simplest kinds of `Selection` are `Star` and `DotStar` which
emits all columns that a `TableExpression` produces. A select `List`
is a list of `Expression`s. A `Selection` could be a list of
`WindowFunction`s `Over` `WindowDefinition`. `Additional` `Selection`s can
be selected with `Also`.
-}
data Selection grp lat with db params from row where
  Star
    :: HasUnique tab from row
    => Selection 'Ungrouped lat with db params from row
    -- ^ `HasUnique` table in the `Squeal.PostgreSQL.Query.From.FromClause`
  DotStar
    :: Has tab from row
    => Alias tab
       -- ^ `Has` table with `Alias`
    -> Selection 'Ungrouped lat with db params from row
  List
    :: SListI row
    => NP (Aliased (Expression grp lat with db params from)) row
       -- ^ `NP` list of `Aliased` `Expression`s
    -> Selection grp lat with db params from row
  Over
    :: SListI row
    => NP (Aliased (WindowFunction grp lat with db params from)) row
       -- ^ `NP` list of `Aliased` `WindowFunction`s
    -> WindowDefinition grp lat with db params from
    -> Selection grp lat with db params from row
  Also
    :: Selection grp lat with db params from right
       -- ^ `Additional` `Selection`
    -> Selection grp lat with db params from left
    -> Selection grp lat with db params from (Join left right)
instance Additional (Selection grp lat with db params from) where
  also = Also
instance (KnownSymbol col, row ~ '[col ::: ty])
  => Aliasable col
    (Expression grp lat with db params from ty)
    (Selection grp lat with db params from row) where
      expr `as` col = List (expr `as` col)
instance (Has tab (Join lat from) row0, Has col row0 ty, row1 ~ '[col ::: ty])
  => IsQualified tab col
    (Selection 'Ungrouped lat with db params from row1) where
      tab ! col = tab ! col `as` col
instance
  ( Has tab (Join lat from) row0
  , Has col row0 ty
  , row1 ~ '[col ::: ty]
  , GroupedBy tab col bys )
  => IsQualified tab col
    (Selection ('Grouped bys) lat with db params from row1) where
      tab ! col = tab ! col `as` col
instance (HasUnique tab (Join lat from) row0, Has col row0 ty, row1 ~ '[col ::: ty])
  => IsLabel col
    (Selection 'Ungrouped lat with db params from row1) where
      fromLabel = fromLabel @col `as` Alias
instance
  ( HasUnique tab (Join lat from) row0
  , Has col row0 ty
  , row1 ~ '[col ::: ty]
  , GroupedBy tab col bys )
  => IsLabel col
    (Selection ('Grouped bys) lat with db params from row1) where
      fromLabel = fromLabel @col `as` Alias

instance RenderSQL (Selection grp lat with db params from row) where
  renderSQL = \case
    List list -> renderCommaSeparated (renderAliased renderSQL) list
    Star -> "*"
    DotStar tab -> renderSQL tab <> ".*"
    Also right left -> renderSQL left <> ", " <> renderSQL right
    Over winFns winDef ->
      let
        renderOver
          :: Aliased (WindowFunction grp lat with db params from) field
          -> ByteString
        renderOver (winFn `As` col) = renderSQL winFn
          <+> "OVER" <+> parenthesized (renderSQL winDef)
          <+> "AS" <+> renderSQL col
      in
        renderCommaSeparated renderOver winFns

instance IsString
  (Selection grp lat with db params from '["fromOnly" ::: 'NotNull 'PGtext]) where
    fromString str = fromString str `as` Alias

-- | the `TableExpression` in the `select` command constructs an intermediate
-- virtual table by possibly combining tables, views, eliminating rows,
-- grouping, etc. This table is finally passed on to processing by
-- the select list. The `Selection` determines which columns of
-- the intermediate table are actually output.
select
  :: (SListI row, row ~ (x ': xs))
  => Selection grp lat with db params from row
  -- ^ selection
  -> TableExpression grp lat with db params from
  -- ^ intermediate virtual table
  -> Query lat with db params row
select selection tabexpr = UnsafeQuery $
  "SELECT"
  <+> renderSQL selection
  <+> renderSQL tabexpr

-- | Like `select` but takes an `NP` list of `Expression`s instead
-- of a general `Selection`.
select_
  :: (SListI row, row ~ (x ': xs))
  => NP (Aliased (Expression grp lat with db params from)) row
  -- ^ select list
  -> TableExpression grp lat with db params from
  -- ^ intermediate virtual table
  -> Query lat with db params row
select_ = select . List

-- | After the select list has been processed, the result table can
-- be subject to the elimination of duplicate rows using `selectDistinct`.
selectDistinct
  :: (SListI columns, columns ~ (col ': cols))
  => Selection grp lat with db params from columns
  -- ^ selection
  -> TableExpression grp lat with db params from
  -- ^ intermediate virtual table
  -> Query lat with db params columns
selectDistinct selection tabexpr = UnsafeQuery $
  "SELECT DISTINCT"
  <+> renderSQL selection
  <+> renderSQL tabexpr

-- | Like `selectDistinct` but takes an `NP` list of `Expression`s instead
-- of a general `Selection`.
selectDistinct_
  :: (SListI columns, columns ~ (col ': cols))
  => NP (Aliased (Expression grp lat with db params from)) columns
  -- ^ select list
  -> TableExpression grp lat with db params from
  -- ^ intermediate virtual table
  -> Query lat with db params columns
selectDistinct_ = selectDistinct . List

{-|
`selectDistinctOn` keeps only the first row of each set of rows where
the given expressions evaluate to equal. The DISTINCT ON expressions are
interpreted using the same rules as for ORDER BY. ORDER BY is used to
ensure that the desired row appears first.

The DISTINCT ON expression(s) must match the leftmost ORDER BY expression(s).
The ORDER BY clause will normally contain additional expression(s) that
determine the desired precedence of rows within each DISTINCT ON group.

In order to guarantee they match and reduce redundancy, this function
will prepend the The DISTINCT ON expressions to the ORDER BY clause.
-}
selectDistinctOn
  :: (SListI columns, columns ~ (col ': cols))
  => [SortExpression grp lat with db params from]
  -- ^ DISTINCT ON expression(s) and prepended to ORDER BY clause
  -> Selection grp lat with db params from columns
  -- ^ selection
  -> TableExpression grp lat with db params from
  -- ^ intermediate virtual table
  -> Query lat with db params columns
selectDistinctOn distincts selection tab = UnsafeQuery $
  "SELECT DISTINCT ON"
  <+> parenthesized (commaSeparated (renderDistinctOn <$> distincts))
  <+> renderSQL selection
  <+> renderSQL (tab {orderByClause = distincts <> orderByClause tab})
  where
    renderDistinctOn = \case
      Asc expression -> renderSQL expression
      Desc expression -> renderSQL expression
      AscNullsFirst expression -> renderSQL expression
      DescNullsFirst expression -> renderSQL expression
      AscNullsLast expression -> renderSQL expression
      DescNullsLast expression -> renderSQL expression

-- | Like `selectDistinctOn` but takes an `NP` list of `Expression`s instead
-- of a general `Selection`.
selectDistinctOn_
  :: (SListI columns, columns ~ (col ': cols))
  => [SortExpression grp lat with db params from]
  -- ^ distinct on and return the first row in ordering
  -> NP (Aliased (Expression grp lat with db params from)) columns
  -- ^ selection
  -> TableExpression grp lat with db params from
  -- ^ intermediate virtual table
  -> Query lat with db params columns
selectDistinctOn_ distincts = selectDistinctOn distincts . List

{-|
Module: Squeal.PostgreSQL.Query.Table
Description: Squeal intermediate table expressions.
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal intermediate table expressions.
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

module Squeal.PostgreSQL.Query.Table
  ( -- * Table Expression
    TableExpression (..)
  , from
  , where_
  , groupBy
  , having
  , limit
  , offset
    -- * Grouping
  , By (..)
  , GroupByClause (..)
  , HavingClause (..)
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.String
import Data.Word
import Generics.SOP hiding (from)
import GHC.TypeLits

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Sort
import Squeal.PostgreSQL.Query.From
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
Table Expressions
-----------------------------------------}

-- | A `TableExpression` computes a table. The table expression contains
-- a `fromClause` that is optionally followed by a `whereClause`,
-- `groupByClause`, `havingClause`, `orderByClause`, `limitClause`
-- and `offsetClause`s. Trivial table expressions simply refer
-- to a table on disk, a so-called base table, but more complex expressions
-- can be used to modify or combine base tables in various ways.
data TableExpression
  (grp :: Grouping)
  (lat :: FromType)
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (from :: FromType)
    = TableExpression
    { fromClause :: FromClause lat with db params from
    -- ^ A table reference that can be a table name, or a derived table such
    -- as a subquery, a @JOIN@ construct, or complex combinations of these.
    , whereClause :: [Condition 'Ungrouped lat with db params from]
    -- ^ optional search coditions, combined with `.&&`. After the processing
    -- of the `fromClause` is done, each row of the derived virtual table
    -- is checked against the search condition. If the result of the
    -- condition is true, the row is kept in the output table,
    -- otherwise it is discarded. The search condition typically references
    -- at least one column of the table generated in the `fromClause`;
    -- this is not required, but otherwise the WHERE clause will
    -- be fairly useless.
    , groupByClause :: GroupByClause grp from
    -- ^ The `groupByClause` is used to group together those rows in a table
    -- that have the same values in all the columns listed. The order in which
    -- the columns are listed does not matter. The effect is to combine each
    -- set of rows having common values into one group row that represents all
    -- rows in the group. This is done to eliminate redundancy in the output
    -- and/or compute aggregates that apply to these groups.
    , havingClause :: HavingClause grp lat with db params from
    -- ^ If a table has been grouped using `groupBy`, but only certain groups
    -- are of interest, the `havingClause` can be used, much like a
    -- `whereClause`, to eliminate groups from the result. Expressions in the
    -- `havingClause` can refer both to grouped expressions and to ungrouped
    -- expressions (which necessarily involve an aggregate function).
    , orderByClause :: [SortExpression grp lat with db params from]
    -- ^ The `orderByClause` is for optional sorting. When more than one
    -- `SortExpression` is specified, the later (right) values are used to sort
    -- rows that are equal according to the earlier (left) values.
    , limitClause :: [Word64]
    -- ^ The `limitClause` is combined with `min` to give a limit count
    -- if nonempty. If a limit count is given, no more than that many rows
    -- will be returned (but possibly fewer, if the query itself yields
    -- fewer rows).
    , offsetClause :: [Word64]
    -- ^ The `offsetClause` is combined with `Prelude.+` to give an offset count
    -- if nonempty. The offset count says to skip that many rows before
    -- beginning to return rows. The rows are skipped before the limit count
    -- is applied.
    } deriving (GHC.Generic)

-- | Render a `TableExpression`
instance RenderSQL (TableExpression grp lat with db params from) where
  renderSQL
    (TableExpression frm' whs' grps' hvs' srts' lims' offs') = mconcat
      [ "FROM ", renderSQL frm'
      , renderWheres whs'
      , renderSQL grps'
      , renderSQL hvs'
      , renderSQL srts'
      , renderLimits lims'
      , renderOffsets offs' ]
      where
        renderWheres = \case
          [] -> ""
          wh:whs -> " WHERE" <+> renderSQL (foldr (.&&) wh whs)
        renderLimits = \case
          [] -> ""
          lims -> " LIMIT" <+> fromString (show (minimum lims))
        renderOffsets = \case
          [] -> ""
          offs -> " OFFSET" <+> fromString (show (sum offs))

-- | A `from` generates a `TableExpression` from a table reference that can be
-- a table name, or a derived table such as a subquery, a JOIN construct,
-- or complex combinations of these. A `from` may be transformed by `where_`,
-- `groupBy`, `having`, `orderBy`, `limit` and `offset`,
-- using the `Data.Function.&` operator
-- to match the left-to-right sequencing of their placement in SQL.
from
  :: FromClause lat with db params from -- ^ table reference
  -> TableExpression 'Ungrouped lat with db params from
from tab = TableExpression tab [] noGroups NoHaving [] [] []

-- | A `where_` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `whereClause`.
where_
  :: Condition 'Ungrouped lat with db params from -- ^ filtering condition
  -> TableExpression grp lat with db params from
  -> TableExpression grp lat with db params from
where_ wh rels = rels {whereClause = wh : whereClause rels}

-- | A `groupBy` is a transformation of `TableExpression`s which switches
-- its `Grouping` from `Ungrouped` to `Grouped`. Use @groupBy Nil@ to perform
-- a "grand total" aggregation query.
groupBy
  :: SListI bys
  => NP (By from) bys -- ^ grouped columns
  -> TableExpression 'Ungrouped lat with db params from
  -> TableExpression ('Grouped bys) lat with db params from
groupBy bys rels = TableExpression
  { fromClause = fromClause rels
  , whereClause = whereClause rels
  , groupByClause = group bys
  , havingClause = Having []
  , orderByClause = []
  , limitClause = limitClause rels
  , offsetClause = offsetClause rels
  }

-- | A `having` is an endomorphism of `TableExpression`s which adds a
-- search condition to the `havingClause`.
having
  :: Condition ('Grouped bys) lat with db params from -- ^ having condition
  -> TableExpression ('Grouped bys) lat with db params from
  -> TableExpression ('Grouped bys) lat with db params from
having hv rels = rels
  { havingClause = case havingClause rels of Having hvs -> Having (hv:hvs) }

instance OrderBy (TableExpression grp) grp where
  orderBy srts rels = rels {orderByClause = orderByClause rels ++ srts}

-- | A `limit` is an endomorphism of `TableExpression`s which adds to the
-- `limitClause`.
limit
  :: Word64 -- ^ limit parameter
  -> TableExpression grp lat with db params from
  -> TableExpression grp lat with db params from
limit lim rels = rels {limitClause = lim : limitClause rels}

-- | An `offset` is an endomorphism of `TableExpression`s which adds to the
-- `offsetClause`.
offset
  :: Word64 -- ^ offset parameter
  -> TableExpression grp lat with db params from
  -> TableExpression grp lat with db params from
offset off rels = rels {offsetClause = off : offsetClause rels}

{-----------------------------------------
Grouping
-----------------------------------------}

-- | `By`s are used in `groupBy` to reference a list of columns which are then
-- used to group together those rows in a table that have the same values
-- in all the columns listed. @By \#col@ will reference an unambiguous
-- column @col@; otherwise @By2 (\#tab \! \#col)@ will reference a table
-- qualified column @tab.col@.
data By
    (from :: FromType)
    (by :: (Symbol,Symbol)) where
    By1
      :: (HasUnique table from columns, Has column columns ty)
      => Alias column
      -> By from '(table, column)
    By2
      :: (Has table from columns, Has column columns ty)
      => Alias table
      -> Alias column
      -> By from '(table, column)
deriving instance Show (By from by)
deriving instance Eq (By from by)
deriving instance Ord (By from by)
instance RenderSQL (By from by) where
  renderSQL = \case
    By1 column -> renderSQL column
    By2 rel column -> renderSQL rel <> "." <> renderSQL column

instance (HasUnique rel rels cols, Has col cols ty, by ~ '(rel, col))
  => IsLabel col (By rels by) where fromLabel = By1 fromLabel
instance (HasUnique rel rels cols, Has col cols ty, bys ~ '[ '(rel, col)])
  => IsLabel col (NP (By rels) bys) where fromLabel = By1 fromLabel :* Nil
instance (Has rel rels cols, Has col cols ty, by ~ '(rel, col))
  => IsQualified rel col (By rels by) where (!) = By2
instance (Has rel rels cols, Has col cols ty, bys ~ '[ '(rel, col)])
  => IsQualified rel col (NP (By rels) bys) where
    rel ! col = By2 rel col :* Nil

-- | A `GroupByClause` indicates the `Grouping` of a `TableExpression`.
newtype GroupByClause grp from = UnsafeGroupByClause
  { renderGroupByClause :: ByteString }
  deriving stock (GHC.Generic,Show,Eq,Ord)
  deriving newtype (NFData)
instance RenderSQL (GroupByClause grp from) where
  renderSQL = renderGroupByClause
noGroups :: GroupByClause 'Ungrouped from
noGroups = UnsafeGroupByClause ""
group
  :: SListI bys
  => NP (By from) bys
  -> GroupByClause ('Grouped bys) from
group bys = UnsafeGroupByClause $ case bys of
  Nil -> ""
  _ -> " GROUP BY" <+> renderCommaSeparated renderSQL bys

-- | A `HavingClause` is used to eliminate groups that are not of interest.
-- An `Ungrouped` `TableExpression` may only use `NoHaving` while a `Grouped`
-- `TableExpression` must use `Having` whose conditions are combined with
-- `.&&`.
data HavingClause grp lat with db params from where
  NoHaving :: HavingClause 'Ungrouped lat with db params from
  Having
    :: [Condition ('Grouped bys) lat with db params from]
    -> HavingClause ('Grouped bys) lat with db params from
deriving instance Show (HavingClause grp lat with db params from)
deriving instance Eq (HavingClause grp lat with db params from)
deriving instance Ord (HavingClause grp lat with db params from)

-- | Render a `HavingClause`.
instance RenderSQL (HavingClause grp lat with db params from) where
  renderSQL = \case
    NoHaving -> ""
    Having [] -> ""
    Having conditions ->
      " HAVING" <+> commaSeparated (renderSQL <$> conditions)

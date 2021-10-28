{-|
Module: Squeal.PostgreSQL.Query.Table
Description: intermediate table expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

intermediate table expressions
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
  , lockRows
    -- * Grouping
  , By (..)
  , GroupByClause (..)
  , HavingClause (..)
    -- * Row Locks
  , LockingClause (..)
  , LockStrength (..)
  , Waiting (..)
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
-- `offsetClause` and `lockingClauses`. Trivial table expressions simply refer
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
    , lockingClauses :: [LockingClause from]
    -- ^ `lockingClauses` can be added to a table expression with `lockRows`.
    } deriving (GHC.Generic)

-- | Render a `TableExpression`
instance RenderSQL (TableExpression grp lat with db params from) where
  renderSQL
    (TableExpression frm' whs' grps' hvs' srts' lims' offs' lks') = mconcat
      [ "FROM ", renderSQL frm'
      , renderWheres whs'
      , renderSQL grps'
      , renderSQL hvs'
      , renderSQL srts'
      , renderLimits lims'
      , renderOffsets offs'
      , renderLocks lks' ]
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
        renderLocks = foldr (\l b -> b <+> renderSQL l) ""

-- | A `from` generates a `TableExpression` from a table reference that can be
-- a table name, or a derived table such as a subquery, a JOIN construct,
-- or complex combinations of these. A `from` may be transformed by `where_`,
-- `groupBy`, `having`, `orderBy`, `limit` and `offset`,
-- using the `Data.Function.&` operator
-- to match the left-to-right sequencing of their placement in SQL.
from
  :: FromClause lat with db params from -- ^ table reference
  -> TableExpression 'Ungrouped lat with db params from
from tab = TableExpression tab [] noGroups NoHaving [] [] [] []

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
  , lockingClauses = lockingClauses rels
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

{- | Add a `LockingClause` to a `TableExpression`.
Multiple `LockingClause`s can be written if it is necessary
to specify different locking behavior for different tables.
If the same table is mentioned (or implicitly affected)
by more than one locking clause, then it is processed
as if it was only specified by the strongest one.
Similarly, a table is processed as `NoWait` if that is specified
in any of the clauses affecting it. Otherwise, it is processed
as `SkipLocked` if that is specified in any of the clauses affecting it.
Further, a `LockingClause` cannot be added to a grouped table expression.
-}
lockRows
  :: LockingClause from -- ^ row-level lock
  -> TableExpression 'Ungrouped lat with db params from
  -> TableExpression 'Ungrouped lat with db params from
lockRows lck tab = tab {lockingClauses = lck : lockingClauses tab}

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

{- |
If specific tables are named in a locking clause,
then only rows coming from those tables are locked;
any other tables used in the `Squeal.PostgreSQL.Query.Select.select` are simply read as usual.
A locking clause with a `Nil` table list affects all tables used in the statement.
If a locking clause is applied to a `view` or `subquery`,
it affects all tables used in the `view` or `subquery`.
However, these clauses do not apply to `Squeal.PostgreSQL.Query.With.with` queries referenced by the primary query.
If you want row locking to occur within a `Squeal.PostgreSQL.Query.With.with` query,
specify a `LockingClause` within the `Squeal.PostgreSQL.Query.With.with` query.
-}
data LockingClause from where
  For
    :: HasAll tabs from tables
    => LockStrength -- ^ lock strength
    -> NP Alias tabs -- ^ table list
    -> Waiting -- ^ wait or not
    -> LockingClause from
instance RenderSQL (LockingClause from) where
  renderSQL (For str tabs wt) =
    "FOR" <+> renderSQL str
    <> case tabs of
        Nil -> ""
        _ -> " OF" <+> renderSQL tabs
    <> renderSQL wt

{- |
Row-level locks, which are listed as below with the contexts
in which they are used automatically by PostgreSQL.
Note that a transaction can hold conflicting locks on the same row,
even in different subtransactions; but other than that,
two transactions can never hold conflicting locks on the same row.
Row-level locks do not affect data querying;
they block only writers and lockers to the same row.
Row-level locks are released at transaction end or during savepoint rollback.
-}
data LockStrength
  = Update
  {- ^ `For` `Update` causes the rows retrieved by the `Squeal.PostgreSQL.Query.Select.select` statement
  to be locked as though for update. This prevents them from being locked,
  modified or deleted by other transactions until the current transaction ends.
  That is, other transactions that attempt `Squeal.PostgreSQL.Manipulation.Update.update`, `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom`,
  `Squeal.PostgreSQL.Query.Select.select` `For` `Update`, `Squeal.PostgreSQL.Query.Select.select` `For` `NoKeyUpdate`,
  `Squeal.PostgreSQL.Query.Select.select` `For` `Share` or `Squeal.PostgreSQL.Query.Select.select` `For` `KeyShare` of these rows will be blocked
  until the current transaction ends; conversely, `Squeal.PostgreSQL.Query.Select.select` `For` `Update` will wait
  for a concurrent transaction that has run any of those commands on the same row,
  and will then lock and return the updated row (or no row, if the row was deleted).
  Within a `Squeal.PostgreSQL.Session.Transaction.RepeatableRead` or `Squeal.PostgreSQL.Session.Transaction.Serializable` transaction, however, an error will be
  thrown if a row to be locked has changed since the transaction started.

  The `For` `Update` lock mode is also acquired by any `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom` a row,
  and also by an `Update` that modifies the values on certain columns.
  Currently, the set of columns considered for the `Squeal.PostgreSQL.Manipulation.Update.update` case are those
  that have a unique index on them that can be used in a foreign key
  (so partial indexes and expressional indexes are not considered),
  but this may change in the future.-}
  | NoKeyUpdate
  {- | Behaves similarly to `For` `Update`, except that the lock acquired is weaker:
  this lock will not block `Squeal.PostgreSQL.Query.Select.select` `For` `KeyShare` commands that attempt to acquire
  a lock on the same rows. This lock mode is also acquired by any `Squeal.PostgreSQL.Manipulation.Update.update`
  that does not acquire a `For` `Update` lock.-}
  | Share
  {- | Behaves similarly to `For` `Share`, except that the lock is weaker:
  `Squeal.PostgreSQL.Query.Select.select` `For` `Update` is blocked, but not `Squeal.PostgreSQL.Query.Select.select` `For` `NoKeyUpdate`.
  A key-shared lock blocks other transactions from performing
  `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom` or any `Squeal.PostgreSQL.Manipulation.Update.update` that changes the key values,
  but not other `Update`, and neither does it prevent `Squeal.PostgreSQL.Query.Select.select` `For` `NoKeyUpdate`,
  `Squeal.PostgreSQL.Query.Select.select` `For` `Share`, or `Squeal.PostgreSQL.Query.Select.select` `For` `KeyShare`.-}
  | KeyShare
  deriving (Eq, Ord, Show, Read, Enum, GHC.Generic)
instance RenderSQL LockStrength where
  renderSQL = \case
    Update -> "UPDATE"
    NoKeyUpdate -> "NO KEY UPDATE"
    Share -> "SHARE"
    KeyShare -> "KEY SHARE"

-- | To prevent the operation from `Waiting` for other transactions to commit,
-- use either the `NoWait` or `SkipLocked` option.
data Waiting
  = Wait
  -- ^ wait for other transactions to commit
  | NoWait
  -- ^ reports an error, rather than waiting
  | SkipLocked
  -- ^ any selected rows that cannot be immediately locked are skipped
  deriving (Eq, Ord, Show, Read, Enum, GHC.Generic)
instance RenderSQL Waiting where
  renderSQL = \case
    Wait -> ""
    NoWait -> " NOWAIT"
    SkipLocked -> " SKIP LOCKED"

{-|
Module: Squeal.PostgreSQL.Manipulation.Insert
Description: Squeal insert statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal insert statements.
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

module Squeal.PostgreSQL.Manipulation.Insert
  ( -- * Insert
    insertInto
  , insertInto_
    -- * Clauses
  , QueryClause (..)
  , pattern Values_
  , inlineValues
  , inlineValues_
  , ConflictClause (..)
  , ConflictTarget (..)
  , ConflictAction (..)
  ) where

import Data.ByteString hiding (foldr)

import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Default
import Squeal.PostgreSQL.Expression.Inline
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Query.Table
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
INSERT statements
-----------------------------------------}

{- |
When a table is created, it contains no data. The first thing to do
before a database can be of much use is to insert data. Data is
conceptually inserted one row at a time. Of course you can also insert
more than one row, but there is no way to insert less than one row.
Even if you know only some column values, a complete row must be created.
-}
insertInto
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , SOP.SListI (TableToColumns table)
     , SOP.SListI row )
  => QualifiedAlias sch tab
  -- ^ table
  -> QueryClause with db params (TableToColumns table)
  -- ^ what to insert
  -> ConflictClause tab with db params table
  -- ^ what to do in case of conflict
  -> ReturningClause with db params '[tab ::: TableToRow table] row
  -- ^ what to return
  -> Manipulation with db params row
insertInto tab qry conflict ret = UnsafeManipulation $
  "INSERT" <+> "INTO" <+> renderSQL tab
  <+> renderSQL qry
  <> renderSQL conflict
  <> renderSQL ret

-- | Like `insertInto` but with `OnConflictDoRaise` and no `ReturningClause`.
insertInto_
  :: ( Has sch db schema
     , Has tab schema ('Table table)
     , SOP.SListI (TableToColumns table) )
  => QualifiedAlias sch tab
  -- ^ table
  -> QueryClause with db params (TableToColumns table)
  -- ^ what to insert
  -> Manipulation with db params '[]
insertInto_ tab qry =
  insertInto tab qry OnConflictDoRaise (Returning_ Nil)

-- | A `QueryClause` describes what to `insertInto` a table.
data QueryClause with db params columns where
  Values
    :: SOP.SListI columns
    => NP (Aliased (Optional (Expression  'Ungrouped '[] with db params '[]))) columns
    -- ^ row of values
    -> [NP (Aliased (Optional (Expression  'Ungrouped '[] with db params '[]))) columns]
    -- ^ additional rows of values
    -> QueryClause with db params columns
  Select
    :: SOP.SListI columns
    => NP (Aliased (Optional (Expression grp '[] with db params from))) columns
    -- ^ row of values
    -> TableExpression grp '[] with db params from
    -- ^ from a table expression
    -> QueryClause with db params columns
  Subquery
    :: ColumnsToRow columns ~ row
    => Query '[] with db params row
    -- ^ subquery to insert
    -> QueryClause with db params columns

instance RenderSQL (QueryClause with db params columns) where
  renderSQL = \case
    Values row0 rows ->
      parenthesized (renderCommaSeparated renderSQLPart row0)
      <+> "VALUES"
      <+> commaSeparated
            ( parenthesized
            . renderCommaSeparated renderValuePart <$> row0 : rows )
    Select row0 tab ->
      parenthesized (renderCommaSeparatedMaybe renderSQLPartMaybe row0)
      <+> "SELECT"
      <+> renderCommaSeparatedMaybe renderValuePartMaybe row0
      <+> renderSQL tab
    Subquery qry -> renderQuery qry
    where
      renderSQLPartMaybe, renderValuePartMaybe
        :: Aliased (Optional (Expression grp '[] with db params from)) column
        -> Maybe ByteString
      renderSQLPartMaybe = \case
        Default `As` _ -> Nothing
        Set _ `As` name -> Just $ renderSQL name
      renderValuePartMaybe = \case
        Default `As` _ -> Nothing
        Set value `As` _ -> Just $ renderExpression value
      renderSQLPart, renderValuePart
        :: Aliased (Optional (Expression grp '[] with db params from)) column
        -> ByteString
      renderSQLPart (_ `As` name) = renderSQL name
      renderValuePart (value `As` _) = renderSQL value

-- | `Values_` describes a single `NP` list of `Aliased` `Optional` `Expression`s
-- whose `ColumnsType` must match the tables'.
pattern Values_
  :: SOP.SListI columns
  => NP (Aliased (Optional (Expression  'Ungrouped '[] with db params '[]))) columns
  -- ^ row of values
  -> QueryClause with db params columns
pattern Values_ vals = Values vals []

-- | `inlineValues_` a Haskell record in `insertInto`.
inlineValues_
  :: ( SOP.IsRecord hask xs
     , SOP.AllZip InlineColumn xs columns )
  => hask -- ^ record
  -> QueryClause with db params columns
inlineValues_ = Values_ . inlineColumns

-- | `inlineValues` Haskell records in `insertInto`.
inlineValues
  :: ( SOP.IsRecord hask xs
     , SOP.AllZip InlineColumn xs columns )
  => hask -- ^ record
  -> [hask] -- ^ more
  -> QueryClause with db params columns
inlineValues hask hasks = Values (inlineColumns hask) (inlineColumns <$> hasks)

-- | A `ConflictClause` specifies an action to perform upon a constraint
-- violation. `OnConflictDoRaise` will raise an error.
-- `OnConflict` `DoNothing` simply avoids inserting a row.
-- `OnConflict` `DoUpdate` updates the existing row that conflicts with the row
-- proposed for insertion.
data ConflictClause tab with db params table where
  OnConflictDoRaise :: ConflictClause tab with db params table
  OnConflict
    :: ConflictTarget table
    -- ^ conflict target
    -> ConflictAction tab with db params table
    -- ^ conflict action
    -> ConflictClause tab with db params table

-- | Render a `ConflictClause`.
instance SOP.SListI (TableToColumns table)
  => RenderSQL (ConflictClause tab with db params table) where
    renderSQL = \case
      OnConflictDoRaise -> ""
      OnConflict target action -> " ON CONFLICT"
        <+> renderSQL target <+> renderSQL action

{- |
`ConflictAction` specifies an alternative `OnConflict` action.
It can be either `DoNothing`, or a `DoUpdate` clause specifying
the exact details of the update action to be performed in case of a conflict.
The `Set` and WHERE `Condition`s in `OnConflict` `DoUpdate` have access to the
existing row using the table's name, and to rows proposed
for insertion using the special @#excluded@ row.
`OnConflict` `DoNothing` simply avoids inserting a row as its alternative action.
`OnConflict` `DoUpdate` updates the existing row that conflicts
with the row proposed for insertion as its alternative action.
-}
data ConflictAction tab with db params table where
  DoNothing :: ConflictAction tab with db params table
  DoUpdate
    :: ( row ~ TableToRow table
       , from ~ '[tab ::: row, "excluded" ::: row]
       , Updatable table updates )
    => NP (Aliased (Optional (Expression  'Ungrouped '[] with db params from))) updates
    -> [Condition  'Ungrouped '[] with db params from]
       -- ^ WHERE `Condition`s
    -> ConflictAction tab with db params table

instance RenderSQL (ConflictAction tab with db params table) where
  renderSQL = \case
    DoNothing -> "DO NOTHING"
    DoUpdate updates whs'
      -> "DO UPDATE SET"
        <+> renderCommaSeparated renderUpdate updates
        <> case whs' of
          [] -> ""
          wh:whs -> " WHERE" <+> renderSQL (foldr (.&&) wh whs)

renderUpdate
  :: (forall x. RenderSQL (expr x))
  => Aliased (Optional expr) ty
  -> ByteString
renderUpdate (expr `As` col) = renderSQL col <+> "=" <+> renderSQL expr

-- | A `ConflictTarget` specifies the constraint violation that triggers a
-- `ConflictAction`.
data ConflictTarget table where
  OnConstraint
    :: Has con constraints constraint
    => Alias con
    -> ConflictTarget (constraints :=> columns)

-- | Render a `ConflictTarget`
instance RenderSQL (ConflictTarget constraints) where
  renderSQL (OnConstraint con) =
    "ON" <+> "CONSTRAINT" <+> renderSQL con

{-|
Module: Squeal.PostgreSQL.Query.From
Description: Squeal from clauses.
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal from clauses.
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

module Squeal.PostgreSQL.Query.From
  ( -- * From Clause
    FromClause (..)
  , table
  , subquery
  , view
  , common
    -- * Join
  , JoinItem (..)
  , cross, crossJoin, crossJoinLateral
  , inner, innerJoin, innerJoinLateral
  , leftOuter, leftOuterJoin, leftOuterJoinLateral
  , rightOuter, rightOuterJoin, rightOuterJoinLateral
  , fullOuter, fullOuterJoin, fullOuterJoinLateral
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Generics.SOP hiding (from)

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
FROM clauses
-----------------------------------------}

{- |
A `FromClause` can be a table name, or a derived table such
as a subquery, a @JOIN@ construct, or complex combinations of these.
-}
newtype FromClause
  (lat :: FromType)
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (from :: FromType)
  = UnsafeFromClause { renderFromClause :: ByteString }
  deriving stock (GHC.Generic,Show,Eq,Ord)
  deriving newtype (NFData)
instance RenderSQL (FromClause lat with db params from) where
  renderSQL = renderFromClause

-- | A real `table` is a table from the database.
table
  :: (Has sch db schema, Has tab schema ('Table table))
  => Aliased (QualifiedAlias sch) (alias ::: tab) -- ^ (renamable) table alias
  -> FromClause lat with db params '[alias ::: TableToRow table]
table (tab `As` alias) = UnsafeFromClause $
  renderSQL tab <+> "AS" <+> renderSQL alias

-- | `subquery` derives a table from a `Query`.
subquery
  :: Aliased (Query lat with db params) query
  -- ^ aliased `Query`
  -> FromClause lat with db params '[query]
subquery = UnsafeFromClause . renderAliased (parenthesized . renderSQL)

-- | `view` derives a table from a `View`.
view
  :: (Has sch db schema, Has vw schema ('View view))
  => Aliased (QualifiedAlias sch) (alias ::: vw) -- ^ (renamable) view alias
  -> FromClause lat with db params '[alias ::: view]
view (vw `As` alias) = UnsafeFromClause $
  renderSQL vw <+> "AS" <+> renderSQL alias

-- | `common` derives a table from a common table expression.
common
  :: Has cte with common
  => Aliased Alias (alias ::: cte) -- ^ (renamable) common table expression alias
  -> FromClause lat with db params '[alias ::: common]
common (cte `As` alias) = UnsafeFromClause $
  renderSQL cte <+> "AS" <+> renderSQL alias

instance Additional (FromClause lat with db params) where
  also right left = UnsafeFromClause $
    renderSQL left <> ", " <> renderSQL right

{- |
A `JoinItem` is the right hand side of a `cross`,
`inner`, `leftOuter`, `rightOuter`, `fullOuter` join of
`FromClause`s.
-}
data JoinItem
  (lat :: FromType)
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (left :: FromType)
  (right :: FromType) where
    -- | A standard `Squeal.PostgreSQL.Query.Join`.
    -- It is not allowed to reference columns provided
    -- by preceding `FromClause` items.
    Join
      :: FromClause lat with db params right
      -> JoinItem lat with db params left right
    -- | Subqueries can be preceded by `JoinLateral`.
    -- This allows them to reference columns provided
    -- by preceding `FromClause` items.
    -- `subquery` is evaluated independently and so
    -- cannot cross-reference any other `FromClause` item.
    JoinLateral
      :: Aliased (Query (Join lat left) with db params) query
      -> JoinItem lat with db params left '[query]
    -- | Set returning functions can be preceded by `JoinFunction`.
    -- This allows them to reference columns provided
    -- by preceding `FromClause` items.
    JoinFunction
      :: (Expression 'Ungrouped lat with db params '[] ty -> FromClause lat with db params from)
      -> Expression 'Ungrouped lat with db params left ty
      -> JoinItem lat with db params left from
    -- | Set returning multi-argument functions
    -- can be preceded by `JoinFunction`.
    -- This allows them to reference columns provided
    -- by preceding `FromClause` items.
    JoinFunctionN
      :: SListI tys
      => (NP (Expression 'Ungrouped lat with db params '[]) tys -> FromClause lat with db params from)
      -> NP (Expression 'Ungrouped lat with db params left) tys
      -> JoinItem lat with db params left from
instance RenderSQL (JoinItem lat with db params left right) where
  renderSQL = \case
    Join tab -> "JOIN" <+> renderSQL tab
    JoinLateral qry -> "JOIN LATERAL" <+>
      renderAliased (parenthesized . renderSQL) qry
    JoinFunction fun x -> "JOIN" <+>
      renderSQL (fun (UnsafeExpression (renderSQL x)))
    JoinFunctionN fun xs -> "JOIN" <+>
      renderSQL (fun (SOP.hmap (UnsafeExpression . renderSQL) xs))

{- |
@left & cross (Join right)@. For every possible combination of rows from
@left@ and @right@ (i.e., a Cartesian product), the joined table will contain
a row consisting of all columns in @left@ followed by all columns in @right@.
If the tables have @n@ and @m@ rows respectively, the joined table will
have @n * m@ rows.
-}
cross
  :: JoinItem lat with db params left right -- ^ right
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left right)
cross item tab = UnsafeFromClause $
  renderSQL tab <+> "CROSS" <+> renderSQL item

{- |
@left & crossJoin right@. For every possible combination of rows from
@left@ and @right@ (i.e., a Cartesian product), the joined table will contain
a row consisting of all columns in @left@ followed by all columns in @right@.
If the tables have @n@ and @m@ rows respectively, the joined table will
have @n * m@ rows.
-}
crossJoin
  :: FromClause lat with db params right -- ^ right
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left right)
crossJoin = cross . Join

{- |
Like `crossJoin` with a `subquery` but allowed to reference columns provided
by preceding `FromClause` items.
-}
crossJoinLateral
  :: Aliased (Query (Join lat left) with db params) query -- ^ right subquery
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left '[query])
crossJoinLateral = cross . JoinLateral

{- | @left & inner (Join right) on@. The joined table is filtered by
the @on@ condition.
-}
inner
  :: JoinItem lat with db params left right -- ^ right
  -> Condition 'Ungrouped lat with db params (Join left right) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left right)
inner item on tab = UnsafeFromClause $
  renderSQL tab <+> "INNER" <+> renderSQL item <+> "ON" <+> renderSQL on

{- | @left & innerJoin right on@. The joined table is filtered by
the @on@ condition.
-}
innerJoin
  :: FromClause lat with db params right -- ^ right
  -> Condition 'Ungrouped lat with db params (Join left right) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left right)
innerJoin = inner . Join

{- |
Like `innerJoin` with a `subquery` but allowed to reference columns provided
by preceding `FromClause` items.
-}
innerJoinLateral
  :: Aliased (Query (Join lat left) with db params) query -- ^ right subquery
  -> Condition 'Ungrouped lat with db params (Join left '[query]) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left '[query])
innerJoinLateral = inner . JoinLateral

{- | @left & leftOuter (Join right) on@. First, an inner join is performed.
Then, for each row in @left@ that does not satisfy the @on@ condition with
any row in @right@, a joined row is added with null values in columns of @right@.
Thus, the joined table always has at least one row for each row in @left@.
-}
leftOuter
  :: JoinItem lat with db params left right -- ^ right
  -> Condition 'Ungrouped lat with db params (Join left right) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left (NullifyFrom right))
leftOuter item on tab = UnsafeFromClause $
  renderSQL tab <+> "LEFT OUTER" <+> renderSQL item <+> "ON" <+> renderSQL on

{- | @left & leftOuterJoin right on@. First, an inner join is performed.
Then, for each row in @left@ that does not satisfy the @on@ condition with
any row in @right@, a joined row is added with null values in columns of @right@.
Thus, the joined table always has at least one row for each row in @left@.
-}
leftOuterJoin
  :: FromClause lat with db params right -- ^ right
  -> Condition 'Ungrouped lat with db params (Join left right) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left (NullifyFrom right))
leftOuterJoin = leftOuter . Join

{- |
Like `leftOuterJoin` with a `subquery` but allowed to reference columns provided
by preceding `FromClause` items.
-}
leftOuterJoinLateral
  :: Aliased (Query (Join lat left) with db params) query -- ^ right subquery
  -> Condition 'Ungrouped lat with db params (Join left '[query]) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join left (NullifyFrom '[query]))
leftOuterJoinLateral = leftOuter . JoinLateral

{- | @left & rightOuter (Join right) on@. First, an inner join is performed.
Then, for each row in @right@ that does not satisfy the @on@ condition with
any row in @left@, a joined row is added with null values in columns of @left@.
This is the converse of a left join: the result table will always
have a row for each row in @right@.
-}
rightOuter
  :: JoinItem lat with db params left right -- ^ right
  -> Condition 'Ungrouped lat with db params (Join left right) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join (NullifyFrom left) right)
rightOuter item on tab = UnsafeFromClause $
  renderSQL tab <+> "RIGHT OUTER" <+> renderSQL item <+> "ON" <+> renderSQL on

{- | @left & rightOuterJoin right on@. First, an inner join is performed.
Then, for each row in @right@ that does not satisfy the @on@ condition with
any row in @left@, a joined row is added with null values in columns of @left@.
This is the converse of a left join: the result table will always
have a row for each row in @right@.
-}
rightOuterJoin
  :: FromClause lat with db params right -- ^ right
  -> Condition 'Ungrouped lat with db params (Join left right) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join (NullifyFrom left) right)
rightOuterJoin = rightOuter . Join

{- |
Like `rightOuterJoin` with a `subquery` but allowed to reference columns provided
by preceding `FromClause` items.
-}
rightOuterJoinLateral
  :: Aliased (Query (Join lat left) with db params) query -- ^ right subquery
  -> Condition 'Ungrouped lat with db params (Join left '[query]) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (Join (NullifyFrom left) '[query])
rightOuterJoinLateral = rightOuter . JoinLateral

{- | @left & fullOuter (Join right) on@. First, an inner join is performed.
Then, for each row in @left@ that does not satisfy the @on@ condition with
any row in @right@, a joined row is added with null values in columns of @right@.
Also, for each row of @right@ that does not satisfy the join condition
with any row in @left@, a joined row with null values in the columns of @left@
is added.
-}
fullOuter
  :: JoinItem lat with db params left right -- ^ right
  -> Condition 'Ungrouped lat with db params (Join left right) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (NullifyFrom (Join left right))
fullOuter item on tab = UnsafeFromClause $
  renderSQL tab <+> "FULL OUTER" <+> renderSQL item <+> "ON" <+> renderSQL on

{- | @left & fullOuterJoin right on@. First, an inner join is performed.
Then, for each row in @left@ that does not satisfy the @on@ condition with
any row in @right@, a joined row is added with null values in columns of @right@.
Also, for each row of @right@ that does not satisfy the join condition
with any row in @left@, a joined row with null values in the columns of @left@
is added.
-}
fullOuterJoin
  :: FromClause lat with db params right -- ^ right
  -> Condition 'Ungrouped lat with db params (Join left right) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (NullifyFrom (Join left right))
fullOuterJoin = fullOuter . Join

{- |
Like `fullOuterJoin` with a `subquery` but allowed to reference columns provided
by preceding `FromClause` items.
-}
fullOuterJoinLateral
  :: Aliased (Query (Join lat left) with db params) query -- ^ right subquery
  -> Condition 'Ungrouped lat with db params (Join left '[query]) -- ^ @ON@ condition
  -> FromClause lat with db params left -- ^ left
  -> FromClause lat with db params (NullifyFrom (Join left '[query]))
fullOuterJoinLateral = fullOuter . JoinLateral

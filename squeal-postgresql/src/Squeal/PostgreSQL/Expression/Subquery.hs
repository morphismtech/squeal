{-|
Module: Squeal.PostgreSQL.Expression.Subquery
Description: Subquery expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Subquery expressions
-}

{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , RankNTypes
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Subquery
  ( exists
  , in_
  , notIn
  , subAll
  , subAny
  ) where

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import Squeal.PostgreSQL.Render

{- |
The argument of `exists` is an arbitrary subquery. The subquery is evaluated
to determine whether it returns any rows. If it returns at least one row,
the result of `exists` is `true`; if the subquery returns no rows,
the result of `exists` is `false`.

The subquery can refer to variables from the surrounding query,
which will act as constants during any one evaluation of the subquery.

The subquery will generally only be executed long enough to determine whether
at least one row is returned, not all the way to completion.
-}
exists
  :: Query (Join outer from) commons schemas params row
  -> Condition outer commons grp schemas params from
exists query = UnsafeExpression $ "EXISTS" <+> parenthesized (renderSQL query)

{- |
The right-hand side is a parenthesized subquery, which must return
exactly one column. The left-hand expression is evaluated and compared to each
row of the subquery result using the given `Operator`,
which must yield a Boolean result. The result of `subAll` is `true`
if all rows yield true (including the case where the subquery returns no rows).
The result is `false` if any `false` result is found.
The result is `Squeal.PostgreSQL.Expression.Null.null_` if
  no comparison with a subquery row returns `false`,
and at least one comparison returns `Squeal.PostgreSQL.Expression.Null.null_`.

>>> printSQL $ subAll true (.==) (values_ (true `as` #foo))
(TRUE = ALL (SELECT * FROM (VALUES (TRUE)) AS t ("foo")))
-}
subAll
  :: Expression outer commons grp schemas params from ty1 -- ^ expression
  -> Operator ty1 ty2 ('Null 'PGbool) -- ^ operator
  -> Query (Join outer from) commons schemas params '[col ::: ty2] -- ^ subquery
  -> Condition outer commons grp schemas params from
subAll expr (?) qry = expr ?
  (UnsafeExpression $ "ALL" <+> parenthesized (renderSQL qry))

{- |
The right-hand side is a parenthesized subquery, which must return exactly one column.
The left-hand expression is evaluated and compared to each row of the subquery result
using the given `Operator`, which must yield a Boolean result. The result of `subAny` is `true`
if any `true` result is obtained. The result is `false` if no true result is found
(including the case where the subquery returns no rows).

>>> printSQL $ subAll "foo" like (values_ ("foobar" `as` #foo))
(E'foo' LIKE ALL (SELECT * FROM (VALUES (E'foobar')) AS t ("foo")))
-}
subAny
  :: Expression outer commons grp schemas params from ty1 -- ^ expression
  -> Operator ty1 ty2 ('Null 'PGbool) -- ^ operator
  -> Query (Join outer from) commons schemas params '[col ::: ty2] -- ^ subquery
  -> Condition outer commons grp schemas params from
subAny expr (?) qry = expr ?
  (UnsafeExpression $ "ANY" <+> parenthesized (renderSQL qry))

{- |
The result is `true` if the left-hand expression's result is equal
to any of the right-hand expressions.

>>> printSQL $ true `in_` [true, false, null_]
TRUE IN (TRUE, FALSE, NULL)
-}
in_
  :: Expression outer commons grp schemas params from ty -- ^ expression
  -> [Expression outer commons grp schemas params from ty]
  -> Condition outer commons grp schemas params from
expr `in_` exprs = UnsafeExpression $ renderSQL expr <+> "IN"
  <+> parenthesized (commaSeparated (renderSQL <$> exprs))

{- |
The result is `true` if the left-hand expression's result is not equal
to any of the right-hand expressions.

>>> printSQL $ true `notIn` [false, null_]
TRUE NOT IN (FALSE, NULL)
-}
notIn
  :: Expression outer commons grp schemas params from ty -- ^ expression
  -> [Expression outer commons grp schemas params from ty]
  -> Condition outer commons grp schemas params from
expr `notIn` exprs = UnsafeExpression $ renderSQL expr <+> "NOT IN"
  <+> parenthesized (commaSeparated (renderSQL <$> exprs))

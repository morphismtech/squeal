{-|
Module: Squeal.PostgreSQL.Expression.Logic
Description: Logical expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Logical expressions and operators
-}

{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Logic
  ( Condition
  , true
  , false
  , not_
  , (.&&)
  , (.||)
  , caseWhenThenElse
  , ifThenElse
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- | A `Condition` is an `Expression`, which can evaluate
-- to `true`, `false` or `Squeal.PostgreSQL.Null.null_`. This is because SQL uses
-- a three valued logic.
type Condition outer commons grp schemas params from =
  Expression outer commons grp schemas params from ('Null 'PGbool)

-- | >>> printSQL true
-- TRUE
true :: Expr (null 'PGbool)
true = UnsafeExpression "TRUE"

-- | >>> printSQL false
-- FALSE
false :: Expr (null 'PGbool)
false = UnsafeExpression "FALSE"

-- | >>> printSQL $ not_ true
-- (NOT TRUE)
not_ :: null 'PGbool --> null 'PGbool
not_ = unsafeLeftOp "NOT"

-- | >>> printSQL $ true .&& false
-- (TRUE AND FALSE)
(.&&) :: Operator (null 'PGbool) (null 'PGbool) (null 'PGbool)
infixr 3 .&&
(.&&) = unsafeBinaryOp "AND"

-- | >>> printSQL $ true .|| false
-- (TRUE OR FALSE)
(.||) :: Operator (null 'PGbool) (null 'PGbool) (null 'PGbool)
infixr 2 .||
(.||) = unsafeBinaryOp "OR"

-- | >>> :{
-- let
--   expression :: Expression outer commons grp schemas params from (null 'PGint2)
--   expression = caseWhenThenElse [(true, 1), (false, 2)] 3
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN 1 WHEN FALSE THEN 2 ELSE 3 END
caseWhenThenElse
  :: [ ( Condition outer commons grp schemas params from
       , Expression outer commons grp schemas params from ty
     ) ]
  -- ^ whens and thens
  -> Expression outer commons grp schemas params from ty
  -- ^ else
  -> Expression outer commons grp schemas params from ty
caseWhenThenElse whenThens else_ = UnsafeExpression $ mconcat
  [ "CASE"
  , mconcat
    [ mconcat
      [ " WHEN ", renderSQL when_
      , " THEN ", renderSQL then_
      ]
    | (when_,then_) <- whenThens
    ]
  , " ELSE ", renderSQL else_
  , " END"
  ]

-- | >>> :{
-- let
--   expression :: Expression outer commons grp schemas params from (null 'PGint2)
--   expression = ifThenElse true 1 0
-- in printSQL expression
-- :}
-- CASE WHEN TRUE THEN 1 ELSE 0 END
ifThenElse
  :: Condition outer commons grp schemas params from
  -> Expression outer commons grp schemas params from ty -- ^ then
  -> Expression outer commons grp schemas params from ty -- ^ else
  -> Expression outer commons grp schemas params from ty
ifThenElse if_ then_ else_ = caseWhenThenElse [(if_,then_)] else_

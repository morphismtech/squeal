{-|
Module: Squeal.PostgreSQL.Query.Values
Description: values statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

values statements
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
  , DataKinds
  , PolyKinds
  , TypeOperators
  , RankNTypes
  , UndecidableInstances
  #-}

module Squeal.PostgreSQL.Query.Values
  ( -- ** Values
    values
  , values_
  ) where

import Data.ByteString (ByteString)
import Generics.SOP hiding (from)

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render

-- $setup
-- >>> import Squeal.PostgreSQL

-- | `values` computes a row value or set of row values
-- specified by value expressions. It is most commonly used
-- to generate a “constant table” within a larger command,
-- but it can be used on its own.
--
-- >>> type Row = '["a" ::: 'NotNull 'PGint4, "b" ::: 'NotNull 'PGtext]
-- >>> let query = values (1 `as` #a :* "one" `as` #b) [] :: Query lat with db '[] Row
-- >>> printSQL query
-- SELECT * FROM (VALUES ((1 :: int4), (E'one' :: text))) AS t ("a", "b")
values
  :: SListI cols
  => NP (Aliased (Expression 'Ungrouped lat with db params '[] )) cols
  -> [NP (Aliased (Expression 'Ungrouped lat with db params '[] )) cols]
  -- ^ When more than one row is specified, all the rows must
  -- must have the same number of elements
  -> Query lat with db params cols
values rw rws = UnsafeQuery $ "SELECT * FROM"
  <+> parenthesized (
    "VALUES"
    <+> commaSeparated
        ( parenthesized
        . renderCommaSeparated renderValuePart <$> rw:rws )
    ) <+> "AS t"
  <+> parenthesized (renderCommaSeparated renderAliasPart rw)
  where
    renderAliasPart, renderValuePart
      :: Aliased (Expression 'Ungrouped lat with db params '[] ) ty -> ByteString
    renderAliasPart (_ `As` name) = renderSQL name
    renderValuePart (value `As` _) = renderSQL value

-- | `values_` computes a row value or set of row values
-- specified by value expressions.
values_
  :: SListI cols
  => NP (Aliased (Expression 'Ungrouped lat with db params '[] )) cols
  -- ^ one row of values
  -> Query lat with db params cols
values_ rw = values rw []

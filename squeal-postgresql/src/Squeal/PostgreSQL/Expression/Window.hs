{-|
Module: Squeal.PostgreSQL.Expression.Window
Description: Window functions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Window functions and definitions
-}

{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  , KindSignatures
#-}

module Squeal.PostgreSQL.Expression.Window
  ( WindowDefinition (..)
  , partitionBy
  , WindowFunction (..)
  , rank
  , rowNumber
  , denseRank
  , percentRank
  , cumeDist
  , ntile
  , lag
  , lead
  , firstValue
  , lastValue
  , nthValue
  , WinFun0
  , WinFun1
  , WinFunN
  ) where

import Control.DeepSeq
import Data.ByteString

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Aggregate
import Squeal.PostgreSQL.Expression.Sort
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

instance Aggregate
  (Expression outer commons grp schemas params from)
  (NP (Expression outer commons grp schemas params from))
  (WindowFunction outer commons grp schemas params from) where
    countStar = UnsafeWindowFunction "count(*)"
    count = unsafeWindowFunction1 "count"
    sum_ = unsafeWindowFunction1 "sum"
    arrayAgg = unsafeWindowFunction1 "array_agg"
    jsonAgg = unsafeWindowFunction1 "json_agg"
    jsonbAgg = unsafeWindowFunction1 "jsonb_agg"
    bitAnd = unsafeWindowFunction1 "bit_and"
    bitOr = unsafeWindowFunction1 "bit_or"
    boolAnd = unsafeWindowFunction1 "bool_and"
    boolOr = unsafeWindowFunction1 "bool_or"
    every = unsafeWindowFunction1 "every"
    max_ = unsafeWindowFunction1 "max"
    min_ = unsafeWindowFunction1 "min"
    avg = unsafeWindowFunction1 "avg"
    corr = unsafeWindowFunctionN "corr"
    covarPop = unsafeWindowFunctionN "covar_pop"
    covarSamp = unsafeWindowFunctionN "covar_samp"
    regrAvgX = unsafeWindowFunctionN "regr_avgx"
    regrAvgY = unsafeWindowFunctionN "regr_avgy"
    regrCount = unsafeWindowFunctionN "regr_count"
    regrIntercept = unsafeWindowFunctionN "regr_intercept"
    regrR2 = unsafeWindowFunctionN "regr_r2"
    regrSlope = unsafeWindowFunctionN "regr_slope"
    regrSxx = unsafeWindowFunctionN "regr_sxx"
    regrSxy = unsafeWindowFunctionN "regr_sxy"
    regrSyy = unsafeWindowFunctionN "regr_syy"
    stddev = unsafeWindowFunction1 "stddev"
    stddevPop = unsafeWindowFunction1 "stddev_pop"
    stddevSamp = unsafeWindowFunction1 "stddev_samp"
    variance = unsafeWindowFunction1 "variance"
    varPop = unsafeWindowFunction1 "var_pop"
    varSamp = unsafeWindowFunction1 "var_samp"


-- | A `WindowDefinition` is a set of table rows that are somehow related
-- to the current row
data WindowDefinition outer commons grp schemas params from where
  WindowDefinition
    :: SOP.SListI bys
    => NP (Expression outer commons grp schemas params from) bys
       -- ^ partitions
    -> [SortExpression outer commons grp schemas params from]
       -- ^ ordering
    -> WindowDefinition outer commons grp schemas params from

instance OrderBy WindowDefinition where
  orderBy sortsR (WindowDefinition parts sortsL)
    = WindowDefinition parts (sortsL ++ sortsR)

instance RenderSQL (WindowDefinition outer commons schemas from grp params) where
  renderSQL (WindowDefinition part ord) =
    renderPartitionByClause part <> renderOrderByClause ord
    where
      renderPartitionByClause = \case
        Nil -> ""
        parts -> "PARTITION" <+> "BY" <+> renderCommaSeparated renderExpression parts
      renderOrderByClause = \case
        [] -> ""
        srts -> " ORDER" <+> "BY"
          <+> commaSeparated (renderSQL <$> srts)

{- |
The `partitionBy` clause within `Squeal.PostgreSQL.Query.Over` divides the rows into groups,
or partitions, that share the same values of the `partitionBy` `Expression`(s).
For each row, the window function is computed across the rows that fall into
the same partition as the current row.
-}
partitionBy
  :: SOP.SListI bys
  => NP (Expression outer commons grp schemas params from) bys -- ^ partitions
  -> WindowDefinition outer commons grp schemas params from
partitionBy bys = WindowDefinition bys []

{- |
A window function performs a calculation across a set of table rows
that are somehow related to the current row. This is comparable to the type
of calculation that can be done with an aggregate function.
However, window functions do not cause rows to become grouped into a single
output row like non-window aggregate calls would.
Instead, the rows retain their separate identities.
Behind the scenes, the window function is able to access more than
just the current row of the query result.
-}
newtype WindowFunction
  (outer :: FromType)
  (commons :: FromType)
  (grp :: Grouping)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (from :: FromType)
  (ty :: NullityType)
    = UnsafeWindowFunction { renderWindowFunction :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance RenderSQL (WindowFunction outer commons grp schemas params from ty) where
  renderSQL = renderWindowFunction

{- |
A @RankNType@ for window functions with no arguments.
-}
type WinFun0 x
  = forall outer commons grp schemas params from
  . WindowFunction outer commons grp schemas params from x
    -- ^ cannot reference aliases

{- |
A @RankNType@ for window functions with 1 argument.
-}
type WinFun1 x y
  =  forall outer commons grp schemas params from
  .  Expression outer commons grp schemas params from x
     -- ^ input
  -> WindowFunction outer commons grp schemas params from y
     -- ^ output

{- | A @RankNType@ for window functions with a fixed-length
list of heterogeneous arguments.
Use the `*:` operator to end your argument lists.
-}
type WinFunN xs y
  =  forall outer commons grp schemas params from
  .  NP (Expression outer commons grp schemas params from) xs
     -- ^ inputs
  -> WindowFunction outer commons grp schemas params from y
     -- ^ output

unsafeWindowFunction1 :: ByteString -> WinFun1 x y
unsafeWindowFunction1 fun x
  = UnsafeWindowFunction $ fun <> parenthesized (renderSQL x)

unsafeWindowFunctionN :: SOP.SListI xs => ByteString -> WinFunN xs y
unsafeWindowFunctionN fun xs = UnsafeWindowFunction $ fun <>
  parenthesized (renderCommaSeparated renderSQL xs)

{- | rank of the current row with gaps; same as `rowNumber` of its first peer

>>> printSQL rank
rank()
-}
rank :: WinFun0 ('NotNull 'PGint8)
rank = UnsafeWindowFunction "rank()"

{- | number of the current row within its partition, counting from 1

>>> printSQL rowNumber
row_number()
-}
rowNumber :: WinFun0 ('NotNull 'PGint8)
rowNumber = UnsafeWindowFunction "row_number()"

{- | rank of the current row without gaps; this function counts peer groups

>>> printSQL denseRank
dense_rank()
-}
denseRank :: WinFun0 ('NotNull 'PGint8)
denseRank = UnsafeWindowFunction "dense_rank()"

{- | relative rank of the current row: (rank - 1) / (total partition rows - 1)

>>> printSQL percentRank
percent_rank()
-}
percentRank :: WinFun0 ('NotNull 'PGfloat8)
percentRank = UnsafeWindowFunction "percent_rank()"

{- | cumulative distribution: (number of partition rows
preceding or peer with current row) / total partition rows

>>> printSQL cumeDist
cume_dist()
-}
cumeDist :: WinFun0 ('NotNull 'PGfloat8)
cumeDist = UnsafeWindowFunction "cume_dist()"

{- | integer ranging from 1 to the argument value,
dividing the partition as equally as possible

>>> printSQL $ ntile 5
ntile(5)
-}
ntile :: WinFun1 ('NotNull 'PGint4) ('NotNull 'PGint4)
ntile = unsafeWindowFunction1 "ntile"

{- | returns value evaluated at the row that is offset rows before the current
row within the partition; if there is no such row, instead return default
(which must be of the same type as value). Both offset and default are
evaluated with respect to the current row.
-}
lag :: WinFunN '[ty, 'NotNull 'PGint4, ty] ty
lag = unsafeWindowFunctionN "lag"

{- | returns value evaluated at the row that is offset rows after the current
row within the partition; if there is no such row, instead return default
(which must be of the same type as value). Both offset and default are
evaluated with respect to the current row.
-}
lead :: WinFunN '[ty, 'NotNull 'PGint4, ty] ty
lead = unsafeWindowFunctionN "lead"

{- | returns value evaluated at the row that is the
first row of the window frame
-}
firstValue :: WinFun1 ty ty
firstValue = unsafeWindowFunction1 "first_value"

{- | returns value evaluated at the row that is the
last row of the window frame
-}
lastValue :: WinFun1 ty ty
lastValue = unsafeWindowFunction1 "last_value"

{- | returns value evaluated at the row that is the nth
row of the window frame (counting from 1); null if no such row
-}
nthValue :: WinFunN '[null ty, 'NotNull 'PGint4] ('Null ty)
nthValue = unsafeWindowFunctionN "nth_value"

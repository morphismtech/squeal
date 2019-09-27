{-|
Module: Squeal.PostgreSQL.Expression.TextSearch
Description: Text search expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Text search functions and operators
-}

{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.TextSearch
  ( (@@)
  , (.&)
  , (.|)
  , (.!)
  , (<->)
  , arrayToTSvector
  , tsvectorLength
  , numnode
  , plainToTSquery
  , phraseToTSquery
  , websearchToTSquery
  , queryTree
  , toTSquery
  , toTSvector
  , setWeight
  , strip
  , jsonToTSvector
  , jsonbToTSvector
  , tsDelete
  , tsFilter
  , tsHeadline
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Schema

-- | `Squeal.PostgreSQL.Expression.Type.tsvector` matches tsquery ?
(@@) :: Operator (null 'PGtsvector) (null 'PGtsquery) ('Null 'PGbool)
(@@) = unsafeBinaryOp "@@"

-- | AND `Squeal.PostgreSQL.Expression.Type.tsquery`s together
(.&) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(.&) = unsafeBinaryOp "&&"

-- | OR `Squeal.PostgreSQL.Expression.Type.tsquery`s together
(.|) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(.|) = unsafeBinaryOp "||"

-- | negate a `Squeal.PostgreSQL.Expression.Type.tsquery`
(.!) :: null 'PGtsquery --> null 'PGtsquery
(.!) = unsafeLeftOp "!!"

-- | `Squeal.PostgreSQL.Expression.Type.tsquery` followed by
-- `Squeal.PostgreSQL.Expression.Type.tsquery`
(<->) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(<->) = unsafeBinaryOp "<->"

-- | convert array of lexemes to `Squeal.PostgreSQL.Expression.Type.tsvector`
arrayToTSvector
  ::   null ('PGvararray ('NotNull 'PGtext))
  --> null 'PGtsvector
arrayToTSvector = unsafeFunction "array_to_tsvector"

-- | number of lexemes in `Squeal.PostgreSQL.Expression.Type.tsvector`
tsvectorLength :: null 'PGtsvector --> null 'PGint4
tsvectorLength = unsafeFunction "length"

-- | number of lexemes plus operators in `Squeal.PostgreSQL.Expression.Type.tsquery`
numnode :: null 'PGtsquery --> null 'PGint4
numnode = unsafeFunction "numnode"

-- | produce `Squeal.PostgreSQL.Expression.Type.tsquery` ignoring punctuation
plainToTSquery :: null 'PGtext --> null 'PGtsquery
plainToTSquery = unsafeFunction "plainto_tsquery"

-- | produce `Squeal.PostgreSQL.Expression.Type.tsquery` that searches for a phrase,
-- ignoring punctuation
phraseToTSquery :: null 'PGtext --> null 'PGtsquery
phraseToTSquery = unsafeFunction "phraseto_tsquery"

-- | produce `Squeal.PostgreSQL.Expression.Type.tsquery` from a web search style query
websearchToTSquery :: null 'PGtext --> null 'PGtsquery
websearchToTSquery = unsafeFunction "websearch_to_tsquery"

-- | get indexable part of a `Squeal.PostgreSQL.Expression.Type.tsquery`
queryTree :: null 'PGtsquery --> null 'PGtext
queryTree = unsafeFunction "query_tree"

-- | normalize words and convert to `Squeal.PostgreSQL.Expression.Type.tsquery`
toTSquery :: null 'PGtext --> null 'PGtsquery
toTSquery = unsafeFunction "to_tsquery"

-- | reduce document text to `Squeal.PostgreSQL.Expression.Type.tsvector`
toTSvector
  :: ty `In` '[ 'PGtext, 'PGjson, 'PGjsonb]
  => null ty --> null 'PGtsvector
toTSvector = unsafeFunction "to_tsvector"

-- | assign weight to each element of `Squeal.PostgreSQL.Expression.Type.tsvector`
setWeight :: '[null 'PGtsvector, null ('PGchar 1)] ---> null 'PGtsvector
setWeight = unsafeFunctionN "set_weight"

-- | remove positions and weights from `Squeal.PostgreSQL.Expression.Type.tsvector`
strip :: null 'PGtsvector --> null 'PGtsvector
strip = unsafeFunction "strip"

-- | @jsonToTSvector (document *: filter)@
-- reduce each value in the document, specified by filter to a `Squeal.PostgreSQL.Expression.Type.tsvector`,
-- and then concatenate those in document order to produce a single `Squeal.PostgreSQL.Expression.Type.tsvector`.
-- filter is a `Squeal.PostgreSQL.Expression.Type.json` array, that enumerates what kind of elements
-- need to be included into the resulting `Squeal.PostgreSQL.Expression.Type.tsvector`.
-- Possible values for filter are "string" (to include all string values),
-- "numeric" (to include all numeric values in the string format),
-- "boolean" (to include all Boolean values in the string format "true"/"false"),
-- "key" (to include all keys) or "all" (to include all above).
-- These values can be combined together to include, e.g. all string and numeric values.
jsonToTSvector :: '[null 'PGjson, null 'PGjson] ---> null 'PGtsvector
jsonToTSvector = unsafeFunctionN "json_to_tsvector"

-- | @jsonbToTSvector (document *: filter)@
-- reduce each value in the document, specified by filter to a `Squeal.PostgreSQL.Expression.Type.tsvector`,
-- and then concatenate those in document order to produce a single `Squeal.PostgreSQL.Expression.Type.tsvector`.
-- filter is a `Squeal.PostgreSQL.Expression.Type.jsonb` array, that enumerates what kind of elements
-- need to be included into the resulting `Squeal.PostgreSQL.Expression.Type.tsvector`.
-- Possible values for filter are "string" (to include all string values),
-- "numeric" (to include all numeric values in the string format),
-- "boolean" (to include all Boolean values in the string format "true"/"false"),
-- "key" (to include all keys) or "all" (to include all above).
-- These values can be combined together to include, e.g. all string and numeric values.
jsonbToTSvector :: '[null 'PGjsonb, null 'PGjsonb] ---> null 'PGtsvector
jsonbToTSvector = unsafeFunctionN "jsonb_to_tsvector"

-- | remove given lexeme from `Squeal.PostgreSQL.Expression.Type.tsvector`
tsDelete ::
  '[null 'PGtsvector, null ('PGvararray ('NotNull 'PGtext))]
   ---> null 'PGtsvector
tsDelete = unsafeFunctionN "ts_delete"

-- | select only elements with given weights from `Squeal.PostgreSQL.Expression.Type.tsvector`
tsFilter ::
  '[null 'PGtsvector, null ('PGvararray ('NotNull ('PGchar 1)))]
   ---> null 'PGtsvector
tsFilter = unsafeFunctionN "ts_filter"

-- | display a `Squeal.PostgreSQL.Expression.Type.tsquery` match
tsHeadline
  :: document `In` '[ 'PGtext, 'PGjson, 'PGjsonb]
  => '[null document, null 'PGtsquery] ---> null 'PGtext
tsHeadline = unsafeFunctionN "ts_headline"

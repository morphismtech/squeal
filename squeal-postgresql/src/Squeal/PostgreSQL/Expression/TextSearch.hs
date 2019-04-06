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

(@@) :: Operator (null 'PGtsvector) (null 'PGtsquery) ('Null 'PGbool)
(@@) = unsafeBinaryOp "@@"

(.&) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(.&) = unsafeBinaryOp "&&"

(.|) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(.|) = unsafeBinaryOp "||"

(.!) :: null 'PGtsquery :--> null 'PGtsquery
(.!) = unsafeUnaryOpL "!!"

(<->) :: Operator (null 'PGtsquery) (null 'PGtsquery) (null 'PGtsquery)
(<->) = unsafeBinaryOp "<->"

arrayToTSvector
  ::   null ('PGvararray ('NotNull 'PGtext))
  :--> null 'PGtsvector
arrayToTSvector = unsafeFunction "array_to_tsvector"

tsvectorLength :: null 'PGtsvector :--> null 'PGint4
tsvectorLength = unsafeFunction "length"

numnode :: null 'PGtsquery :--> null 'PGint4
numnode = unsafeFunction "numnode"

plainToTSquery :: null 'PGtext :--> null 'PGtsquery
plainToTSquery = unsafeFunction "plainto_tsquery"

phraseToTSquery :: null 'PGtext :--> null 'PGtsquery
phraseToTSquery = unsafeFunction "phraseto_tsquery"

websearchToTSquery :: null 'PGtext :--> null 'PGtsquery
websearchToTSquery = unsafeFunction "websearch_to_tsquery"

queryTree :: null 'PGtsquery :--> null 'PGtext
queryTree = unsafeFunction "query_tree"

toTSquery :: null 'PGtext :--> null 'PGtsquery
toTSquery = unsafeFunction "to_tsquery"

toTSvector
  :: ty `In` '[ 'PGtext, 'PGjson, 'PGjsonb]
  => null ty :--> null 'PGtsvector
toTSvector = unsafeFunction "to_tsvector"

setWeight
  :: FunctionN '[null 'PGtsvector, null ('PGchar 1)] (null 'PGtsvector)
setWeight = unsafeFunctionN "set_weight"

strip :: null 'PGtsvector :--> null 'PGtsvector
strip = unsafeFunction "strip"

jsonToTSvector :: FunctionN '[null 'PGjson, null 'PGjson] (null 'PGtsvector)
jsonToTSvector = unsafeFunctionN "json_to_tsvector"

jsonbToTSvector :: FunctionN '[null 'PGjsonb, null 'PGjsonb] (null 'PGtsvector)
jsonbToTSvector = unsafeFunctionN "jsonb_to_tsvector"

tsDelete :: FunctionN
  '[null 'PGtsvector, null ('PGvararray ('NotNull 'PGtext))]
   (null 'PGtsvector)
tsDelete = unsafeFunctionN "ts_delete"

tsFilter :: FunctionN
  '[null 'PGtsvector, null ('PGvararray ('NotNull ('PGchar 1)))]
   (null 'PGtsvector)
tsFilter = unsafeFunctionN "ts_filter"

tsHeadline
  :: document `In` '[ 'PGtext, 'PGjson, 'PGjsonb]
  => FunctionN '[null document, null 'PGtsquery] (null 'PGtext)
tsHeadline = unsafeFunctionN "ts_headline"

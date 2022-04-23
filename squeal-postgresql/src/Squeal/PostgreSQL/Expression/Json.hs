{-|
Module: Squeal.PostgreSQL.Expression.Json
Description: json and jsonb functions and operators
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

json and jsonb functions and operators
-}

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , OverloadedLabels
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Expression.Json
  ( -- * Json and Jsonb Operators
    (.->)
  , (.->>)
  , (.#>)
  , (.#>>)
    -- * Jsonb Operators
  , (.?)
  , (.?|)
  , (.?&)
  , (.-.)
  , (#-.)
    -- * Json and Jsonb Functions
  , toJson
  , toJsonb
  , arrayToJson
  , rowToJson
  , jsonBuildArray
  , jsonbBuildArray
  , JsonBuildObject (..)
  , jsonObject
  , jsonbObject
  , jsonZipObject
  , jsonbZipObject
  , jsonArrayLength
  , jsonbArrayLength
  , jsonTypeof
  , jsonbTypeof
  , jsonStripNulls
  , jsonbStripNulls
  , jsonbSet
  , jsonbInsert
  , jsonbPretty
    -- * Json and Jsonb Set Functions
  , jsonEach
  , jsonbEach
  , jsonEachText
  , jsonArrayElementsText
  , jsonbEachText
  , jsonbArrayElementsText
  , jsonObjectKeys
  , jsonbObjectKeys
  , JsonPopulateFunction
  , jsonPopulateRecord
  , jsonbPopulateRecord
  , jsonPopulateRecordSet
  , jsonbPopulateRecordSet
  , JsonToRecordFunction
  , jsonToRecord
  , jsonbToRecord
  , jsonToRecordSet
  , jsonbToRecordSet
  ) where

import Data.ByteString (ByteString)
import GHC.TypeLits

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Query.From
import Squeal.PostgreSQL.Query.From.Set
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

import qualified Generics.SOP as SOP

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import Data.Aeson

{-----------------------------------------
 -- json and jsonb support

See https://www.postgresql.org/docs/10/static/functions-json.html -- most
comments lifted directly from this page.

Table 9.44: json and jsonb operators
-----------------------------------------}

-- | Get JSON value (object field or array element) at a key.
(.->)
  :: (json `In` PGJsonType, key `In` PGJsonKey)
  => Operator (null json) (null key) ('Null json)
infixl 8 .->
(.->) = unsafeBinaryOp "->"

-- | Get JSON value (object field or array element) at a key, as text.
(.->>)
  :: (json `In` PGJsonType, key `In` PGJsonKey)
  => Operator (null json) (null key) ('Null 'PGtext)
infixl 8 .->>
(.->>) = unsafeBinaryOp "->>"

-- | Get JSON value at a specified path.
(.#>)
  :: json `In` PGJsonType
  => Operator (null json) (null ('PGvararray ('NotNull 'PGtext))) ('Null json)
infixl 8 .#>
(.#>) = unsafeBinaryOp "#>"

-- | Get JSON value at a specified path as text.
(.#>>)
  :: json `In` PGJsonType
  => Operator (null json) (null ('PGvararray ('NotNull 'PGtext))) ('Null 'PGtext)
infixl 8 .#>>
(.#>>) = unsafeBinaryOp "#>>"

-- Additional jsonb operators

-- | Does the string exist as a top-level key within the JSON value?
(.?) :: Operator (null 'PGjsonb) (null 'PGtext) ('Null 'PGbool)
infixl 9 .?
(.?) = unsafeBinaryOp "?"

-- | Do any of these array strings exist as top-level keys?
(.?|) :: Operator
  (null 'PGjsonb)
  (null ('PGvararray ('NotNull 'PGtext)))
  ('Null 'PGbool)
infixl 9 .?|
(.?|) = unsafeBinaryOp "?|"

-- | Do all of these array strings exist as top-level keys?
(.?&) :: Operator
  (null 'PGjsonb)
  (null ('PGvararray ('NotNull 'PGtext)))
  ('Null 'PGbool)
infixl 9 .?&
(.?&) = unsafeBinaryOp "?&"

-- | Delete a key or keys from a JSON object, or remove an array element.
--
-- If the right operand is
--
-- @ text @: Delete key / value pair or string element from left operand.
-- Key / value pairs are matched based on their key value,
--
-- @ text[] @: Delete multiple key / value pairs or string elements
-- from left operand. Key / value pairs are matched based on their key value,
--
-- @ integer @: Delete the array element with specified index (Negative integers
-- count from the end). Throws an error if top level container is not an array.
(.-.)
  :: key `In` '[ 'PGtext, 'PGvararray ('NotNull 'PGtext), 'PGint4, 'PGint2 ]
  => Operator (null 'PGjsonb) (null key) (null 'PGjsonb)
infixl 6 .-.
(.-.) = unsafeBinaryOp "-"

-- | Delete the field or element with specified path (for JSON arrays, negative
-- integers count from the end)
(#-.) :: Operator (null 'PGjsonb) (null ('PGvararray ('NotNull 'PGtext))) (null 'PGjsonb)
infixl 6 #-.
(#-.) = unsafeBinaryOp "#-"

{-----------------------------------------
Table 9.45: JSON creation functions
-----------------------------------------}

-- | Returns the value as json. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid json value.
toJson :: null ty --> null 'PGjson
toJson = unsafeFunction "to_json"

-- | Returns the value as jsonb. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid jsonb value.
toJsonb :: null ty --> null 'PGjsonb
toJsonb = unsafeFunction "to_jsonb"

-- | Returns the array as a JSON array. A PostgreSQL multidimensional array
-- becomes a JSON array of arrays.
arrayToJson :: null ('PGvararray ty) --> null 'PGjson
arrayToJson = unsafeFunction "array_to_json"

-- | Returns the row as a JSON object.
rowToJson :: null ('PGcomposite ty) --> null 'PGjson
rowToJson = unsafeFunction "row_to_json"

-- | Builds a possibly-heterogeneously-typed JSON array out of a variadic
-- argument list.
jsonBuildArray :: SOP.SListI tuple => tuple ---> null 'PGjson
jsonBuildArray = unsafeFunctionN "json_build_array"

-- | Builds a possibly-heterogeneously-typed (binary) JSON array out of a
-- variadic argument list.
jsonbBuildArray :: SOP.SListI tuple => tuple ---> null 'PGjsonb
jsonbBuildArray = unsafeFunctionN "jsonb_build_array"

-- | Builds a possibly-heterogeneously-typed JSON object out of a variadic
-- argument list. The elements of the argument list must alternate between text
-- and values.
class SOP.SListI tys => JsonBuildObject tys where

  jsonBuildObject :: tys ---> null 'PGjson
  jsonBuildObject = unsafeFunctionN "json_build_object"

  jsonbBuildObject :: tys ---> null 'PGjsonb
  jsonbBuildObject = unsafeFunctionN "jsonb_build_object"

instance JsonBuildObject '[]
instance (JsonBuildObject tys, key `In` PGJsonKey)
  => JsonBuildObject ('NotNull key ': value ': tys)

-- | Builds a JSON object out of a text array.
-- The array must have two dimensions
-- such that each inner array has exactly two elements,
-- which are taken as a key/value pair.
jsonObject
  ::   null ('PGfixarray '[n,2] ('NotNull 'PGtext))
  --> null 'PGjson
jsonObject = unsafeFunction "json_object"

-- | Builds a binary JSON object out of a text array.
-- The array must have two dimensions
-- such that each inner array has exactly two elements,
-- which are taken as a key/value pair.
jsonbObject
  ::   null ('PGfixarray '[n,2] ('NotNull 'PGtext))
  --> null 'PGjsonb
jsonbObject = unsafeFunction "jsonb_object"

-- | This is an alternate form of 'jsonObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a JSON object.
jsonZipObject ::
  '[ null ('PGvararray ('NotNull 'PGtext))
   , null ('PGvararray ('NotNull 'PGtext)) ]
   ---> null 'PGjson
jsonZipObject = unsafeFunctionN "json_object"

-- | This is an alternate form of 'jsonbObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a binary JSON
-- object.
jsonbZipObject ::
  '[ null ('PGvararray ('NotNull 'PGtext))
   , null ('PGvararray ('NotNull 'PGtext)) ]
   ---> null 'PGjsonb
jsonbZipObject = unsafeFunctionN "jsonb_object"

{-----------------------------------------
Table 9.46: JSON processing functions
-----------------------------------------}

-- | Returns the number of elements in the outermost JSON array.
jsonArrayLength :: null 'PGjson --> null 'PGint4
jsonArrayLength = unsafeFunction "json_array_length"

-- | Returns the number of elements in the outermost binary JSON array.
jsonbArrayLength :: null 'PGjsonb --> null 'PGint4
jsonbArrayLength = unsafeFunction "jsonb_array_length"

-- | Returns the type of the outermost JSON value as a text string. Possible
-- types are object, array, string, number, boolean, and null.
jsonTypeof :: null 'PGjson --> null 'PGtext
jsonTypeof = unsafeFunction "json_typeof"

-- | Returns the type of the outermost binary JSON value as a text string.
-- Possible types are object, array, string, number, boolean, and null.
jsonbTypeof :: null 'PGjsonb --> null 'PGtext
jsonbTypeof = unsafeFunction "jsonb_typeof"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonStripNulls :: null 'PGjson --> null 'PGjson
jsonStripNulls = unsafeFunction "json_strip_nulls"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonbStripNulls :: null 'PGjsonb --> null 'PGjsonb
jsonbStripNulls = unsafeFunction "jsonb_strip_nulls"

-- | @ jsonbSet target path new_value create_missing @
--
-- Returns target with the section designated by path replaced by @new_value@,
-- or with @new_value@ added if create_missing is
-- `Squeal.PostgreSQL.Expression.Logic.true` and the
-- item designated by path does not exist. As with the path orientated
-- operators, negative integers that appear in path count from the end of JSON
-- arrays.
jsonbSet ::
  '[ null 'PGjsonb, null ('PGvararray ('NotNull 'PGtext))
   , null 'PGjsonb, null 'PGbool ] ---> null 'PGjsonb
jsonbSet = unsafeFunctionN "jsonbSet"

-- | @ jsonbInsert target path new_value insert_after @
--
-- Returns target with @new_value@ inserted. If target section designated by
-- path is in a JSONB array, @new_value@ will be inserted before target or after
-- if @insert_after@ is `Squeal.PostgreSQL.Expression.Logic.true`.
-- If target section designated by
-- path is in JSONB object, @new_value@ will be inserted only if target does not
-- exist. As with the path orientated operators, negative integers that appear
-- in path count from the end of JSON arrays.
jsonbInsert ::
  '[ null 'PGjsonb, null ('PGvararray ('NotNull 'PGtext))
   , null 'PGjsonb, null 'PGbool ] ---> null 'PGjsonb
jsonbInsert = unsafeFunctionN "jsonb_insert"

-- | Returns its argument as indented JSON text.
jsonbPretty :: null 'PGjsonb --> null 'PGtext
jsonbPretty = unsafeFunction "jsonb_pretty"

{- | Expands the outermost JSON object into a set of key/value pairs.

>>> printSQL (select Star (from (jsonEach (inline (Json (object ["a" .= "foo"]))))))
SELECT * FROM json_each(('{"a":"foo"}' :: json))
-}
jsonEach :: null 'PGjson -|->
  ("json_each" ::: '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGjson])
jsonEach = unsafeSetFunction "json_each"

{- | Expands the outermost binary JSON object into a set of key/value pairs.

>>> printSQL (select Star (from (jsonbEach (inline (Jsonb (object ["a" .= "foo"]))))))
SELECT * FROM jsonb_each(('{"a":"foo"}' :: jsonb))
-}
jsonbEach
  :: null 'PGjsonb -|->
    ("jsonb_each" ::: '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGjson])
jsonbEach = unsafeSetFunction "jsonb_each"

{- | Expands the outermost JSON object into a set of key/value pairs.

>>> printSQL (select Star (from (jsonEachText (inline (Json (object ["a" .= "foo"]))))))
SELECT * FROM json_each_text(('{"a":"foo"}' :: json))
-}
jsonEachText
  :: null 'PGjson -|->
    ("json_each_text" ::: '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGtext])
jsonEachText = unsafeSetFunction "json_each_text"

{- | Returns a set of text values from a JSON array

>>> printSQL (select Star (from (jsonArrayElementsText (inline (Json (toJSON ["monkey", "pony", "bear"] ))))))
SELECT * FROM json_array_elements_text(('["monkey","pony","bear"]' :: json))
-}
jsonArrayElementsText
  :: null 'PGjson -|->
    ("json_array_elements_text" ::: '["value" ::: 'NotNull 'PGtext])
jsonArrayElementsText = unsafeSetFunction "json_array_elements_text"

{- | Expands the outermost binary JSON object into a set of key/value pairs.

>>> printSQL (select Star (from (jsonbEachText (inline (Jsonb (object ["a" .= "foo"]))))))
SELECT * FROM jsonb_each_text(('{"a":"foo"}' :: jsonb))
-}
jsonbEachText
  :: null 'PGjsonb -|->
    ("jsonb_each_text" ::: '["key" ::: 'NotNull 'PGtext, "value" ::: 'NotNull 'PGtext])
jsonbEachText = unsafeSetFunction "jsonb_each_text"

{- | Returns set of keys in the outermost JSON object.

>>> printSQL (jsonObjectKeys (inline (Json (object ["a" .= "foo"]))))
json_object_keys(('{"a":"foo"}' :: json))
-}
jsonObjectKeys
  :: null 'PGjson -|->
    ("json_object_keys" ::: '["json_object_keys" ::: 'NotNull 'PGtext])
jsonObjectKeys = unsafeSetFunction "json_object_keys"

{- | Returns set of keys in the outermost JSON object.

>>> printSQL (jsonbObjectKeys (inline (Jsonb (object ["a" .= "foo"]))))
jsonb_object_keys(('{"a":"foo"}' :: jsonb))
-}
jsonbObjectKeys
  :: null 'PGjsonb -|->
    ("jsonb_object_keys" ::: '["jsonb_object_keys" ::: 'NotNull 'PGtext])
jsonbObjectKeys = unsafeSetFunction "jsonb_object_keys"

{- | Returns a set of text values from a binary JSON array

>>> printSQL (select Star (from (jsonbArrayElementsText (inline (Jsonb (toJSON ["red", "green", "cyan"] ))))))
SELECT * FROM jsonb_array_elements_text(('["red","green","cyan"]' :: jsonb))
-}
jsonbArrayElementsText
  :: null 'PGjsonb -|->
    ("jsonb_array_elements_text" ::: '["value" ::: 'NotNull 'PGtext])
jsonbArrayElementsText = unsafeSetFunction "jsonb_array_elements_text"

-- | Build rows from Json types.
type JsonPopulateFunction fun json
  =  forall db row lat with params
  .  json `In` PGJsonType
  => TypeExpression db ('NotNull ('PGcomposite row)) -- ^ row type
  -> Expression 'Ungrouped lat with db params '[] ('NotNull json)
      -- ^ json type
  -> FromClause lat with db params '[fun ::: row]

unsafePopulateFunction
  :: forall fun ty
   . KnownSymbol fun => Alias fun -> JsonPopulateFunction fun ty
unsafePopulateFunction _fun ty expr = UnsafeFromClause $ renderSymbol @fun
  <> parenthesized ("null::" <> renderSQL ty <> ", " <> renderSQL expr)

-- | Expands the JSON expression to a row whose columns match the record
-- type defined by the given table.
jsonPopulateRecord :: JsonPopulateFunction "json_populate_record" 'PGjson
jsonPopulateRecord = unsafePopulateFunction #json_populate_record

-- | Expands the binary JSON expression to a row whose columns match the record
-- type defined by the given table.
jsonbPopulateRecord :: JsonPopulateFunction "jsonb_populate_record" 'PGjsonb
jsonbPopulateRecord = unsafePopulateFunction #jsonb_populate_record

-- | Expands the outermost array of objects in the given JSON expression to a
-- set of rows whose columns match the record type defined by the given table.
jsonPopulateRecordSet :: JsonPopulateFunction "json_populate_record_set" 'PGjson
jsonPopulateRecordSet = unsafePopulateFunction #json_populate_record_set

-- | Expands the outermost array of objects in the given binary JSON expression
-- to a set of rows whose columns match the record type defined by the given
-- table.
jsonbPopulateRecordSet :: JsonPopulateFunction "jsonb_populate_record_set" 'PGjsonb
jsonbPopulateRecordSet = unsafePopulateFunction #jsonb_populate_record_set

-- | Build rows from Json types.
type JsonToRecordFunction json
  =  forall lat with db params tab row
  .  (SOP.SListI row, json `In` PGJsonType)
  => Expression 'Ungrouped lat with db params '[] ('NotNull json)
      -- ^ json type
  -> Aliased (NP (Aliased (TypeExpression db))) (tab ::: row)
      -- ^ row type
  -> FromClause lat with db params '[tab ::: row]

unsafeRecordFunction :: ByteString -> JsonToRecordFunction json
unsafeRecordFunction fun expr (types `As` tab) = UnsafeFromClause $
  fun <> parenthesized (renderSQL expr) <+> "AS" <+> renderSQL tab
    <> parenthesized (renderCommaSeparated renderTy types)
    where
      renderTy :: Aliased (TypeExpression db) ty -> ByteString
      renderTy (ty `As` alias) = renderSQL alias <+> renderSQL ty

-- | Builds an arbitrary record from a JSON object.
jsonToRecord :: JsonToRecordFunction 'PGjson
jsonToRecord = unsafeRecordFunction "json_to_record"

-- | Builds an arbitrary record from a binary JSON object.
jsonbToRecord :: JsonToRecordFunction 'PGjsonb
jsonbToRecord = unsafeRecordFunction "jsonb_to_record"

-- | Builds an arbitrary set of records from a JSON array of objects.
jsonToRecordSet :: JsonToRecordFunction 'PGjson
jsonToRecordSet = unsafeRecordFunction "json_to_record_set"

-- | Builds an arbitrary set of records from a binary JSON array of objects.
jsonbToRecordSet :: JsonToRecordFunction 'PGjsonb
jsonbToRecordSet = unsafeRecordFunction "jsonb_to_record_set"

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings
  , PolyKinds
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Json
  ( -- * Json and Jsonb operators
    (.->)
  , (.->>)
  , (.#>)
  , (.#>>)
    -- * Jsonb operators
  , (.?)
  , (.?|)
  , (.?&)
  , (.-.)
  , (#-.)
    -- * Json and Jsonb functions
  , toJson
  , toJsonb
  , arrayToJson
  , rowToJson
  , jsonBuildArray
  , jsonbBuildArray
  , PGBuildObject
  , jsonBuildObject
  , jsonbBuildObject
  , jsonObject
  , jsonbObject
  , jsonZipObject
  , jsonbZipObject
  , jsonArrayLength
  , jsonbArrayLength
  , jsonExtractPath
  , jsonbExtractPath
  , jsonExtractPathAsText
  , jsonbExtractPathAsText
  , jsonTypeof
  , jsonbTypeof
  , jsonStripNulls
  , jsonbStripNulls
  , jsonbSet
  , jsonbInsert
  , jsonbPretty
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Schema

import qualified Generics.SOP as SOP

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
-- If the right operand is..
--
-- @ text @: Delete key/value pair or string element from left operand. Key/value pairs
-- are matched based on their key value.
--
-- @ text[] @: Delete multiple key/value pairs or string elements
-- from left operand. Key/value pairs are matched based on their key value.
--
-- @ integer @: Delete the array element with specified index (Negative integers
-- count from the end). Throws an error if top level container is not an array.
(.-.)
  :: (key `In` '[ 'PGtext, 'PGvararray ('NotNull 'PGtext), 'PGint4, 'PGint2 ]) -- hlint error without parens here
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
toJson :: null ty :--> null 'PGjson
toJson = unsafeFunction "to_json"

-- | Returns the value as jsonb. Arrays and composites are converted
-- (recursively) to arrays and objects; otherwise, if there is a cast from the
-- type to json, the cast function will be used to perform the conversion;
-- otherwise, a scalar value is produced. For any scalar type other than a
-- number, a Boolean, or a null value, the text representation will be used, in
-- such a fashion that it is a valid jsonb value.
toJsonb :: null ty :--> null 'PGjsonb
toJsonb = unsafeFunction "to_jsonb"

-- | Returns the array as a JSON array. A PostgreSQL multidimensional array
-- becomes a JSON array of arrays.
arrayToJson :: null ('PGvararray ty) :--> null 'PGjson
arrayToJson = unsafeFunction "array_to_json"

-- | Returns the row as a JSON object.
rowToJson :: null ('PGcomposite ty) :--> null 'PGjson
rowToJson = unsafeFunction "row_to_json"

-- | Builds a possibly-heterogeneously-typed JSON array out of a variadic
-- argument list.
jsonBuildArray :: SOP.SListI tuple => FunctionN tuple (null 'PGjson)
jsonBuildArray = unsafeFunctionN "json_build_array"

-- | Builds a possibly-heterogeneously-typed (binary) JSON array out of a
-- variadic argument list.
jsonbBuildArray :: SOP.SListI tuple => FunctionN tuple (null 'PGjsonb)
jsonbBuildArray = unsafeFunctionN "jsonb_build_array"

class PGBuildObject tys where
instance PGBuildObject '[]
instance (PGBuildObject tys, key `In` PGJsonKey)
  => PGBuildObject ('NotNull key ': value ': tys)

-- | Builds a possibly-heterogeneously-typed JSON object out of a variadic
-- argument list. The elements of the argument list must alternate between text
-- and values.
jsonBuildObject
  :: (SOP.SListI elems, PGBuildObject elems)
  => FunctionN elems (null 'PGjson)
jsonBuildObject = unsafeFunctionN "json_build_object"

-- | Builds a possibly-heterogeneously-typed (binary) JSON object out of a
-- variadic argument list. The elements of the argument list must alternate
-- between keys and values.
jsonbBuildObject
  :: (SOP.SListI elems, PGBuildObject elems)
  => FunctionN elems (null 'PGjsonb)
jsonbBuildObject = unsafeFunctionN "jsonb_build_object"

-- | Builds a JSON object out of a text array.
-- The array must have two dimensions
-- such that each inner array has exactly two elements,
-- which are taken as a key/value pair.
jsonObject
  ::   null ('PGfixarray '[n,2] ('NotNull 'PGtext))
  :--> null 'PGjson
jsonObject = unsafeFunction "json_object"

-- | Builds a binary JSON object out of a text array.
-- The array must have two dimensions
-- such that each inner array has exactly two elements,
-- which are taken as a key/value pair.
jsonbObject
  ::   null ('PGfixarray '[n,2] ('NotNull 'PGtext))
  :--> null 'PGjsonb
jsonbObject = unsafeFunction "jsonb_object"

-- | This is an alternate form of 'jsonObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a JSON object.
jsonZipObject :: FunctionN
  '[ null ('PGvararray ('NotNull 'PGtext))
   , null ('PGvararray ('NotNull 'PGtext)) ]
   ( null 'PGjson )
jsonZipObject = unsafeFunctionN "json_object"

-- | This is an alternate form of 'jsonbObject' that takes two arrays; one for
-- keys and one for values, that are zipped pairwise to create a binary JSON
-- object.
jsonbZipObject :: FunctionN
  '[ null ('PGvararray ('NotNull 'PGtext))
   , null ('PGvararray ('NotNull 'PGtext)) ]
   ( null 'PGjsonb )
jsonbZipObject = unsafeFunctionN "jsonb_object"

{-----------------------------------------
Table 9.46: JSON processing functions
-----------------------------------------}

-- | Returns the number of elements in the outermost JSON array.
jsonArrayLength :: null 'PGjson :--> null 'PGint4
jsonArrayLength = unsafeFunction "json_array_length"

-- | Returns the number of elements in the outermost binary JSON array.
jsonbArrayLength :: null 'PGjsonb :--> null 'PGint4
jsonbArrayLength = unsafeFunction "jsonb_array_length"

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator).
jsonExtractPath
  :: SOP.SListI elems
  => Expression outer commons grp schemas params from (null 'PGjson)
  -> NP (Expression outer commons grp schemas params from) elems
  -> Expression outer commons grp schemas params from (null 'PGjsonb)
jsonExtractPath x xs =
  unsafeFunctionN "json_extract_path" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator).
jsonbExtractPath
  :: SOP.SListI elems
  => Expression outer commons grp schemas params from (null 'PGjsonb)
  -> NP (Expression outer commons grp schemas params from) elems
  -> Expression outer commons grp schemas params from (null 'PGjsonb)
jsonbExtractPath x xs =
  unsafeFunctionN "jsonb_extract_path" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator), as text.
jsonExtractPathAsText
  :: SOP.SListI elems
  => Expression outer commons grp schemas params from (null 'PGjson)
  -> NP (Expression outer commons grp schemas params from) elems
  -> Expression outer commons grp schemas params from (null 'PGjson)
jsonExtractPathAsText x xs =
  unsafeFunctionN "json_extract_path_text" (x :* xs)

-- | Returns JSON value pointed to by the given path (equivalent to #>
-- operator), as text.
jsonbExtractPathAsText
  :: SOP.SListI elems
  => Expression outer commons grp schemas params from (null 'PGjsonb)
  -> NP (Expression outer commons grp schemas params from) elems
  -> Expression outer commons grp schemas params from (null 'PGjsonb)
jsonbExtractPathAsText x xs =
  unsafeFunctionN "jsonb_extract_path_text" (x :* xs)

-- | Returns the type of the outermost JSON value as a text string. Possible
-- types are object, array, string, number, boolean, and null.
jsonTypeof :: null 'PGjson :--> null 'PGtext
jsonTypeof = unsafeFunction "json_typeof"

-- | Returns the type of the outermost binary JSON value as a text string.
-- Possible types are object, array, string, number, boolean, and null.
jsonbTypeof :: null 'PGjsonb :--> null 'PGtext
jsonbTypeof = unsafeFunction "jsonb_typeof"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonStripNulls :: null 'PGjson :--> null 'PGjson
jsonStripNulls = unsafeFunction "json_strip_nulls"

-- | Returns its argument with all object fields that have null values omitted.
-- Other null values are untouched.
jsonbStripNulls :: null 'PGjsonb :--> null 'PGjsonb
jsonbStripNulls = unsafeFunction "jsonb_strip_nulls"

-- | @ jsonbSet target path new_value create_missing @
--
-- Returns target with the section designated by path replaced by new_value,
-- or with new_value added if create_missing is true ( default is true) and the
-- item designated by path does not exist. As with the path orientated
-- operators, negative integers that appear in path count from the end of JSON
-- arrays.
jsonbSet
  :: Expression outer commons grp schemas params from (null 'PGjsonb)
  -> Expression outer commons grp schemas params from (null ('PGvararray ('NotNull 'PGtext)))
  -> Expression outer commons grp schemas params from (null 'PGjsonb)
  -> Maybe (Expression outer commons grp schemas params from (null 'PGbool))
  -> Expression outer commons grp schemas params from (null 'PGjsonb)
jsonbSet tgt path val createMissing = case createMissing of
  Just m -> unsafeFunctionN "jsonb_set" (tgt :* path :* val :* m :* Nil)
  Nothing -> unsafeFunctionN "jsonb_set" (tgt :* path :* val :* Nil)

-- | @ jsonbInsert target path new_value insert_after @
--
-- Returns target with new_value inserted. If target section designated by
-- path is in a JSONB array, new_value will be inserted before target or after
-- if insert_after is true (default is false). If target section designated by
-- path is in JSONB object, new_value will be inserted only if target does not
-- exist. As with the path orientated operators, negative integers that appear
-- in path count from the end of JSON arrays.
jsonbInsert
  :: Expression outer commons grp schemas params from (null 'PGjsonb)
  -> Expression outer commons grp schemas params from (null ('PGvararray ('NotNull 'PGtext)))
  -> Expression outer commons grp schemas params from (null 'PGjsonb)
  -> Maybe (Expression outer commons grp schemas params from (null 'PGbool))
  -> Expression outer commons grp schemas params from (null 'PGjsonb)
jsonbInsert tgt path val insertAfter = case insertAfter of
  Just i -> unsafeFunctionN "jsonb_insert" (tgt :* path :* val :* i :* Nil)
  Nothing -> unsafeFunctionN "jsonb_insert" (tgt :* path :* val :* Nil)

-- | Returns its argument as indented JSON text.
jsonbPretty :: null 'PGjsonb :--> null 'PGtext
jsonbPretty = unsafeFunction "jsonb_pretty"

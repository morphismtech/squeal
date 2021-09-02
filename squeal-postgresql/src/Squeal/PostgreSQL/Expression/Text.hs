{-|
Module: Squeal.PostgreSQL.Expression.Text
Description: text functions and operators
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

text functions and operators
-}

{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Text
  ( -- * Text Function
    lower
  , upper
  , charLength
  , like
  , ilike
  , replace
  , strpos
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | >>> printSQL $ lower "ARRRGGG"
-- lower((E'ARRRGGG' :: text))
lower :: null 'PGtext --> null 'PGtext
lower = unsafeFunction "lower"

-- | >>> printSQL $ upper "eeee"
-- upper((E'eeee' :: text))
upper :: null 'PGtext --> null 'PGtext
upper = unsafeFunction "upper"

-- | >>> printSQL $ charLength "four"
-- char_length((E'four' :: text))
charLength :: null 'PGtext --> null 'PGint4
charLength = unsafeFunction "char_length"

-- | The `like` expression returns true if the @string@ matches
-- the supplied @pattern@. If @pattern@ does not contain percent signs
-- or underscores, then the pattern only represents the string itself;
-- in that case `like` acts like the equals operator. An underscore (_)
-- in pattern stands for (matches) any single character; a percent sign (%)
-- matches any sequence of zero or more characters.
--
-- >>> printSQL $ "abc" `like` "a%"
-- ((E'abc' :: text) LIKE (E'a%' :: text))
like :: Operator (null 'PGtext) (null 'PGtext) ('Null 'PGbool)
like = unsafeBinaryOp "LIKE"

-- | The key word ILIKE can be used instead of LIKE to make the
-- match case-insensitive according to the active locale.
--
-- >>> printSQL $ "abc" `ilike` "a%"
-- ((E'abc' :: text) ILIKE (E'a%' :: text))
ilike :: Operator (null 'PGtext) (null 'PGtext) ('Null 'PGbool)
ilike = unsafeBinaryOp "ILIKE"

-- | Determines the location of the substring match using the `strpos`
-- function. Returns the 1-based index of the first match, if no
-- match exists the function returns (0).
--
-- >>> printSQL $ strpos ("string" *: "substring")
-- strpos((E'string' :: text), (E'substring' :: text))
strpos
  :: '[null 'PGtext, null 'PGtext] ---> null 'PGint4
strpos = unsafeFunctionN "strpos"

-- | Over the string in the first argument, replace all occurrences of
-- the second argument with the third and return the modified string.
--
-- >>> printSQL $ replace ("string" :* "from" *: "to")
-- replace((E'string' :: text), (E'from' :: text), (E'to' :: text))
replace
  :: '[ null 'PGtext, null 'PGtext, null 'PGtext ] ---> null 'PGtext
replace = unsafeFunctionN "replace"

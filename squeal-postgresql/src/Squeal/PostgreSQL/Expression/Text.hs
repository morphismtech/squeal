{-|
Module: Squeal.PostgreSQL.Expression.Text
Description: Text expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

String functions and operators
-}

{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Text
  ( lower
  , upper
  , charLength
  , like
  , ilike
  ) where

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | >>> printSQL $ lower "ARRRGGG"
-- lower(E'ARRRGGG')
lower :: null 'PGtext --> null 'PGtext
lower = unsafeFunction "lower"

-- | >>> printSQL $ upper "eeee"
-- upper(E'eeee')
upper :: null 'PGtext --> null 'PGtext
upper = unsafeFunction "upper"

-- | >>> printSQL $ charLength "four"
-- char_length(E'four')
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
-- (E'abc' LIKE E'a%')
like :: Operator (null 'PGtext) (null 'PGtext) ('Null 'PGbool)
like = unsafeBinaryOp "LIKE"

-- | The key word ILIKE can be used instead of LIKE to make the
-- match case-insensitive according to the active locale.
--
-- >>> printSQL $ "abc" `ilike` "a%"
-- (E'abc' ILIKE E'a%')
ilike :: Operator (null 'PGtext) (null 'PGtext) ('Null 'PGbool)
ilike = unsafeBinaryOp "ILIKE"

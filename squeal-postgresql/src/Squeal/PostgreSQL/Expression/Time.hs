{-|
Module: Squeal.PostgreSQL.Expression.Time
Description: Date/Time expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Date/Time functions and operators
-}

{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FunctionalDependencies
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , TypeOperators
#-}

module Squeal.PostgreSQL.Expression.Time
  ( -- * Time Operation
    TimeOp (..)
    -- * Time Function
  , currentDate
  , currentTime
  , currentTimestamp
  , localTime
  , localTimestamp
  , now
  , makeDate
  , makeTime
  , makeTimestamp
  , makeTimestamptz
    -- * Interval
  , interval_
  , TimeUnit (..)
  ) where

import Data.Fixed
import Data.String

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | >>> printSQL currentDate
-- CURRENT_DATE
currentDate :: Expr (null 'PGdate)
currentDate = UnsafeExpression "CURRENT_DATE"

-- | >>> printSQL currentTime
-- CURRENT_TIME
currentTime :: Expr (null 'PGtimetz)
currentTime = UnsafeExpression "CURRENT_TIME"

-- | >>> printSQL currentTimestamp
-- CURRENT_TIMESTAMP
currentTimestamp :: Expr (null 'PGtimestamptz)
currentTimestamp = UnsafeExpression "CURRENT_TIMESTAMP"

-- | >>> printSQL localTime
-- LOCALTIME
localTime :: Expr (null 'PGtime)
localTime = UnsafeExpression "LOCALTIME"

-- | >>> printSQL localTimestamp
-- LOCALTIMESTAMP
localTimestamp :: Expr (null 'PGtimestamp)
localTimestamp = UnsafeExpression "LOCALTIMESTAMP"

-- | Current date and time (equivalent to `currentTimestamp`)
--
-- >>> printSQL now
-- now()
now :: Expr (null 'PGtimestamptz)
now = UnsafeExpression "now()"

{-|
Create date from year, month and day fields

>>> printSQL (makeDate (1984 :* 7 *: 3))
make_date((1984 :: int4), (7 :: int4), (3 :: int4))
-}
makeDate :: '[ null 'PGint4, null 'PGint4, null 'PGint4 ] ---> null 'PGdate
makeDate = unsafeFunctionN "make_date"

{-|
Create time from hour, minute and seconds fields

>>> printSQL (makeTime (8 :* 15 *: 23.5))
make_time((8 :: int4), (15 :: int4), (23.5 :: float8))
-}
makeTime :: '[ null 'PGint4, null 'PGint4, null 'PGfloat8 ] ---> null 'PGtime
makeTime = unsafeFunctionN "make_time"

{-|
Create timestamp from year, month, day, hour, minute and seconds fields

>>> printSQL (makeTimestamp (2013 :* 7 :* 15 :* 8 :* 15 *: 23.5))
make_timestamp((2013 :: int4), (7 :: int4), (15 :: int4), (8 :: int4), (15 :: int4), (23.5 :: float8))
-}
makeTimestamp ::
  '[ null 'PGint4, null 'PGint4, null 'PGint4
   , null 'PGint4, null 'PGint4, null 'PGfloat8 ] ---> null 'PGtimestamp
makeTimestamp = unsafeFunctionN "make_timestamp"

{-|
Create timestamp with time zone from
year, month, day, hour, minute and seconds fields;
the current time zone is used

>>> printSQL (makeTimestamptz (2013 :* 7 :* 15 :* 8 :* 15 *: 23.5))
make_timestamptz((2013 :: int4), (7 :: int4), (15 :: int4), (8 :: int4), (15 :: int4), (23.5 :: float8))
-}
makeTimestamptz ::
  '[ null 'PGint4, null 'PGint4, null 'PGint4
   , null 'PGint4, null 'PGint4, null 'PGfloat8 ] ---> null 'PGtimestamptz
makeTimestamptz = unsafeFunctionN "make_timestamptz"

{-|
Affine space operations on time types.
-}
class TimeOp time diff | time -> diff where
  {-|
  >>> printSQL (makeDate (1984 :* 7 *: 3) !+ 365)
  (make_date((1984 :: int4), (7 :: int4), (3 :: int4)) + (365 :: int4))
  -}
  (!+) :: Operator (null time) (null diff) (null time)
  (!+) = unsafeBinaryOp "+"
  {-|
  >>> printSQL (365 +! makeDate (1984 :* 7 *: 3))
  ((365 :: int4) + make_date((1984 :: int4), (7 :: int4), (3 :: int4)))
  -}
  (+!) :: Operator (null diff) (null time) (null time)
  (+!) = unsafeBinaryOp "+"
  {-|
  >>> printSQL (makeDate (1984 :* 7 *: 3) !- 365)
  (make_date((1984 :: int4), (7 :: int4), (3 :: int4)) - (365 :: int4))
  -}
  (!-) :: Operator (null time) (null diff) (null time)
  (!-) = unsafeBinaryOp "-"
  {-|
  >>> printSQL (makeDate (1984 :* 7 *: 3) !-! currentDate)
  (make_date((1984 :: int4), (7 :: int4), (3 :: int4)) - CURRENT_DATE)
  -}
  (!-!) :: Operator (null time) (null time) (null diff)
  (!-!) = unsafeBinaryOp "-"
instance TimeOp 'PGtimestamp 'PGinterval
instance TimeOp 'PGtimestamptz 'PGinterval
instance TimeOp 'PGtime 'PGinterval
instance TimeOp 'PGtimetz 'PGinterval
instance TimeOp 'PGinterval 'PGinterval
instance TimeOp 'PGdate 'PGint4
infixl 6 !+
infixl 6 +!
infixl 6 !-
infixl 6 !-!

-- | A `TimeUnit` to use in `interval_` construction.
data TimeUnit
  = Years | Months | Weeks | Days
  | Hours | Minutes | Seconds
  | Microseconds | Milliseconds
  | Decades | Centuries | Millennia
  deriving (Eq, Ord, Show, Read, Enum, GHC.Generic)
instance SOP.Generic TimeUnit
instance SOP.HasDatatypeInfo TimeUnit
instance RenderSQL TimeUnit where
  renderSQL = \case
    Years -> "years"
    Months -> "months"
    Weeks -> "weeks"
    Days -> "days"
    Hours -> "hours"
    Minutes -> "minutes"
    Seconds -> "seconds"
    Microseconds -> "microseconds"
    Milliseconds -> "milliseconds"
    Decades -> "decades"
    Centuries -> "centuries"
    Millennia -> "millennia"

-- | >>> printSQL $ interval_ 7 Days
-- (INTERVAL '7.000 days')
interval_ :: Milli -> TimeUnit -> Expr (null 'PGinterval)
interval_ num unit = UnsafeExpression . parenthesized $ "INTERVAL" <+>
  "'" <> fromString (show num) <+> renderSQL unit <> "'"

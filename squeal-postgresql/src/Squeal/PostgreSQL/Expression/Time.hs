{-|
Module: Squeal.PostgreSQL.Expression.Time
Description: date/time functions and operators
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

date/time functions and operators
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
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Time
  ( -- * Time Operation
    TimeOp (..)
    -- * Time Function
  , atTimeZone
  , currentDate
  , currentTime
  , currentTimestamp
  , dateTrunc
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
import GHC.TypeLits

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.List
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
Truncate a timestamp with the specified precision

>>> printSQL $ dateTrunc Quarter (makeTimestamp (2010 :* 5 :* 6 :* 14 :* 45 *: 11.4))
date_trunc('quarter', make_timestamp((2010 :: int4), (5 :: int4), (6 :: int4), (14 :: int4), (45 :: int4), (11.4 :: float8)))
-}
dateTrunc
  :: time `In` '[ 'PGtimestamp, 'PGtimestamptz ]
  => TimeUnit -> null time --> null time
dateTrunc tUnit args = unsafeFunctionN "date_trunc" (timeUnitExpr *: args)
  where
  timeUnitExpr :: forall grp lat with db params from null0.
    Expression grp lat with db params from (null0 'PGtext)
  timeUnitExpr = UnsafeExpression . singleQuotedUtf8 . renderSQL $ tUnit

type family PGAtTimeZone ty where
  PGAtTimeZone 'PGtimestamptz = 'PGtimestamp
  PGAtTimeZone 'PGtimestamp = 'PGtimestamptz
  PGAtTimeZone 'PGtimetz = 'PGtimetz
  PGAtTimeZone pg = TypeError
    ( 'Text "Squeal type error: AT TIME ZONE cannot be applied to "
      ':<>: 'ShowType pg )

{-|
Convert a timestamp, timestamp with time zone, or time of day with timezone to a different timezone using an interval offset or specific timezone denoted by text. When using the interval offset, the interval duration must be less than one day or 24 hours.

>>> printSQL $ (makeTimestamp (2009 :* 7 :* 22 :* 19 :* 45 *: 11.4)) `atTimeZone` (interval_ 8 Hours)
(make_timestamp((2009 :: int4), (7 :: int4), (22 :: int4), (19 :: int4), (45 :: int4), (11.4 :: float8)) AT TIME ZONE (INTERVAL '8.000 hours'))

>>> :{
 let
   timezone :: Expression grp lat with db params from (null 'PGtext)
   timezone = "EST"
 in printSQL $ (makeTimestamptz (2015 :* 9 :* 15 :* 4 :* 45 *: 11.4)) `atTimeZone` timezone
:}
(make_timestamptz((2015 :: int4), (9 :: int4), (15 :: int4), (4 :: int4), (45 :: int4), (11.4 :: float8)) AT TIME ZONE (E'EST' :: text))
-}
atTimeZone
  :: zone `In` '[ 'PGtext, 'PGinterval]
  => Operator (null time) (null zone) (null (PGAtTimeZone time))
atTimeZone = unsafeBinaryOp "AT TIME ZONE"

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
  = Years | Quarter | Months | Weeks | Days
  | Hours | Minutes | Seconds
  | Microseconds | Milliseconds
  | Decades | Centuries | Millennia
  deriving (Eq, Ord, Show, Read, Enum, GHC.Generic)
instance SOP.Generic TimeUnit
instance SOP.HasDatatypeInfo TimeUnit
instance RenderSQL TimeUnit where
  renderSQL = \case
    Years -> "years"
    Quarter -> "quarter"
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

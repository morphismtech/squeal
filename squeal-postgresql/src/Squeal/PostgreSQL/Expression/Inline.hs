{-|
Module: Squeal.PostgreSQL.Expression.Inline
Description: Inline expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Inline expressions
-}

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , MultiWayIf
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Inline
  ( -- * Inline
    Inline (..)
  , InlineParam (..)
  , InlineField (..)
  , inlineFields
  , InlineColumn (..)
  , inlineColumns
  ) where

import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (doubleDec, floatDec, int16Dec, int32Dec, int64Dec)
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.String
import Data.Text (Text)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.LocalTime (LocalTime(LocalTime), TimeOfDay(TimeOfDay), TimeZone)
import Data.UUID.Types (UUID, toASCIIBytes)
import Data.Vector (Vector, toList)
import Database.PostgreSQL.LibPQ (Oid(Oid))
import GHC.TypeLits

import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Array
import Squeal.PostgreSQL.Expression.Default
import Squeal.PostgreSQL.Expression.Composite
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Null
import Squeal.PostgreSQL.Expression.Range
import Squeal.PostgreSQL.Expression.Time
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

{- |
The `Inline` class allows embedding a Haskell value directly
as an `Expression` using `inline`.

>>> printSQL (inline 'a')
(E'a' :: char(1))

>>> printSQL (inline (1 :: Double))
(1.0 :: float8)

>>> printSQL (inline (Json ([1, 2] :: [Double])))
('[1.0,2.0]' :: json)

>>> printSQL (inline (Enumerated GT))
'GT'
-}
class Inline x where inline :: x -> Expr (null (PG x))
instance Inline Bool where
  inline = \case
    True -> true
    False -> false
instance JSON.ToJSON x => Inline (Json x) where
  inline = inferredtype . UnsafeExpression
    . singleQuotedUtf8 . toStrict . JSON.encode . getJson
instance JSON.ToJSON x => Inline (Jsonb x) where
  inline = inferredtype . UnsafeExpression
    . singleQuotedUtf8 . toStrict . JSON.encode . getJsonb
instance Inline Char where
  inline chr = inferredtype . UnsafeExpression $
    "E\'" <> fromString (escape chr) <> "\'"
instance Inline String where inline = fromString
instance Inline Int16 where
  inline
    = inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    . int16Dec
instance Inline Int32 where
  inline
    = inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    . int32Dec
instance Inline Int64 where
  inline x =
    if x == minBound
    -- For some reason Postgres throws an error with
    -- (-9223372036854775808 :: int8)
    -- even though it's a valid lowest value for int8
    then inline (x+1) - 1
    else inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    $ int64Dec x
instance Inline Float where
  inline x = inferredtype . UnsafeExpression $
    if isNaN x || isInfinite x
    then singleQuotedUtf8 (decimal x)
    else decimal x
    where
      decimal = toStrict . toLazyByteString . floatDec
instance Inline Double where
  inline x = inferredtype . UnsafeExpression $
    if isNaN x || isInfinite x
    then singleQuotedUtf8 (decimal x)
    else decimal x
    where
      decimal = toStrict . toLazyByteString . doubleDec
instance Inline Scientific where
  inline
    = inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    . scientificBuilder
instance Inline Text where inline = fromString . Text.unpack
instance Inline Lazy.Text where inline = fromString . Lazy.Text.unpack
instance (KnownNat n, 1 <= n) => Inline (VarChar n) where
  inline
    = inferredtype
    . UnsafeExpression
    . escapeQuotedText
    . getVarChar
instance (KnownNat n, 1 <= n) => Inline (FixChar n) where
  inline
    = inferredtype
    . UnsafeExpression
    . escapeQuotedText
    . getFixChar
instance Inline DiffTime where
  inline dt =
    let
      picosecs = diffTimeToPicoseconds dt
      (secs,leftover) = picosecs `quotRem` 1000000000000
      microsecs = leftover `quot` 1000000
    in
      inferredtype $
        interval_ (fromIntegral secs) Seconds
        +! interval_ (fromIntegral microsecs) Microseconds
instance Inline Day where
  inline day =
    let (y,m,d) = toGregorian day
    in inferredtype $ makeDate (fromInteger y :* fromIntegral m *: fromIntegral d)
instance Inline UTCTime where
  inline
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%z"
instance Inline (TimeOfDay, TimeZone) where
  inline (hms, tz)
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    $ formatTime defaultTimeLocale "%H:%M:%S" hms
      <> formatTime defaultTimeLocale "%z" tz
instance Inline TimeOfDay where
  inline (TimeOfDay hr mn sc) = inferredtype $ makeTime
    (fromIntegral hr :* fromIntegral mn *: fromRational (toRational sc))
instance Inline LocalTime where
  inline (LocalTime day t) =
    let
      (y,m,d) = toGregorian day
      TimeOfDay hr mn sc = t
    in inferredtype $ makeTimestamp
      ( fromInteger y :* fromIntegral m :* fromIntegral d
        :* fromIntegral hr :* fromIntegral mn *: fromRational (toRational sc) )
instance Inline (Range Int32) where
  inline = range int4range . fmap inline
instance Inline (Range Int64) where
  inline = range int8range . fmap inline
instance Inline (Range Scientific) where
  inline = range numrange . fmap inline
instance Inline (Range LocalTime) where
  inline = range tsrange . fmap inline
instance Inline (Range UTCTime) where
  inline = range tstzrange . fmap inline
instance Inline (Range Day) where
  inline = range daterange . fmap inline
instance Inline UUID where
  inline
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . toASCIIBytes
instance Inline Money where
  inline moolah = inferredtype . UnsafeExpression $
    fromString (show dollars)
    <> "." <> fromString (show pennies)
    where
      (dollars,pennies) = cents moolah `divMod` 100
instance InlineParam x (NullPG x)
  => Inline (VarArray [x]) where
    inline (VarArray xs) = array (inlineParam <$> xs)
instance InlineParam x (NullPG x)
  => Inline (VarArray (Vector x)) where
    inline (VarArray xs) = array (inlineParam <$> toList xs)
instance Inline Oid where
  inline (Oid o) = inferredtype . UnsafeExpression . fromString $ show o
instance
  ( SOP.IsEnumType x
  , SOP.HasDatatypeInfo x
  ) => Inline (Enumerated x) where
    inline =
      let
        gshowConstructor
          :: NP SOP.ConstructorInfo xss
          -> SOP.SOP SOP.I xss
          -> String
        gshowConstructor Nil _ = ""
        gshowConstructor (constructor :* _) (SOP.SOP (SOP.Z _)) =
          SOP.constructorName constructor
        gshowConstructor (_ :* constructors) (SOP.SOP (SOP.S xs)) =
          gshowConstructor constructors (SOP.SOP xs)
      in
        UnsafeExpression
        . singleQuotedUtf8
        . fromString
        . gshowConstructor
            (SOP.constructorInfo (SOP.datatypeInfo (SOP.Proxy @x)))
        . SOP.from
        . getEnumerated
instance
  ( SOP.IsRecord x xs
  , SOP.AllZip InlineField xs (RowPG x)
  ) => Inline (Composite x) where
    inline
      = row
      . SOP.htrans (SOP.Proxy @InlineField) inlineField
      . SOP.toRecord
      . getComposite

-- | Lifts `Inline` to `NullType`s.
class InlineParam x ty where inlineParam :: x -> Expr ty
instance (Inline x, pg ~ PG x) => InlineParam x ('NotNull pg) where inlineParam = inline
instance (Inline x, pg ~ PG x) => InlineParam (Maybe x) ('Null pg) where
  inlineParam = maybe null_ inline

-- | Lifts `Inline` to fields.
class InlineField
  (field :: (Symbol, Type))
  (fieldpg :: (Symbol, NullType)) where
    inlineField
      :: SOP.P field
      -> Aliased (Expression grp lat with db params from) fieldpg
instance (KnownSymbol alias, InlineParam x ty)
  => InlineField (alias ::: x) (alias ::: ty) where
    inlineField (SOP.P x) = inlineParam x `as` Alias @alias

-- | Use a Haskell record as a inline a row of expressions.
inlineFields
  :: ( SOP.IsRecord hask fields
     , SOP.AllZip InlineField fields row )
  => hask -- ^ record
  -> NP (Aliased (Expression  'Ungrouped '[] with db params '[])) row
inlineFields
  = SOP.htrans (SOP.Proxy @InlineField) inlineField
  . SOP.toRecord


-- | Lifts `Inline` to a column entry
class InlineColumn
  (field :: (Symbol, Type))
  (column :: (Symbol, ColumnType)) where
  -- | Haskell record field as a inline column
  inlineColumn
    :: SOP.P field
    -> Aliased (Optional (Expression grp lat with db params from)) column
instance (KnownSymbol col, InlineParam x ty)
  => InlineColumn (col ::: x) (col ::: 'NoDef :=> ty) where
    inlineColumn (SOP.P x) = Set (inlineParam x) `as` (Alias @col)
instance (KnownSymbol col, InlineParam x ty)
  => InlineColumn
    (col ::: Optional SOP.I ('Def :=> x))
    (col ::: 'Def :=> ty) where
    inlineColumn (SOP.P optional) = case optional of
      Default -> Default `as` (Alias @col)
      Set (SOP.I x) -> Set (inlineParam x) `as` (Alias @col)

-- | Use a Haskell record as a inline list of columns
inlineColumns
  :: ( SOP.IsRecord hask xs
     , SOP.AllZip InlineColumn xs columns )
  => hask -- ^ record
  -> NP (Aliased (Optional (Expression 'Ungrouped '[] with db params '[]))) columns
inlineColumns
  = SOP.htrans (SOP.Proxy @InlineColumn) inlineColumn
  . SOP.toRecord

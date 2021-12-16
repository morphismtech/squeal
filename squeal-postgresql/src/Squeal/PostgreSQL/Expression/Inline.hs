{-|
Module: Squeal.PostgreSQL.Expression.Inline
Description: inline expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

inline expressions
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
import Data.Coerce (coerce)
import Data.Functor.Const (Const(Const))
import Data.Functor.Constant (Constant(Constant))
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import Data.String
import Data.Text (Text)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, UTCTime)
import Data.Time.Format.ISO8601 (formatShow, timeOfDayAndOffsetFormat, FormatExtension(ExtendedFormat), iso8601Show)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime, TimeOfDay, TimeZone)
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

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Array
import Squeal.PostgreSQL.Expression.Default
import Squeal.PostgreSQL.Expression.Composite
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Null
import Squeal.PostgreSQL.Expression.Range
import Squeal.PostgreSQL.Expression.Time
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.PG
import Squeal.PostgreSQL.Type.Schema

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
  inline (Json x)
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . toStrict
    . JSON.encode
    $ x
instance JSON.ToJSON x => Inline (Jsonb x) where
  inline (Jsonb x)
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . toStrict
    . JSON.encode
    $ x
instance Inline Char where
  inline chr = inferredtype . UnsafeExpression $
    "E\'" <> fromString (escape chr) <> "\'"
instance Inline String where inline x = fromString x
instance Inline Int16 where
  inline x
    = inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    . int16Dec
    $ x
instance Inline Int32 where
  inline x
    = inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    . int32Dec
    $ x
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
  inline x
    = inferredtype
    . UnsafeExpression
    . toStrict
    . toLazyByteString
    . scientificBuilder
    $ x
instance Inline Text where inline x = fromString . Text.unpack $ x
instance Inline Lazy.Text where inline x = fromString . Lazy.Text.unpack $ x
instance (KnownNat n, 1 <= n) => Inline (VarChar n) where
  inline x
    = inferredtype
    . UnsafeExpression
    . escapeQuotedText
    . getVarChar
    $ x
instance (KnownNat n, 1 <= n) => Inline (FixChar n) where
  inline x
    = inferredtype
    . UnsafeExpression
    . escapeQuotedText
    . getFixChar
    $ x
instance Inline x => Inline (Const x tag) where inline (Const x) = inline x
instance Inline x => Inline (SOP.K x tag) where inline (SOP.K x) = inline x
instance Inline x => Inline (Constant x tag) where
  inline (Constant x) = inline x
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
  inline x
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    . iso8601Show
    $ x
instance Inline UTCTime where
  inline x
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    . iso8601Show
    $ x
instance Inline (TimeOfDay, TimeZone) where
  inline x
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    . formatShow (timeOfDayAndOffsetFormat ExtendedFormat)
    $ x
instance Inline TimeOfDay where
  inline x
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    . iso8601Show
    $ x
instance Inline LocalTime where
  inline x
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . fromString
    . iso8601Show
    $ x
instance Inline (Range Int32) where
  inline x = range int4range . fmap (\y -> inline y) $ x
instance Inline (Range Int64) where
  inline x = range int8range . fmap (\y -> inline y) $ x
instance Inline (Range Scientific) where
  inline x = range numrange . fmap (\y -> inline y) $ x
instance Inline (Range LocalTime) where
  inline x = range tsrange . fmap (\y -> inline y) $ x
instance Inline (Range UTCTime) where
  inline x = range tstzrange . fmap (\y -> inline y) $ x
instance Inline (Range Day) where
  inline x = range daterange . fmap (\y -> inline y) $ x
instance Inline UUID where
  inline x
    = inferredtype
    . UnsafeExpression
    . singleQuotedUtf8
    . toASCIIBytes
    $ x
instance Inline Money where
  inline moolah = inferredtype . UnsafeExpression $
    fromString (show dollars)
    <> "." <> fromString (show pennies)
    where
      (dollars,pennies) = cents moolah `divMod` 100
instance InlineParam x (NullPG x)
  => Inline (VarArray [x]) where
    inline (VarArray xs) = array ((\x -> inlineParam x) <$> xs)
instance InlineParam x (NullPG x)
  => Inline (VarArray (Vector x)) where
    inline (VarArray xs) = array ((\x -> inlineParam x) <$> toList xs)
instance Inline Oid where
  inline (Oid o) = inferredtype . UnsafeExpression . fromString $ show o
instance
  ( SOP.IsEnumType x
  , SOP.HasDatatypeInfo x
  ) => Inline (Enumerated x) where
    inline (Enumerated x) =
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
        $ x
instance
  ( SOP.IsRecord x xs
  , SOP.AllZip InlineField xs (RowPG x)
  ) => Inline (Composite x) where
    inline (Composite x)
      = row
      . SOP.htrans (SOP.Proxy @InlineField) inlineField
      . SOP.toRecord
      $ x

-- | Lifts `Inline` to `NullType`s.
class InlineParam x ty where inlineParam :: x -> Expr ty
instance (Inline x, pg ~ PG x) => InlineParam x ('NotNull pg) where inlineParam = inline
instance (Inline x, pg ~ PG x) => InlineParam (Maybe x) ('Null pg) where
  inlineParam x = maybe null_ (\y -> inline y) x

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

-- | Inline a Haskell record as a row of expressions.
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

-- | Inline a Haskell record as a list of columns.
inlineColumns
  :: ( SOP.IsRecord hask xs
     , SOP.AllZip InlineColumn xs columns )
  => hask -- ^ record
  -> NP (Aliased (Optional (Expression 'Ungrouped '[] with db params '[]))) columns
inlineColumns
  = SOP.htrans (SOP.Proxy @InlineColumn) inlineColumn
  . SOP.toRecord

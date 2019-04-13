{-|
Module: Squeal.PostgreSQL.Expression.Literal
Description: Literal expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Literal expressions
-}

{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeSynonymInstances
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Literal (Literal (..)) where

import ByteString.StrictBuilder (builderBytes)
import Data.ByteString.Lazy (toStrict)
import Data.Int
import Data.String
import Data.Text (Text)

import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text

import Squeal.PostgreSQL.Binary
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render

{- |
The `Literal` class allows embedding a Haskell value directly
as an `Expression` using `literal`.

>>> printSQL (literal 'a')
E'a'

>>> printSQL (literal (1 :: Double))
1.0

>>> printSQL (literal (Json [1 :: Double, 2]))
('[1.0,2.0]' :: json)

>>> printSQL (literal (Enumerated GT))
'GT'
-}
class Literal hask where literal :: hask -> Expr (null (PG hask))
instance Literal Bool where
  literal = \case
    True -> true
    False -> false
instance JSON.ToJSON hask => Literal (Json hask) where
  literal = UnsafeExpression . parenthesized . (<> " :: json")
    . singleQuotedUtf8 . toStrict . JSON.encode . getJson
instance JSON.ToJSON hask => Literal (Jsonb hask) where
  literal =  UnsafeExpression . parenthesized . (<> " :: jsonb")
    . singleQuotedUtf8 . toStrict . JSON.encode . getJsonb
instance Literal Char where
  literal chr = UnsafeExpression $
    "E\'" <> fromString (escape chr) <> "\'"
instance Literal String where literal = fromString
instance Literal Int16 where literal = fromIntegral
instance Literal Int32 where literal = fromIntegral
instance Literal Int64 where literal = fromIntegral
instance Literal Float where literal = fromRational . toRational
instance Literal Double where literal = fromRational . toRational
instance Literal Text where literal = fromString . Text.unpack
instance Literal Lazy.Text where literal = fromString . Lazy.Text.unpack
instance ToParam (Enumerated enum) (PG (Enumerated enum))
  => Literal (Enumerated enum) where
    literal
      = UnsafeExpression
      . singleQuotedUtf8
      . builderBytes
      . unK
      . toParam @(Enumerated enum) @(PG (Enumerated enum))

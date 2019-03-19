{-|
Module: Squeal.PostgreSQL.Render
Description: Rendering helper functions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Rendering helper functions.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , FlexibleContexts
  , MagicHash
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
#-}

module Squeal.PostgreSQL.Render
  ( -- * Render
    parenthesized
  , bracketed
  , (<+>)
  , commaSeparated
  , doubleQuoted
  , singleQuotedText
  , singleQuotedUtf8
  , renderCommaSeparated
  , renderCommaSeparatedConstraint
  , renderCommaSeparatedMaybe
  , renderNat
  , renderSymbol
  , RenderSQL (..)
  , printSQL
  ) where

import Control.Monad.Base
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Generics.SOP
import GHC.Exts
import GHC.TypeLits hiding (Text)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

-- | Parenthesize a `ByteString`.
parenthesized :: ByteString -> ByteString
parenthesized str = "(" <> str <> ")"

-- | Square bracket a `ByteString`
bracketed :: ByteString -> ByteString
bracketed str = "[" <> str <> "]"

-- | Concatenate two `ByteString`s with a space between.
(<+>) :: ByteString -> ByteString -> ByteString
infixr 7 <+>
str1 <+> str2 = str1 <> " " <> str2

-- | Comma separate a list of `ByteString`s.
commaSeparated :: [ByteString] -> ByteString
commaSeparated = ByteString.intercalate ", "

-- | Add double quotes around a `ByteString`.
doubleQuoted :: ByteString -> ByteString
doubleQuoted str = "\"" <> str <> "\""

-- | Add single quotes around a `Text` and escape single quotes within it.
singleQuotedText :: Text -> ByteString
singleQuotedText str =
  "'" <> Text.encodeUtf8 (Text.replace "'" "''" str) <> "'"

-- | Add single quotes around a `ByteString` and escape single quotes within it.
singleQuotedUtf8 :: ByteString -> ByteString
singleQuotedUtf8 = singleQuotedText . Text.decodeUtf8

-- | Comma separate the renderings of a heterogeneous list.
renderCommaSeparated
  :: SListI xs
  => (forall x. expression x -> ByteString)
  -> NP expression xs -> ByteString
renderCommaSeparated render
  = commaSeparated
  . hcollapse
  . hmap (K . render)

-- | Comma separate the renderings of a heterogeneous list.
renderCommaSeparatedConstraint
  :: forall c xs expression. (All c xs, SListI xs)
  => (forall x. c x => expression x -> ByteString)
  -> NP expression xs -> ByteString
renderCommaSeparatedConstraint render
  = commaSeparated
  . hcollapse
  . hcmap (Proxy @c) (K . render)

-- | Comma separate the `Maybe` renderings of a heterogeneous list, dropping
-- `Nothing`s.
renderCommaSeparatedMaybe
  :: SListI xs
  => (forall x. expression x -> Maybe ByteString)
  -> NP expression xs -> ByteString
renderCommaSeparatedMaybe render
  = commaSeparated
  . catMaybes
  . hcollapse
  . hmap (K . render)

-- | Render a promoted `Nat`.
renderNat :: forall n. KnownNat n => ByteString
renderNat = fromString (show (natVal' (proxy# :: Proxy# n)))

-- | Render a promoted `Symbol`.
renderSymbol :: forall s. KnownSymbol s => ByteString
renderSymbol = fromString (symbolVal' (proxy# :: Proxy# s))

-- | A class for rendering SQL
class RenderSQL sql where renderSQL :: sql -> ByteString

-- | Print SQL.
printSQL :: (RenderSQL sql, MonadBase IO io) => sql -> io ()
printSQL = liftBase . Char8.putStrLn . renderSQL

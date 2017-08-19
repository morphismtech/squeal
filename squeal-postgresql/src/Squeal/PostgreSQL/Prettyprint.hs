{-# LANGUAGE
    MagicHash
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
#-}

module Squeal.PostgreSQL.Prettyprint where

import Data.ByteString (ByteString)
import Data.Maybe
import Data.Monoid ((<>))
import Generics.SOP
import GHC.Exts
import GHC.TypeLits

import qualified Data.ByteString as ByteString

parenthesized :: ByteString -> ByteString
parenthesized str = "(" <> str <> ")"

(<+>) :: ByteString -> ByteString -> ByteString
str1 <+> str2 = str1 <> " " <> str2

commaSeparated :: [ByteString] -> ByteString
commaSeparated = ByteString.intercalate ", "

renderCommaSeparated
  :: SListI xs
  => (forall x. expression x -> ByteString)
  -> NP expression xs -> ByteString
renderCommaSeparated render
  = commaSeparated
  . hcollapse
  . hmap (K . render)

renderCommaSeparatedMaybe
  :: SListI xs
  => (forall x. expression x -> Maybe ByteString)
  -> NP expression xs -> ByteString
renderCommaSeparatedMaybe render
  = commaSeparated
  . catMaybes
  . hcollapse
  . hmap (K . render)

renderNat :: KnownNat n => proxy n -> ByteString
renderNat (_ :: proxy n) = fromString (show (natVal' (proxy# :: Proxy# n)))

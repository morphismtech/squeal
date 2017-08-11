{-# LANGUAGE
    DefaultSignatures
  , FlexibleContexts
  , GADTs
  , MultiParamTypeClasses
#-}

module Squeel.PostgreSQL.Binary2 where

import Generics.SOP

import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding

class ToParam x pg where
  toParam :: x -> K Encoding.Encoding pg
class ToParams x pgs where
  toParams :: x -> NP (K Encoding.Encoding) pgs
  default toParams
    :: (IsProductType x xs, AllZip ToParam xs pgs)
    => x -> NP (K Encoding.Encoding) pgs
  toParams
    = htrans (Proxy :: Proxy ToParam) (toParam . unI) . unZ . unSOP . from

class FromValue pg y where
  fromValue :: proxy pg -> Decoding.Value y
class FromRow pgs y where
  fromRow :: NP proxy pgs -> Decoding.Value y
  default fromRow
    :: (IsProductType y ys, AllZip FromValue pgs ys)
    => NP proxy pgs -> Decoding.Value y
  fromRow
    = fmap to . hsequence
    . htrans (Proxy :: Proxy FromValue) fromValue . SOP . Z

{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , RankNTypes
#-}

module Squeal.PostgreSQL.PQ.Statement
  ( Statement (..)
  , query
  , manipulation
  ) where

import Data.Functor.Contravariant
import Data.Profunctor (Profunctor (..))

import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.PQ.Decode
import Squeal.PostgreSQL.PQ.Encode
import Squeal.PostgreSQL.PQ.Oid
import Squeal.PostgreSQL.Query

data Statement db x y where
  Manipulation
    :: SOP.All OidOfNull params
    => EncodeParams params x
    -> DecodeRow row y
    -> Manipulation '[] db params row
    -> Statement db x y
  Query
    :: SOP.All OidOfNull params
    => EncodeParams params x
    -> DecodeRow row y
    -> Query '[] '[] db params row
    -> Statement db x y

instance Profunctor (Statement db) where
  lmap f (Manipulation encode decode q) =
    Manipulation (contramap f encode) decode q
  lmap f (Query encode decode q) =
    Query (contramap f encode) decode q
  rmap f (Manipulation encode decode q) =
    Manipulation encode (fmap f decode) q
  rmap f (Query encode decode q) =
    Query encode (fmap f decode) q
  dimap f g (Manipulation encode decode q) =
    Manipulation (contramap f encode) (fmap g decode) q
  dimap f g (Query encode decode q) =
    Query (contramap f encode) (fmap g decode) q

query ::
  ( SOP.All OidOfNull params
  , SOP.IsProductType x xs
  , SOP.AllZip ToNullParam params xs
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Query '[] '[] db params row
    -> Statement db x y
query = Query genericParams genericRow

manipulation ::
  ( SOP.All OidOfNull params
  , SOP.IsProductType x xs
  , SOP.AllZip ToNullParam params xs
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Manipulation '[] db params row
    -> Statement db x y
manipulation = Manipulation genericParams genericRow

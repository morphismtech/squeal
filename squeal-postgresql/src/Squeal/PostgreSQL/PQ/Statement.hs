{-|
Module: Squeal.PostgreSQL.PQ.Statement
Description: Statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

A top-level `Statement` type wraps a `Squeal.PostgreSQL.Query.Query`
or `Squeal.PostgreSQL.Manipulation.Manipulation`
together with an `EncodeParams` and a `DecodeRow`.
-}

{-# LANGUAGE
    DataKinds
  , DeriveFunctor
  , DeriveFoldable
  , DeriveGeneric
  , DeriveTraversable
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

-- | A `Statement` consists of a `Squeal.PostgreSQL.Statement.Manipulation`
-- or a `Squeal.PostgreSQL.PQ.Statement.Query` that can be run
-- in a `Squeal.PostgreSQL.PQ.Monad.MonadPQ`.
data Statement db x y where
  -- | Constructor for a data manipulation language statement
  Manipulation
    :: (SOP.All OidOfNull params, SOP.SListI row)
    => EncodeParams params x -- ^ encoding of parameters
    -> DecodeRow row y -- ^ decoding of returned rows
    -> Manipulation '[] db params row
    -- ^ `insertInto`, `update`, `deleteFrom`, ...
    -> Statement db x y
  -- | Constructor for a structured query language statement
  Query
    :: (SOP.All OidOfNull params, SOP.SListI row)
    => EncodeParams params x -- ^ encoding of parameters
    -> DecodeRow row y -- ^ decoding of returned rows
    -> Query '[] '[] db params row -- ^ `select`, `values`, ...
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

instance Functor (Statement db x) where fmap = rmap

-- | Smart constructor for a structured query language statement
query ::
  ( SOP.All OidOfNull params
  , SOP.IsProductType x xs
  , SOP.AllZip ToNullParam params xs
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Query '[] '[] db params row -- ^ `select`, `values`, ...
    -> Statement db x y
query = Query genericParams genericRow

-- | Smart constructor for a data manipulation language statement
manipulation ::
  ( SOP.All OidOfNull params
  , SOP.IsProductType x xs
  , SOP.AllZip ToNullParam params xs
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Manipulation '[] db params row
    -- ^ `insertInto`, `update`, `deleteFrom`, ...
    -> Statement db x y
manipulation = Manipulation genericParams genericRow

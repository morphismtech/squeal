{-|
Module: Squeal.PostgreSQL.Session.Statement2
Description: Statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

A top-level `Statement` type wraps a `Squeal.PostgreSQL.Query.Query`
or `Squeal.PostgreSQL.Manipulation.Manipulation`
together with an `EncodeParams` and a `DecodeRow`.
-}

{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , DeriveFunctor
  , DeriveFoldable
  , DeriveGeneric
  , DeriveTraversable
  , FlexibleContexts
  , GADTs
  , RankNTypes
#-}

module Squeal.PostgreSQL.Session.Statement
  ( Statement (..)
  , query
  , manipulation
  , GenericParams
  , GenericRow
  ) where

import Data.Functor.Contravariant
import Data.Profunctor (Profunctor (..))

import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Session.Decode
import Squeal.PostgreSQL.Session.Encode
import Squeal.PostgreSQL.Session.Oid
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render

-- | A `Statement` consists of a `Squeal.PostgreSQL.Statement.Manipulation`
-- or a `Squeal.PostgreSQL.Session.Statement.Query` that can be run
-- in a `Squeal.PostgreSQL.Session.Monad.MonadPQ`.
data Statement db x y where
  -- | Constructor for a data manipulation language statement
  Manipulation
    :: (SOP.All (OidOfNull db) params, SOP.SListI row)
    => EncodeParams db params x -- ^ encoding of parameters
    -> DecodeRow row y -- ^ decoding of returned rows
    -> Manipulation '[] db params row
    -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto`,
    -- `Squeal.PostgreSQL.Manipulation.Update.update`,
    -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom`, ...
    -> Statement db x y
  -- | Constructor for a structured query language statement
  Query
    :: (SOP.All (OidOfNull db) params, SOP.SListI row)
    => EncodeParams db params x -- ^ encoding of parameters
    -> DecodeRow row y -- ^ decoding of returned rows
    -> Query '[] '[] db params row
    -- ^ `Squeal.PostgreSQL.Query.Select.select`,
    -- `Squeal.PostgreSQL.Query.Values.values`, ...
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

instance RenderSQL (Statement db x y) where
  renderSQL (Manipulation _ _ q) = renderSQL q
  renderSQL (Query _ _ q) = renderSQL q

-- | A `GenericParams` constraint to ensure that
-- a Haskell type is a product type,
-- all its terms have known Oids,
-- and can be encoded to corresponding
-- Postgres types.
type GenericParams db params x xs =
  ( SOP.All (OidOfNull db) params
  , SOP.IsProductType x xs
  , SOP.AllZip (ToParam db) params xs )

-- | A `GenericRow` constraint to ensure that
-- a Haskell type is a record type,
-- and all its fields and can be decoded from corresponding
-- Postgres fields.
type GenericRow row y ys =
  ( SOP.IsRecord y ys
  , SOP.AllZip FromField row ys )

-- | Smart constructor for a structured query language statement
query ::
  ( GenericParams db params x xs
  , GenericRow row y ys
  ) => Query '[] '[] db params row
    -- ^ `Squeal.PostgreSQL.Query.Select.select`,
    -- `Squeal.PostgreSQL.Query.Values.values`, ...
    -> Statement db x y
query = Query genericParams genericRow

-- | Smart constructor for a data manipulation language statement
manipulation ::
  ( GenericParams db params x xs
  , GenericRow row y ys
  ) => Manipulation '[] db params row
    -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto`,
    -- `Squeal.PostgreSQL.Manipulation.Update.update`,
    -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom`, ...
    -> Statement db x y
manipulation = Manipulation genericParams genericRow

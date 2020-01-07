{-|
Module: Squeal.PostgreSQL.PQ.Monad
Description: MonadPQ
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Run `Squeal.PostgreSQL.PQ.Statement`s in the mtl-style
typeclass `MonadPQ`.
-}
{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , PolyKinds
  , MultiParamTypeClasses
  , QuantifiedConstraints
  , RankNTypes
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ.Monad2 where

import Control.Category (Category (..))
import Control.Monad
import Control.Monad.Morph
import Data.Functor.Contravariant
import Data.Profunctor
import Prelude hiding (id, (.))

import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.PQ.Decode
import Squeal.PostgreSQL.PQ.Encode3
import Squeal.PostgreSQL.PQ.Oid3
import Squeal.PostgreSQL.PQ.Result
import Squeal.PostgreSQL.Query

-- For `MonadPQ` transformer instances
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict

{- | `MonadPQ` is an @mtl@ style constraint, similar to
`Control.Monad.State.Class.MonadState`, for using `Database.PostgreSQL.LibPQ`
to run `Statement`s.
-}
class Monad pq => MonadPQ db pq | pq -> db where

  {- |
  `executeParams` runs a `Statement`.
  It calls `LibPQ.execParams` and doesn't afraid of anything.
  -}
  executeParams :: Statement db x y -> x -> pq (Result y)
  default executeParams
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Statement db x y -> x -> pq (Result y)
  executeParams statement params = lift $ executeParams statement params

  {- |
  `executeParams_` runs a returning-free `Statement`.
  It calls `LibPQ.execParams` and doesn't afraid of anything.
  -}
  executeParams_ :: Statement db x () -> x -> pq ()
  executeParams_ statement params = void $ executeParams statement params

  {- |
  `execute` runs a parameter-free `Statement`.
  -}
  execute :: Statement db () y -> pq (Result y)
  execute statement = executeParams statement ()

  {- |
  `execute_` runs a parameter-free, returning-free `Statement`.
  -}
  execute_ :: Statement db () () -> pq ()
  execute_ = void . execute

  {- |
  `executePrepared` runs a `Statement` on a `Traversable`
  container by first preparing the statement, then running the prepared
  statement on each element.
  -}
  executePrepared
    :: Traversable list
    => Statement db x y -> list x -> pq (list (Result y))
  default executePrepared
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Traversable list
    => Statement db x y -> list x -> pq (list (Result y))
  executePrepared statement x = lift $ executePrepared statement x

  {- |
  `executePrepared_` runs a returning-free `Statement` on a `Foldable`
  container by first preparing the statement, then running the prepared
  statement on each element.
  -}
  executePrepared_
    :: Foldable list
    => Statement db x () -> list x -> pq ()
  default executePrepared_
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Foldable list
    => Statement db x () -> list x -> pq ()
  executePrepared_ statement x = lift $ executePrepared_ statement x

{- |
`manipulateParams` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`.
-}
manipulateParams ::
  ( MonadPQ db pq
  , SOP.IsProductType x xs
  , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
  , SOP.All (OidOfNull (MonadPQ db)) params
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Manipulation '[] db params row
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> x -> pq (Result y)
manipulateParams = executeParams . manipulation

{- |
`manipulateParams_` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`,
for a returning-free statement.
-}
manipulateParams_ ::
  ( MonadPQ db pq
  , SOP.IsProductType x xs
  , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
  , SOP.All (OidOfNull (MonadPQ db)) params
  ) => Manipulation '[] db params '[]
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> x -> pq ()
manipulateParams_ = executeParams_ . manipulation

{- |
`manipulate` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`,
for a parameter-free statement.
-}
manipulate
  :: (MonadPQ db pq, SOP.IsRecord y ys, SOP.AllZip FromField row ys)
  => Manipulation '[] db '[] row
  -> pq (Result y)
manipulate = execute . manipulation

{- |
`manipulate_` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`,
for a returning-free, parameter-free statement.
-}
manipulate_
  :: MonadPQ db pq
  => Manipulation '[] db '[] '[]
  -> pq ()
manipulate_ = execute_ . manipulation

{- |
`runQueryParams` runs a `Squeal.PostgreSQL.Query.Query`.
-}
runQueryParams ::
  ( MonadPQ db pq
  , SOP.IsProductType x xs
  , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
  , SOP.All (OidOfNull (MonadPQ db)) params
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Query '[] '[] db params row
    -- ^ `select` and friends
    -> x -> pq (Result y)
runQueryParams = executeParams . query

{- |
`runQuery` runs a `Squeal.PostgreSQL.Query.Query`,
for a parameter-free statement.
-}
runQuery
  :: (MonadPQ db pq, SOP.IsRecord y ys, SOP.AllZip FromField row ys)
  => Query '[] '[] db '[] row
  -- ^ `select` and friends
  -> pq (Result y)
runQuery = execute . query

{- |
`traversePrepared` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`
on a `Traversable` container by first preparing the statement,
then running the prepared statement on each element.
-}
traversePrepared
  :: ( MonadPQ db pq
     , SOP.IsProductType x xs
     , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
     , SOP.All (OidOfNull (MonadPQ db)) params
     , Traversable list
     , SOP.IsRecord y ys
     , SOP.AllZip FromField row ys )
  => Manipulation '[] db params row
  -- ^ `insertInto`, `update`, or `deleteFrom`, and friends
  -> list x -> pq (list (Result y))
traversePrepared = executePrepared . manipulation

{- |
`forPrepared` is a flipped `traversePrepared`
-}
forPrepared
  :: ( MonadPQ db pq
     , SOP.IsProductType x xs
     , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
     , SOP.All (OidOfNull (MonadPQ db)) params
     , Traversable list
     , SOP.IsRecord y ys
     , SOP.AllZip FromField row ys )
  => list x
  -> Manipulation '[] db params row
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> pq (list (Result y))
forPrepared = flip traversePrepared

{- |
`traversePrepared` runs a returning-free
`Squeal.PostgreSQL.Manipulation.Manipulation` on a `Foldable`
container by first preparing the statement, then running the prepared
statement on each element.
-}
traversePrepared_
  :: ( MonadPQ db pq
     , SOP.IsProductType x xs
     , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
     , SOP.All (OidOfNull (MonadPQ db)) params
     , Foldable list )
  => Manipulation '[] db params '[]
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> list x -> pq ()
traversePrepared_ = executePrepared_ . manipulation

{- |
`forPrepared_` is a flipped `traversePrepared_`
-}
forPrepared_
  :: ( MonadPQ db pq
     , SOP.IsProductType x xs
     , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
     , SOP.All (OidOfNull (MonadPQ db)) params
     , Foldable list )
  => list x
  -> Manipulation '[] db params '[]
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> pq ()
forPrepared_ = flip traversePrepared_

instance MonadPQ db m => MonadPQ db (IdentityT m)
instance MonadPQ db m => MonadPQ db (ReaderT r m)
instance MonadPQ db m => MonadPQ db (Strict.StateT s m)
instance MonadPQ db m => MonadPQ db (Lazy.StateT s m)
instance (Monoid w, MonadPQ db m) => MonadPQ db (Strict.WriterT w m)
instance (Monoid w, MonadPQ db m) => MonadPQ db (Lazy.WriterT w m)
instance MonadPQ db m => MonadPQ db (MaybeT m)
instance MonadPQ db m => MonadPQ db (ExceptT e m)
instance (Monoid w, MonadPQ db m) => MonadPQ db (Strict.RWST r w s m)
instance (Monoid w, MonadPQ db m) => MonadPQ db (Lazy.RWST r w s m)
instance MonadPQ db m => MonadPQ db (ContT r m)

-- | A `Statement` consists of a `Squeal.PostgreSQL.Statement.Manipulation`
-- or a `Squeal.PostgreSQL.PQ.Statement.Query` that can be run
-- in a `Squeal.PostgreSQL.PQ.Monad.MonadPQ`.
data Statement db x y where
  -- | Constructor for a data manipulation language statement
  Manipulation
    :: (SOP.All (OidOfNull (MonadPQ db)) params, SOP.SListI row)
    => EncodeParams (MonadPQ db) params x -- ^ encoding of parameters
    -> DecodeRow row y -- ^ decoding of returned rows
    -> Manipulation '[] db params row
    -- ^ `insertInto`, `update`, `deleteFrom`, ...
    -> Statement db x y
  -- | Constructor for a structured query language statement
  Query
    :: (SOP.All (OidOfNull (MonadPQ db)) params, SOP.SListI row)
    => EncodeParams (MonadPQ db) params x -- ^ encoding of parameters
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
  ( SOP.IsProductType x xs
  , SOP.All (OidOfNull (MonadPQ db)) params
  , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Query '[] '[] db params row -- ^ `select`, `values`, ...
    -> Statement db x y
query = Query genericParams genericRow

-- | Smart constructor for a data manipulation language statement
manipulation ::
  ( SOP.IsProductType x xs
  , SOP.All (OidOfNull (MonadPQ db)) params
  , SOP.AllZip (ToNullParam (MonadPQ db)) params xs
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Manipulation '[] db params row
    -- ^ `insertInto`, `update`, `deleteFrom`, ...
    -> Statement db x y
manipulation = Manipulation genericParams genericRow

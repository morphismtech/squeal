{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , PolyKinds
  , MultiParamTypeClasses
  , QuantifiedConstraints
  , RankNTypes
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ.Monad where

import Control.Category (Category (..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Data.Function ((&))
import Prelude hiding (id, (.))

import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.PQ.Decode
import Squeal.PostgreSQL.PQ.Encode
import Squeal.PostgreSQL.PQ.Oid
import Squeal.PostgreSQL.PQ.Result
import Squeal.PostgreSQL.PQ.Statement
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

{- | An [Atkey indexed monad]
(https://bentnib.org/paramnotions-jfp.pdf)
is a `Functor` [enriched category]
(https://ncatlab.org/nlab/show/enriched+category).
An indexed monad transformer transforms a `Monad` into an indexed monad,
and is a monad transformer when its source and target are the same,
enabling use of standard @do@ notation for endo-index operations.
-}
class
  ( forall i j m. Monad m => Functor (t i j m)
  , forall i j m. (i ~ j, Monad m) => Monad (t i j m)
  , forall i j. i ~ j => MonadTrans (t i j)
  ) => IndexedMonadTrans t where

  {-# MINIMAL pqJoin | pqBind #-}

  -- | indexed analog of `<*>`
  pqAp
    :: Monad m
    => t i j m (x -> y)
    -> t j k m x
    -> t i k m y
  pqAp tf tx = pqBind (<$> tx) tf

  -- | indexed analog of `join`
  pqJoin
    :: Monad m
    => t i j m (t j k m y)
    -> t i k m y
  pqJoin t = t & pqBind id

  -- | indexed analog of `=<<`
  pqBind
    :: Monad m
    => (x -> t j k m y)
    -> t i j m x
    -> t i k m y
  pqBind f t = pqJoin (f <$> t)

  -- | indexed analog of flipped `>>`
  pqThen
    :: Monad m
    => t j k m y
    -> t i j m x
    -> t i k m y
  pqThen pq2 pq1 = pq1 & pqBind (\ _ -> pq2)

  -- | indexed analog of `<=<`
  pqAndThen
    :: Monad m
    => (y -> t j k m z)
    -> (x -> t i j m y)
    -> x -> t i k m z
  pqAndThen g f x = pqBind g (f x)

{- | `Indexed` reshuffles the arguments of an `IndexedMonadTrans`,
exposing its `Category` instance.-}
newtype Indexed t m r i j = Indexed {runIndexed :: t i j m r}
instance
  ( IndexedMonadTrans t
  , Monad m
  , Monoid r
  ) => Category (Indexed t m r) where
    id = Indexed (pure mempty)
    Indexed g . Indexed f = Indexed $ pqAp (fmap (<>) f) g

{- | `IndexedMonadTransPQ` is a class for indexed monad transformers
that support running `Definition`s using `define` which acts functorially in effect.

* @define id = return ()@
* @define (statement1 >> statement2) = define statement1 & pqThen (define statement2)@
-}
class IndexedMonadTrans pq => IndexedMonadTransPQ pq where
  define :: MonadIO io => Definition db0 db1 -> pq db0 db1 io ()

{- | `MonadPQ` is an @mtl@ style constraint, similar to
`Control.Monad.State.Class.MonadState`, for using `Database.PostgreSQL.LibPQ` to

* `manipulateParams` runs a `Manipulation` with params from a type
   with a `ToParams` constraint. It calls `LibPQ.execParams` and
   doesn't afraid of anything.

* `manipulateParams_` is like `manipulateParams` for a returning-free statement.

* `manipulate` is like `manipulateParams` for a parameter-free statement.

* `manipulate_` is like `manipulate` for a returning-free statement.

* `runQueryParams` is like `manipulateParams` for query statements.

* `runQuery` is like `runQueryParams` for a parameter-free statement.

* `traversePrepared` has the same type signature as a composition of
  `traverse` and `manipulateParams` but provides an optimization by
  preparing the statement with `LibPQ.prepare` and then traversing a
  `Traversable` container with `LibPQ.execPrepared`. The temporary prepared
  statement is then deallocated.

* `forPrepared` is a flipped `traversePrepared`

* `traversePrepared_` is like `traversePrepared` but works on `Foldable`
  containers for a returning-free statement.

* `forPrepared_` is a flipped `traversePrepared_`.

* `liftPQ` lets you lift actions from `Database.PostgreSQL.LibPQ` that require a connection
  into your monad.

To define an instance, you can minimally define only `manipulateParams`,
`traversePrepared`, `traversePrepared_` and `liftPQ`. Monad transformers get
a default instance.

-}
class Monad pq => MonadPQ db pq | pq -> db where

  executeParams :: Statement db x y -> x -> pq (Result y)
  default executeParams
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Statement db x y -> x -> pq (Result y)
  executeParams statement params = lift $ executeParams statement params

  executeParams_ :: Statement db x () -> x -> pq ()
  executeParams_ statement params = void $ executeParams statement params

  execute :: Statement db () y -> pq (Result y)
  execute statement = executeParams statement ()

  execute_ :: Statement db () () -> pq ()
  execute_ = void . execute

  executePrepared
    :: Traversable list
    => Statement db x y -> list x -> pq (list (Result y))
  default executePrepared
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Traversable list
    => Statement db x y -> list x -> pq (list (Result y))
  executePrepared statement x = lift $ executePrepared statement x

  executePrepared_
    :: Foldable list
    => Statement db x () -> list x -> pq ()
  default executePrepared_
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Foldable list
    => Statement db x () -> list x -> pq ()
  executePrepared_ statement x = lift $ executePrepared_ statement x

manipulateParams ::
  ( MonadPQ db pq
  , SOP.IsProductType x xs
  , SOP.AllZip ToNullParam params xs
  , SOP.All OidOfNull params
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Manipulation '[] db params row
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> x -> pq (Result y)
manipulateParams = executeParams . manipulation

manipulateParams_ ::
  ( MonadPQ db pq
  , SOP.IsProductType x xs
  , SOP.AllZip ToNullParam params xs
  , SOP.All OidOfNull params
  ) => Manipulation '[] db params '[]
    -- ^ `insertInto`, `update` or `deleteFrom`
    -> x -> pq ()
manipulateParams_ = executeParams_ . manipulation

manipulate
  :: (MonadPQ db pq, SOP.IsRecord y ys, SOP.AllZip FromField row ys)
  => Manipulation '[] db '[] row
  -> pq (Result y)
manipulate = execute . manipulation

manipulate_
  :: MonadPQ db pq
  => Manipulation '[] db '[] '[]
  -> pq ()
manipulate_ = execute_ . manipulation

runQueryParams ::
  ( MonadPQ db pq
  , SOP.IsProductType x xs
  , SOP.AllZip ToNullParam params xs
  , SOP.All OidOfNull params
  , SOP.IsRecord y ys
  , SOP.AllZip FromField row ys
  ) => Query '[] '[] db params row
    -- ^ `select` and friends
    -> x -> pq (Result y)
runQueryParams = executeParams . query

runQuery
  :: (MonadPQ db pq, SOP.IsRecord y ys, SOP.AllZip FromField row ys)
  => Query '[] '[] db '[] row
  -- ^ `select` and friends
  -> pq (Result y)
runQuery = execute . query

traversePrepared
  :: ( MonadPQ db pq
     , SOP.IsProductType x xs
     , SOP.AllZip ToNullParam params xs
     , Traversable list
     , SOP.All OidOfNull params
     , SOP.IsRecord y ys
     , SOP.AllZip FromField row ys )
  => Manipulation '[] db params row
  -- ^ `insertInto`, `update`, or `deleteFrom`, and friends
  -> list x -> pq (list (Result y))
traversePrepared = executePrepared . manipulation

forPrepared
  :: ( MonadPQ db pq
     , SOP.IsProductType x xs
     , SOP.AllZip ToNullParam params xs
     , Traversable list
     , SOP.All OidOfNull params
     , SOP.IsRecord y ys
     , SOP.AllZip FromField row ys )
  => list x
  -> Manipulation '[] db params row
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> pq (list (Result y))
forPrepared = flip traversePrepared

traversePrepared_
  :: ( MonadPQ db pq
     , SOP.IsProductType x xs
     , SOP.AllZip ToNullParam params xs
     , Foldable list
     , SOP.All OidOfNull params )
  => Manipulation '[] db params '[]
  -- ^ `insertInto`, `update` or `deleteFrom`
  -> list x -> pq ()
traversePrepared_ = executePrepared_ . manipulation

forPrepared_
  :: ( MonadPQ db pq
     , SOP.IsProductType x xs
     , SOP.AllZip ToNullParam params xs
     , Foldable list
     , SOP.All OidOfNull params )
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

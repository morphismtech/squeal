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

module Squeal.PostgreSQL.PQ.Indexed
  ( IndexedMonadTrans (..)
  , Indexed (..)
  , IndexedMonadTransPQ (..)
  , indexedDefine
  ) where

import Control.Category (Category (..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Function ((&))
import Prelude hiding (id, (.))

import Squeal.PostgreSQL.Definition

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
* @define (statement1 >>> statement2) = define statement1 & pqThen (define statement2)@
-}
class IndexedMonadTrans pq => IndexedMonadTransPQ pq where
  define :: MonadIO io => Definition db0 db1 -> pq db0 db1 io ()

{- | Run a pure SQL `Definition` functorially in effect

* @indexedDefine id = id@
* @indexedDefine (def1 >>> def2) = indexedDefine def1 >>> indexedDefine def2@
-}
indexedDefine
  :: (IndexedMonadTransPQ pq, MonadIO io)
  => Definition db0 db1 -> Indexed pq io () db0 db1
indexedDefine = Indexed . define

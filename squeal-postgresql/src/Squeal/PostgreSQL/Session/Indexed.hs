{-|
Module: Squeal.PostgreSQL.Session.Indexed
Description: indexed session monad
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

`Squeal.PostgreSQL.Indexed` provides an indexed monad transformer
class and a class extending it to run `Definition`s.
-}

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

module Squeal.PostgreSQL.Session.Indexed
  ( IxMonadTrans (..)
  , Indexed (..)
  , IxMonadTransPQ (..)
  , indexedDefine
  ) where

import Control.Category (Category (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Indexed
import Prelude hiding (id, (.))

import Squeal.PostgreSQL.Definition

{- | `IndexedMonadTransPQ` is a class for indexed monad transformers
that support running `Definition`s using `define` which acts functorially in effect.

* @define id = return ()@
* @define (statement1 >>> statement2) = define statement1 & pqThen (define statement2)@
-}
class IxMonadTrans pq => IxMonadTransPQ pq where
  define :: MonadIO io => Definition db0 db1 -> pq db0 db1 io ()

{- | Run a pure SQL `Definition` functorially in effect

* @indexedDefine id = id@
* @indexedDefine (def1 >>> def2) = indexedDefine def1 >>> indexedDefine def2@
-}
indexedDefine
  :: (IxMonadTransPQ pq, MonadIO io)
  => Definition db0 db1 -> Indexed pq io () db0 db1
indexedDefine = Indexed . define

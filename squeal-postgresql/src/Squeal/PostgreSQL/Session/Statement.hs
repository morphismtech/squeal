{-|
Module: Squeal.PostgreSQL.Session.Statement
Description: statements
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

module Squeal.PostgreSQL.Session.Statement
  ( Statement (..)
  , query
  , manipulation
  , Prepared (..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Data.Functor.Contravariant
import Data.Profunctor
import GHC.Generics
import Prelude hiding ((.),id)

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Session.Decode
import Squeal.PostgreSQL.Session.Encode
import Squeal.PostgreSQL.Session.Oid
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Render hiding ((<+>))

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

{- |
`Squeal.PostgreSQL.Session.Monad.prepare` and
`Squeal.PostgreSQL.Session.Monad.prepare_` create a `Prepared` statement.
A `Prepared` statement is a server-side object
that can be used to optimize performance.
When the PREPARE statement is executed,
the specified statement is parsed, analyzed, and rewritten.

When the `runPrepared` command is subsequently issued,
the prepared statement is planned and executed.
This division of labor avoids repetitive parse analysis work,
while allowing the execution plan to
depend on the specific parameter values supplied.

`Prepared` statements only last for the duration
of the current database session. When the session ends,
the prepared statement is forgotten,
so it must be recreated before being used again.
This also means that a single prepared statement
cannot be used by multiple simultaneous database clients;
however, each client can create their own `Prepared` statement to use.
Prepared statements can be manually cleaned up using the `deallocate` command.
-}
data Prepared f x y = Prepared
  { runPrepared :: x -> f y -- ^ execute a prepared statement
  , deallocate :: f () -- ^ manually clean up a prepared statement
  } deriving (Functor, Generic, Generic1)

instance Applicative f => Applicative (Prepared f x) where
  pure a = Prepared (\_ -> pure a) (pure ())
  p1 <*> p2 = Prepared
    (kleisliRun2 (<*>) p1 p2)
    (deallocate p1 *> deallocate p2)

instance Alternative f => Alternative (Prepared f x) where
  empty = Prepared (runKleisli empty) empty
  p1 <|> p2 = Prepared
    (kleisliRun2 (<|>) p1 p2)
    (deallocate p1 *> deallocate p2)

instance Functor f => Profunctor (Prepared f) where
  dimap g f prepared = Prepared
    (fmap f . runPrepared prepared . g)
    (deallocate prepared)

instance Monad f => Strong (Prepared f) where
  first' p = Prepared (kleisliRun1 first' p) (deallocate p)
  second' p = Prepared (kleisliRun1 second' p) (deallocate p)

instance Monad f => Choice (Prepared f) where
  left' p = Prepared (kleisliRun1 left' p) (deallocate p)
  right' p = Prepared (kleisliRun1 right' p) (deallocate p)

instance Monad f => Category (Prepared f) where
  id = Prepared return (return ())
  cd . ab = Prepared
    (runPrepared ab >=> runPrepared cd)
    (deallocate ab >> deallocate cd)

instance Monad f => Arrow (Prepared f) where
  arr ab = Prepared (return . ab) (return ())
  first = first'
  second = second'
  ab *** cd = first ab >>> second cd
  ab &&& ac = Prepared
    (kleisliRun2 (&&&) ab ac)
    (deallocate ab >> deallocate ac)

instance Monad f => ArrowChoice (Prepared f) where
  left = left'
  right = right'
  ab +++ cd = left ab >>> right cd
  bd ||| cd = Prepared
    (kleisliRun2 (|||) bd cd)
    (deallocate bd >> deallocate cd)

instance MonadFix f => ArrowLoop (Prepared f) where
  loop p = Prepared (kleisliRun1 loop p) (deallocate p)

instance MonadPlus f => ArrowZero (Prepared f) where
  zeroArrow = Prepared (runKleisli zeroArrow) (return ())

instance MonadPlus f => ArrowPlus (Prepared f) where
  p1 <+> p2 = Prepared
    (kleisliRun2 (<+>) p1 p2)
    (deallocate p1 >> deallocate p2)

-- helper functions

kleisliRun1
  :: (Kleisli m a b -> Kleisli m c d)
  -> Prepared m a b -> c -> m d
kleisliRun1 f = runKleisli . f . Kleisli . runPrepared

kleisliRun2
  :: (Kleisli m a b -> Kleisli m c d -> Kleisli m e f)
  -> Prepared m a b -> Prepared m c d -> e -> m f
kleisliRun2 (?) p1 p2 = runKleisli $
  Kleisli (runPrepared p1) ? Kleisli (runPrepared p2)

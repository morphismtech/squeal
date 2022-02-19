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
  ( -- * Statement
    Statement (..)
  , query
  , manipulation
    -- * Prepared
  , Prepared (..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Profunctor.Traversing
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
  -- | Constructor for a data manipulation language `Statement`
  Manipulation
    :: (SOP.All (OidOfNull db) params, SOP.SListI row)
    => EncodeParams db params x -- ^ encoding of parameters
    -> DecodeRow row y -- ^ decoding of returned rows
    -> Manipulation '[] db params row
    -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto`,
    -- `Squeal.PostgreSQL.Manipulation.Update.update`,
    -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom`, ...
    -> Statement db x y
  -- | Constructor for a structured query language `Statement`
  Query
    :: (SOP.All (OidOfNull db) params, SOP.SListI row)
    => EncodeParams db params x -- ^ encoding of parameters
    -> DecodeRow row y -- ^ decoding of returned rows
    -> Query '[] '[] db params row
    -- ^ `Squeal.PostgreSQL.Query.Select.select`,
    -- `Squeal.PostgreSQL.Query.Values.values`, ...
    -> Statement db x y

instance Profunctor (Statement db) where
  lmap m (Manipulation encode decode q) =
    Manipulation (contramap m encode) decode q
  lmap m (Query encode decode q) =
    Query (contramap m encode) decode q
  rmap m (Manipulation encode decode q) =
    Manipulation encode (fmap m decode) q
  rmap m (Query encode decode q) =
    Query encode (fmap m decode) q
  dimap m g (Manipulation encode decode q) =
    Manipulation (contramap m encode) (fmap g decode) q
  dimap m g (Query encode decode q) =
    Query (contramap m encode) (fmap g decode) q

instance Functor (Statement db x) where fmap = rmap

instance RenderSQL (Statement db x y) where
  renderSQL (Manipulation _ _ q) = renderSQL q
  renderSQL (Query _ _ q) = renderSQL q

-- | Smart constructor for a structured query language `Statement`
query ::
  ( GenericParams db params x xs
  , GenericRow row y ys
  ) => Query '[] '[] db params row
    -- ^ `Squeal.PostgreSQL.Query.Select.select`,
    -- `Squeal.PostgreSQL.Query.Values.values`, ...
    -> Statement db x y
query = Query genericParams genericRow

-- | Smart constructor for a data manipulation language `Statement`
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
When `Squeal.PostgreSQL.Session.Monad.prepare`
or `Squeal.PostgreSQL.Session.Monad.prepare_` is executed,
the specified `Statement` is parsed, analyzed, and rewritten.

When the `runPrepared` command is subsequently issued,
the `Prepared` statement is planned and executed.
This division of labor avoids repetitive parse analysis work,
while allowing the execution plan to
depend on the specific parameter values supplied.

`Prepared` statements only last for the duration
of the current database session.
`Prepared` statements can be manually cleaned up
using the `deallocate` command.
-}
data Prepared m x y = Prepared
  { runPrepared :: x -> m y -- ^ execute a prepared statement
  , deallocate :: m () -- ^ manually clean up a prepared statement
  } deriving (Functor, Generic, Generic1)

instance Applicative m => Applicative (Prepared m x) where
  pure a = Prepared (\_ -> pure a) (pure ())
  p1 <*> p2 = Prepared
    (run2 (<*>) p1 p2)
    (deallocate p1 *> deallocate p2)

instance Alternative m => Alternative (Prepared m x) where
  empty = Prepared (runKleisli empty) empty
  p1 <|> p2 = Prepared
    (run2 (<|>) p1 p2)
    (deallocate p1 *> deallocate p2)

instance Functor m => Profunctor (Prepared m) where
  dimap g m prepared = Prepared
    (fmap m . runPrepared prepared . g)
    (deallocate prepared)

instance Monad m => Strong (Prepared m) where
  first' p = Prepared (run1 first' p) (deallocate p)
  second' p = Prepared (run1 second' p) (deallocate p)

instance Monad m => Choice (Prepared m) where
  left' p = Prepared (run1 left' p) (deallocate p)
  right' p = Prepared (run1 right' p) (deallocate p)

instance MonadFix m => Costrong (Prepared m) where
  unfirst p = Prepared (run1 unfirst p) (deallocate p)
  unsecond p = Prepared (run1 unsecond p) (deallocate p)

instance Monad m => Category (Prepared m) where
  id = Prepared return (return ())
  cd . ab = Prepared
    (runPrepared ab >=> runPrepared cd)
    (deallocate ab >> deallocate cd)

instance Monad m => Arrow (Prepared m) where
  arr ab = Prepared (return . ab) (return ())
  first = first'
  second = second'
  ab *** cd = first ab >>> second cd
  ab &&& ac = Prepared
    (run2 (&&&) ab ac)
    (deallocate ab >> deallocate ac)

instance Monad m => ArrowChoice (Prepared m) where
  left = left'
  right = right'
  ab +++ cd = left ab >>> right cd
  bd ||| cd = Prepared
    (run2 (|||) bd cd)
    (deallocate bd >> deallocate cd)

instance MonadFix m => ArrowLoop (Prepared m) where
  loop p = Prepared (run1 loop p) (deallocate p)

instance MonadPlus m => ArrowZero (Prepared m) where
  zeroArrow = Prepared (runKleisli zeroArrow) (return ())

instance MonadPlus m => ArrowPlus (Prepared m) where
  p1 <+> p2 = Prepared
    (run2 (<+>) p1 p2)
    (deallocate p1 >> deallocate p2)

instance Monad m => Traversing (Prepared m) where
  traverse' p = Prepared (run1 traverse' p) (deallocate p)

-- helper functions

run1
  :: (Kleisli m a b -> Kleisli m c d)
  -> Prepared m a b -> c -> m d
run1 m = runKleisli . m . Kleisli . runPrepared

run2
  :: (Kleisli m a b -> Kleisli m c d -> Kleisli m e f)
  -> Prepared m a b -> Prepared m c d -> e -> m f
run2 (?) p1 p2 = runKleisli $
  Kleisli (runPrepared p1) ? Kleisli (runPrepared p2)

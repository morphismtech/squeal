{-|
Module: Squeal.PostgreSQL.Session.Monad
Description: session monad
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Run `Squeal.PostgreSQL.Session.Statement`s in the mtl-style
typeclass `MonadPQ`.
-}
{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , DeriveFunctor
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , MultiParamTypeClasses
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Session.Monad
  ( -- * MonadPQ
    MonadPQ (..)
    -- * Prepared
  , forPrepared
  , forPrepared_
  , withPrepared
    -- * IxMonadTransPQ
  , IxMonadTransPQ (..)
  ) where

import Control.Category (Category (..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Trans.Indexed
import Data.Foldable
import Data.Profunctor.Traversing
import Data.Traversable
import Prelude hiding (id, (.))

import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Session.Decode
import Squeal.PostgreSQL.Session.Result
import Squeal.PostgreSQL.Session.Statement

-- For `MonadPQ` transformer instances
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict

-- $setup
-- >>> import Squeal.PostgreSQL

{- | `MonadPQ` is an @mtl@ style constraint, similar to
`Control.Monad.State.Class.MonadState`, for using `Database.PostgreSQL.LibPQ`
to run `Statement`s.
-}
class MonadResult pq => MonadPQ db pq | pq -> db where

  {- |
  `executeParams` runs a `Statement` which takes out-of-line
  `Squeal.PostgreSQL.Expression.Parameter.parameter`s.

  >>> import Data.Int (Int32, Int64)
  >>> import Data.Monoid (Sum(Sum))
  >>> :{
  let
    sumOf :: Statement db (Int32, Int32) (Sum Int32)
    sumOf = query $ values_ $
      ( param @1 @('NotNull 'PGint4) +
        param @2 @('NotNull 'PGint4)
      ) `as` #getSum
  in
    withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $ do
      result <- executeParams sumOf (2,2)
      firstRow result
  :}
  Just (Sum {getSum = 4})
  -}
  executeParams
    :: Statement db x y
    -- ^ query or manipulation
    -> x
    -- ^ parameters
    -> pq (Result y)
  default executeParams
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Statement db x y
    -- ^ query or manipulation
    -> x
    -- ^ parameters
    -> pq (Result y)
  executeParams statement params = lift $ executeParams statement params

  {- |
  `executeParams_` runs a returning-free `Statement`.

  >>> type Column = 'NoDef :=> 'NotNull 'PGint4
  >>> type Columns = '["col1" ::: Column, "col2" ::: Column]
  >>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
  >>> type DB = Public Schema
  >>> import Data.Int(Int32)
  >>> :{
  let
    insertion :: Statement DB (Int32, Int32) ()
    insertion = manipulation $ insertInto_ #tab $ Values_ $
      Set (param @1 @('NotNull 'PGint4)) `as` #col1 :*
      Set (param @2 @('NotNull 'PGint4)) `as` #col2
    setup :: Definition (Public '[]) DB
    setup = createTable #tab
      ( notNullable int4 `as` #col1 :*
        notNullable int4 `as` #col2
      ) Nil
    teardown :: Definition DB (Public '[])
    teardown = dropTable #tab
  in
    withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $
      define setup
      & pqThen (executeParams_ insertion (2,2))
      & pqThen (define teardown)
  :}
  -}
  executeParams_
    :: Statement db x ()
    -- ^ query or manipulation
    -> x
    -- ^ parameters
    -> pq ()
  executeParams_ statement params = void $ executeParams statement params

  {- | `execute` runs a parameter-free `Statement`.

  >>> import Data.Int(Int32)
  >>> :{
  let
    two :: Expr ('NotNull 'PGint4)
    two = 2
    twoPlusTwo :: Statement db () (Only Int32)
    twoPlusTwo = query $ values_ $ (two + two) `as` #fromOnly
  in
    withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $ do
      result <- execute twoPlusTwo
      firstRow result
  :}
  Just (Only {fromOnly = 4})
  -}
  execute
    :: Statement db () y
    -- ^ query or manipulation
    -> pq (Result y)
  execute statement = executeParams statement ()

  {- | `execute_` runs a parameter-free, returning-free `Statement`.

  >>> :{
  let
    silence :: Statement db () ()
    silence = manipulation $
      UnsafeManipulation "Set client_min_messages TO WARNING"
  in
    withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $ execute_ silence
  :}
  -}
  execute_
    :: Statement db () ()
    -- ^ query or manipulation
    -> pq ()
  execute_ = void . execute

  {- |
  `prepare` creates a `Prepared` statement. When `prepare` is executed,
  the specified `Statement` is parsed, analyzed, and rewritten.

  >>> import Data.Int (Int32, Int64)
  >>> import Data.Monoid (Sum(Sum))
  >>> :{
  let
    sumOf :: Statement db (Int32, Int32) (Sum Int32)
    sumOf = query $ values_ $
      ( param @1 @('NotNull 'PGint4) +
        param @2 @('NotNull 'PGint4)
      ) `as` #getSum
  in
    withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $ do
      prepared <- prepare sumOf
      result <- runPrepared prepared (2,2)
      deallocate prepared
      firstRow result
  :}
  Just (Sum {getSum = 4})
  -}
  prepare
    :: Statement db x y
    -- ^ query or manipulation
    -> pq (Prepared pq x (Result y))
  default prepare
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Statement db x y
    -- ^ query or manipulation
    -> pq (Prepared pq x (Result y))
  prepare statement = do
    prepared <- lift $ prepare statement
    return $ Prepared
      (lift . runPrepared prepared)
      (lift (deallocate prepared))

  {- |
  `prepare_` creates a `Prepared` statement. When `prepare_` is executed,
  the specified `Statement` is parsed, analyzed, and rewritten.

  >>> type Column = 'NoDef :=> 'NotNull 'PGint4
  >>> type Columns = '["col1" ::: Column, "col2" ::: Column]
  >>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
  >>> type DB = Public Schema
  >>> import Data.Int(Int32)
  >>> :{
  let
    insertion :: Statement DB (Int32, Int32) ()
    insertion = manipulation $ insertInto_ #tab $ Values_ $
      Set (param @1 @('NotNull 'PGint4)) `as` #col1 :*
      Set (param @2 @('NotNull 'PGint4)) `as` #col2
    setup :: Definition (Public '[]) DB
    setup = createTable #tab
      ( notNullable int4 `as` #col1 :*
        notNullable int4 `as` #col2
      ) Nil
    session :: PQ DB DB IO ()
    session = do
      prepared <- prepare_ insertion
      runPrepared prepared (2,2)
      deallocate prepared
    teardown :: Definition DB (Public '[])
    teardown = dropTable #tab
  in
    withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $
      define setup
      & pqThen session
      & pqThen (define teardown)
  :}
  -}
  prepare_
    :: Statement db x ()
    -- ^ query or manipulation
    -> pq (Prepared pq x ())
  prepare_ = fmap void . prepare

{- |
* `prepare` a statement
* transforming its inputs and outputs with an optic,
  run the `Prepared` statement
* deallocate the `Prepared` statement

>>> :type withPrepared traverse'
withPrepared traverse'
  :: (MonadPQ db pq, Traversable f) =>
     Statement db a b -> f a -> pq (f (Result b))
-}
withPrepared
  :: MonadPQ db pq
  => (Prepared pq a (Result b) -> Prepared pq s t)
  -- ^ transform the input and output
  -> Statement db a b -- ^ query or manipulation
  -> s -> pq t
withPrepared optic statement x' = do
  prepared <- prepare statement
  y' <- runPrepared (optic prepared) x'
  deallocate prepared
  return y'

{- |
`forPrepared` runs a `Statement` on a `Traversable`
container by first preparing the statement, then running the prepared
statement on each element.

>>> import Data.Int (Int32, Int64)
>>> import Data.Monoid (Sum(Sum))
>>> :{
let
  sumOf :: Statement db (Int32, Int32) (Sum Int32)
  sumOf = query $ values_ $
    ( param @1 @('NotNull 'PGint4) +
      param @2 @('NotNull 'PGint4)
    ) `as` #getSum
in
  withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $ do
    results <- forPrepared sumOf [(2,2),(3,3),(4,4)]
    traverse firstRow results
:}
[Just (Sum {getSum = 4}),Just (Sum {getSum = 6}),Just (Sum {getSum = 8})]
-}
forPrepared
  :: (MonadPQ db pq, Traversable list)
  => Statement db x y
  -- ^ query or manipulation
  -> list x
  -- ^ list of parameters
  -> pq (list (Result y))
forPrepared = withPrepared traverse'

{- |
`forPrepared_` runs a returning-free `Statement` on a `Foldable`
container by first preparing the statement, then running the prepared
statement on each element.

>>> type Column = 'NoDef :=> 'NotNull 'PGint4
>>> type Columns = '["col1" ::: Column, "col2" ::: Column]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> type DB = Public Schema
>>> import Data.Int(Int32)
>>> :{
let
  insertion :: Statement DB (Int32, Int32) ()
  insertion = manipulation $ insertInto_ #tab $ Values_ $
    Set (param @1 @('NotNull 'PGint4)) `as` #col1 :*
    Set (param @2 @('NotNull 'PGint4)) `as` #col2
  setup :: Definition (Public '[]) DB
  setup = createTable #tab
    ( notNullable int4 `as` #col1 :*
      notNullable int4 `as` #col2
    ) Nil
  teardown :: Definition DB (Public '[])
  teardown = dropTable #tab
in
  withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $
    define setup
    & pqThen (forPrepared_ insertion [(2,2),(3,3),(4,4)])
    & pqThen (define teardown)
:}
-}
forPrepared_
  :: (MonadPQ db pq, Foldable list)
  => Statement db x ()
  -- ^ query or manipulation
  -> list x
  -- ^ list of parameters
  -> pq ()
forPrepared_ = withPrepared (wander traverse_)

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

{- | `IndexedMonadTransPQ` is a class for indexed monad transformers
that support running `Definition`s using `define` which acts functorially in effect.

* @define id = return ()@
* @define (statement1 >>> statement2) = define statement1 & pqThen (define statement2)@
-}
class IxMonadTrans pq => IxMonadTransPQ pq where
  define :: MonadIO io => Definition db0 db1 -> pq db0 db1 io ()

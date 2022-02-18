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
  , PolyKinds
  , MultiParamTypeClasses
  , QuantifiedConstraints
  , RankNTypes
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Session.Monad where

import Control.Category (Category (..))
import Control.Monad
import Control.Monad.Morph
import Data.Foldable
import Data.Traversable
import Prelude hiding (id, (.))

import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Session.Decode
import Squeal.PostgreSQL.Session.Encode
import Squeal.PostgreSQL.Session.Result
import Squeal.PostgreSQL.Session.Statement
import Squeal.PostgreSQL.Query

-- For `MonadPQ` transformer instances
import Control.Monad.Trans.Cont
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
class Monad pq => MonadPQ db pq | pq -> db where

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
  execute_ :: Statement db () () -> pq ()
  execute_ = void . execute

  prepare
    :: Statement db x y
    -> pq (Prepared pq x (Result y))
  default prepare
    :: (MonadTrans t, MonadPQ db m, pq ~ t m)
    => Statement db x y
    -- ^ query or manipulation
    -> pq (Prepared pq x (Result y))
  prepare statement = do
    prepared <- lift $ prepare statement
    return $ Prepared
      { runPrepared = lift . runPrepared prepared
      , deallocate = lift (deallocate prepared)
      }

  prepare_
    :: Statement db x ()
    -> pq (Prepared pq x ())
  prepare_ statement = do
    prepared <- prepare statement
    return $ Prepared
      { runPrepared = void . runPrepared prepared
      , deallocate = deallocate prepared
      }

{- |
`executePrepared` runs a `Statement` on a `Traversable`
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
    results <- executePrepared sumOf [(2,2),(3,3),(4,4)]
    traverse firstRow results
:}
[Just (Sum {getSum = 4}),Just (Sum {getSum = 6}),Just (Sum {getSum = 8})]
-}
executePrepared
  :: (MonadPQ db pq, Traversable list)
  => Statement db x y
  -- ^ query or manipulation
  -> list x
  -- ^ list of parameters
  -> pq (list (Result y))
executePrepared statement list = do
  prepared <- prepare statement
  results <- traverse (runPrepared prepared) list
  deallocate prepared
  return results

{- |
`executePrepared_` runs a returning-free `Statement` on a `Foldable`
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
    & pqThen (executePrepared_ insertion [(2,2),(3,3),(4,4)])
    & pqThen (define teardown)
:}
-}
executePrepared_
  :: (MonadPQ db pq, Foldable list)
  => Statement db x ()
  -- ^ query or manipulation
  -> list x
  -- ^ list of parameters
  -> pq ()
executePrepared_ statement list = do
  prepared <- prepare_ statement
  traverse_ (runPrepared prepared) list
  deallocate prepared

{- |
`manipulateParams` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`.

>>> type Column = 'NoDef :=> 'NotNull 'PGint4
>>> type Columns = '["col1" ::: Column, "col2" ::: Column]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> type DB = Public Schema
>>> import Control.Monad.IO.Class
>>> import Data.Int(Int32)
>>> :{
let
  insertAdd :: Manipulation_ DB (Int32, Int32) (Only Int32)
  insertAdd = insertInto #tab 
    ( Values_ $
        Set (param @1 @('NotNull 'PGint4)) `as` #col1 :*
        Set (param @2 @('NotNull 'PGint4)) `as` #col2
    ) OnConflictDoRaise
    ( Returning_ ((#col1 + #col2) `as` #fromOnly) )
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
    & pqThen
      ( do
          result <- manipulateParams insertAdd (2::Int32,2::Int32)
          Just (Only answer) <- firstRow result
          liftIO $ print (answer :: Int32)
      )
    & pqThen (define teardown)
:}
4
-}
manipulateParams ::
  ( MonadPQ db pq
  , GenericParams db params x xs
  , GenericRow row y ys
  ) => Manipulation '[] db params row
    -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto`,
    -- `Squeal.PostgreSQL.Manipulation.Update.update`,
    -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom`, and friends
    -> x -> pq (Result y)
manipulateParams = executeParams . manipulation

{- |
`manipulateParams_` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`,
for a returning-free statement.

>>> type Column = 'NoDef :=> 'NotNull 'PGint4
>>> type Columns = '["col1" ::: Column, "col2" ::: Column]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> type DB = Public Schema
>>> import Data.Int(Int32)
>>> :{
let
  insertion :: Manipulation_ DB (Int32, Int32) ()
  insertion = insertInto_ #tab $ Values_ $
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
    & pqThen (manipulateParams_ insertion (2::Int32,2::Int32))
    & pqThen (define teardown)
:}
-}
manipulateParams_ ::
  ( MonadPQ db pq
  , GenericParams db params x xs
  ) => Manipulation '[] db params '[]
    -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto_`,
    -- `Squeal.PostgreSQL.Manipulation.Update.update_`,
    -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom_`, and friends
    -> x -> pq ()
manipulateParams_ = executeParams_ . manipulation

{- |
`manipulate` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`,
for a parameter-free statement.

>>> type Column = 'NoDef :=> 'NotNull 'PGint4
>>> type Columns = '["col1" ::: Column, "col2" ::: Column]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> type DB = Public Schema
>>> import Control.Monad.IO.Class
>>> import Data.Int(Int32)
>>> :{
let
  insertTwoPlusTwo :: Manipulation_ DB () (Only Int32)
  insertTwoPlusTwo = insertInto #tab 
    (Values_ $ Set 2 `as` #col1 :* Set 2 `as` #col2)
    OnConflictDoRaise
    (Returning_ ((#col1 + #col2) `as` #fromOnly))
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
    & pqThen
      ( do
          result <- manipulate insertTwoPlusTwo
          Just (Only answer) <- firstRow result
          liftIO $ print (answer :: Int32)
      )
    & pqThen (define teardown)
:}
4
-}
manipulate
  :: (MonadPQ db pq, GenericRow row y ys)
  => Manipulation '[] db '[] row
  -> pq (Result y)
manipulate = execute . manipulation

{- |
`manipulate_` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`,
for a returning-free, parameter-free statement.

>>> :{
let
  silence :: Manipulation_ db () ()
  silence = UnsafeManipulation "Set client_min_messages TO WARNING"
in
  withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $ manipulate_ silence
:}
-}
manipulate_
  :: MonadPQ db pq
  => Manipulation '[] db '[] '[]
  -> pq ()
manipulate_ = execute_ . manipulation

{- |
`runQueryParams` runs a `Squeal.PostgreSQL.Query.Query`.

>>> import Data.Int (Int32, Int64)
>>> import Control.Monad.IO.Class
>>> import Data.Monoid (Sum(Sum))
>>> :{
let
  sumOf :: Query_ db (Int32, Int32) (Sum Int32)
  sumOf = values_ $
    ( param @1 @('NotNull 'PGint4) +
      param @2 @('NotNull 'PGint4)
    ) `as` #getSum
in
  withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $ do
    result <- runQueryParams sumOf (2::Int32,2::Int32)
    Just (Sum four) <- firstRow result
    liftIO $ print (four :: Int32)
:}
4
-}
runQueryParams ::
  ( MonadPQ db pq
  , GenericParams db params x xs
  , GenericRow row y ys
  ) => Query '[] '[] db params row
    -- ^ `Squeal.PostgreSQL.Query.Select.select` and friends
    -> x -> pq (Result y)
runQueryParams = executeParams . query

{- |
`runQuery` runs a `Squeal.PostgreSQL.Query.Query`,
for a parameter-free statement.

>>> import Data.Int (Int32, Int64)
>>> import Control.Monad.IO.Class
>>> import Data.Monoid (Sum(Sum))
>>> :{
let
  twoPlusTwo :: Query_ db () (Sum Int32)
  twoPlusTwo = values_ $ (2 + 2) `as` #getSum
in
  withConnection "host=localhost port=5432 dbname=exampledb user=postgres password=postgres" $ do
    result <- runQuery twoPlusTwo
    Just (Sum four) <- firstRow result
    liftIO $ print (four :: Int32)
:}
4
-}
runQuery
  :: (MonadPQ db pq, GenericRow row y ys)
  => Query '[] '[] db '[] row
  -- ^ `Squeal.PostgreSQL.Query.Select.select` and friends
  -> pq (Result y)
runQuery = execute . query

{- |
`traversePrepared` runs a `Squeal.PostgreSQL.Manipulation.Manipulation`
on a `Traversable` container by first preparing the statement,
then running the prepared statement on each element.

>>> type Column = 'NoDef :=> 'NotNull 'PGint4
>>> type Columns = '["col1" ::: Column, "col2" ::: Column]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> type DB = Public Schema
>>> import Control.Monad.IO.Class
>>> import Data.Int(Int32)
>>> :{
let
  insertAdd :: Manipulation_ DB (Int32, Int32) (Only Int32)
  insertAdd = insertInto #tab 
    ( Values_ $
        Set (param @1 @('NotNull 'PGint4)) `as` #col1 :*
        Set (param @2 @('NotNull 'PGint4)) `as` #col2
    ) OnConflictDoRaise
    ( Returning_ ((#col1 + #col2) `as` #fromOnly) )
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
    & pqThen
      ( do
          results <- traversePrepared insertAdd [(2::Int32,2::Int32),(3,3),(4,4)]
          answers <- traverse firstRow results
          liftIO $ print [answer :: Int32 | Just (Only answer) <- answers]
      )
    & pqThen (define teardown)
:}
[4,6,8]
-}
traversePrepared
  :: ( MonadPQ db pq
     , GenericParams db params x xs
     , GenericRow row y ys
     , Traversable list )
  => Manipulation '[] db params row
  -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto`,
  -- `Squeal.PostgreSQL.Manipulation.Update.update`,
  -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom`, and friends
  -> list x -> pq (list (Result y))
traversePrepared = executePrepared . manipulation

{- |
`forPrepared` is a flipped `traversePrepared`

>>> type Column = 'NoDef :=> 'NotNull 'PGint4
>>> type Columns = '["col1" ::: Column, "col2" ::: Column]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> type DB = Public Schema
>>> import Control.Monad.IO.Class
>>> import Data.Int(Int32)
>>> :{
let
  insertAdd :: Manipulation_ DB (Int32, Int32) (Only Int32)
  insertAdd = insertInto #tab 
    ( Values_ $
        Set (param @1 @('NotNull 'PGint4)) `as` #col1 :*
        Set (param @2 @('NotNull 'PGint4)) `as` #col2
    ) OnConflictDoRaise
    ( Returning_ ((#col1 + #col2) `as` #fromOnly) )
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
    & pqThen
      ( do
          results <- forPrepared [(2::Int32,2::Int32),(3,3),(4,4)] insertAdd
          answers <- traverse firstRow results
          liftIO $ print [answer :: Int32 | Just (Only answer) <- answers]
      )
    & pqThen (define teardown)
:}
[4,6,8]
-}
forPrepared
  :: ( MonadPQ db pq
     , GenericParams db params x xs
     , GenericRow row y ys
     , Traversable list )
  => list x
  -> Manipulation '[] db params row
  -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto`,
  -- `Squeal.PostgreSQL.Manipulation.Update.update`,
  -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom`, and friends
  -> pq (list (Result y))
forPrepared = flip traversePrepared

{- |
`traversePrepared_` runs a returning-free
`Squeal.PostgreSQL.Manipulation.Manipulation` on a `Foldable`
container by first preparing the statement, then running the prepared
statement on each element.

>>> type Column = 'NoDef :=> 'NotNull 'PGint4
>>> type Columns = '["col1" ::: Column, "col2" ::: Column]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> type DB = Public Schema
>>> import Data.Int(Int32)
>>> :{
let
  insertion :: Manipulation_ DB (Int32, Int32) ()
  insertion = insertInto_ #tab $ Values_ $
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
    & pqThen (traversePrepared_ insertion [(2::Int32,2::Int32),(3,3),(4,4)])
    & pqThen (define teardown)
:}
-}
traversePrepared_
  :: ( MonadPQ db pq
     , GenericParams db params x xs
     , Foldable list )
  => Manipulation '[] db params '[]
  -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto_`,
  -- `Squeal.PostgreSQL.Manipulation.Update.update_`,
  -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom_`, and friends
  -> list x -> pq ()
traversePrepared_ = executePrepared_ . manipulation

{- |
`forPrepared_` is a flipped `traversePrepared_`

>>> type Column = 'NoDef :=> 'NotNull 'PGint4
>>> type Columns = '["col1" ::: Column, "col2" ::: Column]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> type DB = Public Schema
>>> import Data.Int(Int32)
>>> :{
let
  insertion :: Manipulation_ DB (Int32, Int32) ()
  insertion = insertInto_ #tab $ Values_ $
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
    & pqThen (forPrepared_ [(2::Int32,2::Int32),(3,3),(4,4)] insertion)
    & pqThen (define teardown)
:}
-}
forPrepared_
  :: ( MonadPQ db pq
     , GenericParams db params x xs
     , Foldable list )
  => list x
  -> Manipulation '[] db params '[]
  -- ^ `Squeal.PostgreSQL.Manipulation.Insert.insertInto_`,
  -- `Squeal.PostgreSQL.Manipulation.Update.update_`,
  -- or `Squeal.PostgreSQL.Manipulation.Delete.deleteFrom_`, and friends
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

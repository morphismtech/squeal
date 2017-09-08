# squeal

![squeal-icon](http://www.emoticonswallpapers.com/emotion/cute-big-pig/cute-pig-smiley-046.gif)

[![CircleCI](https://circleci.com/gh/echatav/squeal.svg?style=svg&circle-token=a699a654ef50db2c3744fb039cf2087c484d1226)](https://circleci.com/gh/echatav/squeal)

Main repository for the squeal database library.

## installation

The easiest way to install `squeal-postgresql` is to use the haskell [stack](https://docs.haskellstack.org/en/stable/README/) tool.

Use `resolver: nightly-2017-08-25`. We're stuck with nightlies until an lts supports GHC-8.2. Do a `stack upgrade && stack update` to make sure you get GHC-8.2 and the nightly. Then add `squeal-postgresql-0.1.1.2` as an extra dependency in your `stack.yaml` and you should be good to go.

## usage

Squeal is a deep embedding of PostgreSQL in Haskell. Let's see an example!

First, we need some language extensions because Squeal uses modern GHC features.

```haskell
{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , TypeOperators
#-}
```

Here comes the Main module and imports.

```haskell
module Main (main) where

import Control.Monad.Base (liftBase)
import Data.Int (Int32)
import Data.Text (Text)

import Squeal.PostgreSQL
```

We'll use generics to easily convert between Haskell and PostgreSQL values.

```haskell
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
```

The first step is to define the schema of our database. This is where we use DataKinds and TypeOperators. The schema consists of a type-level list of tables, a `:::` pairing of a type level string or `Symbol` and a list a columns, itself a `:::` pairing of a `Symbol` and a `ColumnType`. The `ColumnType` describes the PostgreSQL type of the column as well as whether or not it may contain `NULL` and whether or not inserts and updates can use a `DEFAULT`. For our schema, we'll define two tables, a users table and an emails table.

```haskell
type Schema =
  '[ "users"  :::
       '[ "id"   ::: 'Optional ('NotNull 'PGint4)
        , "name" ::: 'Required ('NotNull 'PGtext)
        ]
   , "emails" :::
       '[ "id"      ::: 'Optional ('NotNull 'PGint4)
        , "user_id" ::: 'Required ('NotNull 'PGint4)
        , "email"   ::: 'Required ('Null 'PGtext)
        ]
   ]
```

Next, we'll write Definitions to set up and tear down the schema. In Squeal, a Definition is a `createTable`, `alterTable` or `dropTable` command and has two type parameters, corresponding to the schema before being run and the schema after. We can compose definitions using `>>>`. Here and in the rest of our commands we make use of overloaded labels to refer to named tables and columns in our schema.

```haskell
setup :: Definition '[] Schema
setup = 
  createTable #users
    ( serial `As` #id :*
      (text & notNull) `As` #name :* Nil )
    [ primaryKey (Column #id :* Nil) ]
  >>>
  createTable #emails
    ( serial `As` #id :*
      (int & notNull) `As` #user_id :*
      text `As` #email :* Nil )
    [ primaryKey (Column #id :* Nil)
    , foreignKey (Column #user_id :* Nil) #users (Column #id :* Nil)
      OnDeleteCascade OnUpdateCascade ]
```

Notice that setup starts with an empty schema `'[]` and produces `Schema`. In our `createTable` commands we included `TableConstraint`s to define primary and foreign keys, making them somewhat complex. Our tear down `Definition` is simpler.

```haskell
teardown :: Definition Schema '[]
teardown = dropTable #emails >>> dropTable #users
```

Next, we'll write `Manipulation`s to insert data into our two tables. A `Manipulation` is an `insertInto`, `update` or `deleteFrom` command and has three type parameters, the schema it refers to, a list of parameters it can take as input, and a list of columns it produces as output. When we insert into the users table, we will need a parameter for the name field but not for the id field. Since it's optional, we can use a default value. However, since the emails table refers to the users table, we will need to retrieve the user id that the insert generates and insert it into the emails table. Take a careful look at the type and definition of both of our inserts.

```haskell
insertUser :: Manipulation Schema
  '[ 'Required ('NotNull 'PGtext)]
  '[ "fromOnly" ::: 'Required ('NotNull 'PGint4) ]
insertUser = insertInto #users
  ( Values (def `As` #id :* param @1 `As` #name :* Nil) [] )
  OnConflictDoNothing (Returning (#id `As` #fromOnly :* Nil))

insertEmail :: Manipulation Schema
  '[ 'Required ('NotNull 'PGint4), 'Required ('Null 'PGtext)] '[]
insertEmail = insertInto #emails ( Values
  ( def `As` #id :*
    param @1 `As` #user_id :*
    param @2 `As` #email :* Nil) [] )
  OnConflictDoNothing (Returning Nil)
```

Next we write a `Query` to retrieve users from the database. We're not interested in the ids here, just the usernames and email addresses. We need to use an inner join to get the right result. A `Query` is like a `Manipulation` with the same kind of type parameters.

```haskell
getUsers :: Query Schema '[]
  '[ "userName" ::: 'Required ('NotNull 'PGtext)
   , "userEmail" ::: 'Required ('Null 'PGtext) ]
getUsers = select
  (#u ! #name `As` #userName :* #e ! #email `As` #userEmail :* Nil)
  ( from (Table (#users `As` #u)
    & InnerJoin (Table (#emails `As` #e))
      (#u ! #id .== #e ! #user_id)) )
```

Now that we've defined the SQL side of things, we'll need a Haskell type for users. We give the type `Generic` and `HasDatatypeInfo` instances so that we can decode the rows we receive when we run getUsers. Notice that the record fields of the `User` type match the column names of `getUsers`.

```haskell
data User = User { userName :: Text, userEmail :: Maybe Text }
  deriving (Show, GHC.Generic)
instance SOP.Generic User
instance SOP.HasDatatypeInfo User
```

Let's also create some users to add to the database.

```haskell
users :: [User]
users = 
  [ User "Alice" (Just "alice@gmail.com")
  , User "Bob" Nothing
  , User "Carole" (Just "carole@hotmail.com")
  ]
```

Now we can put together all the pieces into a program. The program connects to the database, sets up the schema, inserts the user data (using prepared statements as an optimization), queries the user data and prints it out and finally closes the connection. We can thread the changing schema information through by using the indexed `PQ` monad transformer and when the schema doesn't change we can use `Monad` and `MonadPQ` functionality.

```haskell
main :: IO ()
main = void $
  withConnection "host=localhost port=5432 dbname=exampledb" . runPQ $
    define setup
    & pqThen session
    & thenDefine teardown
  where
    session = do
      idResults <- traversePrepared insertUser (Only . userName <$> users)
      ids <- traverse (fmap fromOnly . getRow (RowNumber 0)) idResults
      traversePrepared_ insertEmail (zip (ids :: [Int32]) (userEmail <$> users))
      usersResult <- runQuery getUsers
      usersRows <- getRows usersResult
      liftBase $ print (usersRows :: [User])
```

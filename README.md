# squeal

![squeal-icon](http://www.emoticonswallpapers.com/emotion/cute-big-pig/cute-pig-smiley-046.gif)

[![CircleCI](https://circleci.com/gh/echatav/squeal.svg?style=svg&circle-token=a699a654ef50db2c3744fb039cf2087c484d1226)](https://circleci.com/gh/morphismtech/squeal)

[Github](https://github.com/morphismtech/squeal)

[Hackage](https://hackage.haskell.org/package/squeal-postgresql)

[Stackage](https://www.stackage.org/package/squeal-postgresql)

[YouTube](https://www.youtube.com/watch?v=rWfEQfAaNc4)

## introduction

Squeal is a deep embedding of SQL into Haskell. By "deep embedding", I am abusing the
term somewhat. What I mean is that Squeal embeds both SQL terms and SQL types
into Haskell at the term and type levels respectively. This leads to a very high level
of type-safety in Squeal.

Squeal embeds not just the structured query language of SQL but also the
data manipulation language and the data definition language; that's `SELECT`,
`INSERT`, `UPDATE`, `DELETE`, `WITH`, `CREATE`, `DROP`, and `ALTER` commands.

Squeal expressions closely match their corresponding SQL expressions so that
the SQL they actually generate is completely predictable. They are also highly
composable and cover a large portion of SQL.

## features

* generic encoding of Haskell tuples and records into query parameters
  and generic decoding of query results into Haskell records
  using [`generics-sop`](https://hackage.haskell.org/package/generics-sop)
* access to SQL alias system using the `OverloadedLabels` extension
* type-safe `NULL` and `DEFAULT`
* type-safe SQL constraints `CHECK`, `UNIQUE`, `PRIMARY KEY` and `FOREIGN KEY`
* type-safe aggregation
* escape hatches for writing raw SQL
* [`mtl`](https://hackage.haskell.org/package/mtl) compatible monad transformer
  for executing as well as preparing queries and manipulations
  and [Atkey](https://bentnib.org/paramnotions-jfp.pdf) indexed monad transformer
  for executing definitions.
* linear, invertible migrations
* connection pools
* transactions

## installation

`stack install squeal-postgresql`

## testing

start postgres on localhost port `5432` and create a database named `exampledb`.

`stack test`

## usage

Let's see an example!

First, we need some language extensions because Squeal uses modern GHC
features.

```haskell
>>> :set -XDataKinds -XDeriveGeneric -XOverloadedLabels
>>> :set -XOverloadedStrings -XTypeApplications -XTypeOperators
```

We'll need some imports.

```haskell
>>> import Control.Monad (void)
>>> import Control.Monad.Base (liftBase)
>>> import Data.Int (Int32)
>>> import Data.Text (Text)
>>> import Squeal.PostgreSQL
```

We'll use generics to easily convert between Haskell and PostgreSQL values.

```haskell
>>> import qualified Generics.SOP as SOP
>>> import qualified GHC.Generics as GHC
```

The first step is to define the schema of our database. This is where
we use `DataKinds` and `TypeOperators`.

```haskell
>>> :{
type Schema =
  '[ "users" :::
       '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
       '[ "id"   :::   'Def :=> 'NotNull 'PGint4
        , "name" ::: 'NoDef :=> 'NotNull 'PGtext
        ]
   , "emails" :::
       '[ "pk_emails"  ::: 'PrimaryKey '["id"]
        , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
        ] :=>
       '[ "id"      :::   'Def :=> 'NotNull 'PGint4
        , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
        , "email"   ::: 'NoDef :=>    'Null 'PGtext
        ]
   ]
:}
```

Notice the use of type operators. `:::` is used
to pair an alias `Symbol` with either a `TableType` or a `ColumnType`.
`:=>` is used to pair a `TableConstraint`s with a `ColumnsType`,
yielding a `TableType`, or to pair a `ColumnConstraint` with a `NullityType`,
yielding a `ColumnType`.

Next, we'll write `Definition`s to set up and tear down the schema. In
Squeal, a `Definition` is a `createTable`, `alterTable` or `dropTable`
command and has two type parameters, corresponding to the schema
before being run and the schema after. We can compose definitions using
`>>>`. Here and in the rest of our commands we make use of overloaded
labels to refer to named tables and columns in our schema.

```haskell
>>> :{
let
  setup :: Definition '[] Schema
  setup = 
   createTable #users
     ( serial `As` #id :*
       (text & hasNotNull) `As` #name :* Nil )
     ( primaryKey #id `As` #pk_users :* Nil ) >>>
   createTable #emails
     ( serial `As` #id :*
       (int & hasNotNull) `As` #user_id :*
       (text & hasNull) `As` #email :* Nil )
     ( primaryKey #id `As` #pk_emails :*
       foreignKey #user_id #users #id
         OnDeleteCascade OnUpdateCascade `As` #fk_user_id :* Nil )
:}
```

We can easily see the generated SQL is unsuprising looking.

```haskell
>>> renderDefinition setup
"CREATE TABLE users (id serial, name text NOT NULL, CONSTRAINT pk_users PRIMARY KEY (id)); CREATE TABLE emails (id serial, user_id int NOT NULL, email text, CONSTRAINT pk_emails PRIMARY KEY (id), CONSTRAINT fk_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE ON UPDATE CASCADE);"
```

Notice that `setup` starts with an empty schema `'[]` and produces `Schema`.
In our `createTable` commands we included `TableConstraint`s to define
primary and foreign keys, making them somewhat complex. Our tear down
`Definition` is simpler.

```haskell
>>> :{
let
  teardown :: Definition Schema '[]
  teardown = dropTable #emails >>> dropTable #users
:}
>>> renderDefinition teardown
"DROP TABLE emails; DROP TABLE users;"
```

Next, we'll write `Manipulation`s to insert data into our two tables.
A `Manipulation` is an `insertRow` (or other inserts), `update`
or `deleteFrom` command and
has three type parameters, the schema it refers to, a list of parameters
it can take as input, and a list of columns it produces as output. When
we insert into the users table, we will need a parameter for the `name`
field but not for the `id` field. Since it's optional, we can use a default
value. However, since the emails table refers to the users table, we will
need to retrieve the user id that the insert generates and insert it into
the emails table. Take a careful look at the type and definition of both
of our inserts.

```haskell
>>> :{
let
  insertUser :: Manipulation Schema '[ 'NotNull 'PGtext ] '[ "fromOnly" ::: 'NotNull 'PGint4 ]
  insertUser = insertRow #users
    (Default `As` #id :* Set (param @1) `As` #name :* Nil)
    OnConflictDoNothing (Returning (#id `As` #fromOnly :* Nil))
:}
>>> :{
let
  insertEmail :: Manipulation Schema '[ 'NotNull 'PGint4, 'Null 'PGtext] '[]
  insertEmail = insertRow #emails
    ( Default `As` #id :*
      Set (param @1) `As` #user_id :*
      Set (param @2) `As` #email :* Nil )
    OnConflictDoNothing (Returning Nil)
:}
>>> renderManipulation insertUser
"INSERT INTO users (id, name) VALUES (DEFAULT, ($1 :: text)) ON CONFLICT DO NOTHING RETURNING id AS fromOnly;"
>>> renderManipulation insertEmail
"INSERT INTO emails (id, user_id, email) VALUES (DEFAULT, ($1 :: int4), ($2 :: text)) ON CONFLICT DO NOTHING;"
```

Next we write a `Query` to retrieve users from the database. We're not
interested in the ids here, just the usernames and email addresses. We
need to use an inner join to get the right result. A `Query` is like a
`Manipulation` with the same kind of type parameters.

```haskell
>>> :{
let
  getUsers :: Query Schema '[]
    '[ "userName"  ::: 'NotNull 'PGtext
     , "userEmail" :::    'Null 'PGtext ]
  getUsers = select
    (#u ! #name `As` #userName :* #e ! #email `As` #userEmail :* Nil)
    ( from (table (#users `As` #u)
      & innerJoin (table (#emails `As` #e))
        (#u ! #id .== #e ! #user_id)) )
:}
>>> renderQuery getUsers
"SELECT u.name AS userName, e.email AS userEmail FROM users AS u INNER JOIN emails AS e ON (u.id = e.user_id)"
```

Now that we've defined the SQL side of things, we'll need a Haskell type
for users. We give the type `Generics.SOP.Generic` and
`Generics.SOP.HasDatatypeInfo` instances so that we can decode the rows
we receive when we run `getUsers`. Notice that the record fields of the
`User` type match the column names of `getUsers`.

```haskell
>>> data User = User { userName :: Text, userEmail :: Maybe Text } deriving (Show, GHC.Generic)
>>> instance SOP.Generic User
>>> instance SOP.HasDatatypeInfo User
```

Let's also create some users to add to the database.

```haskell
>>> :{
let
  users :: [User]
  users = 
    [ User "Alice" (Just "alice@gmail.com")
    , User "Bob" Nothing
    , User "Carole" (Just "carole@hotmail.com")
    ]
:}
```

Now we can put together all the pieces into a program. The program
connects to the database, sets up the schema, inserts the user data
(using prepared statements as an optimization), queries the user
data and prints it out and finally closes the connection. We can thread
the changing schema information through by using the indexed `PQ` monad
transformer and when the schema doesn't change we can use `Monad` and
`MonadPQ` functionality.

```haskell
>>> :{
let
  session :: PQ Schema Schema IO ()
  session = do
    idResults <- traversePrepared insertUser (Only . userName <$> users)
    ids <- traverse (fmap fromOnly . getRow 0) idResults
    traversePrepared_ insertEmail (zip (ids :: [Int32]) (userEmail <$> users))
    usersResult <- runQuery getUsers
    usersRows <- getRows usersResult
    liftBase $ print (usersRows :: [User])
:}
>>> :{
void . withConnection "host=localhost port=5432 dbname=exampledb" $
  define setup
  & pqThen session
  & pqThen (define teardown)
:}
[User {userName = "Alice", userEmail = Just "alice@gmail.com"},User {userName = "Bob", userEmail = Nothing},User {userName = "Carole", userEmail = Just "carole@hotmail.com"}]
```

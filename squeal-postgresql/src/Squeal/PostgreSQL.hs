{-|
Module: Squeal.PostgreSQL
Description: Squeel export module
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal is a deep embedding of [PostgreSQL](https://www.postgresql.org) in Haskell.
Let's see an example!

First, we need some language extensions because Squeal uses modern GHC
features.

>>> :set -XDataKinds -XDeriveGeneric -XOverloadedLabels
>>> :set -XOverloadedStrings -XTypeApplications -XTypeOperators
 
We'll need some imports.

>>> import Control.Monad (void)
>>> import Control.Monad.Base (liftBase)
>>> import Data.Int (Int32)
>>> import Data.Text (Text)
>>> import Squeal.PostgreSQL
>>> import Squeal.PostgreSQL.Render

We'll use generics to easily convert between Haskell and PostgreSQL values.

>>> import qualified Generics.SOP as SOP
>>> import qualified GHC.Generics as GHC

The first step is to define the schema of our database. This is where
we use @DataKinds@ and @TypeOperators@.

>>> :{
type Schema =
  '[ "users" ::: 'Table (
      '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
      '[ "id"   :::   'Def :=> 'NotNull 'PGint4
       , "name" ::: 'NoDef :=> 'NotNull 'PGtext
       ])
  , "emails" ::: 'Table (
      '[ "pk_emails"  ::: 'PrimaryKey '["id"]
       , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
       ] :=>
      '[ "id"      :::   'Def :=> 'NotNull 'PGint4
       , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
       , "email"   ::: 'NoDef :=>    'Null 'PGtext
       ])
  ]
:}

Notice the use of type operators.

`:::` is used to pair an alias `GHC.TypeLits.Symbol` with a `SchemumType`,
a `TableConstraint` or a `ColumnType`. It is intended to connote Haskell's @::@
operator.

`:=>` is used to pair `TableConstraints` with a `ColumnsType`,
yielding a `TableType`, or to pair a `ColumnConstraint` with a `NullityType`,
yielding a `ColumnType`. It is intended to connote Haskell's @=>@ operator

Next, we'll write `Definition`s to set up and tear down the schema. In
Squeal, a `Definition` like `createTable`, `alterTable` or `dropTable` 
has two type parameters, corresponding to the schema
before being run and the schema after. We can compose definitions using `>>>`.
Here and in the rest of our commands we make use of overloaded
labels to refer to named tables and columns in our schema.

>>> :{
let
  setup :: Definition '[] Schema
  setup = 
    createTable #users
      ( serial `as` #id :*
        (text & notNullable) `as` #name )
      ( primaryKey #id `as` #pk_users ) >>>
    createTable #emails
      ( serial `as` #id :*
        (int & notNullable) `as` #user_id :*
        (text & nullable) `as` #email )
      ( primaryKey #id `as` #pk_emails :*
        foreignKey #user_id #users #id
          OnDeleteCascade OnUpdateCascade `as` #fk_user_id )
:}

We can easily see the generated SQL is unsurprising looking.

>>> printSQL setup
CREATE TABLE "users" ("id" serial, "name" text NOT NULL, CONSTRAINT "pk_users" PRIMARY KEY ("id"));
CREATE TABLE "emails" ("id" serial, "user_id" int NOT NULL, "email" text NULL, CONSTRAINT "pk_emails" PRIMARY KEY ("id"), CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "users" ("id") ON DELETE CASCADE ON UPDATE CASCADE);

Notice that @setup@ starts with an empty schema @'[]@ and produces @Schema@.
In our `createTable` commands we included `TableConstraint`s to define
primary and foreign keys, making them somewhat complex. Our @teardown@
`Definition` is simpler.

>>> :{
let
  teardown :: Definition Schema '[]
  teardown = dropTable #emails >>> dropTable #users
:}

>>> printSQL teardown
DROP TABLE "emails";
DROP TABLE "users";

Next, we'll write `Manipulation`s to insert data into our two tables.
A `Manipulation` like `insertRow`, `update` or `deleteFrom`
has three type parameters, the schema it refers to, a list of parameters
it can take as input, and a list of columns it produces as output. When
we insert into the users table, we will need a parameter for the @name@
field but not for the @id@ field. Since it's serial, we can use a default
value. However, since the emails table refers to the users table, we will
need to retrieve the user id that the insert generates and insert it into
the emails table. Take a careful look at the type and definition of both
of our inserts.

>>> :{
let
  insertUser :: Manipulation Schema '[ 'NotNull 'PGtext ] '[ "fromOnly" ::: 'NotNull 'PGint4 ]
  insertUser = insertRow #users
    (Default `as` #id :* Set (param @1) `as` #name)
    OnConflictDoNothing (Returning (#id `as` #fromOnly))
:}

>>> :{
let
  insertEmail :: Manipulation Schema '[ 'NotNull 'PGint4, 'Null 'PGtext] '[]
  insertEmail = insertRow #emails
    ( Default `as` #id :*
      Set (param @1) `as` #user_id :*
      Set (param @2) `as` #email )
    OnConflictDoNothing (Returning Nil)
:}

>>> printSQL insertUser
INSERT INTO "users" ("id", "name") VALUES (DEFAULT, ($1 :: text)) ON CONFLICT DO NOTHING RETURNING "id" AS "fromOnly"
>>> printSQL insertEmail
INSERT INTO "emails" ("id", "user_id", "email") VALUES (DEFAULT, ($1 :: int4), ($2 :: text)) ON CONFLICT DO NOTHING

Next we write a `Query` to retrieve users from the database. We're not
interested in the ids here, just the usernames and email addresses. We
need to use an inner join to get the right result. A `Query` is like a
`Manipulation` with the same kind of type parameters.

>>> :{
let
  getUsers :: Query Schema '[]
    '[ "userName"  ::: 'NotNull 'PGtext
     , "userEmail" :::    'Null 'PGtext ]
  getUsers = select
    (#u ! #name `as` #userName :* #e ! #email `as` #userEmail)
    ( from (table (#users `as` #u)
      & innerJoin (table (#emails `as` #e))
        (#u ! #id .== #e ! #user_id)) )
:}

>>> printSQL getUsers
SELECT "u"."name" AS "userName", "e"."email" AS "userEmail" FROM "users" AS "u" INNER JOIN "emails" AS "e" ON ("u"."id" = "e"."user_id")

Now that we've defined the SQL side of things, we'll need a Haskell type
for users. We give the type `Generics.SOP.Generic` and
`Generics.SOP.HasDatatypeInfo` instances so that we can decode the rows
we receive when we run @getUsers@. Notice that the record fields of the
@User@ type match the column names of @getUsers@.
 
>>> data User = User { userName :: Text, userEmail :: Maybe Text } deriving (Show, GHC.Generic)
>>> instance SOP.Generic User
>>> instance SOP.HasDatatypeInfo User

Let's also create some users to add to the database.

>>> :{
let
  users :: [User]
  users = 
    [ User "Alice" (Just "alice@gmail.com")
    , User "Bob" Nothing
    , User "Carole" (Just "carole@hotmail.com")
    ]
:}

Now we can put together all the pieces into a program. The program
connects to the database, sets up the schema, inserts the user data
(using prepared statements as an optimization), queries the user
data and prints it out and finally closes the connection. We can thread
the changing schema information through by using the indexed `PQ` monad
transformer and when the schema doesn't change we can use `Monad` and
`MonadPQ` functionality.

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
in
  void . withConnection "host=localhost port=5432 dbname=exampledb" $
    define setup
    & pqThen session
    & pqThen (define teardown)
:}
[User {userName = "Alice", userEmail = Just "alice@gmail.com"},User {userName = "Bob", userEmail = Nothing},User {userName = "Carole", userEmail = Just "carole@hotmail.com"}]
-}
module Squeal.PostgreSQL
  ( module Squeal.PostgreSQL.Binary
  , module Squeal.PostgreSQL.Definition
  , module Squeal.PostgreSQL.Expression
  , module Squeal.PostgreSQL.Manipulation
  , module Squeal.PostgreSQL.PQ
  , module Squeal.PostgreSQL.Query
  , module Squeal.PostgreSQL.Schema
  , module Squeal.PostgreSQL.Transaction
  ) where

import Squeal.PostgreSQL.Binary
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema
import Squeal.PostgreSQL.Transaction

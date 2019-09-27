{-|
Module: Squeal.PostgreSQL
Description: Squeal export module
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal is a deep embedding of [PostgreSQL](https://www.postgresql.org) in Haskell.
Let's see an example!

First, we need some language extensions because Squeal uses modern GHC
features.

>>> :set -XDataKinds -XDeriveGeneric -XOverloadedLabels -XFlexibleContexts
>>> :set -XOverloadedStrings -XTypeApplications -XTypeOperators -XGADTs

We'll need some imports.

>>> import Control.Monad.IO.Class (liftIO)
>>> import Data.Int (Int32)
>>> import Data.Text (Text)
>>> import Squeal.PostgreSQL

We'll use generics to easily convert between Haskell and PostgreSQL values.

>>> import qualified Generics.SOP as SOP
>>> import qualified GHC.Generics as GHC

The first step is to define the schema of our database. This is where
we use @DataKinds@ and @TypeOperators@.

>>> :{
type UsersColumns =
  '[ "id"   :::   'Def :=> 'NotNull 'PGint4
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext ]
type UsersConstraints = '[ "pk_users" ::: 'PrimaryKey '["id"] ]
type EmailsColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
   , "email" ::: 'NoDef :=> 'Null 'PGtext ]
type EmailsConstraints =
  '[ "pk_emails"  ::: 'PrimaryKey '["id"]
   , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"] ]
type Schema =
  '[ "users" ::: 'Table (UsersConstraints :=> UsersColumns)
   , "emails" ::: 'Table (EmailsConstraints :=> EmailsColumns) ]
type Schemas = Public Schema
:}

Notice the use of type operators.

`:::` is used to pair an alias `GHC.TypeLits.Symbol` with a `SchemasType`, a `SchemumType`,
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
  setup :: Definition (Public '[]) Schemas
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

Notice that @setup@ starts with an empty public schema @(Public '[])@ and produces @Schemas@.
In our `createTable` commands we included `TableConstraint`s to define
primary and foreign keys, making them somewhat complex. Our @teardown@
`Definition` is simpler.

>>> :{
let
  teardown :: Definition Schemas (Public '[])
  teardown = dropTable #emails >>> dropTable #users
:}

>>> printSQL teardown
DROP TABLE "emails";
DROP TABLE "users";

We'll need a Haskell type for @User@s. We give the type `Generics.SOP.Generic` and
`Generics.SOP.HasDatatypeInfo` instances so that we can encode and decode @User@s.

>>> data User = User { userName :: Text, userEmail :: Maybe Text } deriving (Show, GHC.Generic)
>>> instance SOP.Generic User
>>> instance SOP.HasDatatypeInfo User

Next, we'll write `Manipulation_`s to insert @User@s into our two tables.
A `Manipulation_` like `insertInto`, `update` or `deleteFrom`
has three type parameters, the schemas it refers to, input parameters
and an output row. When
we insert into the users table, we will need a parameter for the @name@
field but not for the @id@ field. Since it's serial, we can use a default
value. However, since the emails table refers to the users table, we will
need to retrieve the user id that the insert generates and insert it into
the emails table. We can do this in a single `Manipulation_` by using a
`with` statement.

>>> :{
let
  insertUser :: Manipulation_ Schemas User ()
  insertUser = with (u `as` #u) e
    where
      u = insertInto #users
        (Values_ (Default `as` #id :* Set (param @1) `as` #name))
        OnConflictDoRaise (Returning_ (#id :* param @2 `as` #email))
      e = insertInto_ #emails $ Select
        (Default `as` #id :* Set (#u ! #id) `as` #user_id :* Set (#u ! #email) `as` #email)
        (from (common #u))
:}

>>> printSQL insertUser
WITH "u" AS (INSERT INTO "users" ("id", "name") VALUES (DEFAULT, ($1 :: text)) RETURNING "id" AS "id", ($2 :: text) AS "email") INSERT INTO "emails" ("user_id", "email") SELECT "u"."id", "u"."email" FROM "u" AS "u"

Next we write a `Query_` to retrieve users from the database. We're not
interested in the ids here, just the usernames and email addresses. We
need to use an `innerJoin` to get the right result. A `Query_` is like a
`Manipulation_` with the same kind of type parameters.

>>> :{
let
  getUsers :: Query_ Schemas () User
  getUsers = select_
    (#u ! #name `as` #userName :* #e ! #email `as` #userEmail)
    ( from (table (#users `as` #u)
      & innerJoin (table (#emails `as` #e))
        (#u ! #id .== #e ! #user_id)) )
:}

>>> printSQL getUsers
SELECT "u"."name" AS "userName", "e"."email" AS "userEmail" FROM "users" AS "u" INNER JOIN "emails" AS "e" ON ("u"."id" = "e"."user_id")

Let's create some users to add to the database.

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
  session :: PQ Schemas Schemas IO ()
  session = do
    _ <- traversePrepared_ insertUser users
    usersResult <- runQuery getUsers
    usersRows <- getRows usersResult
    liftIO $ print (usersRows :: [User])
in
  withConnection "host=localhost port=5432 dbname=exampledb" $
    define setup
    & pqThen session
    & pqThen (define teardown)
:}
[User {userName = "Alice", userEmail = Just "alice@gmail.com"},User {userName = "Bob", userEmail = Nothing},User {userName = "Carole", userEmail = Just "carole@hotmail.com"}]
-}
module Squeal.PostgreSQL (module X, RenderSQL(..), printSQL) where

import Squeal.PostgreSQL.Alias as X
import Squeal.PostgreSQL.Binary as X
import Squeal.PostgreSQL.Definition as X
import Squeal.PostgreSQL.Definition.Function as X
import Squeal.PostgreSQL.Definition.Index as X
import Squeal.PostgreSQL.Definition.Schema as X
import Squeal.PostgreSQL.Definition.Table as X
import Squeal.PostgreSQL.Definition.Table.Column as X
import Squeal.PostgreSQL.Definition.Table.Constraint as X
import Squeal.PostgreSQL.Definition.Type as X
import Squeal.PostgreSQL.Definition.View as X
import Squeal.PostgreSQL.Expression as X
import Squeal.PostgreSQL.Expression.Aggregate as X
import Squeal.PostgreSQL.Expression.Collection as X
import Squeal.PostgreSQL.Expression.Comparison as X
import Squeal.PostgreSQL.Expression.Json as X
import Squeal.PostgreSQL.Expression.Literal as X
import Squeal.PostgreSQL.Expression.Logic as X
import Squeal.PostgreSQL.Expression.Math as X
import Squeal.PostgreSQL.Expression.Null as X
import Squeal.PostgreSQL.Expression.Parameter as X
import Squeal.PostgreSQL.Expression.Range as X
import Squeal.PostgreSQL.Expression.Set as X
import Squeal.PostgreSQL.Expression.Sort as X
import Squeal.PostgreSQL.Expression.Subquery as X
import Squeal.PostgreSQL.Expression.Text as X
import Squeal.PostgreSQL.Expression.TextSearch as X
import Squeal.PostgreSQL.Expression.Time as X
import Squeal.PostgreSQL.Expression.Type as X
import Squeal.PostgreSQL.Expression.Window as X
import Squeal.PostgreSQL.List as X
import Squeal.PostgreSQL.Manipulation as X
import Squeal.PostgreSQL.Migration as X
import Squeal.PostgreSQL.PG as X
import Squeal.PostgreSQL.Pool as X
import Squeal.PostgreSQL.PQ as X
import Squeal.PostgreSQL.Query as X
import Squeal.PostgreSQL.Render (RenderSQL(..), printSQL)
import Squeal.PostgreSQL.Schema as X
import Squeal.PostgreSQL.Transaction as X

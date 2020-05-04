# Squeal

_A bridge between SQL and Haskell_

by [@Raveline](https://github.com/raveline)

---

# Rationale

Using `postgresql-simple` you have a typical "Trial, error, despair" workflow:

- Write query manually or semi-automatically

- Write haskell code using the query

- Pray that the query syntax is correct

- Pray that it returns what you want

- Realize the inefficiency of prayer when it comes to SQL

- Iterate till it works

---

![Can't take this anymore](http://gif.eraveline.eu/static/img/0x16e.gif)

---

# Use your best friend: GHC

Squeal provides several eDSL to make your SQL typesafe:

- Type level eDSL to express schema;

- Value level eDSL to manipulate schema (plus migration, yeehaw !);

- Type level eDSL to express queries;

- Value level eDSL to perform queries

However, it's not an ORM. There's no caching, lazy loading - you retain control over your memory. Also, joins - and mostly aggregation after joins - have to be handled manually.

---

# Part I. Schema & migration

---

# The Schema

- The schema will be used to validate _everything_: migration, queries, etc.

- A schema is mostly a collection of tables.

- In this presentation, we will create a very basic database modeling a parliament. We will store _members of parliament_ and _parliamentary groups_.

- A simple Schema to represent a Parliament could be:

```haskell
type Schema = '[ "mp" ::: 'Table MemberOfParliament
               , "groupp" ::: 'Table ParliamentGroup ]
```

---

> _note_ We need the `DataKinds` extension to be able to express heterogenous lists containing
> specific types like this one.

> You can perfectly call your table "group" and not "groupp" even though it is a
> keyword in SQL - Squeal queries will be properly escaped.

---

# A small example

```haskell
type ParliamentaryGroup =
    '[ "pk_groupp" ::: 'PrimaryKey '["groupp_id"]]
      :=>
      '[ "groupp_id" ::: 'NoDef  :=> 'NotNull 'PGuuid
       , "name" ::: 'NoDef :=> 'NotNull 'PGtext
      ]
```

- That's a whole simple table defined in one go.

- `:::` lets us define a column or a constraint.

- `:=>` associates the constraints to the column.

- Let's split constraints and column to study the syntax a bit more.

---

> _note_ We are using `:::` and `:=>` to quickly express associations when writing
> our schema. We need the `TypeOperators` extension.]

---

![What does this means ?!](http://gif.eraveline.eu/static/img/0x27e.gif)

---

# Defining a table

```haskell
type MemberOfParliament =
    '[ "pk_mp" ::: 'PrimaryKey '["mp_id"]
    , "fk_mp_groupp" ::: 'ForeignKey '["mp_group"] "public" "groupp" '["groupp_id"]
    ] :=> MpCols
```

- A table is a collection of constraints associated to a collection of columns.

- We defined a table named "mp".

- It has a constraint named "pk_mp", defining its primary key on column "mp_id".

- It has a constraint named "fk_mp_groupp" defining a foreign key on column "mg_group", connected to the column "groupp_id" of table "groupp".

- These constraints will be associated to the columns defined in `MpCols`.

---

# Defining columns

```haskell
type MpCols =
      '[ "mp_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "first_name" ::: 'NoDef  :=> 'NotNull 'PGtext
       , "last_name" ::: 'NoDef :=> 'NotNull 'PGtext
       ]
```

- A listing of column associate, for each element:

    * a name;

    * the mention of an eventual default value;

    * the nullability;

    * the type (obviously).

---

> _note_ GHC is already helping. If I named the "mp_id" column differently,
> GHC would yell because I promised a primary key constraint on a column named `mp_id`, so there must be one.]

---

# Implementating the schema

- We will carry this schema type pretty much everywhere.

- But before we play with this schema, we need to implement it.

---

```haskell
setup :: Definition '[] Schema
setup =
  createTable #groupp
    ( notNullable uuid `as` #groupp_id
    :* notNullable text `as` #name)
    ( primaryKey #groupp_id `as` #pk_groupp )
  >>> createTable #mp
    ( notNullable uuid `as` #mp_id
    :* notNullable text `as` #first_name
    :* notNullable text `as` #last_name
    :* notNullable text `as` #mp_group)
    ( primaryKey #mp_id `as` #pk_mp
    :* foreignKey #mp_group #groupp #groupp_id
        OnDeleteCascade OnUpdateCascade 
        `as` #fk_mp_groupp)
```

---

# Damn, that's verbose

- Yes. But you need verbosity to get type safety.

- On the plus side, it's fairly straightforward.

- You use `:*` to compose the element of the heterogenous list of columns.

- You use `>>>` to compose table creation.

> _note_ The compiler will catch any mistype between Schema and
> definition; wrong nullability, wrong type, wrong name, etc.

---

> _note_ You'll also need `OverloadedLabels`, for naming stuff.
> This is mostly to avoid having to write manual proxies all
> the time and for convenience.

---

# Setting up the schema

- Squeal comes with a very good migration manager, handling _upgrades_ AND
_downgrades_.

- A migration is a simple type:

```haskell
Migration io schema0 schema1
```

- The first parameter is a BaseMonad (typically, IO).

- The second parameter is "the current schema of your DB".

- The last one is "what you will migrate to".

- The `Definition` type used to define upgrade and downgrade functions use the
  same logic (from one schema to the other).

---

# Defining our first migration

- We have the setup bit, we need the teardown:

```haskell
tearDown :: Definition Schema '[]
tearDown = dropTable #mp >>> dropTable #groupp
```

> _note_ GHC will also detect the _proper_ order of what you typed in
> downgrade and upgrade should there be any conflict (with foreign keys).

```haskell
initDB :: Migration IO '[] Schema
initDB =
  Migration { name = "Schema creation"
            , up = void $ define setup
            , down = void $ define tearDown }
```

---

# Simple migrator example

```haskell
main :: IO ()
main = do
  printSQL setup
  void $ withConnection connectionString $
    migrateUp $ single initDB
```

- We print the migration query; you can do this for all queries generated through Squeal.

- We use `migrateUp` to perform the migration.

- You can run several migration at the same time, to run only one use `single`.

---

![That was easy](http://gif.eraveline.eu/static/img/0x47f.gif)

---

# Part II. Insertions

---

# Manipulations & Queries

Besides the specific case of migration, you will mostly perform:

- `Manipulation` : inserting, updating and deleting data.

- `Query` : fetching data.

- Both types are parametric over the same things:

  * A schema

  * Input parameters

  * Output

- Simplified, they look like this:

```haskell
Manipulation schema params columns

Query schema params columns
```


---

# Our Haskell model

```haskell
type Parliament = [Group]

data Group =
  Group { name :: Text
        , members :: [MemberOfParliament] }

data MemberOfParliament =
  MemberOfParliament { firstName :: Text
                     , lastName :: Text }
```

> _note_ We didn't use anything from Squeal.
> The model can be entirely separated from the persistence layer.

---

# Inserting a Parliamentary Group

- A group is very simple: it's a uuid and a name. Let's define our params:

```haskell
type GroupInsertionParams = '[ 'NotNull 'PGuuid
                             , 'NotNull 'PGtext ]
```

- Params are not named, but they are indexed. You just need nullability and type.

---

```haskell
groupInsertion :: Manipulation Schema GroupInsertionParams '[]
groupInsertion =
  insertRow_ #groupp ( Set (param @1) `as` #groupp_id
                     :* Set (param @2) `as` #name )
```

---

- `TypeApplication` lets us use the index of parameters (counting from 1).

- Once again: all this is checked by the compiler. Wrong name, wrong type... you'll get a compile error.

- `insertRow_` is a simplified version of `insertRow`. `insertRow` lets you express a `RETURNING` clause and the expected behaviour in case of conflict.

---

_Inserting a Member of Parliament_
# Doing an INSERT INTO ... SELECT

- We could create a naive query that takes MP uuid, first name, last name and group uuid...

---

- But that's boring. So let's use the `INSERT INTO ... SELECT`.

---

- We will build a query that will return as constants our MP's uuid, first name and last name...

---

- ... and fetch the uuid of a group given the name of the group.

---

- There's a `insertQuery` utility function for that. All we have to do is write the `select` !

```haskell
mpInsertion :: Manipulation Schema MpParams '[]
mpInsertion =
  insertQuery_ #mp selectGroup -- we just have to write selectGroup !
```

---

_Inserting a Member of Parliament_
# The query type

- A query looks just like a `Manipulation`:

```haskell
Query schema params returns
```

- Returns use a special syntax, that demands names, nullability and types:

```haskell
type ExampleReturnType = '[ "some_column" ::: 'NotNull 'PGuuid
                          , "other_column" ::: 'Null 'PGtext ]
```

- It is not the same as the column definition we used in the schema. That one also expects that you specify, for each column, an eventual default:

```haskell
type ExampleSchemaCols =
      '[ "some_column" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "other_column" ::: 'NoDef  :=> 'Null 'PGtext ]
```

---

_Inserting a Member of Parliament_
# Don't rewrite column definitions

- We want our return type to be "all columns from table MP expressed as return type".

- But we cannot reuse our neat `MpCols` alias, since the type do not match as we've just seen.

- Hopefully, there's a neat Type Family that will let you convert any table you defined to the type of the equivalent row: `TableToRow`.

- We finally have the signature for our SELECT query.

```haskell
selectGroup :: Query Schema MpParams (TableToRow MemberOfParliament)
```

---

_Inserting a Member of Parliament_
# Our final Select query

- Our intermediate query looks like this:

```haskell
selectGroup =
  select
    ( param @1 `as` #mp_id
    :* param @2 `as` #first_name
    :* param @3 `as` #last_name
    :* #groupp ! #groupp_id `as` #mp_group
    )
  ( from (table #groupp)
    & where_ ( #groupp ! #name .== param @4 )
  )
```

- The first three columns are constant defined through our params;

- ... we alias the column name using `as`...

- ... and when we need the result from the table we use the `#table ! #column` syntax.

- We'll go back to the "from" block later.

---

# Dealing with the connection context

- Queries are runned in a connection context:

  * When dealing with a single-connection context, use the type `PQ`.

  * When dealing with a pool of connection, use the type `PoolPQ`.

- Or get rid of the context and use `mtl` style, with the typeclass `MonadPQ`.

- We'll use that to demonstrate how to actually run our queries.

---

# Inserting a whole group

```haskell
insertGroup :: (MonadPQ Schema m, MonadBaseControl IO m) => Group -> m ()
insertGroup g@(Group name _) = do
  uuid' <- liftBase nextRandom
  void $ manipulateParams groupInsertion (uuid', name)
  insertMps g
```

- Remember that we defined a group as a name and a list of MPs.

- We are in `MonadBaseControl`, so we cannot use `liftIO`, we need `liftBase`.

- To perform a simple insertion, use `manipulateParams`. It takes into parameter
instances of `ToParam`, but you will typically use tuples or Generic-SOP.

---

# Inserting a bunch of MPs

```haskell
insertMps :: (MonadPQ Schema m, MonadBaseControl IO m) => Group -> m ()
insertMps (Group groupName mps) =
  let tuplify Mp{..} = (, firstName, lastName, groupName) <$> nextRandom
      params = traverse tuplify mps
  in void $ liftBase params >>= traversePrepared mpInsertion
```

- This time, we want to do a `preparedStatement`. We'll use `traversePrepare`
which behaves like `manipulateParams`, but expect a list of `ToParam`
instances.

- We build our tuple manually again, mostly because we want to generate UUID on
the fly and we don't want to have them in our model.

---

# Inserting a whole parliament

```haskell
insertParliament ::
    (MonadPQ Schema m, MonadBaseControl IO m) => Parliament -> m ()
insertParliament = traverse_ insertGroup
```

- That's simple enough !

- If we want to be a bit safer, though, we can wrap this call in a
`transactionally_` function, which will put all that in a transaction.

---

![Very simple](https://media1.tenor.com/images/0188c63209aced59f1583e1ca94e509e/tenor.gif?itemid=3550689)


---

# Part III. Selects

---

# Composable queries

- Building basic queries is easy, and is well documented.

- However, the real interest of a tool like Squeal is in DRYness, and the documentation is still lacking in "how-to" related to composability.

- I'll build an example showing how column selections and from clause can be factorized.

- We want to write two queries:

  * One to get all members of a specific group.

  * One to get all the parliament.

---

# Decomposing a `Query`

- A query typically stars with `select` (variants are available).

- It then takes:

  * A heterogenous list of fields with a scary signature;

  * A virtual table (the from clause and filter clauses), called a `TableExpression`.

- And return fields. Ours will look like this:

```haskell
type GroupRowResult =
  '[ "groupName" ::: 'NotNull 'PGtext
   , "firstName" ::: 'NotNull 'PGtext
   , "lastName" ::: 'NotNull 'PGtext ]
```

---

# The Table Expression

- It necessarily contains a `fromClause` (table, view or subquery, plus optional joins).

- You can add where, groups, "HAVING" clause, order, etc.

- `from` creates a basic `TableExpression` that you extend through various function to add clauses.

- For our queries, we will share a common `FromClause`.

---

# The From Clause

```
FromClause schema params from
```

- We need Schema, input parameters and a `FromType` giving the available fields of the expression.

- In our case, we'll join the table `mp` and the table `groupp`, meaning the `from` will be all fields of these tables.

- We can use `TableToRow` to be more DRY:

```haskell
type BaseParliamentSelection =
  '[ "g" ::: TableToRow GroupCols
   , "m" ::: TableToRow MpCols ]
```

> _note_ We've also put everything with table aliases: "g" and "m".

---

# Writing our from clause

```haskell
baseParliamentTables ::
    FromClause Schema (param :: [NullType]) BaseParliamentSelection
baseParliamentTables =
  table
    (#groupp `as` #g)
    & innerJoin
      (table (#mp `as` #m))
      (#m ! #mp_group .== #g ! #groupp_id)
```

- When picking "from" something, you need to specify the type with a function: `table` for a table, `view` for a view, etc.

- All joins are available. `innerJoin` is the most basic one. It takes the joined table and the joining condition.

- We have to specifiy the type of `param` (current limitation of the lib), even though _any_ params will be compatible with this, so we can plug `where` clauses depending on params should we need to !

---

# Typing the common selection

- Both our querie will need the same fields. This has to be factorized too.

- The scary signature of selection fields:

```haskell
NP (Aliased (Expression schema from grouping params)) cols
```

- `NP` is for heterogenous lists ("n-ary product").

- `from` is the virtual table type, so our `BaseParliamentSelection`.

- `grouping` is there to make a distinction between aggregated / unaggregated queries.

- And finally, cols is the return type.

---

# Our common selection

```haskell
groupSelection ::
    NP (Aliased (Expression Schema BaseParliamentSelection 'Ungrouped param))
        GroupRowResult
groupSelection =
       #g ! #name `as` #groupName
    :* #m ! #first_name `as` #firstName
    :* #m ! #last_name `as` #lastName
```

- I hope you like big signatures.

- But it's what let GHC checks that all alias and columns are available.

- And that you're returning what you really intend to return.

---

# Putting it all together

```haskell
selectParliament :: Query Schema '[] GroupRowResult
selectParliament =
  select groupSelection (from baseParliamentTables)
```

```haskell
selectGroupMembers :: Query Schema '[ 'NotNull 'PGtext] GroupRowResult
selectGroupMembers =
  select groupSelection
    (from baseParliamentTables
      & where_ (#g ! #name .== param @1))
```

- That is fairly DRY. And _entirely typesafe_.

---

# Actually fetching the data

- We'll build an intermediary datatype representing our rows.

- We'll make it match our row.

- And we'll add generic-SOP so that we can build them from query results.

```haskell
type GroupRowResult =
  '[ "groupName" ::: 'NotNull 'PGtext
   , "firstName" ::: 'NotNull 'PGtext
   , "lastName" ::: 'NotNull 'PGtext ]
```

```haskell
data GroupRow =
  GroupRow { groupName :: Text
           , firstName :: Text
           , lastName :: Text }
  deriving (Generic)

instance SOP.Generic GroupRow
instance SOP.HasDatatypeInfo GroupRow
```

---

# Aggregate result logic

- We will get tabular, SQL data.

- Our results will be `[GroupRow]`.

- We'll build `Groups` from this:

```haskell
buildGroup :: NE.NonEmpty GroupRow -> Group
buildGroup grs =
  let buildMP (GroupRow _ f l) = Mp f l
      name = groupName . NE.head $ grs
      members = NE.toList (buildMP <$> grs)
  in Group{..}

rowToGroups :: [GroupRow] -> [Group]
rowToGroups =
  let grouped = fmap NE.fromList . L.groupBy ((==) `on` groupName)
  in fmap buildGroup . grouped
```

---

# Finally calling our queries

- Query with params will use `runQueryParams`.

- Query without params will use `runQuery`.

- Result is inside a `MonadPQ` and of type `K` from generic-sop.

- You get your actual result using `getRows`. Exemple:

```haskell
getParliament :: (MonadPQ Schema m, MonadBaseControl IO m) => m [Group]
getParliament = do
  res <- runQuery selectParliament
  rows <- getRows res
  pure $ rowToGroups rows
```

- In real life of course you'll write:

```haskell
getParliament =
    rowToGroups <$> (runQuery selectParliament >>= getRows)
```

---

# Our query with params

```haskell
getGroupMembers ::
    (MonadPQ Schema m, MonadBaseControl IO m) => Text -> m (Maybe Group)
getGroupMembers =
    listToMaybe . rowToGroups <$>
        (runQueryParams selectGroupMembers (Only t) >>= getRows)
```

- And that's it.

---

![Eazy](https://media.tenor.com/images/8fc7c4077efe11b4a3a3b9ae4e643e87/tenor.gif)

---

# Conclusion

---

# Pros & Cons

- Not everything is included: some cool stuff like array_agg and window functions are not available yet.

- But development is _very_ active. `IN` clauses were missing but were added in the 4.0 released recently.

- It's bleeding edge. You need latest LTS to be comfortable and psql >= 9.5.

- It's Postgres-only but I would say that's a feature. Multi-DB tools are even more complex.

- The author & maintainer is <3.

- Typesafe. 'nuff said.

---

# Thank you !

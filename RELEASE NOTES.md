## RELEASE NOTES

### Version 0.5

Version 0.5 makes a number of large changes and additions to Squeal.
I want to thank folks who contributed issues and pull requests;
Raveline, rimmington, ilyakooo0, chshersh, reygoch, and more.

**Multi-schema support**

Previous versions of Squeal only supported definitions in the "public"
schema. Squeal 0.5 adds support for multiple schemas with the new kind

```Haskell
type SchemasType = [(Symbol,SchemaType)]
```

In order to upgrade your queries, manipulations and definitions from
Squeal 0.4, you will have to apply the `Public` type family, which indicates
that your only schema is the "public" schema.

```Haskell
-- Squeal 0.4
setup :: Definition '[] Schema

-- Squeal 0.5
setup :: Definition (Public '[]) (Public Schema)
```

You can create non-public schemas using `createSchema` and inversely
remove them using `dropSchema`. There is also an idempotent
`createSchemaIfNotExists`.

In order to handle aliases which may refer to non-public schemas, Squeal 0.5
introduces `QualifiedAlias`es. A `QualifiedAlias` can be referred to using
the `(!)` operator from the `IsQualified` typeclass; but if you want to refer
to an alias from the "public" schema, you can continue to use a single
overloaded label, no need to write `#public ! #tab`, just write `#tab`.

**Top-level statements**

As a consequence of multi-schema support, common table expressions that
a `Query` or `Manipulation` may refer to had to be broken into a new parameter.
Additionally, `Query` gained another new parameter for its outer scope which
will be discussed in the next section.

To simplify type signatures that you have to write for top-level queries
and manipulations, Squeal 0.5 introduces the `Query_` and `Manipulation_`
type families.

```
type family Query_
  (schemas :: SchemasType)
  (parameters :: Type)
  (row :: Type) where
    Query_ schemas params row =
      Query '[] '[] schemas (TuplePG params) (RowPG row)
```

As you see, a top-level `Query_` has no outer scope and no common
table expressions in scope, they are empty `'[]`. Moreover,
its parameters and row parameters are now `Type`s, which are
converted to corresponding Postgres types using the `TuplePG`
and `RowPG` type families. This means you can write the type signatures
for your top-level statements using the Haskell types that you
intend to input and retrieve when running the statements. Similar
to `Query_`, there is a `Manipulation_` type, the only difference
being that a general `Manipulation` doesn't have an outer scope,
only a scope for common table expressions.

**Subquery expressions**

Previous versions of Squeal offered subquery expression support
but there were issues. The common use case of using the `IN` operator
from SQL was overly complex. There was a proliferation of subquery
functions to handle different comparison operators, each with two versions
for comparing `ALL` or `ANY` of the query rows. And, most confoundedly,
there was no way to support the `EXISTS` subquery because the "outer scope"
for a query was not available.

Squeal adds an outer scope to the `Query` type.

```Haskell
newtype Query
  (outer :: FromType)
  (commons :: FromType)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (row :: RowType)
    = UnsafeQuery { renderQuery :: ByteString }
```

This enables a well-typed `exists` function.

```Haskell
exists
  :: Query (Join outer from) commons schemas params row
  -> Condition outer commons grp schemas params from
```

The `in_` and `notIn` functions were simplified to handle the most
common use case of comparing to a list of values.

```Haskell
in_
  :: Expression outer commons grp schemas params from ty -- ^ expression
  -> [Expression outer commons grp schemas params from ty]
  -> Condition outer commons grp schemas params from
```

Finally, general subquery comparisons were abstracted to work with
any comparison operators, using the `Operator` type which will be discussed in
the next section.

```Haskell
subAll
  :: Expression outer commons grp schemas params from ty1
  -> Operator ty1 ty2 ('Null 'PGbool)
  -> Query (Join outer from) commons schemas params '[col ::: ty2]
  -> Condition outer commons grp schemas params from

subAny
  :: Expression outer commons grp schemas params from ty1
  -> Operator ty1 ty2 ('Null 'PGbool)
  -> Query (Join outer from) commons schemas params '[col ::: ty2]
  -> Condition outer commons grp schemas params from
```

**Expression RankNTypes**

Squeal 0.5 introduces RankNType type synonyms for common expression patterns.
The simplest example is the `Expr` type, a type for "closed" expressions
that cannot reference any aliases or parameters.

```Haskell
type Expr x
  = forall outer commons grp schemas params from
  . Expression outer commons grp schemas params from x
```

There is also a function type `(-->)`, which is a subtype of the usual Haskell function
type `(->)`.

```Haskell
type (-->) x y
  =  forall outer commons grp schemas params from
  .  Expression outer commons grp schemas params from x
  -> Expression outer commons grp schemas params from y
```

We saw in the subquery section that there is an `Operator` type.

```Haskell
type Operator x1 x2 y
  =  forall outer commons grp schemas params from
  .  Expression outer commons grp schemas params from x1
  -> Expression outer commons grp schemas params from x2
  -> Expression outer commons grp schemas params from y
```

There are also types `FunctionN` and `FunctionVar` for n-ary functions
and variadic functions.

```
type FunctionN xs y
  =  forall outer commons grp schemas params from
  .  NP (Expression outer commons grp schemas params from) xs
  -> Expression outer commons grp schemas params from y
```

An n-ary function takes an `NP` list of `Expression`s as its argument.
Squeal 0.5 adds a helpful operator `(*:)`, to help scrap your `Nil`s.
You can construct an `NP` list now by using the operator `(:*)`,
until your last element, using `(*:)` there. For instance, the function
`atan2_` takes two arguments, `atan2_ (pi *: 2)`.

**Selections**

Previously, Squeal provided a couple versions of `SELECT` depending
on whether you wanted to select all columns of a unique table
in the from clause, i.e. `*`, all columns of a particular table
in the from clause, i.e. `.*`, or a list of columns from the from clause.

Squeal 0.5 refactors this pattern into a `Selection` GADT type, allowing
for abstraction and combinations that were not possible before.

```Haskell
data Selection outer commons grp schemas params from row where
  Star
    :: HasUnique tab from row
    => Selection outer commons 'Ungrouped schemas params from row
  DotStar
    :: Has tab from row
    => Alias tab
    -> Selection outer commons 'Ungrouped schemas params from row
  List
    :: SListI row
    => NP (Aliased (Expression outer commons grp schemas params from)) row
    -> Selection outer commons grp schemas params from row
  Over
    :: SListI row
    => NP (Aliased (WindowFunction outer commons grp schemas params from)) row
    -> WindowDefinition outer commons grp schemas params from
    -> Selection outer commons grp schemas params from row
  Also
    :: Selection outer commons grp schemas params from right
    -> Selection outer commons grp schemas params from left
    -> Selection outer commons grp schemas params from (Join left right)
```

In addition to the `Star`, `DotStar` and `List` constructors, there is an
`Also` constructor combinator, which enables users to combine `Selection`s.
Additionally, there is an `Over` constructor that is used to enable
window functions, described in the next section.

To upgrade from Squeal 0.4, you will replace `selectStar` with `select Star`,
replace `selectDotStar #tab` with `select (DotStar #tab)`. The `List`
selection is such a common use case that there is a function `select_` which
automatically applies it, so to upgrade from Squeal 0.4, replace `select`
with `select_`. There are also independent `selectDistinct` and `selectDistinct_`
functions to filter out duplicate rows.

**Query clauses**

Previously, Squeal provided a couple versions of `INSERT` depending
on whether you wanted to insert `VALUES` or a query.

Squeal 0.5 refactors this pattern into a `QueryClause` GADT.

```
-- Squeal 0.4
insertRow_ #tab (Set 2 `as` #col1 :* Default `as` #col2)
insertQuery_ #tab (selectStar (from (table #other_tab)))

-- Squeal 0.5
insertInto_ #tab (Values_ (Set 2 `as` #col1 :* Default `as` #col2))
insertInto_ #tab (Subquery (select Star (from (table #other_tab))))
```

**Window functions and aggregation**

Previous versions of Squeal provided no support for window functions.
Squeal 0.5 adds support for window functions. We saw `Over` in the
previous section.

```Haskell
Over
  :: SListI row
  => NP (Aliased (WindowFunction outer commons grp schemas params from)) row
  -> WindowDefinition outer commons grp schemas params from
  -> Selection outer commons grp schemas params from row
```

`Over` combines window functions with window definitions. A `WindowDefinition`
is constructed using the `partitionBy` function, optionally with an `orderBy`
clause. For example,

```Haskell
query :: Query_ (Public Schema) () (Row Int32 Int32)
query = select
  (#col1 & Also (rank `as` #col2 `Over` (partitionBy #col1 & orderBy [#col2 & Asc])))
  (from (table #tab))
```

Here the `rank` function is a `WindowFunction`

```Haskell
rank :: WinFun0 ('NotNull 'PGint8)
rank = UnsafeWindowFunction "rank()"
```

`WinFun0` is a RankNType, like `Expr` was, used for no argument window functions.
Similarly, there are also `WinFun1` and `WinFunN` types for
window functions and n-ary window functions.

In order to use the same syntax for certain window functions and aggregate functions,
a new typeclass `Aggregate` was introduced. So for instance, `sum_` can be
either a `Distinction` `Expression` or a `WindowFunction`, used either with
a `groupBy` or with a `partitionBy` in the appropriate way.

```
data Distinction (expr :: kind -> Type) (ty :: kind)
  = All (expr ty)
  | Distinct (expr ty)
```

Aggregate functions can be run over all rows or only over distinct rows,
while window functions bear no such distinction.

**Migrations**

Thanks to [https://github.com/Raveline](Raveline) Squeal 0.5 introduces
a function `defaultMain` to easily create a command line program to
run or rewind your migrations.

Previously, Squeal's migrations were impure, allowing in addition to
running `Definition`s to run arbitrary `IO` operations, such as inserting
data into the database or printing out status messages. In order to
provide compatibility with other migration systems, Squeal 0.5 introduces
pure migrations, that is migrations that are only `Definition`s.

```Haskell
data Migration p schemas0 schemas1 = Migration
  { name :: Text -- ^ The `name` of a `Migration`.
    -- Each `name` in a `Migration` should be unique.
  , up   :: p schemas0 schemas1 -- ^ The `up` instruction of a `Migration`.
  , down :: p schemas1 schemas0 -- ^ The `down` instruction of a `Migration`.
  } deriving (GHC.Generic)
```

A pure migration is a `Migration Definition`, a pair of inverse
`Definition`s with a unique name. To recover impure migrations, Squeal 0.5
introduces the `Terminally` type.

```Haskell
newtype Terminally trans monad x0 x1 = Terminally
  { runTerminally :: trans x0 x1 monad () }
```

`Terminally` applies the indexed monad transformer and the monad it transforms
to the unit type `()`, thereby turning an indexed monad into a `Category`. An
impure migration is a `Migration (Terminally PQ IO)`. You can always cast
a pure migration into an impure migration with the functor, `pureMigration`.

```Haskell
pureMigration
  :: Migration Definition schemas0 schemas1
  -> Migration (Terminally PQ IO) schemas0 schemas1
```

To run either pure or impure migrations, Squeal 0.5 introduces
a typeclass, `Migratory`.

```
class Category p => Migratory p where

  migrateUp
    :: AlignedList (Migration p) schemas0 schemas1
    -> PQ schemas0 schemas1 IO ()

  migrateDown
    :: AlignedList (Migration p) schemas0 schemas1
    -> PQ schemas1 schemas0 IO ()
```

The signatures of `migrateUp` and `migrateDown` have been changed
to make them easier to compose with other `PQ` actions.

**Transactions**

You can now run transactions `ephemerally`, guaranteed to roll back,
but return the result or throw the exception that the transaction
would have generated. This is useful for testing. You can also
`transactionallyRetry` computations, retrying the transaction if
a [serialization failure](https://www.postgresql.org/docs/11/transaction-iso.html#XACT-REPEATABLE-READ) occurs.

**Types**

Squeal now supports money via a `Money` Haskell `Type` and a `'PGmoney`
`PGType`. Squeal 0.5 also adds support for creating domain types
with `createDomain`.

**Time**

Squeal 0.5 adds support for new functions; `now`, `makeDate`, `makeTime`,
, `makeTimestamp`, and `makeTimestamptz`, a function `interval_` for
constructing time intervals out multiples of `TimeUnit`s, and a new
`TimeOp` class, defining affine space operators for time types and their
differences.

**Literals**

Squeal allows you to include Haskell values in your statements using
out of line `parameter`s, but often you want to include them inline,
as a SQL `literal`. Squeal 0.5 enables this using the `Literal` class.

**Set returning functions**

Squeal 0.5 adds support for set returning functions such as `unnest`,
with the `RankNType` `SetOfFunction`, which can be used as as a `FromClause`.

**Text search**

Squeal 0.5 adds extensive support for text search types, functions and operators.

**Much more**

Lots more changes went into and under the hood of the new version.
The `Expression` module was split into coherent submodules since it
had grown to immense proportions. New modules `Alias`, `PG` and `List`
were added to relieve some of the burden that the `Schema` module
had been carrying. Rendering has been better unified with a new
`RenderSQL` typeclass. Type level list concatenation with the `Additional`
typeclass has been added. `Manipulation`s `update` and `delete`
were upgraded, so you can leave out fields you don't want to update and
use `USING` clauses in deletes. Upserts which were previously broken
now work. The `IO` typeclass hierarchy was changed from `MonadBase`
and `MonadBaseControl` to `MonadIO` and `MonadUnliftIO`. A new
`withRecursive` `Manipulation` was added. `SquealException`s were
refactored and a `trySqueal` function added. Arrays were refactored.
And there was probably more I've forgotten.

### Version 0.4

Version 0.4 strengthens Squeal's type system, adds
support for multidimensional arrays, improves support
for container type, improves `with` statements,
improves runtime exceptions, accomodates SQL's three-valued logic,
adds subquery expressions, and adds table and view type expressions.

**Types**

Squeal 0.4 renames some kinds to aid intuition:

```Haskell
type RowType = [(Symbol, NullityType)] -- previously RelationType
type FromType = [(Symbol, RowType)] -- previously RelationsType
```

Null safety for array and composite types is gained by having the base
type of an array be a `NullityType` and the base type of a composite
a `RowType`.

```Haskell
data PGType
  = ..
  | PGvararray NullityType
  | PGfixarray Nat NullityType
  | PGcomposite RowType
```

Squeal embeds Postgres types into Haskell using data kinds and type-in-type:

```Haskell
data PGType = PGbool | ..
data NullityType = Null PGType | NotNull PGType
type RowType = [(Symbol, NullityType)] -- previously RelationType
type FromType = [(Symbol, RowType)] -- previously RelationsType
```

In another sense, we can embed Haskell types
into Postgres types by providing type families:

```Haskell
type family PG (hask :: Type) :: PGType
type family NullPG (hask :: Type) :: NullityType
type family TuplePG (hask :: Type) :: [NullityType]
type family RowPG (hask :: Type) :: RowType
```

Let's look at these one by one.

`PG` was introduced in Squeal 0.3. It was a closed type family that
associates some Haskell types to their obvious corresponding Postgres
types like `PG Double = 'PGfloat8`. It only worked on base types,
no arrays or composites. Squeal 0.4 extends it to
such container types and makes it an open type family so that
users can make their own type instances.

`NullPG` had a different name before but it does the obvious
thing for Haskell with `Maybe`s:

```Haskell
type family NullPG hask where
  NullPG (Maybe hask) = 'Null (PG hask)
  NullPG hask = 'NotNull (PG hask)
```

`TuplePG` uses generics to turn tuple types (including records)
into lists of `NullityType`s in the logical way, e.g.
`TuplePG (Bool, Day) = '[ 'PGbool, 'PGdate]`.

`RowPG` also uses generics to turn record types into a `RowType` in the logical way, e.g.

```Haskell
>>> data Person = Person { name :: Text, age :: Int32 } deriving GHC.Generic
>>> instance SOP.Generic Person
>>> instance SOP.HasDatatypeInfo Person
>>> :kind! TuplePG Person
TuplePG Person :: [NullityType]
= '['NotNull 'PGtext, 'NotNull 'PGint4]
>>> :kind! RowPG Person
RowPG Person :: [(Symbol, NullityType)]
= '["name" ::: 'NotNull 'PGtext, "age" ::: 'NotNull 'PGint4]
```

We've already seen a hint of why these types are useful in one construction
from Squeal 0.3. Creating composite types in Postgres directly from a Haskell
record type essentially uses `RowPG`. Another important use is in simplifying
the type signatures for a `Query` or `Manipulation`. Very often, you will have
a tuple type corresponding to the parameters and a record type corresponding
to the returned columns of a `Query` or `Manipulation`. Instead of writing
boilerplate signature you can reuse these with the help of `TuplePG` and `RowPG`

For instance:

```Haskell
>>> :{
let
  query :: Query '["user" ::: 'View (RowPG Person)] (TuplePG (Only Int32)) (RowPG Person)
  query = selectStar (from (view #user) & where_ (#age .> param @1))
:}
```

**Arrays**

In addition to being able to encode and decode basic Haskell types
like `Int16` and `Text`, Squeal 0.4 permits you to encode and decode Haskell types to
Postgres array types. The `Vector` type corresponds to to variable length arrays.
And thanks to an idea from [Mike Ledger](https://github.com/mikeplus64),
homogeneous tuples correspond to fixed length arrays. We can even
create multi-dimensional fixed length arrays. Let's see an example.

```Haskell
>>> :{
data Row = Row
  { col1 :: Vector Int16
  , col2 :: (Maybe Int16,Maybe Int16)
  , col3 :: ((Int16,Int16),(Int16,Int16),(Int16,Int16))
  } deriving (Eq, GHC.Generic)
:}

>>> instance Generic Row
>>> instance HasDatatypeInfo Row
```

Define a simple round trip query.

```Haskell
>>> :{
let
  roundTrip :: Query '[] (TuplePG Row) (RowPG Row)
  roundTrip = values_ $
    parameter @1 (int2 & vararray)                  `as` #col1 :*
    parameter @2 (int2 & fixarray @2)               `as` #col2 :*
    parameter @3 (int2 & fixarray @2 & fixarray @3) `as` #col3
:}

>>> :set -XOverloadedLists
>>> let input = Row [1,2] (Just 1,Nothing) ((1,2),(3,4),(5,6))
>>> :{
void . withConnection "host=localhost port=5432 dbname=exampledb" $ do
  result <- runQueryParams roundTrip input
  Just output <- firstRow result
  liftBase . print $ input == output
:}
True
```

**Containers**

Squeal aims to provide a correspondence between Haskell types and Postgres types.
In particular, Haskell ADTs with nullary constructors can correspond to
Postgres enum types and Haskell record types can correspond to Postgres
composite types. However, it's not always obvious that that's how a user
will choose to store values of those types. So Squeal 0.4 introduces newtypes
whose purpose is to specify how a user wants to store values of a type.

```Haskell
newtype Json hask = Json {getJson :: hask}
newtype Jsonb hask = Jsonb {getJsonb :: hask}
newtype Composite record = Composite {getComposite :: record}
newtype Enumerated enum = Enumerated {getEnumerated :: enum}
```

Let's see an example:

```Haskell
>>> data Schwarma = Beef | Lamb | Chicken deriving (Eq, Show, GHC.Generic)
>>> instance SOP.Generic Schwarma
>>> instance SOP.HasDatatypeInfo Schwarma
>>>
>>> data Person = Person {name :: Text, age :: Int32} deriving (Eq, Show, GHC.Generic)
>>> instance SOP.Generic Person
>>> instance SOP.HasDatatypeInfo Person
>>> instance Aeson.FromJSON Person
>>> instance Aeson.ToJSON Person
```

We can create the equivalent Postgres types directly from their Haskell types.

```Haskell
>>> :{
type Schema =
  '[ "schwarma" ::: 'Typedef (PG (Enumerated Schwarma))
   , "person" ::: 'Typedef (PG (Composite Person))
   ]
:}

>>> :{
let
  setup :: Definition '[] Schema
  setup =
    createTypeEnumFrom @Schwarma #schwarma >>>
    createTypeCompositeFrom @Person #person
:}
```

Let's demonstrate how to associate our Haskell types `Schwarma` and `Person`
with enumerated, composite or json types in Postgres. First create a Haskell
`Row` type using the `Enumerated`, `Composite` and `Json` newtypes as fields.

```Haskell
>>> :{
data Row = Row
  { schwarma :: Enumerated Schwarma
  , person1 :: Composite Person
  , person2 :: Json Person
  } deriving (Eq, GHC.Generic)
:}

>>> instance Generic Row
>>> instance HasDatatypeInfo Row
>>> :{
let
  input = Row
    (Enumerated Chicken)
    (Composite (Person "Faisal" 24))
    (Json (Person "Ahmad" 48))
:}
```

Once again, define a round trip query.

```Haskell
>>> :{
let
  roundTrip :: Query Schema (TuplePG Row) (RowPG Row)
  roundTrip = values_ $
    parameter @1 (typedef #schwarma) `as` #schwarma :*
    parameter @2 (typedef #person)   `as` #person1  :*
    parameter @3 json                `as` #person2
:}
```

Finally, we can drop our type definitions.

```Haskell
>>> :{
let
  teardown :: Definition Schema '[]
  teardown = dropType #schwarma >>> dropType #person
:}
```

Now let's run it.

```Haskell
>>> :{
let
  session = do
    result <- runQueryParams roundTrip input
    Just output <- firstRow result
    liftBase . print $ input == output
in
  void . withConnection "host=localhost port=5432 dbname=exampledb" $
    define setup
    & pqThen session
    & pqThen (define teardown)
:}
True
```

**With**

Squeal 0.3 supported WITH statements but there's a couple problems with them.
Here's the type signature for `with` in Squeal 0.3.

```Haskell
with
  :: SOP.SListI commons
  => NP (Aliased (Manipulation schema params)) (common ': commons)
  -- ^ common table expressions
  -> Manipulation (With (common ': commons) schema) params results
  -> Manipulation schema params results
```

The first problem is that `with` only works with `Manipulations`.
It can work on `Query`s by using `queryStatement` but it still will
return a `Manipulation`. We can fix this issue by making `with` a
method of a type class with instances for both `Query` and `Manipulation`.

The second problem is that all the common table expressions
refer the base schema, whereas in SQL, each subsequent CTE can
refer to previous CTEs as well. But this can be fixed! First define a datatype:

```Haskell
data CommonTableExpression statement params schema0 schema1 where
  CommonTableExpression
    :: Aliased (statement schema params) (alias ::: cte)
    -> CommonTableExpression statement params schema (alias ::: 'View cte ': schema)
```

It's just a wrapper around an aliased statement, where the statement
could be either a `Query` or `Manipulation`, but it augments the schema
by adding a view to it. It almost looks like a morphism between schemas
but there is no way yet to compose them. Luckily, Squeal already has
a datatype for this, which is used for migrations, the `AlignedList`
type which is really the "free category". So we can then define a `With` type class:

```Haskell
class With statement where
  with
    :: AlignedList (CommonTableExpression statement params) schema0 schema1
    -> statement schema1 params row
    -> statement schema0 params row
```

By giving `Aliasable` instances to CTEs and aligned singleton lists of CTEs (i.e. scrap-your-nils), we get a nice syntax for WITH statements in Squeal.

Here's an example of using `with` for a `Query` and a `Manipulation`:

```Haskell
>>> :{
let
  query :: Query
    '[ "t1" ::: 'View
       '[ "c1" ::: 'NotNull 'PGtext
        , "c2" ::: 'NotNull 'PGtext] ]
    '[]
    '[ "c1" ::: 'NotNull 'PGtext
     , "c2" ::: 'NotNull 'PGtext ]
  query = with (
    selectStar (from (view #t1)) `as` #t2 :>>
    selectStar (from (view #t2)) `as` #t3
    ) (selectStar (from (view #t3)))
in printSQL query
:}
WITH "t2" AS (SELECT * FROM "t1" AS "t1"), "t3" AS (SELECT * FROM "t2" AS "t2") SELECT * FROM "t3" AS "t3"

>>> type ProductsTable = '[] :=> '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]

>>> :{
let
  manipulation :: Manipulation
    '[ "products" ::: 'Table ProductsTable
     , "products_deleted" ::: 'Table ProductsTable
     ] '[ 'NotNull 'PGdate] '[]
  manipulation = with
    (deleteFrom #products (#date .< param @1) ReturningStar `as` #deleted_rows)
    (insertQuery_ #products_deleted (selectStar (from (view (#deleted_rows `as` #t)))))
in printSQL manipulation
:}
WITH "deleted_rows" AS (DELETE FROM "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" SELECT * FROM "deleted_rows" AS "t"
```

**Three Valued Logic**

In previous versions of Squeal, conditions followed classical two valued logic
of `true` and `false`.

```Haskell
-- Squeal 0.3
type Condition schema from grouping params =
  Expression schema from grouping params ('NotNull 'PGbool)
```

I had thought that three valued logic, which is what SQL uses, was confusing.
However, multiple users reported being confused at being forced to do `NULL`
handling, particularly in their left joins. Since the original motivation
of being less confusing evaporated I decided to switch to three valued logic
of `true`, `false` and `null_`.

```Haskell
-- Squeal 0.4
type Condition schema from grouping params =
  Expression schema from grouping params ('Null 'PGbool)
```

**Subquery Expressions**

Squeal 0.4 adds support for subquery expressions such as `IN` and `op ANY/ALL`.

**Row Types**

Squeal 0.4 adds functions to define type expressions from tables and views
and a type expression for user defined types, `typetable`, `typeview` and
`typedef`.

**Runtime Exceptions**

Squeal now has an exception type which gives details on the sort of error
encountered and handler functions.

```Haskell
data SquealException
  = PQException
  { sqlExecStatus :: LibPQ.ExecStatus
  , sqlStateCode :: Maybe ByteString
    -- ^ https://www.postgresql.org/docs/current/static/errcodes-appendix.html
  , sqlErrorMessage :: Maybe ByteString
  }
  | ResultException Text
  | ParseException Text
  deriving (Eq, Show)
instance Exception SquealException

catchSqueal
  :: MonadBaseControl IO io
  => io a
  -> (SquealException -> io a) -- ^ handler
  -> io a

handleSqueal
  :: MonadBaseControl IO io
  => (SquealException -> io a) -- ^ handler
  -> io a -> io a
```

**Additional Changes**

Squeal 0.4 adds `field` and `index` functions to get components of composite
and array expressions.

Squeal 0.4 adds a dependency on `records-sop` to offload a lot of boilerplate
type family logic that was needed for `RowPG`.

The above changes required major and minor changes to Squeal DSL functions.
Please consult the documentation.

### Version 0.3.2 - August 4, 2018

Version 0.3.2 features extensive support for `JSON` functionality with
more than 50 new functions.
This work is entirely due to [Mike Ledger](https://github.com/mikeplus64)
who has been making terrific contributions to Squeal. Thanks!
We also got some examples in the documentation for pools submitted by
[Raveline](https://github.com/Raveline). I'm so pleased to be
getting pull requests and issue submissions from you all!

### Version 0.3.1 - July 7, 2018

Version 0.3.1 of Squeal enables the "Scrap your Nils" trick for
heterogeneous lists of `Alias`es, `Aliased` expressions, `PGlabel`s and `By`s
with the typeclasses `IsLabel`, `IsQualified`, `IsPGlabel`,
and the new `Aliasable` typeclass, to eliminate all need of using `Nil` in a list.
There were a couple minor name changes, i.e. the function `group` was renamed to `groupBy`.
Please consult the documentation.

### Version 0.3 - June 26, 2018

Version 0.3 of Squeal adds views as well as composite and enumerated types to Squeal.
To support these features, a new kind `SchemumType` was added.

```Haskell
data SchemumType
  = Table TableType
  | View RelationType
  | Typedef PGType
```

As a consequence, you will have to update your schema definitions like so:

```Haskell
-- Squeal 0.2
type Schema =
  '[ "users" :::
      '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
      '[ "id"   :::   'Def :=> 'NotNull 'PGint4
       , "name" ::: 'NoDef :=> 'NotNull 'PGtext
       ]
   ]

-- Squeal 0.3
type Schema =
  '[ "users" ::: 'Table (
      '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
      '[ "id"   :::   'Def :=> 'NotNull 'PGint4
       , "name" ::: 'NoDef :=> 'NotNull 'PGtext
       ])
   ]
```

**Views**

You can now create, drop, and query views.

```Haskell
>>> :{
type ABC =
  ('[] :: TableConstraints) :=>
  '[ "a" ::: 'NoDef :=> 'Null 'PGint4
   , "b" ::: 'NoDef :=> 'Null 'PGint4
   , "c" ::: 'NoDef :=> 'Null 'PGint4
   ]
type BC =
  '[ "b" ::: 'Null 'PGint4
   , "c" ::: 'Null 'PGint4
   ]
:}

>>> :{
let
  definition :: Definition '["abc" ::: 'Table ABC ] '["abc" ::: 'Table ABC, "bc"  ::: 'View BC]
  definition = createView #bc (select (#b :* #c :* Nil) (from (table #abc)))
in printSQL definition
:}
CREATE VIEW "bc" AS SELECT "b" AS "b", "c" AS "c" FROM "abc" AS "abc";

>>> :{
let
  definition :: Definition '["abc" ::: 'Table ABC, "bc"  ::: 'View BC] '["abc" ::: 'Table ABC]
  definition = dropView #bc
in printSQL definition
:}
DROP VIEW "bc";

>>> :{
let
  query :: Query '["abc" ::: 'Table ABC, "bc"  ::: 'View BC] '[] BC
  query = selectStar (from (view #bc))
in printSQL query
:}
SELECT * FROM "bc" AS "bc"
```

**Enumerated Types**

PostgreSQL has a powerful type system. It even allows for user defined types.
For instance, you can define enumerated types which are data types that comprise
a static, ordered set of values. They are equivalent to Haskell algebraic data
types whose constructors are nullary. An example of an enum type might be the days of the week,
or a set of status values for a piece of data.

Enumerated types are created using the `createTypeEnum` command, for example:

```Haskell
>>> :{
let
  definition :: Definition '[] '["mood" ::: 'Typedef ('PGenum '["sad", "ok", "happy"])]
  definition = createTypeEnum #mood (label @"sad" :* label @"ok" :* label @"happy" :* Nil)
:}
>>> printSQL definition
CREATE TYPE "mood" AS ENUM ('sad', 'ok', 'happy');
```

Enumerated types can also be generated from a Haskell algebraic data type with nullary constructors, for example:

```Haskell
>>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
>>> instance SOP.Generic Schwarma
>>> instance SOP.HasDatatypeInfo Schwarma

>>> :kind! EnumFrom Schwarma
EnumFrom Schwarma :: PGType
= 'PGenum '["Beef", "Lamb", "Chicken"]

>>> :{
let
  definition :: Definition '[] '["schwarma" ::: 'Typedef (EnumFrom Schwarma)]
  definition = createTypeEnumFrom @Schwarma #schwarma
:}
>>> printSQL definition
CREATE TYPE "schwarma" AS ENUM ('Beef', 'Lamb', 'Chicken');
```

You can express values of an enum type using `label`, which is an overloaded method
of the `IsPGlabel` typeclass.

```Haskell
>>> :{
let
  expression :: Expression sch rels grp params ('NotNull (EnumFrom Schwarma))
  expression = label @"Chicken"
in printSQL expression
:}
'Chicken'
```

**Composite Types**

In addition to enum types, you can define composite types.
A composite type represents the structure of a row or record;
it is essentially just a list of field names and their data types.


`createTypeComposite` creates a composite type. The composite type is
specified by a list of attribute names and data types.

```Haskell
>>> :{
let
  definition :: Definition '[] '["complex" ::: 'Typedef ('PGcomposite '["real" ::: 'PGfloat8, "imaginary" ::: 'PGfloat8])]
  definition = createTypeComposite #complex (float8 `As` #real :* float8 `As` #imaginary :* Nil)
:}
>>> printSQL definition
CREATE TYPE "complex" AS ("real" float8, "imaginary" float8);
```

Composite types are almost equivalent to Haskell record types.
However, because of the potential presence of `NULL`
all the record fields must be `Maybe`s of basic types.
Composite types can be generated from a Haskell record type, for example:

```Haskell
>>> data Complex = Complex {real :: Maybe Double, imaginary :: Maybe Double} deriving GHC.Generic
>>> instance SOP.Generic Complex
>>> instance SOP.HasDatatypeInfo Complex

>>> :kind! CompositeFrom Complex
CompositeFrom Complex :: PGType
= 'PGcomposite '['("real", 'PGfloat8), '("imaginary", 'PGfloat8)]

>>> :{
let
  definition :: Definition '[] '["complex" ::: 'Typedef (CompositeFrom Complex)]
  definition = createTypeCompositeFrom @Complex #complex
in printSQL definition
:}
CREATE TYPE "complex" AS ("real" float8, "imaginary" float8);
```

A row constructor is an expression that builds a row value
(also called a composite value) using values for its member fields.

```Haskell
>>> :{
let
  i :: Expression '[] '[] 'Ungrouped '[] ('NotNull (CompositeFrom Complex))
  i = row (0 `As` #real :* 1 `As` #imaginary :* Nil)
:}
>>>  printSQL i
ROW(0, 1)
```

You can also use `(&)` to apply a field label to a composite value.

```Haskell
>>> :{
let
  expr :: Expression '[] '[] 'Ungrouped '[] ('Null 'PGfloat8)
  expr = i & #imaginary
in printSQL expr
:}
(ROW(0, 1)).imaginary
```

Both composite and enum types can be automatically encoded from and decoded to their equivalent Haskell types.
And they can be dropped.

```Haskell
>>> :{
let
  definition :: Definition '["mood" ::: 'Typedef ('PGenum '["sad", "ok", "happy"])] '[]
  definition = dropType #mood
:}
>>> printSQL definition
DROP TYPE "mood";
```

**Additional Changes**

Squeal 0.3 also introduces a typeclass `HasAll` similar to `Has` but for a list of aliases.
This makes it possible to clean up some unfortunately messy Squeal 0.2 definitions.

```Haskell
-- Squeal 0.2
>>> unique (Column #a :* Column #b :* Nil)

-- Squeal 0.3
>>> unique (#a :* #b :* Nil)
```

Squeal 0.3 also adds `IsLabel` instances for `Aliased` expressions and tables as well as
heterogeneous lists, allowing for some more economy of code.

```Haskell
-- Squeal 0.2 (or 0.3)
>>> select (#a `As` #a :* Nil) (from (table (#t `As` #t)))

-- Squeal 0.3
>>> select #a (from (table #t))
```

Squeal 0.3 also fixes a bug that prevented joined queries on self-referencing tables.

The above changes required major and minor changes to Squeal DSL functions.
Please consult the documentation.

### Version 0.2.1 - April 7, 2018

This minor update fixes an issue where alias identifiers could conflict with
reserved words in PostgreSQL. To fix the issue, alias identifiers are now
quoted. Thanks to Petter Rasmussen for the fix.

### Version 0.2 - March 26, 2018

**Changes**
- **Constraints** - Type level table constraints like primary and foreign keys and column constraints like having `DEFAULT`.
- **Migrations** - Support for linear, invertible migrations tracked in an auxiliary table
- **Arrays** - Support for fixed- and variable-length arrays
- **Aliases** - Generalized `Has` constraint
- **Pools** - Support for pooled connections
- **Transactions** - Support for transaction control language
- **Queries, Manipulations, Definitions** - Small and large changes to Squeal's DSL

Well, a lot of things changed!

**Constraints**

An important request was to bring constraints to the type level.
This means that more of the schema is statically known. In `0.1` column constraints - which boil down
to having `DEFAULT` or not - were at the type level, but they were confusingly named.

```haskell
0.1: 'Optional ('NotNull 'PGInt4)
0.2: 'Def :=> 'NotNull 'PGInt4
0.1: 'Required ('NotNull 'PGInt4)
0.2: 'NoDef :=> 'NotNull 'PGInt4
```

The `:=>` type operator is intended to helpfully connote a constraint relation.
It's also used for table constraints which are new in `0.2`.

```haskell
"emails" :::
    '[ "pk_emails"  ::: 'PrimaryKey '["id"]
     , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
     ] :=>
    '[ "id"      :::   'Def :=> 'NotNull 'PGint4
     , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
     , "email"   ::: 'NoDef :=>    'Null 'PGtext
     ]
```

Another change in the constraint system was the removal of column constraints from
query and manipulation results, as result columns don't support a notion of `DEFAULT`.
This necessitates a distinction between `TableType`s which have both column and table
constraints and `RelationType`s which have neither.

**Migrations**

Migrations are a hot topic and many people have requested this feature. Squeal `0.2`
adds support for linear, invertible migrations. That is, a migration is a named,
invertible, schema-tracking computation:

```haskell
data Migration io schema0 schema1 = Migration
  { name :: Text -- ^ The `name` of a `Migration`.
    -- Each `name` in a `Migration` should be unique.
  , up :: PQ schema0 schema1 io () -- ^ The `up` instruction of a `Migration`.
  , down :: PQ schema1 schema0 io () -- ^ The `down` instruction of a `Migration`.
  }
```

And, `Migration`s can be put together in an "aligned" list:

```haskell
data AlignedList p x0 x1 where
  Done :: AlignedList p x x
  (:>>) :: p x0 x1 -> AlignedList p x1 x2 -> AlignedList p x0 x2
```

These aligned lists are free categories and might look familiar from
the [reflections without remorse](http://okmij.org/ftp/Haskell/zseq.pdf) technique,
which uses their cousins, aligned sequences.

In the context of migration, they allow one to chain new migrations as a
schema evolves over time. `Migration`s' execution is tracked in an auxiliary
migrations table. Migration lists can then be run or rewinded.

```haskell
migrateUp
  :: MonadBaseControl IO io
  => AlignedList (Migration io) schema0 schema1 -- ^ migrations to run
  -> PQ
    ("schema_migrations" ::: MigrationsTable ': schema0)
    ("schema_migrations" ::: MigrationsTable ': schema1)
    io ()

migrateDown
  :: MonadBaseControl IO io
  => AlignedList (Migration io) schema0 schema1 -- ^ migrations to rewind
  -> PQ
    ("schema_migrations" ::: MigrationsTable ': schema1)
    ("schema_migrations" ::: MigrationsTable ': schema0)
    io ()
```

**Aliases**

In Squeal `0.1`, we had different typeclasses `HasColumn` and `HasTable` to indicate
that a table has a column or that a schema has a table. In Squeal `0.2` this has been
unified to a single typeclass,

```haskell
class KnownSymbol alias =>
  Has (alias :: Symbol) (fields :: [(Symbol,kind)]) (field :: kind)
  | alias fields -> field where
```

**Arrays**

Support for array types has been added to Squeal `0.2` through
the `'PGfixarray` and `'PGvararray` `PGType`s. Array values can be
constructed using the `array` function and can be encoded from and decoded to
Haskell `Vector`s.

**Pools**

Squeal `0.2` provides a monad transformer `PoolPQ` that's an instance of `MonadPQ`.
The `resource-pool` library is leveraged to provide striped pools of `Connection`s.
`PoolPQ` should be a drop in replacement for running `Manipulation`s and `Query`s with
`PQ`.

**Transactions**

Squeal `0.2` supports a simple transaction control language. A computation in
`MonadPQ` can be called `transactionally` with different levels of isolation.
Additionally, a schema changing computation, a data definition, can be run in a
transaction. Running a computation in a transaction means that all SQL statements
will be rolled back if an exception is encountered.

**Queries, Manipulations and Definitions**

The above changes required major and minor changes to Squeal DSL functions.
Please consult the documentation.

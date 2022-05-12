## RELEASE NOTES

## Version 0.9.0.0

Thanks to William Yao and Cullin Poresky for new contributions!

### Prepared Statements

Squeal 0.9 adds a new `Prepared` type for prepared statements.

```Haskell
data Prepared m x y = Prepared
  { runPrepared :: x -> m y -- ^ execute a prepared statement
  , deallocate :: m () -- ^ manually clean up a prepared statement
  }
```

This allows to factor and generalize the functions `executePrepared`
and `executePrepared_`. Squeal 0.9 adds new methods `prepare` and
`prepare_` to the `MonadPQ` typeclass.

```Haskell
prepare
  :: MonadPQ pq
  => Statement db x y
  -> pq (Prepared pq x (Result y))

prepare_
  :: MonadPQ pq
  => Statement db x ()
  -> pq (Prepared pq x (Result ()))
```

They may then be run using `runPrepared` and manually cleaned up
using `deallocate`. The `Prepared` type has a cornucopia of instances
allowing you to combine prepared statements in lots of ways. Instances
`Prepared` supports include `Category`, `Arrow`, `Profunctor` and more.

A function `preparedFor` abstracts the pattern of preparing a statement,
then doing _something_ with it and finally deallocating. That something
can be thought of as an "optic", a generalization of lenses.

```Haskell
preparedFor
  :: MonadPQ db pq
  => (Prepared pq a (Result b) -> Prepared pq s t)
  -- ^ transform the input and output using an "optic"
  -> Statement db a b -- ^ query or manipulation
  -> s -> pq t
```

For instance, using the optic `traverse'` recovers the function
`executePrepared` and using the optic `wander traverse_` recovers the
function `executePrepared_`, which are used for running prepared statements
over `Traversable` or `Foldable` containers of parameters respectively.

With the generalized notion of a `Prepared` object, you can prepare statments
and optionally combine them at the beginning of a database session and
have a `Prepared` object to use with `runPrepared` throughout the session.
This is a lower level primitive and closer to the model that PostgreSQL
actually provides than what Squeal previously allowed.

### Row and Enum Types

Squeal 0.9 adds a number of new features for row and enum types.

First, a new type families `DbRelations` and `DbEnums` have been added,
which filter a `SchemasType` down to all row or enum types.
A relation means a table, view or composite type;
other kinds of relations are not currently supported. Previously,
Squeal's support for row types only covered composite types but
Squeal 0.9 adds more support for tables and views. New `TypeExpression`s
`typerow` and `typeenum` have been added. A new type family `FindQualified`,
which looks through database schemas to find a row or enum type has been added.
Squeal will now look through all tables, views and composite types when
trying to find a user-defined relation to match your row type where previously
it had only looked for composites.

Next, new functions have been added to allow users to define manual encodings
for row and enum types. Previously, Squeal only had functions to allow users to
define manual decodings for row and enum types. For enum types,
you can now use `enumParam` like so:

```Haskell
data Dir = North | South | East | West
instance IsPG Dir where
  type PG Dir = 'PGenum '["north", "south", "east", "west"]
instance ToPG db Dir where
  toPG = enumParam $ \case
    North -> label @"north"
    South -> label @"south"
    East -> label @"east"
    West -> label @"west"
```

For row types you can now use `rowParam` together with new combinators
`.#` to cons and `#.` to end the row like so:

```Haskell
data Quaternion = Quaternion
  { real :: Double
  , imaginaryI :: Double
  , imaginaryJ :: Double
  , imaginaryK :: Double
  }
instance IsPG Complex where
  type PG Complex = 'PGcomposite '[
    "re" ::: 'NotNull 'PGfloat8,
    "im" ::: 'NotNull 'PGfloat8,
    "jim" ::: 'NotNull 'PGfloat8,
    "kim" ::: 'NotNull 'PGfloat8]
instance ToPG db Complex where
  toPG = rowParam $
             real `as` #re
    .# imaginaryI `as` #im
    .# imaginaryJ `as` #jim
    #. imaginaryK `as` #kim
```

There is also a function `genericRowParams`, which can
be used with combinators like so:

```Haskell
data L = L {frst :: Int16, scnd :: Char}
  deriving stock (GHC.Generic, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
data R = R {thrd :: Bool, frth :: Bool}
  deriving stock (GHC.Generic, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
instance IsPG (L,R) where
  type PG (L,R) = 'PGcomposite '[
    "frst" ::: 'NotNull 'PGint2,
    "scnd" ::: 'NotNull ('PGchar 1),
    "thrd" ::: 'NotNull 'PGbool,
    "frth" ::: 'NotNull 'PGbool]
instance ToPG db (L,R) where
  toPG = rowParam $
    contramap fst genericRowParams
    `appendParams`
    contramap snd genericRowParams
```

All of this is made possible by generalizing the `EncodeParams`
type to polykinded:

```Haskell
newtype EncodeParams
  (db :: SchemasType)
-  (tys :: [NullType])
+  (tys :: [k])
  (x :: Type)
```

Since it is polykinded, the `tys` parameter can have either
the kind `[NullType]` or the kind `RowType`.

### Other Changes

Squeal 0.9 adds support for GHC 9, which required fully saturating
a number of functions that involved `RankNTypes`, a change
required by the "simplified subsumption" proposal.

A bug for `Has` custom type errors, which made it expensive
in the case of a lookup failure because of use of the strict `If`
type family, has been fixed, thanks to Cullin.

Missing images and some small fixes were added for the Squeal
"Core Concepts Handbook", thanks to William.

CI has been fixed to properly test across different versions of GHC.

The function `notNull` which has a non-intuitive name has been
deprecated and a replacement function `just_` has been added. The
terms `null_` and `just_` now intentionally connote `Nothing` and
`Just` from Haskell's `Maybe` type.

The operators `(.<@)` and `(@>.)` as well as the functions `arrAny`
and `arrAll` have been generalized to allow `Null` arguments. Previously,
they had been improperly restricted to have `NotNull` arguments.

## Version 0.8.1.1

Fix a bug in how the new `Has` type mismatch errors
were implemented, which made it do the expensive pretty-printing
even in the non-error case, resulting in extreme memory usage
at compile time for non-trivial cases.

## Version 0.8.1.0

Improvements to type errors for `Has`/`HasErr`, `HasParameter`,
and trying to aggregate without grouping.

### `Has`

#### Lookup failed

Now tells you specifically that lookup failed,
the kind of thing we were trying to look up and in what,
and a pretty-printed (usually, alphabetized and names-only)
version of what we were looking in.

```
exe/Example.hs:112:11-41: error:
    • Could not find table, view, typedef, index, function, or procedure (SchemumType) named "sers"
      in schema (SchemaType):
        Tables:
          '["emails", "users"]


      *Raw schema (SchemaType)*:
      '[ '("users",
           'Table
             ('["pk_users" ::: 'PrimaryKey '["id"]]
              :=> '["id" ::: ('Def :=> 'NotNull 'PGint4),
                    "name" ::: ('NoDef :=> 'NotNull 'PGtext),
                    "vec" ::: ('NoDef :=> 'NotNull ('PGvararray ('Null 'PGint2)))])),
         "emails"
         ::: 'Table
               ('["pk_emails" ::: 'PrimaryKey '["id"],
                  "fk_user_id" ::: 'ForeignKey '["user_id"] "user" "users" '["id"]]
                :=> '["id" ::: ('Def :=> 'NotNull 'PGint4),
                      "user_id" ::: ('NoDef :=> 'NotNull 'PGint4),
                      "email" ::: ('NoDef :=> 'Null 'PGtext)])]

    • In the first argument of ‘(&)’, namely
        ‘table ((#user ! #sers) `as` #u)’
      In the first argument of ‘from’, namely
        ‘(table ((#user ! #sers) `as` #u)
            & innerJoin
                (table ((#user ! #emails) `as` #e)) (#u ! #id .== #e ! #user_id))’
      In the second argument of ‘select_’, namely
        ‘(from
            (table ((#user ! #sers) `as` #u)
               & innerJoin
                   (table ((#user ! #emails) `as` #e)) (#u ! #id .== #e ! #user_id)))’
    |
112 |   ( from (table ((#user ! #sers) `as` #u)
    |           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
exe/Example.hs:(106,15)-(107,92): error:
    • Could not find schema (SchemaType) named "use"
      in database (SchemasType):
        '["org", "public", "user"]

      *Raw database (SchemasType)*:
      '[ '("public", PublicSchema), "user" ::: UserSchema,
         "org" ::: OrgSchema]

    • In the expression:
        insertInto_
          (#use ! #emails)
          (Values_
             (Default `as` #id
                :* Set (param @1) `as` #user_id :* Set (param @2) `as` #email))
      In an equation for ‘insertEmail’:
          insertEmail
            = insertInto_
                (#use ! #emails)
                (Values_
                   (Default `as` #id
                      :* Set (param @1) `as` #user_id :* Set (param @2) `as` #email))
    |
106 | insertEmail = insertInto_ (#use ! #emails)
    |               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^...
```

#### Lookup succeeded, but types don't match

Now specifies the lookup that the type mismatch comes from with kind
information. Does not pretty-print because the value of the key is important.

```
exe/Example.hs:111:4-12: error:
    • Type mismatch when looking up column (NullType) named "vec"
      in row (RowType):
      '[ '("id", 'NotNull 'PGint4), "name" ::: 'NotNull 'PGtext,
         "vec" ::: 'NotNull ('PGvararray ('Null 'PGint2))]

      Expected: 'NotNull 'PGtext
      But found: 'NotNull ('PGvararray ('Null 'PGint2))

    • In the first argument of ‘as’, namely ‘#u ! #vec’
      In the first argument of ‘(:*)’, namely ‘#u ! #vec `as` #userName’
      In the first argument of ‘select_’, namely
        ‘(#u ! #vec `as` #userName
            :* #e ! #email `as` #userEmail :* #u ! #vec `as` #userVec)’
    |
111 |   (#u ! #vec `as` #userName :* #e ! #email `as` #userEmail :* #u ! #vec `as` #userVec)
    |    ^^^^^^^^^
```

#### Ambiguous types

Generally identical, except complains about being unable to satisfy `Has` instead of exposing `HasErr`

```

exe/Example.hs:103:16-37: error:
    • Ambiguous type variables ‘constraints0’,
                               ‘constraint0’ arising from a use of ‘OnConstraint’
      prevents the constraint ‘(Has
                                  "pk_users" constraints0 constraint0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘constraints0’,
                                                          ‘constraint0’ should be.
      These potential instances exist:
        three instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘OnConflict’, namely
        ‘(OnConstraint #pk_users)’
      In the third argument of ‘insertInto’, namely
        ‘(OnConflict (OnConstraint #pk_users) DoNothing)’
      In the expression:
        insertInto
          (#user ! #users)
          (Values_
             (Default `as` #id
                :* Set (param @1) `as` #name :* Set (param @2) `as` #vec))
          (OnConflict (OnConstraint #pk_users) DoNothing)
          (Returning_ (#id `as` #fromOnly))
    |
103 |   (OnConflict (OnConstraint #pk_users) DoNothing) (Returning_ (#id `as` #fromOnly))
    |                ^^^^^^^^^^^^^^^^^^^^^^
```

### `HasParameter`

#### Looking up index 0

Now gives a special error about params being 1-indexed.

```
exe/Example.hs:118:18-25: error:
    • Tried to get the param at index 0, but params are 1-indexed
    • In the first argument of ‘Set’, namely ‘(param @0)’
      In the first argument of ‘as’, namely ‘Set (param @0)’
      In the first argument of ‘(:*)’, namely ‘Set (param @0) `as` #id’
    |
118 |   (Values_ (Set (param @0) `as` #id :* setUser))
    |                  ^^^^^^^^
```

#### Looking up an out-of-bounds parameter

Now gives a special error, returning the entire parameter list as well.
Does not pretty-print since order is important, and there's no separate
keys and values.

```

exe/Example.hs:118:18-25: error:
    • Index 4 is out of bounds in 1-indexed parameter list:
      '[ 'NotNull 'PGint4, 'NotNull 'PGtext,
         'NotNull ('PGvararray ('Null 'PGint2))]
    • In the first argument of ‘Set’, namely ‘(param @4)’
      In the first argument of ‘as’, namely ‘Set (param @4)’
      In the first argument of ‘(:*)’, namely ‘Set (param @4) `as` #id’
    |
118 |   (Values_ (Set (param @4) `as` #id :* setUser))
    |                  ^^^^^^^^
```

#### Type mismatch when doing lookup

Now gives a custom error similar to the one added for `Has`.

```
exe/Example.hs:118:18: error:
    • Type mismatch when looking up param at index 2
      in 1-indexed parameter list:
        '[ 'NotNull 'PGint4, 'NotNull 'PGtext,
           'NotNull ('PGvararray ('Null 'PGint2))]

      Expected: 'NotNull 'PGtext
      But found: 'NotNull 'PGint4

    • In the first argument of ‘Set’, namely ‘(param @2)’
      In the first argument of ‘as’, namely ‘Set (param @2)’
      In the first argument of ‘(:*)’, namely ‘Set (param @2) `as` #id’
    |
118 |   (Values_ (Set (param @2) `as` #id :* setUser))
    |                  ^^^^^^^^
```

### Using aggregates with an `'Ungrouped` `Expression`

Now gives a custom error with some guidance.

```
exe/Example.hs:118:4: error:
    • Cannot use aggregate functions to construct an Ungrouped Expression. Add a 'groupBy' to your TableExpression. If you want to aggregate across the entire result set, use 'groupBy Nil'.
    • In the first argument of ‘as’, namely ‘countStar’
      In the first argument of ‘(:*)’, namely ‘countStar `as` #count’
      In the first argument of ‘select_’, namely
        ‘(countStar `as` #count :* Nil)’
    |
118 |   (countStar `as` #count :* Nil)
    |    ^^^^^^^^^
```

### Version 0.8

Thanks to Adam Wespiser, Cullin Poresky, Scott Fleischman
and William Yao for lots of contributions.

### Materialized CTEs

Scott Fleischman contributed materialization support to Squeal's
WITH statements.

### LTrees and UUID

New packages `squeal-postgresql-ltree` and `squeal-postgresql-uuid-ossp`
were created to offer functionality from those Postgres extensions.

### Safe Transactions

Previously, Squeal transactions were "unsafe", allowing for arbitrary
`IO`. Now, Squeal provides a new type `Transaction` that is a RankNType.

```Haskell
type Transaction db x = forall m.
  ( MonadPQ db m
  , MonadResult m
  , MonadCatch m
  ) => m x
```

A `Transaction` only permits database operations and error handling,
no arbitrary `IO`. The class `MonadResult` is new but all of its
methods are old and used to be constrained as `MonadIO`,
now as `MonadResult`.

Additionally, a new function `withSavepoint` was added, allowing
for a kind of nested transactions.

### Bug fixes

Various bugs were fixed. Most importantly, poor asynchronous exception
handling was ameliorated.

### Version 0.7

Thanks to Samuel Schlesinger, Adam Wespiser, Cullin Poresky,
Matthew Doty and Mark Wotton for tons of contributions.
Version 0.7 of Squeal makes many changes.

**Inter-schema Foreign Key Bug**
Unfortunately, there was a bug in inter-schema foreign keys in previous
versions of Squeal. Essentially, it was erroneously assumed that
foreign keys always point to tables in the public schema. To remedy this
the `ForeignKey` type has changed kind from

```Haskell
>>> :kind 'ForeignKey
'ForeignKey :: [Symbol]
               -> Symbol -> [Symbol] -> TableConstraint
```

to

```Haskell
>>> :kind 'ForeignKey
'ForeignKey :: [Symbol]
               -> Symbol -> Symbol -> [Symbol] -> TableConstraint
```

To upgrade your database schemas type, you will have to change, e.g.

```Haskell
'ForeignKey '["foo_id1", "foo_id2"] "foo" '["id1", "id2"]
```

to

```Haskell
'ForeignKey '["foo_id1", "foo_id2"] "public" "foo" '["id1", "id2"]
```

**Locking Clauses**

You can now add row level locking clauses to your `select` queries

**Polymorphic Lateral Contexts**

Previously, lateral contexts which are used for lateral joins
and subquery expressions had to have monomorphic lateral contexts,
which greatly reduced composability of queries involving lateral
joins. Squeal 0.7 fixes this limitation, making it possible to
have polymorphic lateral context! When looking up a column heretofore,
the relevant typeclasses would search through `Join lat from`.
This is the "correct" ordering as far as the structure from
left to right in the query, making lat consistently ordered as
one goes through nested lateral joins or nested subquery expressions.
However, it doesn't really matter how the lookup orders the columns.
And if the lookup searches through Join from lat instead then thanks
to good old Haskell lazy list appending, if a query only references
columns in from then it will work no matter the lat.
With a small proviso; if you leave lat polymorphic,
then you must qualify all columns since there could be more than
one table even if from has only one table in it.

**Decoders**

The `DecodeRow` `Monad` now has a `MonadFail` instance.

New row decoder combinators have been added. The functions
`appendRows` and `consRow` let you build row decoders up
from pieces.

Previously, Squeal made it easy to decode enum types to Haskell
enum types (sum types with nullary constructors) so long as
the Haskell type exactly matches the enum type. However, because
of limitations in Haskell - constructors must be capitalized,
name conflicts are often disambiguated with extra letters, etc -
it's often the case that their constructors won't exactly match the
Postgres enum type's labels. The new function `enumValue` allows
to define typesafe custom enum decoders, similar to how `rowValue`
allows to define typesafe custom composite decoders.

```Haskell
>>> :{
data Dir = North | East | South | West
instance IsPG Dir where
  type PG Dir = 'PGenum '["north", "south", "east", "west"]
instance FromPG Dir where
  fromPG = enumValue $
    label @"north" North :*
    label @"south" South :*
    label @"east" East :*
    label @"west" West
:}
```

**Definitions**

New DDL statements have been added allowing to rename and
reset the schema of different schemum objects. Also, new DDL statements
have been added for adding comments to schemum objects.

**Procedures**

Squeal now supports procedure definitions and calls.

**cmdTuples and cmdStatus**

The `cmdTuples` and `cmdStatus` functions from `LibPQ` are now
included.

**PQ Monad Instances**

the `PQ` `Monad` has been given instances for `MonadCatch`,
`MonadThrow`, `MonadMask`, `MonadBase`, `MonadBaseControl`, and
`MonadTransControl`.

**Referential Actions**

A new type `ReferentialAction` has been factored out of
`OnDeleteClause`s and `OnUpdateClause`s. And Missing actions,
`SetNotNull` and `SetDefault` are now included.

To upgrade, change from e.g. `OnDeleteCascade` to `OnDelete Cascade`.

**Array functions**

Squeal now offers typesafe indexing for fixed length arrays and matrices,
with new functions `index1` and `index2`. And new functions `arrAny`
and `arrAll` have been added to enable comparisons to any or all elements
of a variable length array.

**Manipulations**

Tables being manipulated are now re-aliasable, and updates can reference
"from" clauses, actually called `UsingClause`s in Squeal, similar to deletes.

**Other changes**
New tests and bugfixes have been added. More support for encoding and decoding
of different types has been added. Time values now use `iso8601` formatting
for inlining. Also, the GitHub repo has moved from using Circle CI to using
GitHub Actions for continuous integration testing.

### Version 0.6

Version 0.6 makes a number of large changes and additions to Squeal.
I want to thank folks who contributed issues and pull requests;
ilyakooo0, tuomohopia, league, Raveline, Sciencei, mwotton, and more.

I particularly would like to thank my employer SimSpace and colleagues.
We are actively using Squeal at SimSpace which has pushed its development.

My colleague Mark Wotton has also created a project
[squealgen](https://github.com/mwotton/squealgen) to generate
a Squeal schema directly from the database which is awesome.

**Module hierarchy**

Squeal had been growing some rather large modules, whereas I prefer
sub-thousand line modules. Accordingly, I split up the module
hierarchy further. This means there's 60 modules which looks a little
overwhelming, but I think it makes it easier to locate functionality.
It also makes working in a single module less overwhelming.
All relevant functionality is still being exported by `Squeal.PostgreSQL`.

**Statement Profunctors**

Squeal's top level queries and manipulations left something to be desired.
Because `Query_` and `Manipulation_` were type families, they could be
a bit confusing to use. For instance,

```Haskell
>>> :{
selectUser :: Query_ DB UserId User
selectUser = select_
  (#id `as` #userId :* #name `as` #userName)
  (from (table #users) & where_ (#id .== param @1))
:}
>>> :t selectUser
selectUser
  :: Query
       '[]
       '[]
       '["public" ::: '["users" ::: 'Table ('[] :=> UsersColumns)]]
       '[ 'NotNull 'PGint4]
       '["userId" ::: 'NotNull 'PGint4, "userName" ::: 'NotNull 'PGtext]
```

So the `UserId` and `User` types are completely replaced by corresponding
Postgres types. This means that the query can be run, for instance,
with any parameter that is a generic singleton container of `Int32`.
We've lost apparent type safety. You could accidentally run `selectUser`
with a `WidgetId` parameter instead of a `UserId` and it could typecheck.

That's because `Query` is a pure SQL construct, with no knowledge for
how to encode or decode Haskell values.

Another annoyance of `Query_` and `Manipulation_` is that they _must_
be applied to Haskell types which exactly match their corresponding
Postgres types. So, in practice, you often end up with one-off
data type definitions just to have a type that exactly matches,
having the same field names, and the same ordering, etc. as the
returned row.

Both of these issues are solved with the new `Statement` type. Let's
see its definition.

```Haskell
data Statement db x y where
  Manipulation
    :: (SOP.All (OidOfNull db) params, SOP.SListI row)
    => EncodeParams db params x
    -> DecodeRow row y
    -> Manipulation '[] db params row
    -> Statement db x y
  Query
    :: (SOP.All (OidOfNull db) params, SOP.SListI row)
    => EncodeParams db params x
    -> DecodeRow row y
    -> Query '[] '[] db params row
    -> Statement db x y
```

You can see that a `Statement` bundles either a `Query` or a `Manipulation`
together with a way to `EncodeParams` and a way to `DecodeRow`. This
ties the statement to actual Haskell types. Going back to the example,

```Haskell
>>> :{
selectUser :: Statement DB UserId User
selectUser = query $ select_
  (#id `as` #userId :* #name `as` #userName)
  (from (table #users) & where_ (#id .== param @1))
:}
```

Now we really do have the type safety of only being able to `executeParams`
`selectUser` with a `UserId` parameter. Here we've used the smart
constructor `query` which automatically uses the generic instances of
`UserId` and `User` to construct a way to `EncodeParams` and a way to
`DecodeRow`. We can use the `Query` constructor to do custom encodings
and decodings.

```Haskell
>>> :{
selectUser :: Statement DB UserId (UserId, Text)
selectUser = Query enc dec sql where
  enc = contramap getUserId aParam
  dec = do
    uid <- #id
    uname <- #name
    return (uid, uname)
  sql = select Star (from (table #users) & where_ (#id .== param @1))
:}
```

`EncodeParams` and `DecodeRow` both have convenient APIs. `EncodeParams`
is `Contravariant` and can be composed with combinators. `DecodeRow`
is a `Monad` and has `IsLabel` instances. Since `Statement`s bundle
both together, they form `Profunctor`s, where you can `lmap` over
parameters and `rmap` over rows.

The `Statement` `Profunctor` is heavily influenced by
the `Statement` `Profunctor` from Nikita Volkov's excellent `hasql` library,
building on the use of `postgresql-binary` for encoding and decoding.

**Deriving**

Many Haskell types have corresponding Postgres types like `Double`
corresponds to `float8`. Squeal makes this an open relationship with the
`PG` type family. Squeal 0.6 makes it easy to generate `PG` of your
Haskell types, though you might have to turn on `-XUndecidableInstances`,
by deriving an `IsPG` instance.
In addition to having a corresponding Postgres type,
to fully embed your Haskell type you want instances of `ToPG db` to
encode your type as an out-of-line parameter, `FromPG` to
decode your type from a result value, and `Inline` to inline
values of your type directly in SQL statements.

```Haskell
>>> :{
newtype CustomerId = CustomerId {getCustomerId :: Int32}
  deriving newtype (IsPG, ToPG db, FromPG, Inline)
:}

>>> :kind! PG CustomerId
PG CustomerId :: PGType
= 'PGint4
```

You can even embed your Haskell records and enum types using
deriving via.

```Haskell
>>> :{
data Complex = Complex {real :: Double, imaginary :: Double}
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsPG, ToPG db, FromPG, Inline) via Composite Complex
:}

>>> :kind! PG Complex
PG Complex :: PGType
= 'PGcomposite
    '["real" ::: 'NotNull 'PGfloat8,
      "imaginary" ::: 'NotNull 'PGfloat8]

>>> printSQL (inline (Complex 0 1))
ROW((0.0 :: float8), (1.0 :: float8))

>>> :{
data Answer = Yes | No
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsPG, ToPG db, FromPG, Inline) via Enumerated Answer
:}

>>> :kind! PG Answer
PG Answer :: PGType
= 'PGenum '["Yes", "No"]

>>> printSQL (inline Yes)
'Yes'
```

You can also embed your types encoded as `Json` or `Jsonb`.

```Haskell
>>> :{
data Foo = Bar Int | Baz Char Text
  deriving stock (GHC.Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (IsPG, ToPG db, FromPG, Inline) via Jsonb Foo
:}

>>> :kind! PG Foo
PG Foo :: PGType
= 'PGjsonb

>>> printSQL (inline (Baz 'a' "aaa"))
('{"tag":"Baz","contents":["a","aaa"]}' :: jsonb)
```

One thing to notice about `ToParam db` is that it has an
extra parameter `db` that the other classes don't have. That's
because for some types, such as arrays and composites, you
need to know the OID of the element types in order to unambiguously
encode those types. And if the element types are user defined,
then they have to be looked up in the database. The extra parameter
lets us look through the schema for a matching type, and then
look up that type's OID.

**Migrations**

Previously Squeal migrations could be either pure, involving only
data definitions, or impure, allowing arbitrary `IO`. But, they
had to be rewindable; that is, every migration step had to have
an inverse. Squeal 0.6 generalizes to allow both invertible and
one-way migrations. The core datatype for migrations, the free
category `Path` has been moved to its own package `free-categories`.

**Aggregation**

Squeal 0.6 enables filtering and ordering for aggregate
arguments and filtering for window function arguments.

```Haskell
arrayAgg (All #col & orderBy [AscNullsFirst #col] & filterWhere (#col .< 100))
```

To upgrade existing code, if you have an aggregate with multiple arguments,
use `Alls` instead of `All` or `Distincts` instead of `Distinct`
and if you have a window function, apply either `Window` or `Windows`
to its argument(s). Additionally, convenient functions `allNotNull` and
`distinctNotNull` safely filter out `NULL`.

**Ranges**

Squeal 0.6 adds both Haskell and corresponding Postgres range types.

```Haskell
data Bound x
  = Infinite -- ^ unbounded
  | Closed x -- ^ inclusive
  | Open x -- ^ exclusive

data Range x = Empty | NonEmpty (Bound x) (Bound x)

(<=..<=), (<..<), (<=..<), (<..<=) :: x -> x -> Range x
moreThan, atLeast, lessThan, atMost :: x -> Range x
singleton :: x -> Range x
whole :: Range x
```

**Indexes and functions**

Squeal 0.6 adds support for creating and dropping user defined
indexes and functions to your schema, which can then be used
in statements.

**Lateral joins**

Squeal 0.6 adds support for lateral joins, which may reference previous
items.

**Null handling**

Some null handling functions were added such as `monoNotNull`
and `unsafeNotNull`. Because Squeal is aggressively `NULL` polymorphic,
sometimes inference errors can occur. You can apply `monoNotNull`
to fix something to be not `NULL`. You can apply `unsafeNotNull`
when you know that something can't be `NULL`, for instance if you've
filtered `NULL` out of a column.

**Other changes**

Lots of other things changed. `Literal` and `literal` are now called
`Inline` and `inline`. `ColumnConstraint` is called `Optionality`.
`NullityType`s are called `NullTypes`.
Squeal 0.6 adds support for domain types. It more carefully types
`CREATE _ IF NOT EXISTS` and `DROP _ IF EXISTS` definitions. The
`Exception` type was refactored to remove `Maybe`s and new pattern
synonyms were defined to easily match on a few common SQL errors.
`VarChar` and `FixChar` types were added with smart constructors.
Many bugs were fixed. Also, many more tests were added and
a new benchmark suite. A lot more things were changed that I've
probably forgotten about.

### Version 0.5.2

Fixes a bug in pool API and implementation.

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
     , "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["id"]
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

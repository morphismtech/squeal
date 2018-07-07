## RELEASE NOTES

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

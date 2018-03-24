## RELEASE NOTES

### Version 0.2 - March 24, 2018

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

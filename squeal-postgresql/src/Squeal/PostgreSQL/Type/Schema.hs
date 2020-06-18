{-|
Module: Squeal.PostgreSQL.Type.Schema
Description: Postgres type system
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

`Squeal.PostgreSQL.Type.Schema` provides a type-level DSL
for kinds of Postgres types, tables, schema, constraints, and more.
It also defines useful type families to operate on these.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveAnyClass
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilyDependencies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Type.Schema
  ( -- * Postgres Type
    PGType (..)
  , NullType (..)
  , RowType
  , FromType
    -- * Schema Type
  , ColumnType
  , ColumnsType
  , TableType
  , SchemumType (..)
  , IndexType (..)
  , FunctionType
  , ReturnsType (..)
  , SchemaType
  , SchemasType
  , Public
    -- * Constraint
  , (:=>)
  , Optionality (..)
  , TableConstraint (..)
  , TableConstraints
  , References
  , Uniquely
    -- * Enumerated Label
  , IsPGlabel (..)
  , PGlabel (..)
    -- * Data Definition
  , Create
  , CreateIfNotExists
  , CreateOrReplace
  , Drop
  , DropSchemum
  , DropIfExists
  , DropSchemumIfExists
  , Alter
  , AlterIfExists
  , Rename
  , RenameIfExists
  , SetSchema
  , ConstraintInvolves
  , DropIfConstraintsInvolve
    -- * Type Classification
  , PGNum
  , PGIntegral
  , PGFloating
  , PGTypeOf
  , PGJsonType
  , PGJsonKey
  , SamePGType
  , AllNotNull
  , NotAllNull
    -- * Nullification
  , NullifyType
  , NullifyRow
  , NullifyFrom
    -- * Table Conversion
  , TableToColumns
  , TableToConstraints
  , ColumnsToRow
  , TableToRow
    -- * Updatable
  , Updatable
  , AllUnique
  , IsNotElem
    -- * User Types
  , UserType
  , UserTypeName
  , UserTypeNamespace
  ) where

import Control.Category
import Data.Kind
import Data.Monoid hiding (All)
import Data.Type.Bool
import Generics.SOP
import GHC.TypeLits
import Prelude hiding (id, (.))

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render

-- $setup
-- >>> import Squeal.PostgreSQL

-- | `PGType` is the promoted datakind of PostgreSQL types.
--
-- >>> :kind 'PGbool
-- 'PGbool :: PGType
data PGType
  = PGbool -- ^ logical Boolean (true/false)
  | PGint2 -- ^ signed two-byte integer
  | PGint4 -- ^ signed four-byte integer
  | PGint8 -- ^ signed eight-byte integer
  | PGnumeric -- ^ arbitrary precision numeric type
  | PGfloat4 -- ^ single precision floating-point number (4 bytes)
  | PGfloat8 -- ^ double precision floating-point number (8 bytes)
  | PGmoney -- ^ currency amount
  | PGchar Nat -- ^ fixed-length character string
  | PGvarchar Nat -- ^ variable-length character string
  | PGtext -- ^ variable-length character string
  | PGbytea -- ^ binary data ("byte array")
  | PGtimestamp -- ^ date and time (no time zone)
  | PGtimestamptz -- ^ date and time, including time zone
  | PGdate -- ^ calendar date (year, month, day)
  | PGtime -- ^ time of day (no time zone)
  | PGtimetz -- ^ time of day, including time zone
  | PGinterval -- ^ time span
  | PGuuid -- ^ universally unique identifier
  | PGinet -- ^ IPv4 or IPv6 host address
  | PGjson -- ^	textual JSON data
  | PGjsonb -- ^ binary JSON data, decomposed
  | PGvararray NullType -- ^ variable length array
  | PGfixarray [Nat] NullType -- ^ fixed length array
  | PGenum [Symbol] -- ^ enumerated (enum) types are data types that comprise a static, ordered set of values.
  | PGcomposite RowType -- ^ a composite type represents the structure of a row or record; it is essentially just a list of field names and their data types.
  | PGtsvector -- ^ A tsvector value is a sorted list of distinct lexemes, which are words that have been normalized to merge different variants of the same word.
  | PGtsquery -- ^ A tsquery value stores lexemes that are to be searched for.
  | PGoid -- Object identifiers (OIDs) are used internally by PostgreSQL as primary keys for various system tables.
  | PGrange PGType -- ^ Range types are data types representing a range of values of some element type (called the range's subtype).
  | UnsafePGType Symbol -- ^ an escape hatch for unsupported PostgreSQL types

-- | `NullType` encodes the potential presence or definite absence of a
-- @NULL@ allowing operations which are sensitive to such to be well typed.
--
-- >>> :kind 'Null 'PGint4
-- 'Null 'PGint4 :: NullType
-- >>> :kind 'NotNull ('PGvarchar 50)
-- 'NotNull ('PGvarchar 50) :: NullType
data NullType
  = Null PGType -- ^ @NULL@ may be present
  | NotNull PGType -- ^ @NULL@ is absent

-- | The constraint  operator, `:=>` is a type level pair
-- between a "constraint" and some type, for use in pairing
-- an `Optionality` with a `NullType` to produce a `ColumnType`
-- or a `TableConstraints` and a `ColumnsType` to produce a `TableType`.
type (:=>) constraint ty = '(constraint,ty)
infixr 7 :=>

-- | `Optionality` encodes the availability of @DEFAULT@ for inserts and updates.
-- A column can be assigned a default value.
-- A data `Squeal.PostgreSQL.Manipulations.Manipulation` command can also
-- request explicitly that a column be set to its default value,
-- without having to know what that value is.
data Optionality
  = Def -- ^ @DEFAULT@ is available for inserts and updates
  | NoDef -- ^ @DEFAULT@ is unavailable for inserts and updates

-- | `ColumnType` encodes the allowance of @DEFAULT@ and @NULL@ and the
-- base `PGType` for a column.
--
-- >>> :set -XTypeFamilies -XTypeInType
-- >>> import GHC.TypeLits
-- >>> type family IdColumn :: ColumnType where IdColumn = 'Def :=> 'NotNull 'PGint4
-- >>> type family EmailColumn :: ColumnType where EmailColumn = 'NoDef :=> 'Null 'PGtext
type ColumnType = (Optionality,NullType)

-- | `ColumnsType` is a row of `ColumnType`s.
--
-- >>> :{
-- type family UsersColumns :: ColumnsType where
--   UsersColumns =
--     '[ "name" ::: 'NoDef :=> 'NotNull 'PGtext
--      , "id"   :::   'Def :=> 'NotNull 'PGint4
--      ]
-- :}
type ColumnsType = [(Symbol,ColumnType)]

-- | `TableConstraint` encodes various forms of data constraints
-- of columns in a table.
-- `TableConstraint`s give you as much control over the data in your tables
-- as you wish. If a user attempts to store data in a column that would
-- violate a constraint, an error is raised. This applies
-- even if the value came from the default value definition.
data TableConstraint
  = Check [Symbol]
  | Unique [Symbol]
  | PrimaryKey [Symbol]
  | ForeignKey [Symbol] Symbol [Symbol]

-- | A type family which calculates an intra- or cross-schema table reference name.
type family References (sch :: Symbol) (tab :: Symbol) (sch0 :: Symbol) where
  References sch tab sch = tab
  References sch tab _   = sch `AppendSymbol` "." `AppendSymbol` tab

{- | A `TableConstraints` is a row of `TableConstraint`s.

>>> :{
type family UsersConstraints :: TableConstraints where
  UsersConstraints = '[ "pk_users" ::: 'PrimaryKey '["id"] ]
:}
-}
type TableConstraints = [(Symbol,TableConstraint)]

-- | A `ForeignKey` must reference columns that either are
-- a `PrimaryKey` or form a `Unique` constraint.
type family Uniquely
  (key :: [Symbol])
  (constraints :: TableConstraints) :: Constraint where
    Uniquely key (uq ::: 'Unique key ': constraints) = ()
    Uniquely key (pk ::: 'PrimaryKey key ': constraints) = ()
    Uniquely key (_ ': constraints) = Uniquely key constraints

-- | `TableType` encodes a row of constraints on a table as well as the types
-- of its columns.
--
-- >>> :{
-- type family UsersTable :: TableType where
--   UsersTable =
--     '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
--     '[ "id"       :::   'Def :=> 'NotNull 'PGint4
--      , "name"     ::: 'NoDef :=> 'NotNull 'PGtext
--      ]
-- :}
type TableType = (TableConstraints,ColumnsType)

{- | A `RowType` is a row of `NullType`. They correspond to Haskell
record types by means of `Squeal.PostgreSQL.Binary.RowPG` and are used in many places.

>>> :{
type family PersonRow :: RowType where
  PersonRow =
    '[ "name"        ::: 'NotNull 'PGtext
     , "age"         ::: 'NotNull 'PGint4
     , "dateOfBirth" :::    'Null 'PGdate
     ]
:}
-}
type RowType = [(Symbol,NullType)]

{- | `FromType` is a row of `RowType`s. It can be thought of as
a product, or horizontal gluing and is used in `Squeal.PostgreSQL.Query.FromClause`s
and `Squeal.PostgreSQL.Query.TableExpression`s.
-}
type FromType = [(Symbol,RowType)]

-- | `ColumnsToRow` removes column constraints.
type family ColumnsToRow (columns :: ColumnsType) :: RowType where
  ColumnsToRow (column ::: _ :=> ty ': columns) =
    column ::: ty ': ColumnsToRow columns
  ColumnsToRow '[] = '[]

-- | `TableToColumns` removes table constraints.
type family TableToColumns (table :: TableType) :: ColumnsType where
  TableToColumns (constraints :=> columns) = columns

-- | `TableToConstraints` removes table columns.
type family TableToConstraints (table :: TableType) :: TableConstraints where
  TableToConstraints (constraints :=> columns) = constraints

-- | Convert a table to a row type.
type family TableToRow (table :: TableType) :: RowType where
  TableToRow tab = ColumnsToRow (TableToColumns tab)

-- | Numeric Postgres types.
type PGNum =
  '[ 'PGint2, 'PGint4, 'PGint8, 'PGnumeric, 'PGfloat4, 'PGfloat8]

-- | Floating Postgres types.
type PGFloating = '[ 'PGfloat4, 'PGfloat8, 'PGnumeric]

-- | Integral Postgres types.
type PGIntegral = '[ 'PGint2, 'PGint4, 'PGint8]

-- | `PGTypeOf` forgets about @NULL@ and any column constraints.
type family PGTypeOf (ty :: NullType) :: PGType where
  PGTypeOf (null pg) = pg

-- | Equality constraint on the underlying `PGType` of two columns.
class SamePGType
  (ty0 :: (Symbol,ColumnType)) (ty1 :: (Symbol,ColumnType)) where
instance ty0 ~ ty1 => SamePGType
  (alias0 ::: def0 :=> null0 ty0)
  (alias1 ::: def1 :=> null1 ty1)

-- | `AllNotNull` is a constraint that proves a `ColumnsType` has no @NULL@s.
type family AllNotNull (columns :: ColumnsType) :: Constraint where
  AllNotNull (_ ::: _ :=> 'NotNull _ ': columns) = AllNotNull columns
  AllNotNull '[] = ()

-- | `NotAllNull` is a constraint that proves a `ColumnsType` has some
-- @NOT NULL@.
type family NotAllNull (columns :: ColumnsType) :: Constraint where
  NotAllNull (_ ::: _ :=> 'NotNull _ ': _) = ()
  NotAllNull (_ ::: _ :=> 'Null _ ': columns) = NotAllNull columns

-- | `NullifyType` is an idempotent that nullifies a `NullType`.
type family NullifyType (ty :: NullType) :: NullType where
  NullifyType ('Null ty) = 'Null ty
  NullifyType ('NotNull ty) = 'Null ty

-- | `NullifyRow` is an idempotent that nullifies a `RowType`.
type family NullifyRow (columns :: RowType) :: RowType where
  NullifyRow (column ::: ty ': columns) =
    column ::: NullifyType ty ': NullifyRow columns
  NullifyRow '[] = '[]

-- | `NullifyFrom` is an idempotent that nullifies a `FromType`
-- used to nullify the left or right hand side of an outer join
-- in a `Squeal.PostgreSQL.Query.FromClause`.
type family NullifyFrom (tables :: FromType) :: FromType where
  NullifyFrom (table ::: columns ': tables) =
    table ::: NullifyRow columns ': NullifyFrom tables
  NullifyFrom '[] = '[]

-- | @Create alias x xs@ adds @alias ::: x@ to the end of @xs@ and is used in
-- `Squeal.PostgreSQL.Definition.createTable` statements and in @ALTER TABLE@
-- `Squeal.PostgreSQL.Definition.addColumn`.
type family Create alias x xs where
  Create alias x '[] = '[alias ::: x]
  Create alias x (alias ::: y ': xs) = TypeError
    ('Text "Create: alias "
    ':<>: 'ShowType alias
    ':<>: 'Text "already exists")
  Create alias y (x ': xs) = x ': Create alias y xs

{-| Similar to `Create` but no error on pre-existence-}
type family CreateIfNotExists alias x xs where
  CreateIfNotExists alias x '[] = '[alias ::: x]
  CreateIfNotExists alias x (alias ::: y ': xs) = alias ::: y ': xs
  CreateIfNotExists alias y (x ': xs) = x ': CreateIfNotExists alias y xs

{-| Similar to `Create` but used to replace values
with the same type.-}
type family CreateOrReplace alias x xs where
  CreateOrReplace alias x '[] = '[alias ::: x]
  CreateOrReplace alias x (alias ::: x ': xs) = alias ::: x ': xs
  CreateOrReplace alias x (alias ::: y ': xs) = TypeError
    ('Text "CreateOrReplace: expected type "
    ':<>: 'ShowType x
    ':<>: 'Text " but alias "
    ':<>: 'ShowType alias
    ':<>: 'Text " has type "
    ':<>: 'ShowType y)
  CreateOrReplace alias y (x ': xs) = x ': CreateOrReplace alias y xs

-- | @Drop alias xs@ removes the type associated with @alias@ in @xs@
-- and is used in `Squeal.PostgreSQL.Definition.dropTable` statements
-- and in @ALTER TABLE@ `Squeal.PostgreSQL.Definition.dropColumn` statements.
type family Drop alias xs where
  Drop alias '[] = TypeError
    ('Text "Drop: alias "
    ':<>: 'ShowType alias
    ':<>: 'Text " does not exist" )
  Drop alias (alias ::: x ': xs) = xs
  Drop alias (x ': xs) = x ': Drop alias xs

-- | Drop a particular flavor of schemum type
type family DropSchemum alias sch xs where
  DropSchemum alias sch '[] = TypeError
    ('Text "DropSchemum: alias "
    ':<>: 'ShowType alias
    ':<>: 'Text " does not exist" )
  DropSchemum alias sch (alias ::: sch x ': xs) = xs
  DropSchemum alias sch0 (alias ::: sch1 x ': xs) = TypeError
    ('Text "DropSchemum: expected schemum "
    ':<>: 'ShowType sch0
    ':<>: 'Text " but alias "
    ':<>: 'ShowType alias
    ':<>: 'Text " has schemum "
    ':<>: 'ShowType sch1)
  DropSchemum alias sch (x ': xs) = x ': DropSchemum alias sch xs

-- | Similar to `Drop` but no error on non-existence
type family DropIfExists alias xs where
  DropIfExists alias '[] = '[]
  DropIfExists alias (alias ::: x ': xs) = xs
  DropIfExists alias (x ': xs) = x ': DropIfExists alias xs

-- | Similar to `DropSchemum` but no error on non-existence
type family DropSchemumIfExists alias sch xs where
  DropSchemumIfExists alias sch '[] = '[]
  DropSchemumIfExists alias sch (alias ::: sch x ': xs) = xs
  DropSchemumIfExists alias sch0 (alias ::: sch1 x ': xs) = TypeError
    ('Text "DropSchemumIfExists: expected schemum "
    ':<>: 'ShowType sch1
    ':<>: 'Text " but alias "
    ':<>: 'ShowType alias
    ':<>: 'Text " has schemum "
    ':<>: 'ShowType sch0)
  DropSchemumIfExists alias sch (x ': xs) = x ': DropSchemumIfExists alias sch xs

-- | @Alter alias x xs@ replaces the type associated with an @alias@ in @xs@
-- with the type @x@ and is used in `Squeal.PostgreSQL.Definition.alterTable`
-- and `Squeal.PostgreSQL.Definition.alterColumn`.
type family Alter alias x xs where
  Alter alias x '[] = TypeError
    ('Text "Alter: alias "
    ':<>: 'ShowType alias
    ':<>: 'Text " does not exist" )
  Alter alias x1 (alias ::: x0 ': xs) = alias ::: x1 ': xs
  Alter alias x1 (x0 ': xs) = x0 ': Alter alias x1 xs

-- | Similar to `Alter` but no error on non-existence
type family AlterIfExists alias x xs where
  AlterIfExists alias x '[] = '[]
  AlterIfExists alias x1 (alias ::: x0 ': xs) = alias ::: x1 ': xs
  AlterIfExists alias x1 (x0 ': xs) = x0 ': AlterIfExists alias x1 xs

-- | @Rename alias0 alias1 xs@ replaces the alias @alias0@ by @alias1@ in @xs@
-- and is used in `Squeal.PostgreSQL.Definition.alterTableRename` and
-- `Squeal.PostgreSQL.Definition.renameColumn`.
type family Rename alias0 alias1 xs where
  Rename alias0 alias1 '[] = TypeError
    ('Text "Rename: alias "
    ':<>: 'ShowType alias0
    ':<>: 'Text " does not exist" )
  Rename alias0 alias1 ((alias0 ::: x0) ': xs) = (alias1 ::: x0) ': xs
  Rename alias0 alias1 (x ': xs) = x ': Rename alias0 alias1 xs

-- | Similar to `Rename` but no error on non-existence
type family RenameIfExists alias0 alias1 xs where
  RenameIfExists alias x '[] = '[]
  RenameIfExists alias0 alias1 ((alias0 ::: x0) ': xs) = (alias1 ::: x0) ': xs
  RenameIfExists alias0 alias1 (x ': xs) = x ': RenameIfExists alias0 alias1 xs

-- | Move an object from one schema to another
type family SetSchema sch0 sch1 schema0 schema1 obj srt ty db where
  SetSchema sch0 sch1 schema0 schema1 obj srt ty db = Alter sch1
    (Create obj (srt ty) schema1)
    (Alter sch0 (DropSchemum obj srt schema0) db)

-- | Check if a `TableConstraint` involves a column
type family ConstraintInvolves column constraint where
  ConstraintInvolves column ('Check columns) = column `Elem` columns
  ConstraintInvolves column ('Unique columns) = column `Elem` columns
  ConstraintInvolves column ('PrimaryKey columns) = column `Elem` columns
  ConstraintInvolves column ('ForeignKey columns tab refcolumns)
    = column `Elem` columns

-- | Drop all `TableConstraint`s that involve a column
type family DropIfConstraintsInvolve column constraints where
  DropIfConstraintsInvolve column '[] = '[]
  DropIfConstraintsInvolve column (alias ::: constraint ': constraints)
    = If (ConstraintInvolves column constraint)
        (DropIfConstraintsInvolve column constraints)
        (alias ::: constraint ': DropIfConstraintsInvolve column constraints)

-- | A `SchemumType` is a user-defined type, either a `Table`,
-- `View` or `Typedef`.
data SchemumType
  = Table TableType
  | View RowType
  | Typedef PGType
  | Index IndexType
  | Function FunctionType
  | Procedure ProcedureType
  | UnsafeSchemum Symbol

-- | Use `:=>` to pair the parameter types with the return
-- type of a function.
type FunctionType = ([NullType], ReturnsType)

type ProcedureType = [NullType]

{- |
PostgreSQL provides several index types:
B-tree, Hash, GiST, SP-GiST, GIN and BRIN.
Each index type uses a different algorithm
that is best suited to different types of queries.
-}
data IndexType
  = Btree
  -- ^ B-trees can handle equality and range queries on data
  -- that can be sorted into some ordering.
  | Hash
  -- ^ Hash indexes can only handle simple equality comparisons.
  | Gist
  -- ^ GiST indexes are not a single kind of index,
  -- but rather an infrastructure within which many different
  -- indexing strategies can be implemented.
  | Spgist
  -- ^ SP-GiST indexes, like GiST indexes,
  -- offer an infrastructure that supports various kinds of searches.
  | Gin
  -- ^ GIN indexes are “inverted indexes” which are appropriate for
  -- data values that contain multiple component values, such as arrays.
  | Brin
  -- ^ BRIN indexes (a shorthand for Block Range INdexes) store summaries
  -- about the values stored in consecutive physical block ranges of a table.

{- | Return type of a function-}
data ReturnsType
  = Returns NullType -- ^ function
  | ReturnsTable RowType -- ^ set returning function

{- | The schema of a database consists of a list of aliased,
user-defined `SchemumType`s.

>>> :{
type family Schema :: SchemaType where
  Schema =
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
-}
type SchemaType = [(Symbol,SchemumType)]

{- |
A database contains one or more named schemas, which in turn contain tables.
The same object name can be used in different schemas without conflict;
for example, both schema1 and myschema can contain tables named mytable.
Unlike databases, schemas are not rigidly separated:
a user can access objects in any of the schemas in the database they are connected to,
if they have privileges to do so.

There are several reasons why one might want to use schemas:

  * To allow many users to use one database without interfering with each other.
  * To organize database objects into logical groups to make them more manageable.
  * Third-party applications can be put into separate schemas
  so they do not collide with the names of other objects.
-}
type SchemasType = [(Symbol,SchemaType)]

-- | A type family to use for a single schema database.
type family Public (schema :: SchemaType) :: SchemasType
  where Public schema = '["public" ::: schema]

-- | `IsPGlabel` looks very much like the `IsLabel` class. Whereas
-- the overloaded label, `fromLabel` is used for column references,
-- `label`s are used for enum terms. A `label` is called with
-- type application like `label @"beef"`.
class IsPGlabel (label :: Symbol) expr where label :: expr
instance label ~ label1
  => IsPGlabel label (PGlabel label1) where label = PGlabel
instance labels ~ '[label]
  => IsPGlabel label (NP PGlabel labels) where label = PGlabel :* Nil
-- | A `PGlabel` unit type with an `IsPGlabel` instance
data PGlabel (label :: Symbol) = PGlabel
instance KnownSymbol label => RenderSQL (PGlabel label) where
  renderSQL _ = "\'" <> renderSymbol @label <> "\'"
instance All KnownSymbol labels => RenderSQL (NP PGlabel labels) where
  renderSQL
    = commaSeparated
    . hcollapse
    . hcmap (Proxy @KnownSymbol) (K . renderSQL)

-- | Is a type a valid JSON key?
type PGJsonKey = '[ 'PGint2, 'PGint4, 'PGtext ]

-- | Is a type a valid JSON type?
type PGJsonType = '[ 'PGjson, 'PGjsonb ]

-- | Utility class for `AllUnique` to provide nicer error messages.
class IsNotElem x isElem where
instance IsNotElem x 'False where
instance (TypeError (      'Text "Cannot assign to "
                      ':<>: 'ShowType alias
                      ':<>: 'Text " more than once"))
    => IsNotElem '(alias, a) 'True where

-- | No elem of @xs@ appears more than once, in the context of assignment.
class AllUnique (xs :: [(Symbol, a)]) where
instance AllUnique '[] where
instance (IsNotElem x (Elem x xs), AllUnique xs) => AllUnique (x ': xs) where

-- | Updatable lists of columns
type Updatable table columns =
  ( All (HasIn (TableToColumns table)) columns
  , AllUnique columns
  , SListI (TableToColumns table) )

-- | Calculate the name of a user defined type.
type family UserTypeName (schema :: SchemaType) (ty :: PGType) where
  UserTypeName '[] ty = 'Nothing
  UserTypeName (td ::: 'Typedef ty ': _) ty = 'Just td
  UserTypeName (_ ': schema) ty = UserTypeName schema ty

-- | Helper to calculate the schema of a user defined type.
type family UserTypeNamespace
  (sch :: Symbol)
  (td :: Maybe Symbol)
  (schemas :: SchemasType)
  (ty :: PGType) where
    UserTypeNamespace sch 'Nothing schemas ty = UserType schemas ty
    UserTypeNamespace sch ('Just td) schemas ty = '(sch, td)

-- | Calculate the schema and name of a user defined type.
type family UserType (db :: SchemasType) (ty :: PGType) where
  UserType '[] ty = TypeError
    ('Text "No such user type: " ':<>: 'ShowType ty)
  UserType (sch ::: schema ': schemas) ty =
    UserTypeNamespace sch (UserTypeName schema ty) schemas ty

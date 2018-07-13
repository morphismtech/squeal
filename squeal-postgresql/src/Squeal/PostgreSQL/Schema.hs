{-|
Module: Squeal.PostgreSQL.Schema
Description: Embedding of PostgreSQL type and alias system
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

This module provides a type-level DSL for kinds of Postgres types,
tables, schema, constraints, aliases, enumerated labels, and groupings.
It also defines useful type families to operate on these. Finally,
it defines an embedding of Haskell types into Postgres types.
-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveAnyClass
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , OverloadedStrings
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

module Squeal.PostgreSQL.Schema
  ( -- * Types
    PGType (..)
  , HasOid (..)
  , NullityType (..)
  , ColumnType
    -- * Tables
  , ColumnsType
  , RelationType
  , NilRelation
  , RelationsType
  , TableType
    -- * Schema
  , SchemumType (..)
  , SchemaType
    -- * Constraints
  , (:=>)
  , ColumnConstraint (..)
  , TableConstraint (..)
  , TableConstraints
  , NilTableConstraints
  , Uniquely
    -- * Aliases
  , (:::)
  , Alias (Alias)
  , renderAlias
  , renderAliases
  , Aliased (As)
  , renderAliasedAs
  , AliasesOf
  , ZipAliased (..)
  , Has
  , HasUnique
  , HasAll
  , IsLabel (..)
  , IsQualified (..)
  , renderAliasString
    -- * Enumerated Labels
  , IsPGlabel (..)
  , PGlabel (..)
  , renderLabel
  , renderLabels
    -- * Grouping
  , Grouping (..)
  , GroupedBy
    -- * Type Families
  , Join
  , With
  , Create
  , Drop
  , Alter
  , Rename
  , Elem
  , In
  , PGNum
  , PGIntegral
  , PGFloating
  , PGTypeOf
  , PGarrayOf
  , PGarray
  , PGtextArray
  , SameTypes
  , SamePGType
  , AllNotNull
  , NotAllNull
  , NullifyType
  , NullifyRelation
  , NullifyRelations
  , ColumnsToRelation
  , TableToColumns
  , TableToRelation
  , RelationToRowType
  , RelationToNullityTypes
  , ConstraintInvolves
  , DropIfConstraintsInvolve
    -- ** JSON support
  , PGjson_
  , PGjsonKey
    -- * Embedding
  , PG
  , EnumFrom
  , LabelsFrom
  , CompositeFrom
  , FieldNamesFrom
  , FieldTypesFrom
  , ConstructorsOf
  , ConstructorNameOf
  , ConstructorNamesOf
  , FieldsOf
  , FieldNameOf
  , FieldNamesOf
  , FieldTypeOf
  , FieldTypesOf
  , RecordCodeOf
  , MapMaybes (..)
  , Nulls
  ) where

import Control.DeepSeq
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Kind
import Data.Monoid hiding (All)
import Data.Scientific (Scientific)
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Word (Word16, Word32, Word64)
import Data.Type.Bool
import Data.UUID.Types (UUID)
import Generics.SOP
import GHC.OverloadedLabels
import GHC.TypeLits
import Network.IP.Addr

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Lazy as Lazy
import qualified GHC.Generics as GHC
import qualified Generics.SOP.Type.Metadata as Type

import Squeal.PostgreSQL.Render

-- | `PGType` is the promoted datakind of PostgreSQL types.
--
-- >>> import Squeal.PostgreSQL.Schema
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
  | PGvararray PGType -- ^ variable length array
  | PGfixarray Nat PGType -- ^ fixed length array
  | PGenum [Symbol]
  | PGcomposite [(Symbol, PGType)]
  | UnsafePGType Symbol -- ^ an escape hatch for unsupported PostgreSQL types

-- | The object identifier of a `PGType`.
--
-- >>> :set -XTypeApplications
-- >>> oid @'PGbool
-- 16
class HasOid (ty :: PGType) where oid :: Word32
instance HasOid 'PGbool where oid = 16
instance HasOid 'PGint2 where oid = 21
instance HasOid 'PGint4 where oid = 23
instance HasOid 'PGint8 where oid = 20
instance HasOid 'PGnumeric where oid = 1700
instance HasOid 'PGfloat4 where oid = 700
instance HasOid 'PGfloat8 where oid = 701
instance HasOid ('PGchar n) where oid = 18
instance HasOid ('PGvarchar n) where oid = 1043
instance HasOid 'PGtext where oid = 25
instance HasOid 'PGbytea where oid = 17
instance HasOid 'PGtimestamp where oid = 1114
instance HasOid 'PGtimestamptz where oid = 1184
instance HasOid 'PGdate where oid = 1082
instance HasOid 'PGtime where oid = 1083
instance HasOid 'PGtimetz where oid = 1266
instance HasOid 'PGinterval where oid = 1186
instance HasOid 'PGuuid where oid = 2950
instance HasOid 'PGinet where oid = 869
instance HasOid 'PGjson where oid = 114
instance HasOid 'PGjsonb where oid = 3802

-- | `NullityType` encodes the potential presence or definite absence of a
-- @NULL@ allowing operations which are sensitive to such to be well typed.
--
-- >>> :kind 'Null 'PGint4
-- 'Null 'PGint4 :: NullityType
-- >>> :kind 'NotNull ('PGvarchar 50)
-- 'NotNull ('PGvarchar 50) :: NullityType
data NullityType
  = Null PGType -- ^ @NULL@ may be present
  | NotNull PGType -- ^ @NULL@ is absent

-- | The constraint  operator, `:=>` is a type level pair
-- between a "constraint" and some type, for use in pairing
-- a `ColumnConstraint` with a `NullityType` to produce a `ColumnType`
-- or a `TableConstraints` and a `ColumnsType` to produce a `TableType`.
type (:=>) constraint ty = '(constraint,ty)
infixr 7 :=>

-- | The alias operator `:::` is like a promoted version of `As`,
-- a type level pair between an alias and some type.
type (:::) (alias :: Symbol) ty = '(alias,ty)
infixr 6 :::

-- | `ColumnConstraint` encodes the availability of @DEFAULT@ for inserts and updates.
-- A column can be assigned a default value.
-- A data `Squeal.PostgreSQL.Manipulations.Manipulation` command can also
-- request explicitly that a column be set to its default value,
-- without having to know what that value is.
data ColumnConstraint
  = Def -- ^ @DEFAULT@ is available for inserts and updates
  | NoDef -- ^ @DEFAULT@ is unavailable for inserts and updates

-- | `ColumnType` encodes the allowance of @DEFAULT@ and @NULL@ and the
-- base `PGType` for a column.
--
-- >>> :set -XTypeFamilies -XTypeInType
-- >>> import GHC.TypeLits
-- >>> type family IdColumn :: ColumnType where IdColumn = 'Def :=> 'NotNull 'PGint4
-- >>> type family EmailColumn :: ColumnType where EmailColumn = 'NoDef :=> 'Null 'PGtext
type ColumnType = (ColumnConstraint,NullityType)

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

-- | A `TableConstraints` is a row of `TableConstraint`s.
type TableConstraints = [(Symbol,TableConstraint)]
-- | A monokinded empty `TableConstraints`.
type family NilTableConstraints :: TableConstraints where
  NilTableConstraints = '[]

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

-- | `RelationType` is a row of `NullityType`
--
-- >>> :{
-- type family PersonRelation :: RelationType where
--   PersonRelation =
--     '[ "name"        ::: 'NotNull 'PGtext
--      , "age"         ::: 'NotNull 'PGint4
--      , "dateOfBirth" :::    'Null 'PGdate
--      ]
-- :}
type RelationType = [(Symbol,NullityType)]
-- | A monokinded empty `RelationType`.
type family NilRelation :: RelationType where NilRelation = '[]

-- | `RelationsType` is a row of `RelationType`s, thought of as a product.
type RelationsType = [(Symbol,RelationType)]

-- | `ColumnsToRelation` removes column constraints.
type family ColumnsToRelation (columns :: ColumnsType) :: RelationType where
  ColumnsToRelation '[] = '[]
  ColumnsToRelation (column ::: constraint :=> ty ': columns) =
    column ::: ty ': ColumnsToRelation columns

-- | `TableToColumns` removes table constraints.
type family TableToColumns (table :: TableType) :: ColumnsType where
  TableToColumns (constraints :=> columns) = columns

-- | Convert a table to a relation.
type family TableToRelation (table :: TableType) :: RelationType where
  TableToRelation tab = ColumnsToRelation (TableToColumns tab)

-- | `Grouping` is an auxiliary namespace, created by
-- @GROUP BY@ clauses (`Squeal.PostgreSQL.Query.group`), and used
-- for typesafe aggregation
data Grouping
  = Ungrouped -- ^ no aggregation permitted
  | Grouped [(Symbol,Symbol)] -- ^ aggregation required for any column which is not grouped

{- | A `GroupedBy` constraint indicates that a table qualified column is
a member of the auxiliary namespace created by @GROUP BY@ clauses and thus,
may be called in an output `Squeal.PostgreSQL.Expression.Expression` without aggregating.
-}
class (KnownSymbol relation, KnownSymbol column)
  => GroupedBy relation column bys where
instance {-# OVERLAPPING #-} (KnownSymbol relation, KnownSymbol column)
  => GroupedBy relation column ('(table,column) ': bys)
instance {-# OVERLAPPABLE #-}
  ( KnownSymbol relation
  , KnownSymbol column
  , GroupedBy relation column bys
  ) => GroupedBy relation column (tabcol ': bys)

-- | `Alias`es are proxies for a type level string or `Symbol`
-- and have an `IsLabel` instance so that with @-XOverloadedLabels@
--
-- >>> :set -XOverloadedLabels
-- >>> #foobar :: Alias "foobar"
-- Alias
data Alias (alias :: Symbol) = Alias
  deriving (Eq,GHC.Generic,Ord,Show,NFData)
instance alias1 ~ alias2 => IsLabel alias1 (Alias alias2) where
  fromLabel = Alias
instance aliases ~ '[alias] => IsLabel alias (NP Alias aliases) where
  fromLabel = fromLabel :* Nil
instance KnownSymbol alias => RenderSQL (Alias alias) where renderSQL = renderAlias

-- | >>> renderAlias #jimbob
-- "\"jimbob\""
renderAlias :: KnownSymbol alias => Alias alias -> ByteString
renderAlias = doubleQuoted . fromString . symbolVal

renderAliasString :: KnownSymbol alias => Alias alias -> ByteString
renderAliasString = singleQuotedText . fromString . symbolVal

-- | >>> import Generics.SOP (NP(..))
-- >>> renderAliases (#jimbob :* #kandi :* Nil)
-- ["\"jimbob\"","\"kandi\""]
renderAliases
  :: All KnownSymbol aliases => NP Alias aliases -> [ByteString]
renderAliases = hcollapse
  . hcmap (Proxy @KnownSymbol) (K . renderAlias)

-- | The `As` operator is used to name an expression. `As` is like a demoted
-- version of `:::`.
--
-- >>> Just "hello" `As` #hi :: Aliased Maybe ("hi" ::: String)
-- As (Just "hello") Alias
data Aliased expression aliased where
  As
    :: KnownSymbol alias
    => expression ty
    -> Alias alias
    -> Aliased expression (alias ::: ty)
deriving instance Show (expression ty)
  => Show (Aliased expression (alias ::: ty))
deriving instance Eq (expression ty)
  => Eq (Aliased expression (alias ::: ty))
deriving instance Ord (expression ty)
  => Ord (Aliased expression (alias ::: ty))
instance (alias0 ~ alias1, alias0 ~ alias2, KnownSymbol alias2)
  => IsLabel alias0 (Aliased Alias (alias1 ::: alias2)) where
    fromLabel = fromLabel @alias2 `As` fromLabel @alias1

-- | >>> let renderMaybe = fromString . maybe "Nothing" (const "Just")
-- >>> renderAliasedAs renderMaybe (Just (3::Int) `As` #an_int)
-- "Just AS \"an_int\""
renderAliasedAs
  :: (forall ty. expression ty -> ByteString)
  -> Aliased expression aliased
  -> ByteString
renderAliasedAs render (expression `As` alias) =
  render expression <> " AS " <> renderAlias alias

-- | `AliasesOf` retains the AliasesOf in a row.
type family AliasesOf aliaseds where
  AliasesOf '[] = '[]
  AliasesOf (alias ::: ty ': tys) = alias ': AliasesOf tys

-- | The `ZipAliased` class provides a type family for zipping
-- `Symbol` lists together with arbitrary lists of the same size,
-- with an associated type family `ZipAs`, together with
-- a method `zipAs` for zipping heterogeneous lists of `Alias`es
-- together with a heterogeneous list of expressions into
-- a heterogeneous list of `Aliased` expressions.
class
  ( SListI (ZipAs ns xs)
  , All KnownSymbol ns
  ) => ZipAliased ns xs where

  type family ZipAs
    (ns :: [Symbol]) (xs :: [k]) = (zs :: [(Symbol,k)]) | zs -> ns xs

  zipAs
    :: NP Alias ns
    -> NP expr xs
    -> NP (Aliased expr) (ZipAs ns xs)

instance ZipAliased '[] '[] where
  type ZipAs '[] '[] = '[]
  zipAs Nil Nil = Nil

instance
  ( KnownSymbol n
  , ZipAliased ns xs
  ) => ZipAliased (n ': ns) (x ': xs) where
  type ZipAs (n ': ns) (x ': xs) = '(n,x) ': ZipAs ns xs
  zipAs (n :* ns) (x :* xs) = x `As` n :* zipAs ns xs

-- | @HasUnique alias fields field@ is a constraint that proves that
-- @fields@ is a singleton of @alias ::: field@.
type HasUnique alias fields field = fields ~ '[alias ::: field]

-- | @Has alias fields field@ is a constraint that proves that
-- @fields@ has a field of @alias ::: field@, inferring @field@
-- from @alias@ and @fields@.
class KnownSymbol alias =>
  Has (alias :: Symbol) (fields :: [(Symbol,kind)]) (field :: kind)
  | alias fields -> field where
instance {-# OVERLAPPING #-} KnownSymbol alias
  => Has alias (alias ::: field ': fields) field
instance {-# OVERLAPPABLE #-} (KnownSymbol alias, Has alias fields field)
  => Has alias (field' ': fields) field

-- | `HasAll` extends `Has` to take lists of @aliases@ and @fields@ and infer
-- a list of @subfields@.
class
  ( All KnownSymbol aliases
  ) => HasAll
    (aliases :: [Symbol])
    (fields :: [(Symbol,kind)])
    (subfields :: [(Symbol,kind)])
    | aliases fields -> subfields where
instance {-# OVERLAPPING #-} HasAll '[] fields '[]
instance {-# OVERLAPPABLE #-}
  (Has alias fields field, HasAll aliases fields subfields)
  => HasAll (alias ': aliases) fields (alias ::: field ': subfields)

-- | Analagous to `IsLabel`, the constraint
-- `IsQualified` defines `!` for a column alias qualified
-- by a table alias.
class IsQualified table column expression where
  (!) :: Alias table -> Alias column -> expression
  infixl 9 !
instance IsQualified table column (Alias table, Alias column) where (!) = (,)

-- | @Elem@ is a promoted `Data.List.elem`.
type family Elem x xs where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

-- | @In x xs@ is a constraint that proves that @x@ is in @xs@.
type family In x xs :: Constraint where In x xs = Elem x xs ~ 'True

-- | `PGNum` is a constraint on `PGType` whose
-- `Squeal.PostgreSQL.Expression.Expression`s have a `Num` constraint.
type PGNum ty =
  In ty '[ 'PGint2, 'PGint4, 'PGint8, 'PGnumeric, 'PGfloat4, 'PGfloat8]

-- | `PGFloating` is a constraint on `PGType` whose
-- `Squeal.PostgreSQL.Expression.Expression`s
-- have `Fractional` and `Floating` constraints.
type PGFloating ty = In ty '[ 'PGfloat4, 'PGfloat8, 'PGnumeric]

-- | `PGIntegral` is a constraint on `PGType` whose
-- `Squeal.PostgreSQL.Expression.Expression`s
-- have `Squeal.PostgreSQL.Expression.div_` and
-- `Squeal.PostgreSQL.Expression.mod_` functions.
type PGIntegral ty = In ty '[ 'PGint2, 'PGint4, 'PGint8]

-- | Error message helper for displaying unavailable\/unknown\/placeholder type
-- variables whose kind is known.
type Placeholder k = 'Text "(_::" :<>: 'ShowType k :<>: 'Text ")"

type ErrArrayOf arr ty = arr :<>: 'Text " " :<>: ty
type ErrPGfixarrayOf t = ErrArrayOf ('ShowType 'PGfixarray :<>: 'Text " " :<>: Placeholder Nat) t
type ErrPGvararrayOf t = ErrArrayOf ('ShowType 'PGvararray) t

-- | Ensure a type is a valid array type.
type family PGarray name arr :: Constraint where
  PGarray name ('PGvararray x) = ()
  PGarray name ('PGfixarray n x) = ()
  PGarray name val = TypeError
    ('Text name :<>: 'Text ": Unsatisfied PGarray constraint. Expected either: "
     :$$: 'Text " • " :<>: ErrPGvararrayOf (Placeholder PGType)
     :$$: 'Text " • " :<>: ErrPGfixarrayOf (Placeholder PGType)
     :$$: 'Text "But got: " :<>: 'ShowType val)

-- | Ensure a type is a valid array type with a specific element type.
type family PGarrayOf name arr ty :: Constraint where
  PGarrayOf name ('PGvararray x) ty = x ~ ty
  PGarrayOf name ('PGfixarray n x) ty = x ~ ty
  PGarrayOf name val ty = TypeError
    ( 'Text name :<>: 'Text "Unsatisfied PGarrayOf constraint. Expected either: "
      :$$: 'Text " • " :<>: ErrPGvararrayOf ( 'ShowType ty )
      :$$: 'Text " • " :<>: ErrPGfixarrayOf ( 'ShowType ty )
      :$$: 'Text "But got: " :<>: 'ShowType val)

-- | Ensure a type is a valid array type whose elements are text.
type PGtextArray name arr = PGarrayOf name arr 'PGtext

-- | `PGTypeOf` forgets about @NULL@ and any column constraints.
type family PGTypeOf (ty :: NullityType) :: PGType where
  PGTypeOf (nullity pg) = pg

-- | `SameTypes` is a constraint that proves two `ColumnsType`s have the same
-- length and the same `ColumnType`s.
type family SameTypes (columns0 :: ColumnsType) (columns1 :: ColumnsType)
  :: Constraint where
  SameTypes '[] '[] = ()
  SameTypes (column0 ::: def0 :=> ty0 ': columns0) (column1 ::: def1 :=> ty1 ': columns1)
    = (ty0 ~ ty1, SameTypes columns0 columns1)

-- | Equality constraint on the underlying `PGType` of two columns.
class SamePGType
  (ty0 :: (Symbol,ColumnType)) (ty1 :: (Symbol,ColumnType)) where
instance ty0 ~ ty1 => SamePGType
  (alias0 ::: def0 :=> nullity0 ty0)
  (alias1 ::: def1 :=> nullity1 ty1)

-- | `AllNotNull` is a constraint that proves a `ColumnsType` has no @NULL@s.
type family AllNotNull (columns :: ColumnsType) :: Constraint where
  AllNotNull '[] = ()
  AllNotNull (column ::: def :=> 'NotNull ty ': columns) = AllNotNull columns

-- | `NotAllNull` is a constraint that proves a `ColumnsType` has some
-- @NOT NULL@.
type family NotAllNull (columns :: ColumnsType) :: Constraint where
  NotAllNull (column ::: def :=> 'NotNull ty ': columns) = ()
  NotAllNull (column ::: def :=> 'Null ty ': columns) = NotAllNull columns

-- | `NullifyType` is an idempotent that nullifies a `ColumnType`.
type family NullifyType (ty :: NullityType) :: NullityType where
  NullifyType ('Null ty) = 'Null ty
  NullifyType ('NotNull ty) = 'Null ty

-- | `NullifyRelation` is an idempotent that nullifies a `ColumnsType`.
type family NullifyRelation (columns :: RelationType) :: RelationType where
  NullifyRelation '[] = '[]
  NullifyRelation (column ::: ty ': columns) =
    column ::: NullifyType ty ': NullifyRelation columns

-- | `NullifyRelations` is an idempotent that nullifies a `RelationsType`
-- used to nullify the left or right hand side of an outer join
-- in a `Squeal.PostgreSQL.Query.FromClause`.
type family NullifyRelations (tables :: RelationsType) :: RelationsType where
  NullifyRelations '[] = '[]
  NullifyRelations (table ::: columns ': tables) =
    table ::: NullifyRelation columns ': NullifyRelations tables

-- | 'RelationToRowType' drops the nullity constraints of its argument relations.
type family RelationToRowType (tables :: RelationType) :: [(Symbol, PGType)] where
  RelationToRowType (nullity x : xs) = x : RelationToRowType xs
  RelationToRowType '[] = '[]

type family RelationToNullityTypes (rel :: RelationType) :: [NullityType] where
  RelationToNullityTypes ('(k, x) : xs) = x : RelationToNullityTypes xs
  RelationToNullityTypes '[]            = '[]

-- | `Join` is simply promoted `++` and is used in @JOIN@s in
-- `Squeal.PostgreSQL.Query.FromClause`s.
type family Join xs ys where
  Join '[] ys = ys
  Join (x ': xs) ys = x ': Join xs ys

-- | @Create alias x xs@ adds @alias ::: x@ to the end of @xs@ and is used in
-- `Squeal.PostgreSQL.Definition.createTable` statements and in @ALTER TABLE@
-- `Squeal.PostgreSQL.Definition.addColumn`.
type family Create alias x xs where
  Create alias x '[] = '[alias ::: x]
  Create alias x (alias ::: y ': xs) = TypeError
    ('Text "Create: alias "
    ':<>: 'ShowType alias
    ':<>: 'Text "already in use")
  Create alias y (x ': xs) = x ': Create alias y xs

-- | @Drop alias xs@ removes the type associated with @alias@ in @xs@
-- and is used in `Squeal.PostgreSQL.Definition.dropTable` statements
-- and in @ALTER TABLE@ `Squeal.PostgreSQL.Definition.dropColumn` statements.
type family Drop alias xs where
  Drop alias ((alias ::: x) ': xs) = xs
  Drop alias (x ': xs) = x ': Drop alias xs

-- | @Alter alias x xs@ replaces the type associated with an @alias@ in @xs@
-- with the type `x` and is used in `Squeal.PostgreSQL.Definition.alterTable`
-- and `Squeal.PostgreSQL.Definition.alterColumn`.
type family Alter alias x xs where
  Alter alias x1 (alias ::: x0 ': xs) = alias ::: x1 ': xs
  Alter alias x1 (x0 ': xs) = x0 ': Alter alias x1 xs

-- type family AddConstraint constraint ty where
--   AddConstraint constraint (constraints :=> ty)
--     = AsSet (constraint ': constraints) :=> ty

-- type family DeleteFromList (e :: elem) (list :: [elem]) where
--   DeleteFromList elem '[] = '[]
--   DeleteFromList elem (x ': xs) =
--     If (Cmp elem x == 'EQ) xs (x ': DeleteFromList elem xs)

-- type family DropConstraint constraint ty where
--   DropConstraint constraint (constraints :=> ty)
--     = (AsSet (DeleteFromList constraint constraints)) :=> ty

-- | @Rename alias0 alias1 xs@ replaces the alias @alias0@ by @alias1@ in @xs@
-- and is used in `Squeal.PostgreSQL.Definition.alterTableRename` and
-- `Squeal.PostgreSQL.Definition.renameColumn`.
type family Rename alias0 alias1 xs where
  Rename alias0 alias1 ((alias0 ::: x0) ': xs) = (alias1 ::: x0) ': xs
  Rename alias0 alias1 (x ': xs) = x ': Rename alias0 alias1 xs

-- | `MapMaybes` is used in the binary instances of composite types.
class MapMaybes xs where
  type family Maybes (xs :: [Type]) = (mxs :: [Type]) | mxs -> xs
  maybes :: NP Maybe xs -> NP I (Maybes xs)
  unMaybes :: NP I (Maybes xs) -> NP Maybe xs
instance MapMaybes '[] where
  type Maybes '[] = '[]
  maybes Nil = Nil
  unMaybes Nil = Nil
instance MapMaybes xs => MapMaybes (x ': xs) where
  type Maybes (x ': xs) = Maybe x ': Maybes xs
  maybes (x :* xs) = I x :* maybes xs
  unMaybes (I mx :* xs) = mx :* unMaybes xs

-- | `Nulls` is used to construct a `Squeal.Postgresql.Expression.row`
-- of a composite type.
type family Nulls tys where
  Nulls '[] = '[]
  Nulls (field ::: ty ': tys) = field ::: 'Null ty ': Nulls tys

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
  | View RelationType
  | Typedef PGType

-- | The schema of a database consists of a list of aliased,
-- user-defined `SchemumType`s.
type SchemaType = [(Symbol,SchemumType)]

-- | Used in `Squeal.Postgresql.Manipulation.with`.
type family With
  (relations :: RelationsType)
  (schema :: SchemaType)
  :: SchemaType where
    With '[] schema = schema
    With (alias ::: rel ': rels) schema =
      alias ::: 'View rel ': With rels schema

-- | `IsPGlabel` looks very much like the `IsLabel` class. Whereas
-- the overloaded label, `fromLabel` is used for column references,
-- `label`s are used for enum terms. A `label` is called with
-- type application like `label @"beef"`.
class IsPGlabel (label :: Symbol) expr where label :: expr
instance label ~ label'
  => IsPGlabel label (PGlabel label') where label = PGlabel
-- | A `PGlabel` unit type with an `IsPGlabel` instance
data PGlabel (label :: Symbol) = PGlabel
-- | Renders a label
renderLabel :: KnownSymbol label => proxy label -> ByteString
renderLabel (_ :: proxy label) =
  "\'" <> fromString (symbolVal (Proxy @label)) <> "\'"
-- | Renders a list of labels
renderLabels
  :: All KnownSymbol labels => NP PGlabel labels -> [ByteString]
renderLabels = hcollapse
  . hcmap (Proxy @KnownSymbol) (K . renderLabel)

-- | The `PG` type family embeds a subset of Haskell types
-- as Postgres basic types.
--
-- >>> :kind! PG LocalTime
-- PG LocalTime :: PGType
-- = 'PGtimestamp
type family PG (hask :: Type) :: PGType where
  PG Bool = 'PGbool
  PG Int16 = 'PGint2
  PG Int32 = 'PGint4
  PG Int64 = 'PGint8
  PG Word16 = 'PGint2
  PG Word32 = 'PGint4
  PG Word64 = 'PGint8
  PG Scientific = 'PGnumeric
  PG Float = 'PGfloat4
  PG Double = 'PGfloat8
  PG Char = 'PGchar 1
  PG Text = 'PGtext
  PG Lazy.Text = 'PGtext
  PG ByteString = 'PGbytea
  PG Lazy.ByteString = 'PGbytea
  PG LocalTime = 'PGtimestamp
  PG UTCTime = 'PGtimestamptz
  PG Day = 'PGdate
  PG TimeOfDay = 'PGtime
  PG (TimeOfDay, TimeZone) = 'PGtimetz
  PG DiffTime = 'PGinterval
  PG UUID = 'PGuuid
  PG (NetAddr IP) = 'PGinet
  PG Value = 'PGjson
  PG ty = TypeError
    ('Text "There is no Postgres basic type for " ':<>: 'ShowType ty)

-- | The `EnumFrom` type family embeds Haskell enum types, ADTs with
-- nullary constructors, as Postgres enum types
--
-- >>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
-- >>> instance Generic Schwarma
-- >>> instance HasDatatypeInfo Schwarma
-- >>> :kind! EnumFrom Schwarma
-- EnumFrom Schwarma :: PGType
-- = 'PGenum '["Beef", "Lamb", "Chicken"]
type family EnumFrom (hask :: Type) :: PGType where
  EnumFrom hask = 'PGenum (LabelsFrom hask)

-- | The `LabelsFrom` type family calculates the constructors of a
-- Haskell enum type.
--
-- >>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
-- >>> instance Generic Schwarma
-- >>> instance HasDatatypeInfo Schwarma
-- >>> :kind! LabelsFrom Schwarma
-- LabelsFrom Schwarma :: [Type.ConstructorName]
-- = '["Beef", "Lamb", "Chicken"]
type family LabelsFrom (hask :: Type) :: [Type.ConstructorName] where
  LabelsFrom hask =
    ConstructorNamesOf (ConstructorsOf (DatatypeInfoOf hask))

-- | The `CompositeFrom` type family embeds Haskell record types as
-- Postgres composite types, as long as the record fields
-- are `Maybe`s of Haskell types that can be embedded as basic types
-- with the `PG` type family.
--
-- >>> data Row = Row { a :: Maybe Int16, b :: Maybe LocalTime } deriving GHC.Generic
-- >>> instance Generic Row
-- >>> instance HasDatatypeInfo Row
-- >>> :kind! CompositeFrom Row
-- CompositeFrom Row :: PGType
-- = 'PGcomposite '['("a", 'PGint2), '("b", 'PGtimestamp)]
type family CompositeFrom (hask :: Type) :: PGType where
  CompositeFrom hask =
    'PGcomposite (ZipAs (FieldNamesFrom hask) (FieldTypesFrom hask))

-- | >>> data Row = Row { a :: Maybe Int16, b :: Maybe LocalTime } deriving GHC.Generic
-- >>> instance Generic Row
-- >>> instance HasDatatypeInfo Row
-- >>> :kind! FieldNamesFrom Row
-- FieldNamesFrom Row :: [Type.FieldName]
-- = '["a", "b"]
type family FieldNamesFrom (hask :: Type) :: [Type.FieldName] where
  FieldNamesFrom hask = FieldNamesOf (FieldsOf (DatatypeInfoOf hask))

-- | >>> data Row = Row { a :: Maybe Int16, b :: Maybe LocalTime } deriving GHC.Generic
-- >>> instance Generic Row
-- >>> instance HasDatatypeInfo Row
-- >>> :kind! FieldTypesFrom Row
-- FieldTypesFrom Row :: [PGType]
-- = '['PGint2, 'PGtimestamp]
type family FieldTypesFrom (hask :: Type) :: [PGType] where
  FieldTypesFrom hask = FieldTypesOf (RecordCodeOf hask (Code hask))

-- | Calculates constructors of a datatype.
type family ConstructorsOf (datatype :: Type.DatatypeInfo)
  :: [Type.ConstructorInfo] where
    ConstructorsOf ('Type.ADT _module _datatype constructors) =
      constructors
    ConstructorsOf ('Type.Newtype _module _datatype constructor) =
      '[constructor]

-- | Calculates the name of a nullary constructor, otherwise
-- generates a type error.
type family ConstructorNameOf (constructors :: Type.ConstructorInfo)
  :: Type.ConstructorName where
    ConstructorNameOf ('Type.Constructor name) = name
    ConstructorNameOf ('Type.Infix name _assoc _fix) = TypeError
      ('Text "ConstructorNameOf error: non-nullary constructor "
        ':<>: 'Text name)
    ConstructorNameOf ('Type.Record name _fields) = TypeError
      ('Text "ConstructorNameOf error: non-nullary constructor "
        ':<>: 'Text name)

-- | Calculate the names of nullary constructors.
type family ConstructorNamesOf (constructors :: [Type.ConstructorInfo])
  :: [Type.ConstructorName] where
    ConstructorNamesOf '[] = '[]
    ConstructorNamesOf (constructor ': constructors) =
      ConstructorNameOf constructor ': ConstructorNamesOf constructors

-- | Calculate the fields of a datatype.
type family FieldsOf (datatype :: Type.DatatypeInfo)
  :: [Type.FieldInfo] where
    FieldsOf ('Type.ADT _module _datatype '[ 'Type.Record _name fields]) =
      fields
    FieldsOf ('Type.Newtype _module _datatype ('Type.Record _name fields)) =
      fields

-- | Calculate the name of a field.
type family FieldNameOf (field :: Type.FieldInfo) :: Type.FieldName where
  FieldNameOf ('Type.FieldInfo name) = name

-- | Calculate the names of fields.
type family FieldNamesOf (fields :: [Type.FieldInfo])
  :: [Type.FieldName] where
    FieldNamesOf '[] = '[]
    FieldNamesOf (field ': fields) = FieldNameOf field ': FieldNamesOf fields

-- | >>> :kind! FieldTypeOf (Maybe Int16)
-- FieldTypeOf (Maybe Int16) :: PGType
-- = 'PGint2
type family FieldTypeOf (maybe :: Type) where
  FieldTypeOf (Maybe hask) = PG hask
  FieldTypeOf ty = TypeError
    ('Text "FieldTypeOf error: non-Maybe type " ':<>: 'ShowType ty)

-- | Calculate the types of fields.
type family FieldTypesOf (fields :: [Type]) where
  FieldTypesOf '[] = '[]
  FieldTypesOf (field ': fields) = FieldTypeOf field ': FieldTypesOf fields

-- | Inspect the code of an algebraic datatype and ensure it's a product,
-- otherwise generate a type error
type family RecordCodeOf (hask :: Type) (code ::[[Type]]) :: [Type] where
  RecordCodeOf _hask '[tys] = tys
  RecordCodeOf hask _tys = TypeError
    ('Text "RecordCodeOf error: non-Record type " ':<>: 'ShowType hask)

-- | Is a type a valid JSON key?
type PGjsonKey key = key `In` '[ 'PGint2, 'PGint4, 'PGtext ]

-- | Is a type a valid JSON type?
type PGjson_ json = json `In` '[ 'PGjson, 'PGjsonb ]


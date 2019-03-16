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

module Squeal.PostgreSQL.Schema
  ( -- * Postgres Types
    PGType (..)
  , NullityType (..)
  , RowType
  , FromType
    -- * Schema Types
  , ColumnType
  , ColumnsType
  , TableType
  , SchemumType (..)
  , SchemaType
  , SchemasType
  , Public
    -- * Constraints
  , (:=>)
  , ColumnConstraint (..)
  , TableConstraint (..)
  , TableConstraints
  , Uniquely
    -- * Aliases
  , (:::)
  , Alias (..)
  , IsLabel (..)
  , Aliased (As)
  , Aliasable (as)
  , renderAliased
  , Has
  , HasUnique
  , HasAll
  , QualifiedAlias (..)
  , IsQualified (..)
  , HasIn
  , IsNotElem
  , AllUnique
  , DefaultAliasable (..)
    -- * Enumerated Labels
  , IsPGlabel (..)
  , PGlabel (..)
    -- * Grouping
  , Grouping (..)
  , GroupedBy
    -- * Aligned lists
  , AlignedList (..)
  , single
    -- * Data Definitions
  , Create
  , Drop
  , Alter
  , Rename
  , DropIfConstraintsInvolve
    -- * Lists
  , Join
  , disjoin
  , Elem
  , In
  , Length
  , (*:)
    -- * Type Classifications
  , HasOid (..)
  , PGNum
  , PGIntegral
  , PGFloating
  , PGTypeOf
  , PGArrayOf
  , PGArray
  , PGTextArray
  , PGJsonType
  , PGJsonKey
  , SamePGType
  , AllNotNull
  , NotAllNull
    -- * Nullifications
  , NullifyType
  , NullifyRow
  , NullifyFrom
    -- * Table Conversions
  , TableToColumns
  , ColumnsToRow
  , TableToRow
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Kind
import Data.Monoid hiding (All)
import Data.String
import Data.Type.Bool
import Data.Word (Word32)
import Generics.SOP
import GHC.OverloadedLabels
import GHC.TypeLits
import Prelude hiding (id, (.))

import qualified GHC.Generics as GHC

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
  | PGvararray NullityType -- ^ variable length array
  | PGfixarray [Nat] NullityType -- ^ fixed length array
  | PGenum [Symbol] -- ^ enumerated (enum) types are data types that comprise a static, ordered set of values.
  | PGcomposite RowType -- ^ a composite type represents the structure of a row or record; it is essentially just a list of field names and their data types.
  | PGtsvector -- ^ A tsvector value is a sorted list of distinct lexemes, which are words that have been normalized to merge different variants of the same word.
  | PGtsquery -- ^ A tsquery value stores lexemes that are to be searched for
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

{- | A `RowType` is a row of `NullityType`. They correspond to Haskell
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
type RowType = [(Symbol,NullityType)]

{- | `FromType` is a row of `RowType`s. It can be thought of as
a product, or horizontal gluing and is used in `Squeal.PostgreSQL.Query.FromClause`s
and `Squeal.PostgreSQL.Query.TableExpression`s.
-}
type FromType = [(Symbol,RowType)]

-- | `ColumnsToRow` removes column constraints.
type family ColumnsToRow (columns :: ColumnsType) :: RowType where
  ColumnsToRow '[] = '[]
  ColumnsToRow (column ::: constraint :=> ty ': columns) =
    column ::: ty ': ColumnsToRow columns

-- | `TableToColumns` removes table constraints.
type family TableToColumns (table :: TableType) :: ColumnsType where
  TableToColumns (constraints :=> columns) = columns

-- | Convert a table to a row type.
type family TableToRow (table :: TableType) :: RowType where
  TableToRow tab = ColumnsToRow (TableToColumns tab)

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
class (KnownSymbol table, KnownSymbol column)
  => GroupedBy table column bys where
instance {-# OVERLAPPING #-} (KnownSymbol table, KnownSymbol column)
  => GroupedBy table column ('(table,column) ': bys)
instance {-# OVERLAPPABLE #-}
  ( KnownSymbol table
  , KnownSymbol column
  , GroupedBy table column bys
  ) => GroupedBy table column (tabcol ': bys)

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
-- | >>> printSQL (#jimbob :: Alias "jimbob")
-- "jimbob"
instance KnownSymbol alias => RenderSQL (Alias alias) where
  renderSQL = doubleQuoted . fromString . symbolVal

-- | >>> import Generics.SOP (NP(..))
-- >>> printSQL (#jimbob :* #kandi :: NP Alias '["jimbob", "kandi"])
-- "jimbob", "kandi"
instance All KnownSymbol aliases => RenderSQL (NP Alias aliases) where
  renderSQL
    = commaSeparated
    . hcollapse
    . hcmap (Proxy @KnownSymbol) (K . renderSQL)

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

-- | The `Aliasable` class provides a way to scrap your `Nil`s
-- in an `NP` list of `Aliased` expressions.
class KnownSymbol alias => Aliasable alias expression aliased
  | aliased -> expression
  , aliased -> alias
  where as :: expression -> Alias alias -> aliased
instance (KnownSymbol alias, aliased ~ (alias ::: ty)) => Aliasable alias
  (expression ty)
  (Aliased expression aliased)
    where
      as = As
instance (KnownSymbol alias, tys ~ '[alias ::: ty]) => Aliasable alias
  (expression ty)
  (NP (Aliased expression) tys)
    where
      expression `as` alias = expression `As` alias :* Nil

-- | >>> let renderMaybe = fromString . maybe "Nothing" (const "Just")
-- >>> renderAliased renderMaybe (Just (3::Int) `As` #an_int)
-- "Just AS \"an_int\""
renderAliased
  :: (forall ty. expression ty -> ByteString)
  -> Aliased expression aliased
  -> ByteString
renderAliased render (expression `As` alias) =
  render expression <> " AS " <> renderSQL alias

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

{-| @HasIn fields (alias ::: field)@ is a constraint that proves that
@fields@ has a field of @alias ::: field@. It is used in @UPDATE@s to
choose which subfields to update.
-}
class HasIn fields field where
instance (Has alias fields field) => HasIn fields (alias ::: field) where

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

{- |
The `DefaultAliasable` class is intended to help with Scrap your Nils
for default inserts and updates.
-}
class KnownSymbol alias
  => DefaultAliasable alias aliased | aliased -> alias where
    defaultAs :: Alias alias -> aliased

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

{-| `QualifiedAlias`es enables multi-schema support by allowing a reference
to a `Table`, `Typedef` or `View` to be qualified by their schemas. By default,
a qualifier of @public@ is provided.

>>> :{
let
  alias1 :: QualifiedAlias "sch" "tab"
  alias1 = #sch ! #tab
  alias2 :: QualifiedAlias "public" "vw"
  alias2 = #vw
in printSQL alias1 >> printSQL alias2
:}
"sch"."tab"
"vw"
-}
data QualifiedAlias (qualifier :: Symbol) (alias :: Symbol) = QualifiedAlias
  deriving (Eq,GHC.Generic,Ord,Show,NFData)
instance (q ~ q', a ~ a') => IsQualified q a (QualifiedAlias q' a') where
  _!_ = QualifiedAlias
instance (q' ~ "public", a ~ a') => IsLabel a (QualifiedAlias q' a') where
  fromLabel = QualifiedAlias
instance (q0 ~ q1, a0 ~ a1, a1 ~ a2, KnownSymbol a2) =>
  IsQualified q0 a0 (Aliased (QualifiedAlias q1) (a1 ::: a2)) where
    _!_ = QualifiedAlias `As` Alias
instance (q ~ "public", a0 ~ a1, a1 ~ a2, KnownSymbol a2) =>
  IsLabel a0 (Aliased (QualifiedAlias q) (a1 ::: a2)) where
    fromLabel = QualifiedAlias `As` Alias

instance (KnownSymbol q, KnownSymbol a)
  => RenderSQL (QualifiedAlias q a) where
    renderSQL _ =
      let
        qualifier = renderSQL (Alias @q)
        alias = renderSQL (Alias @a)
      in
        if qualifier == "\"public\"" then alias else qualifier <> "." <> alias

-- | @Elem@ is a promoted `Data.List.elem`.
type family Elem x xs where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

-- | @In x xs@ is a constraint that proves that @x@ is in @xs@.
type family In x xs :: Constraint where In x xs = Elem x xs ~ 'True

-- | Numeric Postgres types.
type PGNum =
  '[ 'PGint2, 'PGint4, 'PGint8, 'PGnumeric, 'PGfloat4, 'PGfloat8]

-- | Floating Postgres types.
type PGFloating = '[ 'PGfloat4, 'PGfloat8, 'PGnumeric]

-- | Integral Postgres types.
type PGIntegral = '[ 'PGint2, 'PGint4, 'PGint8]

-- | Error message helper for displaying unavailable\/unknown\/placeholder type
-- variables whose kind is known.
type Placeholder k = 'Text "(_::" :<>: 'ShowType k :<>: 'Text ")"

type ErrArrayOf arr ty = arr :<>: 'Text " " :<>: ty
type ErrPGfixarrayOf t = ErrArrayOf ('ShowType 'PGfixarray :<>: 'Text " " :<>: Placeholder Nat) t
type ErrPGvararrayOf t = ErrArrayOf ('ShowType 'PGvararray) t

-- | Ensure a type is a valid array type.
type family PGArray name arr :: Constraint where
  PGArray name ('PGvararray x) = ()
  PGArray name ('PGfixarray n x) = ()
  PGArray name val = TypeError
    ('Text name :<>: 'Text ": Unsatisfied PGArray constraint. Expected either: "
     :$$: 'Text " • " :<>: ErrPGvararrayOf (Placeholder PGType)
     :$$: 'Text " • " :<>: ErrPGfixarrayOf (Placeholder PGType)
     :$$: 'Text "But got: " :<>: 'ShowType val)

-- | Ensure a type is a valid array type with a specific element type.
type family PGArrayOf name arr ty :: Constraint where
  PGArrayOf name ('PGvararray x) ty = x ~ ty
  PGArrayOf name ('PGfixarray n x) ty = x ~ ty
  PGArrayOf name val ty = TypeError
    ( 'Text name :<>: 'Text "Unsatisfied PGArrayOf constraint. Expected either: "
      :$$: 'Text " • " :<>: ErrPGvararrayOf ( 'ShowType ty )
      :$$: 'Text " • " :<>: ErrPGfixarrayOf ( 'ShowType ty )
      :$$: 'Text "But got: " :<>: 'ShowType val)

-- | Ensure a type is a valid array type whose elements are text.
type PGTextArray name arr = PGArrayOf name arr ('NotNull 'PGtext)

-- | `PGTypeOf` forgets about @NULL@ and any column constraints.
type family PGTypeOf (ty :: NullityType) :: PGType where
  PGTypeOf (nullity pg) = pg

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

-- | `NullifyType` is an idempotent that nullifies a `NullityType`.
type family NullifyType (ty :: NullityType) :: NullityType where
  NullifyType ('Null ty) = 'Null ty
  NullifyType ('NotNull ty) = 'Null ty

-- | `NullifyRow` is an idempotent that nullifies a `RowType`.
type family NullifyRow (columns :: RowType) :: RowType where
  NullifyRow '[] = '[]
  NullifyRow (column ::: ty ': columns) =
    column ::: NullifyType ty ': NullifyRow columns

-- | `NullifyFrom` is an idempotent that nullifies a `FromType`
-- used to nullify the left or right hand side of an outer join
-- in a `Squeal.PostgreSQL.Query.FromClause`.
type family NullifyFrom (tables :: FromType) :: FromType where
  NullifyFrom '[] = '[]
  NullifyFrom (table ::: columns ': tables) =
    table ::: NullifyRow columns ': NullifyFrom tables

-- | `Join` is simply promoted `++` and is used in @JOIN@s in
-- `Squeal.PostgreSQL.Query.FromClause`s.
type family Join xs ys where
  Join '[] ys = ys
  Join (x ': xs) ys = x ': Join xs ys

-- | `disjoin` is a utility function for splitting an `NP` list into pieces.
disjoin
 :: forall xs ys expr. SListI xs
 => NP expr (Join xs ys)
 -> (NP expr xs, NP expr ys)
disjoin = case sList @xs of
  SNil -> \ys -> (Nil, ys)
  SCons -> \(x :* xsys) ->
    case disjoin xsys of (xs,ys) -> (x :* xs, ys)

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
-- with the type @x@ and is used in `Squeal.PostgreSQL.Definition.alterTable`
-- and `Squeal.PostgreSQL.Definition.alterColumn`.
type family Alter alias x xs where
  Alter alias x1 (alias ::: x0 ': xs) = alias ::: x1 ': xs
  Alter alias x1 (x0 ': xs) = x0 ': Alter alias x1 xs

-- | @Rename alias0 alias1 xs@ replaces the alias @alias0@ by @alias1@ in @xs@
-- and is used in `Squeal.PostgreSQL.Definition.alterTableRename` and
-- `Squeal.PostgreSQL.Definition.renameColumn`.
type family Rename alias0 alias1 xs where
  Rename alias0 alias1 ((alias0 ::: x0) ': xs) = (alias1 ::: x0) ': xs
  Rename alias0 alias1 (x ': xs) = x ': Rename alias0 alias1 xs

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

{- | Calculate the `Length` of a type level list

>>> :kind! Length '[Char,String,Bool,Double]
Length '[Char,String,Bool,Double] :: Nat
= 4
-}
type family Length (xs :: [k]) :: Nat where
  Length (x : xs) = 1 + Length xs
  Length '[] = 0

-- | A `SchemumType` is a user-defined type, either a `Table`,
-- `View` or `Typedef`.
data SchemumType
  = Table TableType
  | View RowType
  | Typedef PGType

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

{-|

-}
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

-- | An `AlignedList` is a type-aligned list or free category.
data AlignedList p x0 x1 where
  Done :: AlignedList p x x
  (:>>) :: p x0 x1 -> AlignedList p x1 x2 -> AlignedList p x0 x2
infixr 7 :>>
instance Category (AlignedList p) where
  id = Done
  (.) list = \case
    Done -> list
    step :>> steps -> step :>> (steps >>> list)
instance (forall t0 t1. RenderSQL (p t0 t1))
  => RenderSQL (AlignedList p x0 x1) where
    renderSQL = \case
      Done -> ""
      step :>> Done -> renderSQL step
      step :>> steps -> renderSQL step <> ", " <> renderSQL steps

-- | A `single` step.
single :: p x0 x1 -> AlignedList p x0 x1
single step = step :>> Done

(*:) :: f x -> f y -> NP f '[x,y]
x *: y = x :* y :* Nil

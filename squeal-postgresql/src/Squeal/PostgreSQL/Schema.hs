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
    -- * Haskell to Postgres Types
  , PG
  , NullPG
  , TuplePG
  , RowPG
  , Json (..)
  , Jsonb (..)
  , Composite (..)
  , Enumerated (..)
    -- * Schema Types
  , ColumnType
  , ColumnsType
  , TableType
  , SchemumType (..)
  , SchemaType
  , DBType
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
  , renderAlias
  , renderAliasString
  , renderAliases
  , Aliased (As)
  , Aliasable (as)
  , renderAliasedAs
  , Has
  , HasUnique
  , HasAll
  , QualifiedAlias (..)
  , IsQualified (..)
  , renderQualifiedAlias
  , HasQualified
    -- * Enumerated Labels
  , IsPGlabel (..)
  , PGlabel (..)
  , renderLabel
  , renderLabels
  , LabelsPG
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
  , Elem
  , In
  , Length
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
  , TableToRow
  ) where

import Control.Category
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
import Data.Vector (Vector)
import Generics.SOP
import Generics.SOP.Record
import GHC.OverloadedLabels
import GHC.TypeLits
import Network.IP.Addr
import Prelude hiding (id, (.))

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
  | PGvararray NullityType -- ^ variable length array
  | PGfixarray Nat NullityType -- ^ fixed length array
  | PGenum [Symbol] -- ^ enumerated (enum) types are data types that comprise a static, ordered set of values.
  | PGcomposite RowType -- ^ a composite type represents the structure of a row or record; it is essentially just a list of field names and their data types.
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
record types by means of `RowPG` and are used in many places.

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
instance KnownSymbol alias => RenderSQL (Alias alias) where renderSQL = renderAlias

-- | >>> renderAlias #jimbob
-- "\"jimbob\""
renderAlias :: KnownSymbol alias => Alias alias -> ByteString
renderAlias = doubleQuoted . fromString . symbolVal

-- | >>> renderAliasString #ohmahgerd
-- "'ohmahgerd'"
renderAliasString :: KnownSymbol alias => Alias alias -> ByteString
renderAliasString = singleQuotedText . fromString . symbolVal

-- | >>> import Generics.SOP (NP(..))
-- >>> renderAliases (#jimbob :* #kandi)
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

-- | The `Aliasable` class provides a way to scrap your `Nil`s
-- in an `NP` list of `Aliased` expressions.
class KnownSymbol alias => Aliasable alias expression aliased
  | aliased -> expression
  , aliased -> alias
  where as :: expression -> Alias alias -> aliased
instance (KnownSymbol alias, alias ~ alias1) => Aliasable alias
  (expression ty)
  (Aliased expression (alias1 ::: ty))
    where
      as = As
instance (KnownSymbol alias, tys ~ '[alias ::: ty]) => Aliasable alias
  (expression ty)
  (NP (Aliased expression) tys)
    where
      expression `as` alias = expression `As` alias :* Nil

-- | >>> let renderMaybe = fromString . maybe "Nothing" (const "Just")
-- >>> renderAliasedAs renderMaybe (Just (3::Int) `As` #an_int)
-- "Just AS \"an_int\""
renderAliasedAs
  :: (forall ty. expression ty -> ByteString)
  -> Aliased expression aliased
  -> ByteString
renderAliasedAs render (expression `As` alias) =
  render expression <> " AS " <> renderAlias alias

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

data QualifiedAlias (qualifier :: Symbol) (alias :: Symbol) = QualifiedAlias
instance (q ~ q', a ~ a') => IsQualified q a (QualifiedAlias q' a') where
  _!_ = QualifiedAlias
instance (q' ~ "public", a ~ a') => IsLabel a (QualifiedAlias q' a') where
  fromLabel = QualifiedAlias

renderQualifiedAlias
  :: forall q a. (KnownSymbol q, KnownSymbol a)
  => QualifiedAlias q a -> ByteString
renderQualifiedAlias _ =
  let
    qualifier = renderAlias (Alias @q)
    alias = renderAlias (Alias @a)
  in
    if qualifier == "public" then alias else qualifier <> "." <> alias

class (KnownSymbol qualifier, KnownSymbol alias) => HasQualified
  (qualifier :: Symbol)
  (alias :: Symbol)
  (xss :: [(Symbol,[(Symbol, kind)])])
  (xs :: [(Symbol,kind)])
  (x :: kind)
  | qualifier xss -> xs
  , alias xs -> x where

instance
  ( KnownSymbol qualifier
  , KnownSymbol alias
  , Has qualifier xss xs
  , Has alias xs x
  ) => HasQualified qualifier alias xss xs x

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

type DBType = [(Symbol,SchemaType)]

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
-- | Renders a label
renderLabel :: KnownSymbol label => proxy label -> ByteString
renderLabel (_ :: proxy label) =
  "\'" <> renderSymbol @label <> "\'"
-- | Renders a list of labels
renderLabels
  :: All KnownSymbol labels => NP PGlabel labels -> [ByteString]
renderLabels = hcollapse
  . hcmap (Proxy @KnownSymbol) (K . renderLabel)

{- | The `PG` type family embeds a subset of Haskell types
as Postgres types. As an open type family, `PG` is extensible.

>>> :kind! PG LocalTime
PG LocalTime :: PGType
= 'PGtimestamp

>>> newtype MyDouble = My Double
>>> type instance PG MyDouble = 'PGfloat8
-}
type family PG (hask :: Type) :: PGType
type instance PG Bool = 'PGbool
type instance PG Int16 = 'PGint2
type instance PG Int32 = 'PGint4
type instance PG Int64 = 'PGint8
type instance PG Word16 = 'PGint2
type instance PG Word32 = 'PGint4
type instance PG Word64 = 'PGint8
type instance PG Scientific = 'PGnumeric
type instance PG Float = 'PGfloat4
type instance PG Double = 'PGfloat8
type instance PG Char = 'PGchar 1
type instance PG Text = 'PGtext
type instance PG Lazy.Text = 'PGtext
type instance PG String = 'PGtext
type instance PG ByteString = 'PGbytea
type instance PG Lazy.ByteString = 'PGbytea
type instance PG LocalTime = 'PGtimestamp
type instance PG UTCTime = 'PGtimestamptz
type instance PG Day = 'PGdate
type instance PG TimeOfDay = 'PGtime
type instance PG (TimeOfDay, TimeZone) = 'PGtimetz
type instance PG DiffTime = 'PGinterval
type instance PG UUID = 'PGuuid
type instance PG (NetAddr IP) = 'PGinet
type instance PG Value = 'PGjson
type instance PG (Json hask) = 'PGjson
type instance PG (Jsonb hask) = 'PGjsonb
type instance PG (Vector hask) = 'PGvararray (NullPG hask)
type instance PG (hask, hask) = 'PGfixarray 2 (NullPG hask)
type instance PG (hask, hask, hask) = 'PGfixarray 3 (NullPG hask)
type instance PG (hask, hask, hask, hask) = 'PGfixarray 4 (NullPG hask)
type instance PG (hask, hask, hask, hask, hask) = 'PGfixarray 5 (NullPG hask)
type instance PG (hask, hask, hask, hask, hask, hask)
  = 'PGfixarray 6 (NullPG hask)
type instance PG (hask, hask, hask, hask, hask, hask, hask)
  = 'PGfixarray 7 (NullPG hask)
type instance PG (hask, hask, hask, hask, hask, hask, hask, hask)
  = 'PGfixarray 8 (NullPG hask)
type instance PG (hask, hask, hask, hask, hask, hask, hask, hask, hask)
  = 'PGfixarray 9 (NullPG hask)
type instance PG (hask, hask, hask, hask, hask, hask, hask, hask, hask, hask)
  = 'PGfixarray 10 (NullPG hask)
type instance PG (Composite hask) = 'PGcomposite (RowPG hask)
type instance PG (Enumerated hask) = 'PGenum (LabelsPG hask)

{- | The `Json` newtype is an indication that the Haskell
type it's applied to should be stored as `PGjson`.
-}
newtype Json hask = Json {getJson :: hask}
  deriving (Eq, Ord, Show, Read, GHC.Generic)

{- | The `Jsonb` newtype is an indication that the Haskell
type it's applied to should be stored as `PGjsonb`.
-}
newtype Jsonb hask = Jsonb {getJsonb :: hask}
  deriving (Eq, Ord, Show, Read, GHC.Generic)

{- | The `Composite` newtype is an indication that the Haskell
type it's applied to should be stored as a `PGcomposite`.
-}
newtype Composite record = Composite {getComposite :: record}
  deriving (Eq, Ord, Show, Read, GHC.Generic)

{- | The `Enumerated` newtype is an indication that the Haskell
type it's applied to should be stored as `PGenum`.
-}
newtype Enumerated enum = Enumerated {getEnumerated :: enum}
  deriving (Eq, Ord, Show, Read, GHC.Generic)

{-| The `LabelsPG` type family calculates the constructors of a
Haskell enum type.

>>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
>>> instance Generic Schwarma
>>> instance HasDatatypeInfo Schwarma
>>> :kind! LabelsPG Schwarma
LabelsPG Schwarma :: [Type.ConstructorName]
= '["Beef", "Lamb", "Chicken"]
-}
type family LabelsPG (hask :: Type) :: [Type.ConstructorName] where
  LabelsPG hask =
    ConstructorNamesOf (ConstructorsOf (DatatypeInfoOf hask))

{- | `RowPG` turns a Haskell record type into a `RowType`.

>>> data Person = Person { name :: Text, age :: Int32 } deriving GHC.Generic
>>> instance Generic Person
>>> instance HasDatatypeInfo Person
>>> :kind! RowPG Person
RowPG Person :: [(Symbol, NullityType)]
= '["name" ::: 'NotNull 'PGtext, "age" ::: 'NotNull 'PGint4]
-}
type family RowPG (hask :: Type) :: RowType where
  RowPG hask = RowOf (RecordCodeOf hask)

type family RowOf (fields :: [(Symbol, Type)]) :: RowType where
  RowOf '[] = '[]
  RowOf (field ': fields) = FieldPG field ': RowOf fields

type family FieldPG (field :: (Symbol, Type)) :: (Symbol, NullityType) where
  FieldPG (field ::: hask) = field ::: NullPG hask

{- | `NullPG` turns a Haskell type into a `NullityType`.

>>> :kind! NullPG Double
NullPG Double :: NullityType
= 'NotNull 'PGfloat8
>>> :kind! NullPG (Maybe Double)
NullPG (Maybe Double) :: NullityType
= 'Null 'PGfloat8
-}
type family NullPG (hask :: Type) :: NullityType where
  NullPG (Maybe hask) = 'Null (PG hask)
  NullPG hask = 'NotNull (PG hask)

{- | `TuplePG` turns a Haskell tuple type (including record types) into
the corresponding list of `NullityType`s.

>>> :kind! TuplePG (Double, Maybe Char)
TuplePG (Double, Maybe Char) :: [NullityType]
= '['NotNull 'PGfloat8, 'Null ('PGchar 1)]
-}
type family TuplePG (hask :: Type) :: [NullityType] where
  TuplePG hask = TupleOf (TupleCodeOf hask (Code hask))

type family TupleOf (tuple :: [Type]) :: [NullityType] where
  TupleOf '[] = '[]
  TupleOf (hask ': tuple) = NullPG hask ': TupleOf tuple

type family TupleCodeOf (hask :: Type) (code :: [[Type]]) :: [Type] where
  TupleCodeOf hask '[tuple] = tuple
  TupleCodeOf hask '[] =
    TypeError
      (    'Text "The type `" :<>: 'ShowType hask :<>: 'Text "' is not a tuple type."
      :$$: 'Text "It is a void type with no constructors."
      )
  TupleCodeOf hask (_ ': _ ': _) =
    TypeError
      (    'Text "The type `" :<>: 'ShowType hask :<>: 'Text "' is not a tuple type."
      :$$: 'Text "It is a sum type with more than one constructor."
      )

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

-- | A `single` step.
single :: p x0 x1 -> AlignedList p x0 x1
single step = step :>> Done

{-|
Module: Squeal.PostgreSQL.Schema
Description: Embedding of PostgreSQL type and alias system
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Embedding of PostgreSQL type and alias system
-}

{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , DeriveAnyClass
  , DeriveDataTypeable
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MagicHash
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Schema
  ( -- * Kinds
    PGType (..)
  , NullityType (..)
  , ColumnType
  , ColumnsType
  , RelationType
  , TableType
  , SchemaType
  , Grouping (..)
    -- * Constraints
  , PGNum
  , PGIntegral
  , PGFloating
    -- * Aliases
  , (:::)
  , Alias (Alias)
  , renderAlias
  , Aliased (As)
  , renderAliased
  , IsLabel (..)
  , IsTableColumn (..)
    -- * Type Families
  , In
  , HasUnique
  , PGTypeOf
  , SameTypes
  , AllNotNull
  , NotAllNull
  , NullifyType
  , NullifyColumns
  , NullifyTables
  , Join
  , Create
  , Add
  , Drop
  , Alter
  , Rename
    -- * Generics
  , SameField
  , SameFields
    -- * PostgreSQL Constraints
  , (:=>)
  , Unconstrain
  , UnconstrainOver
  , ColumnConstraint (..)
  -- , DropDefault
  -- , DropDefaultList
  , AddConstraint
  , DropConstraint
  , TableConstraint' (..)
  , AsSet
  , Aliases
  ) where

import Control.DeepSeq
import Data.ByteString
import Data.Monoid
import Data.String
import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Set
import Generics.SOP (AllZip)
import GHC.Generics (Generic)
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits

import qualified Generics.SOP.Type.Metadata as Type

-- | `PGType` is the promoted datakind of PostgreSQL types.
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
  | UnsafePGType Symbol -- ^ an escape hatch for unsupported PostgreSQL types

-- | `NullityType` encodes the potential presence or definite absence of a
-- @NULL@ allowing operations which are sensitive to such to be well typed.
data NullityType
  = Null PGType -- ^ @NULL@ may be present
  | NotNull PGType -- ^ @NULL@ is absent

-- | `ColumnType` encodes the allowance of @DEFAULT@ and the only way
-- to generate an `Optional` `Squeal.PostgreSQL.Expression.Expression`
-- is to use `Squeal.PostgreSQL.Expression.def`,
-- `Squeal.PostgreSQL.Expression.unDef` or
-- `Squeal.PostgreSQL.Expression.param`.
type ColumnType = ([ColumnConstraint],NullityType)

-- | `ColumnsType` is a kind synonym for a row of `ColumnType`s.
type ColumnsType = [(Symbol,ColumnType)]

-- | `RelationType` is a kind synonym for a row of `ColumnsType`s.
-- It is used as a kind for both a schema, a disjoint union of tables,
-- and a joined table `Squeal.PostgreSQL.Query.FromClause`,
-- a product of tables.
type RelationType = [(Symbol,ColumnsType)]

type TableType = ([TableConstraint'],ColumnsType)
type SchemaType = [(Symbol,TableType)]

type family Unconstrain (ty :: ([constraint],kind)) :: kind where
  Unconstrain '(constraints,ty) = ty

type family UnconstrainOver (tys :: SchemaType) :: RelationType where
  UnconstrainOver '[] = '[]
  UnconstrainOver ((alias ::: x) ': xs) =
    (alias ::: Unconstrain x) ': UnconstrainOver xs

-- | `Grouping` is an auxiliary namespace, created by
-- @GROUP BY@ clauses (`Squeal.PostgreSQL.Query.group`), and used
-- for typesafe aggregation
data Grouping
  = Ungrouped
  | Grouped [(Symbol,Symbol)]

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

-- | `:::` is like a promoted version of `As`, a type level pair between
-- an alias and some type, usually a column alias and a `ColumnType` or
-- a table alias and a `ColumnsType`.
type (:::) (alias :: Symbol) (ty :: polykind) = '(alias,ty)
infixr 6 :::

-- | `Alias`es are proxies for a type level string or `Symbol`
-- and have an `IsLabel` instance so that with @-XOverloadedLabels@
--
-- >>> :set -XOverloadedLabels
-- >>> #foobar :: Alias "foobar"
-- Alias
data Alias (alias :: Symbol) = Alias
  deriving (Eq,Generic,Ord,Show,NFData)
instance alias1 ~ alias2 => IsLabel alias1 (Alias alias2) where
  fromLabel = Alias

-- | >>> renderAlias #alias
-- "alias"
renderAlias :: KnownSymbol alias => Alias alias -> ByteString
renderAlias = fromString . symbolVal

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

-- | >>> let renderMaybe = fromString . maybe "Nothing" (const "Just")
-- >>> renderAliased renderMaybe (Just (3::Int) `As` #an_int)
-- "Just AS an_int"
renderAliased
  :: (forall ty. expression ty -> ByteString)
  -> Aliased expression aliased
  -> ByteString
renderAliased render (expression `As` alias) =
  render expression <> " AS " <> renderAlias alias

-- | Analagous to `IsLabel`, the constraint
-- `IsTableColumn` defines `!` for a column alias qualified
-- by a table alias.
class IsTableColumn table column expression where
  (!) :: Alias table -> Alias column -> expression
  infixl 9 !
instance IsTableColumn table column (Alias table, Alias column) where (!) = (,)

-- | @In x xs@ is a constraint that proves that @x@ is in @xs@.
type family In x xs :: Constraint where
  In x (x ': xs) = ()
  In x (y ': xs) = In x xs

-- | @HasUnique alias xs x@ is a constraint that proves that @xs@ is a singleton
-- of @alias ::: x@.
type HasUnique alias xs x = xs ~ '[alias ::: x]

-- | `PGTypeOf` forgets about @NULL@ and any column constraints.
type family PGTypeOf (ty :: ColumnType) :: PGType where
  PGTypeOf '(constraints, nullity pg) = pg

-- | `SameTypes` is a constraint that proves two `ColumnsType`s have the same
-- length and the same `ColumnType`s.
type family SameTypes
  (columns0 :: ColumnsType) (columns1 :: ColumnsType)
    :: Constraint where
  SameTypes '[] '[] = ()
  SameTypes ((column0 ::: ty0) ': columns0) ((column1 ::: ty1) ': columns1)
    = (ty0 ~ ty1, SameTypes columns0 columns1)

-- | `AllNotNull` is a constraint that proves a `ColumnsType` has no @NULL@s.
type family AllNotNull (columns :: ColumnsType) :: Constraint where
  AllNotNull '[] = ()
  AllNotNull ((column ::: '(constraints, ('NotNull ty))) ': columns)
    = AllNotNull columns

-- | `NotAllNull` is a constraint that proves a `ColumnsType` has some
-- @NOT NULL@.
type family NotAllNull columns :: Constraint where
  NotAllNull ((column ::: '(constraints, ('NotNull ty))) ': columns) = ()
  NotAllNull ((column ::: '(constraints, ('Null ty))) ': columns)
    = NotAllNull columns

-- | `NullifyType` is an idempotent that nullifies a `ColumnType`.
type family NullifyType (ty :: ColumnType) :: ColumnType where
  NullifyType (optionality ('Null ty)) = optionality ('Null ty)
  NullifyType (optionality ('NotNull ty)) = optionality ('Null ty)

-- | `NullifyColumns` is an idempotent that nullifies a `ColumnsType`.
type family NullifyColumns (columns :: ColumnsType) :: ColumnsType where
  NullifyColumns '[] = '[]
  NullifyColumns ((column ::: ty) ': columns) =
    (column ::: NullifyType ty) ': NullifyColumns columns

type family NullifyTable (table :: TableType) :: TableType where
  NullifyTable '( '[], columns) = '( '[], NullifyColumns columns)

-- | `NullifyTables` is an idempotent that nullifies a `RelationType`
-- used to nullify the left or right hand side of an outer join
-- in a `Squeal.PostgreSQL.Query.FromClause`.
type family NullifyTables (tables :: RelationType) :: RelationType where
  NullifyTables '[] = '[]
  NullifyTables ((tab ::: columns) ': tables) =
    (tab ::: NullifyColumns columns) ': NullifyTables tables

-- | `Join` is simply promoted `++` and is used in @JOIN@s in
-- `Squeal.PostgreSQL.Query.FromClause`s.
type family Join xs ys where
  Join '[] ys = ys
  Join (x ': xs) ys = x ': Join xs ys

-- | @Create alias x xs@ adds @alias ::: x@ to the end of @xs@ and is used in
-- `Squeal.PostgreSQL.Definition.createTable` statements and in @ALTER TABLE@
-- `Squeal.PostgreSQL.Definition.addColumnDefault` and
-- `Squeal.PostgreSQL.Definition.addColumnNull` statements.
type family Create alias x xs where
  Create alias x '[] = '[alias ::: x]
  Create alias y (x ': xs) = x ': Create alias y xs

type family Add alias x y where
  Add alias x (constraints :=> xs) = constraints :=> Create alias x xs

-- | @Drop alias xs@ removes the type associated with @alias@ in @xs@
-- and is used in `Squeal.PostgreSQL.Definition.dropTable` statements
-- and in @ALTER TABLE@ `Squeal.PostgreSQL.Definition.dropColumn` statements.
type family Drop alias xs where
  Drop alias ((alias ::: x) ': xs) = xs
  Drop alias (x ': xs) = x ': Drop alias xs

-- | @Alter alias xs x@ replaces the type associated with an @alias@ in @xs@
-- with the type `x` and is used in `Squeal.PostgreSQL.Definition.alterTable`
-- and `Squeal.PostgreSQL.Definition.alterColumn`.
type family Alter alias xs x where
  Alter alias ((alias ::: x0) ': xs) x1 = (alias ::: x1) ': xs
  Alter alias (x0 ': xs) x1 = x0 ': Alter alias xs x1

type family AddConstraint constraint ty where
  AddConstraint constraint (constraints :=> ty)
    = AsSet (constraint ': constraints) :=> ty

type family DeleteFromList (e :: elem) (list :: [elem]) where
  DeleteFromList elem '[] = '[]
  DeleteFromList elem (x ': xs) =
    If (Cmp elem x == 'EQ) xs (x ': DeleteFromList elem xs)

type family DropConstraint constraint ty where
  DropConstraint constraint (constraints :=> ty)
    = (AsSet (DeleteFromList constraint constraints)) :=> ty

-- | @Rename alias0 alias1 xs@ replaces the alias @alias0@ by @alias1@ in @xs@
-- and is used in `Squeal.PostgreSQL.Definition.alterTableRename` and
-- `Squeal.PostgreSQL.Definition.renameColumn`.
type family Rename alias0 alias1 xs where
  Rename alias0 alias1 ((alias0 ::: x0) ': xs) = (alias1 ::: x0) ': xs
  Rename alias0 alias1 (x ': xs) = x ': Rename alias0 alias1 xs

-- | A `SameField` constraint is an equality constraint on a
-- `Generics.SOP.Type.Metadata.FieldInfo` and the column alias in a `:::` pair.
class SameField
  (fieldInfo :: Type.FieldInfo) (fieldty :: (Symbol,ColumnType)) where
instance field ~ column => SameField ('Type.FieldInfo field) (column ::: ty)

-- | A `SameFields` constraint proves that a
-- `Generics.SOP.Type.Metadata.DatatypeInfo` of a record type has the same
-- field names as the column aliases of a `ColumnsType`.
type family SameFields
  (datatypeInfo :: Type.DatatypeInfo) (columns :: ColumnsType)
    :: Constraint where
  SameFields
    ('Type.ADT _module _datatype '[ 'Type.Record _constructor fields])
    columns
      = AllZip SameField fields columns
  SameFields
    ('Type.Newtype _module _datatype ('Type.Record _constructor fields))
    columns
      = AllZip SameField fields columns

type (:=>) (constraints :: [constraint]) ty = '(constraints,ty)
infixr 7 :=>

data ColumnConstraint
  = Default
  | Unique
  | References Symbol Symbol

type instance Cmp 'Default 'Default = 'EQ
type instance Cmp 'Unique 'Unique = 'EQ
type instance Cmp ('References tab0 col0) ('References tab1 col1) =
  If (Cmp tab0 tab1 == 'EQ) (Cmp col0 col1) (Cmp tab0 tab1)
type instance Cmp 'Default 'Unique = 'LT
type instance Cmp 'Unique 'Default = 'GT
type instance Cmp 'Default ('References tab col) = 'LT
type instance Cmp ('References tab col) 'Default = 'GT
type instance Cmp 'Unique ('References tab col) = 'LT
type instance Cmp ('References tab col) 'Unique = 'GT

data TableConstraint'
  = Check
  | Uniques [Symbol]
  | ForeignKey [Symbol] Symbol [Symbol]

type family Aliases xs where
  Aliases '[] = '[]
  Aliases ((alias ::: x) ': xs) = alias ': Aliases xs

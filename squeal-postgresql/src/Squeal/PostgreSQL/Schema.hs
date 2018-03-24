{-|
Module: Squeal.PostgreSQL.Schema
Description: Embedding of PostgreSQL type and alias system
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

A type-level DSL for kinds of PostgreSQL types, constraints, and aliases.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , DeriveAnyClass
  , DeriveDataTypeable
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
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
  ( -- * Types
    PGType (..)
  , HasOid (..)
  , NullityType (..)
  , ColumnType
  , ColumnsType
  , RelationType
  , NilRelation
  , RelationsType
  , TableType
  , TablesType
  , NilTables
  , Grouping (..)
    -- * Constraints
  , (:=>)
  , ColumnsToRelation
  , TableToColumns
  , TablesToRelations
  , ColumnConstraint (..)
  , TableConstraint (..)
    -- * Aliases
  , (:::)
  , Alias (Alias)
  , renderAlias
  , Aliased (As)
  , renderAliasedAs
  , AliasesOf
  , Has
  , HasUnique
  , IsLabel (..)
  , IsTableColumn (..)
    -- * Type Families
  , In
  , PGNum
  , PGIntegral
  , PGFloating
  , PGTypeOf
  , SameTypes
  , AllNotNull
  , NotAllNull
  , NullifyType
  , NullifyRelation
  , NullifyRelations
  , Join
  , Create
  , AddColumn
  , AddConstraint
  , Drop
  , Alter
  , Rename
    -- * Generics
  , SameField
  , SameFields
  ) where

import Control.DeepSeq
import Data.ByteString
import Data.Monoid
import Data.String
import Data.Word
import Generics.SOP (AllZip)
import GHC.Generics (Generic)
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits

import qualified Generics.SOP.Type.Metadata as Type

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
-- between an "constraint" and some type,
-- either a `ColumnConstraint` and a `NullityType` or a row of
-- `TableConstraint`s and a `TableType`.
type (:=>) constraint ty = '(constraint,ty)
infixr 7 :=>

-- | The alias operator `:::` is like a promoted version of `As`,
-- a type level pair between
-- an alias and some type, like a column alias and either a `ColumnType` or
-- `NullityType` or a table alias and either a `TableType` or a `RelationType`
-- or a constraint alias and a `TableConstraint`.
type (:::) (alias :: Symbol) ty = '(alias,ty)
infixr 6 :::

-- | `ColumnConstraint` encodes the allowance of @DEFAULT@.
data ColumnConstraint
  = Def -- ^ @DEFAULT@ is available for inserts and updates
  | NoDef -- ^ @DEFAULT@ is unavailable for inserts and updates

-- | `ColumnType` encodes the allowance of @DEFAULT@ and @NULL@ and the
-- base `PGType` for a column.
--
-- >>> :kind 'NoDef :=> 'NotNull 'PGint4
-- 'NoDef :=> 'NotNull 'PGint4 :: (ColumnConstraint, NullityType)
-- >>> :kind 'NoDef :=> 'NotNull 'PGtext
-- 'NoDef :=> 'NotNull 'PGtext :: (ColumnConstraint, NullityType)
-- >>> import Data.Type.Equality
-- >>> Refl :: ColumnType :~: (ColumnConstraint, NullityType)
-- Refl
type ColumnType = (ColumnConstraint,NullityType)

-- | `ColumnsType` is a row of `ColumnType`s.
--
-- >>> import GHC.TypeLits
-- >>> :{
-- type Columns =
--   '[ "name" ::: 'NoDef :=> 'NotNull 'PGtext
--    , "id"   :::   'Def :=> 'NotNull 'PGint4
--    ]
-- :}
--
-- >>> :kind Columns
-- Columns :: [(Symbol, (ColumnConstraint, NullityType))]
-- >>> import Data.Type.Equality
-- >>> Refl :: ColumnsType :~: [(Symbol, (ColumnConstraint, NullityType))]
-- Refl
type ColumnsType = [(Symbol,ColumnType)]

-- | `TableConstraint` encodes various forms of data constraints
-- of columns in a table.
data TableConstraint
  = Check
  | Unique [Symbol]
  | PrimaryKey [Symbol]
  | ForeignKey [Symbol] Symbol [Symbol]

-- | `TableType` encodes a row of constraints on a table as well as the types
-- of its columns.
--
-- >>> :{
-- type UsersTable =
--   '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
--   '[ "id"       :::   'Def :=> 'NotNull 'PGint4
--    , "name"     ::: 'NoDef :=> 'NotNull 'PGtext
--    ]
-- :}
--
-- >>> :kind UsersTable
-- UsersTable :: ([(Symbol, TableConstraint)],
--                [(Symbol, (ColumnConstraint, NullityType))])
type TableType = ([(Symbol,TableConstraint)],ColumnsType)

-- | `TablesType` is a row of `TableType`s, thought of as a union.
--
-- >>> :set -XTypeFamilies -XTypeInType
-- >>> :{
-- type family Schema :: TablesType where
--   Schema =
--     '[ "users" :::
--          '[ "pk_users" ::: 'PrimaryKey '["id"] ] :=>
--          '[ "id" ::: 'Def :=> 'NotNull 'PGint4
--           , "name" ::: 'NoDef :=> 'NotNull 'PGtext
--           , "vec" ::: 'NoDef :=> 'NotNull ('PGvararray 'PGint2)
--           ]
--      , "emails" :::
--          '[  "pk_emails" ::: 'PrimaryKey '["id"]
--           , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
--           ] :=>
--          '[ "id" ::: 'Def :=> 'NotNull 'PGint4
--           , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
--           , "email" ::: 'NoDef :=> 'Null 'PGtext
--           ]
--      ]
-- :}
--
type TablesType = [(Symbol,TableType)]
-- | A monokinded `'[]` of `TablesType`.
type family NilTables :: TablesType where NilTables = '[]

-- | `RelationType` is a row of `NullityType`
--
-- >>> :{
-- type Relation =
--   '[ "name"        ::: 'NotNull 'PGtext
--    , "age"         ::: 'NotNull 'PGint4
--    , "dateOfBirth" :::    'Null 'PGdate
--    ]
-- :}
--
-- >>> :kind Relation
-- Relation :: [(Symbol, NullityType)]
type RelationType = [(Symbol,NullityType)]
-- | A monokinded `'[]` of `RelationType`.
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

-- | `TablesToRelations` removes both table and column constraints.
type family TablesToRelations (tables :: TablesType) :: RelationsType where
  TablesToRelations '[] = '[]
  TablesToRelations (alias ::: constraint :=> columns ': tables) =
    alias ::: ColumnsToRelation columns ': TablesToRelations tables

-- | `Grouping` is an auxiliary namespace, created by
-- @GROUP BY@ clauses (`Squeal.PostgreSQL.Query.group`), and used
-- for typesafe aggregation
data Grouping
  = Ungrouped -- ^ no aggregation permitted
  | Grouped [(Symbol,Symbol)] -- ^ aggregation required for any column which is not grouped

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

-- | >>> renderAlias #jimbob
-- "jimbob"
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
-- >>> renderAliasedAs renderMaybe (Just (3::Int) `As` #an_int)
-- "Just AS an_int"
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

-- | @HasUnique alias xs x@ is a constraint that proves that @xs@ is a singleton
-- of @alias ::: x@.
type HasUnique alias xs x = xs ~ '[alias ::: x]

-- | @Has alias xs x@ is a constraint that proves that @xs@ has a field
-- of @alias ::: x@.
class KnownSymbol alias =>
  Has (alias :: Symbol) (fields :: [(Symbol,kind)]) (field :: kind)
  | alias fields -> field where
instance {-# OVERLAPPING #-} KnownSymbol alias
  => Has alias (alias ::: field ': fields) field
instance {-# OVERLAPPABLE #-} (KnownSymbol alias, Has alias fields field)
  => Has alias (field' ': fields) field

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

-- | Add a column to a `TableType` with
-- `Squeal.PostgreSQL.Definition.addColumn`.
type family AddColumn alias x y where
  AddColumn alias x (constraints :=> xs) = constraints :=> Create alias x xs

-- | Add a constraint to a `TableType` with
-- `Squeal.PostgreSQL.Definition.addConstraint`.
type family AddConstraint alias x y where
  AddConstraint alias x (constraints :=> xs) = Create alias x constraints :=> xs

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

-- | A `SameField` constraint is an equality constraint on a
-- `Generics.SOP.Type.Metadata.FieldInfo` and the column alias in a `:::` pair.
class SameField
  (fieldInfo :: Type.FieldInfo) (fieldty :: (Symbol,NullityType)) where
instance field ~ column => SameField ('Type.FieldInfo field) (column ::: ty)

-- | A `SameFields` constraint proves that a
-- `Generics.SOP.Type.Metadata.DatatypeInfo` of a record type has the same
-- field names as the column AliasesOf of a `ColumnsType`.
type family SameFields
  (datatypeInfo :: Type.DatatypeInfo) (columns :: RelationType)
    :: Constraint where
  SameFields
    ('Type.ADT _module _datatype '[ 'Type.Record _constructor fields])
    columns
      = AllZip SameField fields columns
  SameFields
    ('Type.Newtype _module _datatype ('Type.Record _constructor fields))
    columns
      = AllZip SameField fields columns

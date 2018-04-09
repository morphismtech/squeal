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
  , UndecidableSuperClasses
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
    -- * Grouping
  , Grouping (..)
  , GroupedBy
    -- * Constraints
  , (:=>)
  , ColumnConstraint (..)
  , TableConstraint (..)
  , TableConstraints
  , NilTableConstraints
    -- * Aliases
  , (:::)
  , Alias (Alias)
  , renderAlias
  , renderAliases
  , Aliased (As)
  , renderAliasedAs
  , AliasesOf
  , Has
  , HasUnique
  , HasAll
  , IsLabel (..)
  , IsQualified (..)
    -- * Type Families
  , Join
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
  , SameTypes
  , SamePGType
  , AllNotNull
  , NotAllNull
  , NullifyType
  , NullifyRelation
  , NullifyRelations
  , ColumnsToRelation
  , RelationToColumns
  , TableToColumns
  , TablesToRelations
  , RelationsToTables
  , ConstraintInvolves
  , DropIfConstraintsInvolve
    -- * Generics
  , SameField
  , SameFields
  ) where

import Control.DeepSeq
import Data.ByteString
import Data.Monoid hiding (All)
import Data.String
import Data.Word
import Data.Type.Bool
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
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
-- a type level pair between
-- an alias and some type, like a column alias and either a `ColumnType` or
-- `NullityType` or a table alias and either a `TableType` or a `RelationType`
-- or a constraint alias and a `TableConstraint`.
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

-- | `TablesType` is a row of `TableType`s, thought of as a union.
--
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
type TablesType = [(Symbol,TableType)]
-- | A monokinded empty `TablesType`.
type family NilTables :: TablesType where NilTables = '[]

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

-- | `RelationToColumns` adds `'NoDef` column constraints.
type family RelationToColumns (relation :: RelationType) :: ColumnsType where
  RelationToColumns '[] = '[]
  RelationToColumns (column ::: ty ': columns) =
    column ::: 'NoDef :=> ty ': RelationToColumns columns

-- | `TableToColumns` removes table constraints.
type family TableToColumns (table :: TableType) :: ColumnsType where
  TableToColumns (constraints :=> columns) = columns

-- | `TablesToRelations` removes both table and column constraints.
type family TablesToRelations (tables :: TablesType) :: RelationsType where
  TablesToRelations '[] = '[]
  TablesToRelations (alias ::: constraint :=> columns ': tables) =
    alias ::: ColumnsToRelation columns ': TablesToRelations tables

-- | `RelationsToTables` adds both trivial table and column constraints.
type family RelationsToTables (tables :: RelationsType) :: TablesType where
  RelationsToTables '[] = '[]
  RelationsToTables (alias ::: columns ': relations) =
    alias ::: '[] :=> RelationToColumns columns ': RelationsToTables relations

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

-- | >>> renderAlias #jimbob
-- "\"jimbob\""
renderAlias :: KnownSymbol alias => Alias alias -> ByteString
renderAlias = doubleQuoted . fromString . symbolVal

-- | >>> import Generics.SOP (NP(..))
-- >>> renderAliases (#jimbob :* #kandi :* Nil)
-- ["\"jimbob\"","\"kandi\""]
renderAliases
  :: SOP.All KnownSymbol aliases => SOP.NP Alias aliases -> [ByteString]
renderAliases = SOP.hcollapse
  . SOP.hcmap (SOP.Proxy @KnownSymbol) (SOP.K . renderAlias)

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

-- | @HasUnique alias fields field@ is a constraint that proves that
-- @fields@ is a singleton of @alias ::: field@.
type HasUnique alias fields field = fields ~ '[alias ::: field]

-- | @Has alias fields field@ is a constraint that proves that
-- @fields@ has a field of @alias ::: field@.
class KnownSymbol alias =>
  Has (alias :: Symbol) (fields :: [(Symbol,kind)]) (field :: kind)
  | alias fields -> field where
instance {-# OVERLAPPING #-} KnownSymbol alias
  => Has alias (alias ::: field ': fields) field
instance {-# OVERLAPPABLE #-} (KnownSymbol alias, Has alias fields field)
  => Has alias (field' ': fields) field

class
  ( SOP.All KnownSymbol aliases
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
      = SOP.AllZip SameField fields columns
  SameFields
    ('Type.Newtype _module _datatype ('Type.Record _constructor fields))
    columns
      = SOP.AllZip SameField fields columns

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

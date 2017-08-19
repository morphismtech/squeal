{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , DeriveGeneric
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , StandaloneDeriving
  , TypeInType
  , TypeOperators
#-}

module Squeal.PostgreSQL.Definition
  (  -- * Data Definition Language
    Definition (UnsafeDefinition, renderDefinition)
  , createTable
  , TableConstraint (UnsafeTableConstraint, renderTableConstraint)
  , check
  , unique
  , primaryKey
  , foreignKey
  , OnDelete (OnDeleteNoAction, OnDeleteRestrict, OnDeleteCascade)
  , renderOnDelete
  , OnUpdate (OnUpdateNoAction, OnUpdateRestrict, OnUpdateCascade)
  , renderOnUpdate
  , dropTable
  , alterTable
  , alterTableRename
  , AlterTable (UnsafeAlterTable, renderAlterTable)
  , addColumnDefault
  , addColumnNull
  , dropColumn
  , renameColumn
  , alterColumn
  , AlterColumn (UnsafeAlterColumn, renderAlterColumn)
  , setDefault
  , dropDefault
  , setNotNull
  , dropNotNull
  , alterType
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString
import Data.Monoid
import Generics.SOP
import GHC.TypeLits
import Prelude hiding ((.), id)

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Prettyprint
import Squeal.PostgreSQL.Schema

{-----------------------------------------
statements
-----------------------------------------}

newtype Definition
  (schema0 :: TablesType)
  (schema1 :: TablesType)
  = UnsafeDefinition { renderDefinition :: ByteString }
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

instance Category Definition where
  id = UnsafeDefinition ";"
  ddl1 . ddl0 = UnsafeDefinition $
    renderDefinition ddl0 <+> renderDefinition ddl1

{-----------------------------------------
CREATE statements
-----------------------------------------}

createTable
  :: (KnownSymbol table, SListI columns)
  => Alias table
  -> NP (Aliased TypeExpression) (column ': columns)
  -> [TableConstraint schema (column ': columns)]
  -> Definition schema (Create table (column ': columns) schema)
createTable table columns constraints = UnsafeDefinition $
  "CREATE TABLE" <+> renderAlias table
  <+> parenthesized
    ( renderCommaSeparated renderColumnDef columns
      <> renderConstraints constraints )
  <> ";"
  where
    renderColumnDef :: Aliased TypeExpression x -> ByteString
    renderColumnDef (ty `As` column) =
      renderAlias column <+> renderTypeExpression ty
    renderConstraints :: [TableConstraint schema columns] -> ByteString
    renderConstraints = \case
      [] -> ""
      _ -> ", " <> commaSeparated (renderTableConstraint <$> constraints)

newtype TableConstraint
  (schema :: TablesType)
  (columns :: ColumnsType)
    = UnsafeTableConstraint { renderTableConstraint :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)

check
  :: Condition '[table ::: columns] 'Ungrouped '[]
  -> TableConstraint schema columns
check condition = UnsafeTableConstraint $
  "CHECK" <+> parenthesized (renderExpression condition)

unique
  :: SListI subcolumns
  => NP (Column columns) subcolumns
  -> TableConstraint schema columns
unique columns = UnsafeTableConstraint $
  "UNIQUE" <+> parenthesized (renderCommaSeparated renderColumn columns)

primaryKey
  :: (SListI subcolumns, AllNotNull subcolumns)
  => NP (Column columns) subcolumns
  -> TableConstraint schema columns
primaryKey columns = UnsafeTableConstraint $
  "PRIMARY KEY" <+> parenthesized (renderCommaSeparated renderColumn columns)

foreignKey
  :: ( HasTable reftable schema refcolumns
     , SameTypes subcolumns refsubcolumns
     , NotAllNull subcolumns
     , SListI subcolumns
     , SListI refsubcolumns)
  => NP (Column columns) subcolumns
  -> Alias reftable
  -> NP (Column refcolumns) refsubcolumns
  -> OnDelete
  -> OnUpdate
  -> TableConstraint schema columns
foreignKey columns reftable refcolumns onDelete onUpdate =
  UnsafeTableConstraint $
    "FOREIGN KEY" <+> parenthesized (renderCommaSeparated renderColumn columns)
    <+> "REFERENCES" <+> renderAlias reftable
    <+> parenthesized (renderCommaSeparated renderColumn refcolumns)
    <+> renderOnDelete onDelete
    <+> renderOnUpdate onUpdate

data OnDelete
  = OnDeleteNoAction
  | OnDeleteRestrict
  | OnDeleteCascade
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnDelete

renderOnDelete :: OnDelete -> ByteString
renderOnDelete = \case
  OnDeleteNoAction -> "ON DELETE NO ACTION"
  OnDeleteRestrict -> "ON DELETE RESTRICT"
  OnDeleteCascade -> "ON DELETE CASCADE"

data OnUpdate
  = OnUpdateNoAction
  | OnUpdateRestrict
  | OnUpdateCascade
  deriving (GHC.Generic,Show,Eq,Ord)
instance NFData OnUpdate

renderOnUpdate :: OnUpdate -> ByteString
renderOnUpdate = \case
  OnUpdateNoAction -> "ON UPDATE NO ACTION"
  OnUpdateRestrict -> "ON UPDATE RESTRICT"
  OnUpdateCascade -> "ON UPDATE CASCADE"

{-----------------------------------------
DROP statements
-----------------------------------------}

dropTable
  :: KnownSymbol table
  => Alias table
  -> Definition schema (Drop table schema)
dropTable table = UnsafeDefinition $ "DROP TABLE" <+> renderAlias table <> ";"

{-----------------------------------------
ALTER statements
-----------------------------------------}

alterTable
  :: HasTable table schema columns0
  => Alias table
  -> AlterTable columns0 columns1
  -> Definition schema (Alter table schema columns1)
alterTable table alteration = UnsafeDefinition $
  "ALTER TABLE"
  <+> renderAlias table
  <+> renderAlterTable alteration
  <> ";"

alterTableRename
  :: (KnownSymbol table0, KnownSymbol table1)
  => Alias table0
  -> Alias table1
  -> Definition schema (Rename table0 table1 schema)
alterTableRename table0 table1 = UnsafeDefinition $
  "ALTER TABLE" <+> renderAlias table0
  <+> "RENAME TO" <+> renderAlias table1 <> ";"

newtype AlterTable
  (columns0 :: ColumnsType)
  (columns1 :: ColumnsType) =
    UnsafeAlterTable {renderAlterTable :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

addColumnDefault
  :: KnownSymbol column
  => Alias column
  -> TypeExpression ('Optional ty)
  -> AlterTable columns (Create column ('Optional ty) columns)
addColumnDefault column ty = UnsafeAlterTable $
  "ADD COLUMN" <+> renderAlias column <+> renderTypeExpression ty

addColumnNull
  :: KnownSymbol column
  => Alias column
  -> TypeExpression ('Required ('Null ty))
  -> AlterTable columns (Create column ('Required ('Null ty)) columns)
addColumnNull column ty = UnsafeAlterTable $
  "ADD COLUMN" <+> renderAlias column <+> renderTypeExpression ty

dropColumn
  :: KnownSymbol column
  => Alias column
  -> AlterTable columns (Drop column columns)
dropColumn column = UnsafeAlterTable $
  "DROP COLUMN" <+> renderAlias column

renameColumn
  :: (KnownSymbol column0, KnownSymbol column1)
  => Alias column0
  -> Alias column1
  -> AlterTable columns (Rename column0 column1 columns)
renameColumn column0 column1 = UnsafeAlterTable $
  "RENAME COLUMN" <+> renderAlias column0  <+> "TO" <+> renderAlias column1

alterColumn
  :: (KnownSymbol column, HasColumn column columns ty0)
  => Alias column
  -> AlterColumn ty0 ty1
  -> AlterTable columns (Alter column columns ty1)
alterColumn column alteration = UnsafeAlterTable $
  "ALTER COLUMN" <+> renderAlias column <+> renderAlterColumn alteration

newtype AlterColumn (ty0 :: ColumnType) (ty1 :: ColumnType) =
  UnsafeAlterColumn {renderAlterColumn :: ByteString}
  deriving (GHC.Generic,Show,Eq,Ord,NFData)

setDefault
  :: Expression '[] 'Ungrouped '[] ('Required ty)
  -> AlterColumn ('Required ty) ('Optional ty)
setDefault expression = UnsafeAlterColumn $
  "SET DEFAULT" <+> renderExpression expression

dropDefault :: AlterColumn ('Optional ty) ('Required ty)
dropDefault = UnsafeAlterColumn $ "DROP DEFAULT"

setNotNull
  :: AlterColumn (optionality ('Null ty)) (optionality ('NotNull ty))
setNotNull = UnsafeAlterColumn $ "SET NOT NULL"

dropNotNull
  :: AlterColumn (optionality ('NotNull ty)) (optionality ('Null ty))
dropNotNull = UnsafeAlterColumn $ "DROP NOT NULL"

alterType
  :: TypeExpression (optionality (nullity ty1))
  -> AlterColumn (optionality (nullity ty0)) (optionality (nullity ty1))
alterType ty = UnsafeAlterColumn $ "TYPE " <> renderTypeExpression ty

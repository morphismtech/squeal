{-|
Module: Squeal.PostgreSQL.Manipulation
Description: data manipulation language
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

data manipulation language
-}

{-# LANGUAGE
    DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PatternSynonyms
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , DataKinds
  , PolyKinds
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Manipulation
  ( -- * Manipulation
    Manipulation (..)
  , Manipulation_
  , queryStatement
  , ReturningClause (..)
  , pattern Returning_
  , UsingClause (..)
  ) where

import Control.DeepSeq
import Data.ByteString hiding (foldr)
import Data.Kind (Type)
import Data.Quiver.Functor

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Query.From
import Squeal.PostgreSQL.Query.Select
import Squeal.PostgreSQL.Query.With
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import Data.Int
-- >>> import Data.Time

{- |
A `Manipulation` is a statement which may modify data in the database,
but does not alter its schemas. Examples are
`Squeal.PostgreSQL.Manipulation.Insert.insertInto`s,
`Squeal.PostgreSQL.Manipulation.Update.update`s and
`Squeal.PostgreSQL.Manipulation.Delete.deleteFrom`s.
A `queryStatement` is also considered a `Manipulation` even though it does not modify data.

The general `Manipulation` type is parameterized by

* @with :: FromType@ - scope for all `Squeal.PostgreSQL.Query.From.common` table expressions,
* @db :: SchemasType@ - scope for all `Squeal.PostgreSQL.Query.From.table`s and `Squeal.PostgreSQL.Query.From.view`s,
* @params :: [NullType]@ - scope for all `Squeal.Expression.Parameter.parameter`s,
* @row :: RowType@ - return type of the `Manipulation`.

Let's see some examples of `Manipulation`s.

simple insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'Null 'PGint4, "col2" ::: 'Def :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manp :: Manipulation with (Public Schema) '[] '[]
  manp =
    insertInto_ #tab (Values_ (Set 2 `as` #col1 :* Default `as` #col2))
in printSQL manp
:}
INSERT INTO "tab" AS "tab" ("col1", "col2") VALUES ((2 :: int4), DEFAULT)

out-of-line parameterized insert:

>>> type Columns = '["col1" ::: 'Def :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manp :: Manipulation with (Public Schema) '[ 'NotNull 'PGint4] '[]
  manp =
    insertInto_ #tab $ Values_
      (Default `as` #col1 :* Set (param @1) `as` #col2)
in printSQL manp
:}
INSERT INTO "tab" AS "tab" ("col1", "col2") VALUES (DEFAULT, ($1 :: int4))

in-line parameterized insert:

>>> type Columns = '["col1" ::: 'Def :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
data Row = Row { col1 :: Optional SOP.I ('Def :=> Int32), col2 :: Int32 }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
:}

>>> :{
let
  manp :: Row -> Row -> Manipulation with (Public Schema) '[] '[]
  manp row1 row2 = insertInto_ #tab $ inlineValues row1 [row2]
  row1 = Row {col1 = Default, col2 = 2 :: Int32}
  row2 = Row {col1 = NotDefault (3 :: Int32), col2 = 4 :: Int32}
in printSQL (manp row1 row2)
:}
INSERT INTO "tab" AS "tab" ("col1", "col2") VALUES (DEFAULT, (2 :: int4)), ((3 :: int4), (4 :: int4))

returning insert:

>>> :{
let
  manp :: Manipulation with (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4]
  manp =
    insertInto #tab (Values_ (Set 2 `as` #col1 :* Set 3 `as` #col2))
      OnConflictDoRaise (Returning #col1)
in printSQL manp
:}
INSERT INTO "tab" AS "tab" ("col1", "col2") VALUES ((2 :: int4), (3 :: int4)) RETURNING "col1" AS "col1"

upsert:

>>> type CustomersColumns = '["name" ::: 'NoDef :=> 'NotNull 'PGtext, "email" ::: 'NoDef :=> 'NotNull 'PGtext]
>>> type CustomersConstraints = '["uq" ::: 'Unique '["name"]]
>>> type CustomersSchema = '["customers" ::: 'Table (CustomersConstraints :=> CustomersColumns)]
>>> :{
let
  manp :: Manipulation with (Public CustomersSchema) '[] '[]
  manp =
    insertInto #customers
      (Values_ (Set "John Smith" `as` #name :* Set "john@smith.com" `as` #email))
      (OnConflict (OnConstraint #uq)
        (DoUpdate (Set (#excluded ! #email <> "; " <> #customers ! #email) `as` #email) []))
      (Returning_ Nil)
in printSQL manp
:}
INSERT INTO "customers" AS "customers" ("name", "email") VALUES ((E'John Smith' :: text), (E'john@smith.com' :: text)) ON CONFLICT ON CONSTRAINT "uq" DO UPDATE SET "email" = ("excluded"."email" || ((E'; ' :: text) || "customers"."email"))

query insert:

>>> :{
let
  manp :: Manipulation with (Public Schema) '[] '[]
  manp = insertInto_ #tab (Subquery (select Star (from (table #tab))))
in printSQL manp
:}
INSERT INTO "tab" AS "tab" SELECT * FROM "tab" AS "tab"

update:

>>> :{
let
  manp :: Manipulation with (Public Schema) '[] '[]
  manp = update_ #tab (Set 2 `as` #col1) (#col1 ./= #col2)
in printSQL manp
:}
UPDATE "tab" AS "tab" SET "col1" = (2 :: int4) WHERE ("col1" <> "col2")

delete:

>>> :{
let
  manp :: Manipulation with (Public Schema) '[] '["col1" ::: 'NotNull 'PGint4, "col2" ::: 'NotNull 'PGint4]
  manp = deleteFrom #tab NoUsing (#col1 .== #col2) (Returning Star)
in printSQL manp
:}
DELETE FROM "tab" AS "tab" WHERE ("col1" = "col2") RETURNING *

delete and using clause:

>>> :{
type Schema3 =
  '[ "tab" ::: 'Table ('[] :=> Columns)
   , "other_tab" ::: 'Table ('[] :=> Columns)
   , "third_tab" ::: 'Table ('[] :=> Columns) ]
:}

>>> :{
let
  manp :: Manipulation with (Public Schema3) '[] '[]
  manp =
    deleteFrom #tab (Using (table #other_tab & also (table #third_tab)))
    ( (#tab ! #col2 .== #other_tab ! #col2)
    .&& (#tab ! #col2 .== #third_tab ! #col2) )
    (Returning_ Nil)
in printSQL manp
:}
DELETE FROM "tab" AS "tab" USING "other_tab" AS "other_tab", "third_tab" AS "third_tab" WHERE (("tab"."col2" = "other_tab"."col2") AND ("tab"."col2" = "third_tab"."col2"))

with manipulation:

>>> type ProductsColumns = '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]
>>> type ProductsSchema = '["products" ::: 'Table ('[] :=> ProductsColumns), "products_deleted" ::: 'Table ('[] :=> ProductsColumns)]
>>> :{
let
  manp :: Manipulation with (Public ProductsSchema) '[ 'NotNull 'PGdate] '[]
  manp = with
    (deleteFrom #products NoUsing (#date .< param @1) (Returning Star) `as` #del)
    (insertInto_ #products_deleted (Subquery (select Star (from (common #del)))))
in printSQL manp
:}
WITH "del" AS (DELETE FROM "products" AS "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" AS "products_deleted" SELECT * FROM "del" AS "del"
-}
newtype Manipulation
  (with :: FromType)
  (db :: SchemasType)
  (params :: [NullType])
  (columns :: RowType)
    = UnsafeManipulation { renderManipulation :: ByteString }
    deriving stock (GHC.Generic,Show,Eq,Ord)
    deriving newtype (NFData)
instance RenderSQL (Manipulation with db params columns) where
  renderSQL = renderManipulation
instance With Manipulation where
  with Done manip = manip
  with ctes manip = UnsafeManipulation $
    "WITH" <+> commaSeparated (qtoList renderSQL ctes) <+> renderSQL manip

{- |
The `Manipulation_` type is parameterized by a @db@ `SchemasType`,
against which it is type-checked, an input @params@ Haskell `Type`,
and an ouput row Haskell `Type`.

Generally, @params@ will be a Haskell tuple or record whose entries
may be referenced using positional
`Squeal.PostgreSQL.Expression.Parameter.param`s and @row@ will be a
Haskell record, whose entries will be targeted using overloaded labels.

A `Manipulation_` can be run
using `Squeal.PostgreSQL.Session.manipulateParams`, or if @params = ()@
using `Squeal.PostgreSQL.Session.manipulate`.

`Manipulation_` is a type family which resolves into a `Manipulation`,
so don't be fooled by the input params and output row Haskell `Type`s,
which are converted into appropriate
Postgres @[@`NullType`@]@ params and `RowType` rows.
Use `Squeal.PostgreSQL.Session.Statement.manipulation` to
fix actual Haskell input params and output rows.

>>> :set -XDeriveAnyClass -XDerivingStrategies
>>> type Columns = '["col1" ::: 'NoDef :=> 'Null 'PGint8, "col2" ::: 'Def :=> 'NotNull 'PGtext]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
data Row = Row { col1 :: Maybe Int64, col2 :: String }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
:}

>>> :{
let
  manp :: Manipulation_ (Public Schema) (Int64, Int64) Row
  manp = deleteFrom #tab NoUsing (#col1 .== param @1 + param @2) (Returning Star)
  stmt :: Statement (Public Schema) (Int64, Int64) Row
  stmt = manipulation manp
:}

>>> :type manp
manp
  :: Manipulation
       '[]
       '["public" ::: '["tab" ::: 'Table ('[] :=> Columns)]]
       '[ 'NotNull 'PGint8, 'NotNull 'PGint8]
       '["col1" ::: 'Null 'PGint8, "col2" ::: 'NotNull 'PGtext]
>>> :type stmt
stmt
  :: Statement
       '["public" ::: '["tab" ::: 'Table ('[] :=> Columns)]]
       (Int64, Int64)
       Row
-}
type family Manipulation_ (db :: SchemasType) (params :: Type) (row :: Type) where
  Manipulation_ db params row = Manipulation '[] db (TuplePG params) (RowPG row)

-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query '[] with db params columns
  -- ^ `Query` to embed as a `Manipulation`
  -> Manipulation with db params columns
queryStatement q = UnsafeManipulation $ renderSQL q

-- | A `ReturningClause` computes and returns value(s) based
-- on each row actually inserted, updated or deleted. This is primarily
-- useful for obtaining values that were supplied by defaults, such as a
-- serial sequence number. However, any expression using the table's columns
-- is allowed. Only rows that were successfully inserted or updated or
-- deleted will be returned. For example, if a row was locked
-- but not updated because an `Squeal.PostgreSQL.Manipulation.Insert.OnConflict`
-- `Squeal.PostgreSQL.Manipulation.Insert.DoUpdate` condition was not satisfied,
-- the row will not be returned. `Returning` `Star` will return all columns
-- in the row. Use `Returning_` `Nil` in the common case where no return
-- values are desired.
newtype ReturningClause with db params from row =
  Returning (Selection  'Ungrouped '[] with db params from row)

instance RenderSQL (ReturningClause with db params from row) where
  renderSQL = \case
    Returning (List Nil) -> ""
    Returning selection -> " RETURNING" <+> renderSQL selection

-- | `Returning` a `List`
pattern Returning_
  :: SOP.SListI row
  => NP (Aliased (Expression  'Ungrouped '[] with db params from)) row
  -- ^ row of values
  -> ReturningClause with db params from row
pattern Returning_ list = Returning (List list)

-- | Specify additional tables with `Using`
-- an `also` list of table expressions, allowing columns
-- from other tables to appear in the WHERE condition.
-- This is similar to the list of tables that can be specified
-- in the FROM Clause of a SELECT statement;
-- for example, an alias for the table name can be specified.
-- Do not repeat the target table in the `Using` list,
-- unless you wish to set up a self-join.
-- `NoUsing` if no additional tables are to be used.
data UsingClause with db params from where
  NoUsing :: UsingClause with db params '[]
  Using
    :: FromClause '[] with db params from
    -- ^ what to use
    -> UsingClause with db params from

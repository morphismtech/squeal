{-|
Module: Squeal.PostgreSQL.Manipulation
Description: Squeal data manipulation language
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal data manipulation language.
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
  , TypeInType
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Manipulation
  ( -- * Manipulation
    Manipulation_
  , Manipulation (..)
  , queryStatement
  , ReturningClause (..)
  , pattern Returning_
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
import Squeal.PostgreSQL.Query.Select
import Squeal.PostgreSQL.Query.With
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import Data.Int
-- >>> import Data.Time

{- |
A `Manipulation` is a statement which may modify data in the database,
but does not alter its schemas. Examples are inserts, updates and deletes.
A `Query` is also considered a `Manipulation` even though it does not modify data.

The general `Manipulation` type is parameterized by

* @with :: FromType@ - scope for all `common` table expressions,
* @db :: SchemasType@ - scope for all `table`s and `view`s,
* @params :: [NullType]@ - scope for all `Squeal.Expression.Parameter.parameter`s,
* @row :: RowType@ - return type of the `Query`.
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
The top level `Manipulation_` type is parameterized by a @db@ `SchemasType`,
against which the query is type-checked, an input @params@ Haskell `Type`,
and an ouput row Haskell `Type`.

A top-level `Manipulation_` can be run
using `Squeal.PostgreSQL.Session.manipulateParams`, or if @params = ()@
using `Squeal.PostgreSQL.Session.manipulate`.

Generally, @params@ will be a Haskell tuple or record whose entries
may be referenced using positional
`Squeal.PostgreSQL.Expression.Parameter.param`s and @row@ will be a
Haskell record, whose entries will be targeted using overloaded labels.

>>> :set -XDeriveAnyClass -XDerivingStrategies
>>> :{
data Row a b = Row { col1 :: a, col2 :: b }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
:}

simple insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'Null 'PGint4, "col2" ::: 'Def :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () ()
  manipulation =
    insertInto_ #tab (Values_ (Set 2 `as` #col1 :* Default `as` #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES ((2 :: int4), DEFAULT)

out-of-line parameterized insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Manipulation_ (Public Schema) (Int32, Int32) ()
  manipulation =
    insertInto_ #tab (Values_ (Set (param @1) `as` #col1 :* Set (param @2) `as` #col2))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES (($1 :: int4), ($2 :: int4))

in-line parameterized insert:

>>> type Columns = '["col1" ::: 'NoDef :=> 'NotNull 'PGint4, "col2" ::: 'NoDef :=> 'NotNull 'PGint4]
>>> type Schema = '["tab" ::: 'Table ('[] :=> Columns)]
>>> :{
let
  manipulation :: Row Int32 Int32 -> Manipulation_ (Public Schema) () ()
  manipulation row =
    insertInto_ #tab (inlineValues_ row)
in printSQL (manipulation (Row 1 2))
:}
INSERT INTO "tab" ("col1", "col2") VALUES ((1 :: int4), (2 :: int4))

returning insert:

>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () (Only Int32)
  manipulation =
    insertInto #tab (Values_ (Set 2 `as` #col1 :* Set 3 `as` #col2))
      OnConflictDoRaise (Returning (#col1 `as` #fromOnly))
in printSQL manipulation
:}
INSERT INTO "tab" ("col1", "col2") VALUES ((2 :: int4), (3 :: int4)) RETURNING "col1" AS "fromOnly"

upsert:

>>> type CustomersColumns = '["name" ::: 'NoDef :=> 'NotNull 'PGtext, "email" ::: 'NoDef :=> 'NotNull 'PGtext]
>>> type CustomersConstraints = '["uq" ::: 'Unique '["name"]]
>>> type CustomersSchema = '["customers" ::: 'Table (CustomersConstraints :=> CustomersColumns)]
>>> :{
let
  manipulation :: Manipulation_ (Public CustomersSchema) () ()
  manipulation =
    insertInto #customers
      (Values_ (Set "John Smith" `as` #name :* Set "john@smith.com" `as` #email))
      (OnConflict (OnConstraint #uq)
        (DoUpdate (Set (#excluded ! #email <> "; " <> #customers ! #email) `as` #email) []))
      (Returning_ Nil)
in printSQL manipulation
:}
INSERT INTO "customers" ("name", "email") VALUES ((E'John Smith' :: text), (E'john@smith.com' :: text)) ON CONFLICT ON CONSTRAINT "uq" DO UPDATE SET "email" = ("excluded"."email" || ((E'; ' :: text) || "customers"."email"))

query insert:

>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () ()
  manipulation = insertInto_ #tab (Subquery (select Star (from (table #tab))))
in printSQL manipulation
:}
INSERT INTO "tab" SELECT * FROM "tab" AS "tab"

update:

>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () ()
  manipulation = update_ #tab (Set 2 `as` #col1) (#col1 ./= #col2)
in printSQL manipulation
:}
UPDATE "tab" SET "col1" = (2 :: int4) WHERE ("col1" <> "col2")

delete:

>>> :{
let
  manipulation :: Manipulation_ (Public Schema) () (Row Int32 Int32)
  manipulation = deleteFrom #tab NoUsing (#col1 .== #col2) (Returning Star)
in printSQL manipulation
:}
DELETE FROM "tab" WHERE ("col1" = "col2") RETURNING *

delete and using clause:

>>> :{
type Schema3 =
  '[ "tab" ::: 'Table ('[] :=> Columns)
   , "other_tab" ::: 'Table ('[] :=> Columns)
   , "third_tab" ::: 'Table ('[] :=> Columns) ]
:}

>>> :{
let
  manipulation :: Manipulation_ (Public Schema3) () ()
  manipulation =
    deleteFrom #tab (Using (table #other_tab & also (table #third_tab)))
    ( (#tab ! #col2 .== #other_tab ! #col2)
    .&& (#tab ! #col2 .== #third_tab ! #col2) )
    (Returning_ Nil)
in printSQL manipulation
:}
DELETE FROM "tab" USING "other_tab" AS "other_tab", "third_tab" AS "third_tab" WHERE (("tab"."col2" = "other_tab"."col2") AND ("tab"."col2" = "third_tab"."col2"))

with manipulation:

>>> type ProductsColumns = '["product" ::: 'NoDef :=> 'NotNull 'PGtext, "date" ::: 'Def :=> 'NotNull 'PGdate]
>>> type ProductsSchema = '["products" ::: 'Table ('[] :=> ProductsColumns), "products_deleted" ::: 'Table ('[] :=> ProductsColumns)]
>>> :{
let
  manipulation :: Manipulation_ (Public ProductsSchema) (Only Day) ()
  manipulation = with
    (deleteFrom #products NoUsing (#date .< param @1) (Returning Star) `as` #del)
    (insertInto_ #products_deleted (Subquery (select Star (from (common #del)))))
in printSQL manipulation
:}
WITH "del" AS (DELETE FROM "products" WHERE ("date" < ($1 :: date)) RETURNING *) INSERT INTO "products_deleted" SELECT * FROM "del" AS "del"
-}
type family Manipulation_ (db :: SchemasType) (params :: Type) (row :: Type) where
  Manipulation_ db params row = Manipulation '[] db (TuplePG params) (RowPG row)

-- | Convert a `Query` into a `Manipulation`.
queryStatement
  :: Query '[] with db params columns
  -- ^ `Query` to embed as a `Manipulation`
  -> Manipulation with db params columns
queryStatement q = UnsafeManipulation $ renderSQL q

-- | A `ReturningClause` computes and return value(s) based
-- on each row actually inserted, updated or deleted. This is primarily
-- useful for obtaining values that were supplied by defaults, such as a
-- serial sequence number. However, any expression using the table's columns
-- is allowed. Only rows that were successfully inserted or updated or
-- deleted will be returned. For example, if a row was locked
-- but not updated because an `OnConflict` `DoUpdate` condition was not satisfied,
-- the row will not be returned. `Returning` `Star` will return all columns
-- in the row. Use `Returning` `Nil` in the common case where no return
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

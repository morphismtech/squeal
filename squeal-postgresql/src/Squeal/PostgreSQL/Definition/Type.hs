{-|
Module: Squeal.PostgreSQL.Definition.Type
Description: create and drop types
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

create and drop types
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeOperators
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Definition.Type
  ( -- * Create
    createTypeEnum
  , createTypeEnumFrom
  , createTypeComposite
  , createTypeCompositeFrom
  , createTypeRange
  , createDomain
    -- * Drop
  , dropType
  , dropTypeIfExists
  ) where

import Data.ByteString
import Data.Monoid
import GHC.TypeLits
import Prelude hiding ((.), id)

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Type.PG
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import qualified GHC.Generics as GHC
-- >>> import qualified Generics.SOP as SOP

-- | Enumerated types are created using the `createTypeEnum` command, for example
--
-- >>> printSQL $ (createTypeEnum #mood (label @"sad" :* label @"ok" :* label @"happy") :: Definition (Public '[]) '["public" ::: '["mood" ::: 'Typedef ('PGenum '["sad","ok","happy"])]])
-- CREATE TYPE "mood" AS ENUM ('sad', 'ok', 'happy');
createTypeEnum
  :: (KnownSymbol enum, Has sch db schema, SOP.All KnownSymbol labels)
  => QualifiedAlias sch enum
  -- ^ name of the user defined enumerated type
  -> NP PGlabel labels
  -- ^ labels of the enumerated type
  -> Definition db (Alter sch (Create enum ('Typedef ('PGenum labels)) schema) db)
createTypeEnum enum labels = UnsafeDefinition $
  "CREATE" <+> "TYPE" <+> renderSQL enum <+> "AS" <+> "ENUM" <+>
  parenthesized (renderSQL labels) <> ";"

-- | Enumerated types can also be generated from a Haskell type, for example
--
-- >>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
-- >>> instance SOP.Generic Schwarma
-- >>> instance SOP.HasDatatypeInfo Schwarma
-- >>> :{
-- let
--   createSchwarma :: Definition (Public '[]) '["public" ::: '["schwarma" ::: 'Typedef (PG (Enumerated Schwarma))]]
--   createSchwarma = createTypeEnumFrom @Schwarma #schwarma
-- in
--   printSQL createSchwarma
-- :}
-- CREATE TYPE "schwarma" AS ENUM ('Beef', 'Lamb', 'Chicken');
createTypeEnumFrom
  :: forall hask sch enum db schema.
  ( SOP.Generic hask
  , SOP.All KnownSymbol (LabelsPG hask)
  , KnownSymbol enum
  , Has sch db schema )
  => QualifiedAlias sch enum
  -- ^ name of the user defined enumerated type
  -> Definition db (Alter sch (Create enum ('Typedef (PG (Enumerated hask))) schema) db)
createTypeEnumFrom enum = createTypeEnum enum
  (SOP.hpure label :: NP PGlabel (LabelsPG hask))

{- | `createTypeComposite` creates a composite type. The composite type is
specified by a list of attribute names and data types.

>>> :{
type PGcomplex = 'PGcomposite
  '[ "real"      ::: 'NotNull 'PGfloat8
   , "imaginary" ::: 'NotNull 'PGfloat8 ]
:}

>>> :{
let
  setup :: Definition (Public '[]) '["public" ::: '["complex" ::: 'Typedef PGcomplex]]
  setup = createTypeComposite #complex
    (float8 `as` #real :* float8 `as` #imaginary)
in printSQL setup
:}
CREATE TYPE "complex" AS ("real" float8, "imaginary" float8);
-}
createTypeComposite
  :: (KnownSymbol ty, Has sch db schema, SOP.SListI fields)
  => QualifiedAlias sch ty
  -- ^ name of the user defined composite type
  -> NP (Aliased (TypeExpression db)) fields
  -- ^ list of attribute names and data types
  -> Definition db (Alter sch (Create ty ('Typedef ('PGcomposite fields)) schema) db)
createTypeComposite ty fields = UnsafeDefinition $
  "CREATE" <+> "TYPE" <+> renderSQL ty <+> "AS" <+> parenthesized
  (renderCommaSeparated renderField fields) <> ";"
  where
    renderField :: Aliased (TypeExpression db) x -> ByteString
    renderField (typ `As` alias) =
      renderSQL alias <+> renderSQL typ

-- | Composite types can also be generated from a Haskell type, for example
--
-- >>> data Complex = Complex {real :: Double, imaginary :: Double} deriving GHC.Generic
-- >>> instance SOP.Generic Complex
-- >>> instance SOP.HasDatatypeInfo Complex
-- >>> type Schema = '["complex" ::: 'Typedef (PG (Composite Complex))]
-- >>> :{
-- let
--   createComplex :: Definition (Public '[]) (Public Schema)
--   createComplex = createTypeCompositeFrom @Complex #complex
-- in
--   printSQL createComplex
-- :}
-- CREATE TYPE "complex" AS ("real" float8, "imaginary" float8);
createTypeCompositeFrom
  :: forall hask sch ty db schema.
  ( SOP.All (FieldTyped db) (RowPG hask)
  , KnownSymbol ty
  , Has sch db schema )
  => QualifiedAlias sch ty
  -- ^ name of the user defined composite type
  -> Definition db (Alter sch (Create ty ( 'Typedef (PG (Composite hask))) schema) db)
createTypeCompositeFrom ty = createTypeComposite ty
  (SOP.hcpure (SOP.Proxy :: SOP.Proxy (FieldTyped db)) fieldtype
    :: NP (Aliased (TypeExpression db)) (RowPG hask))

{-|
`createDomain` creates a new domain. A domain is essentially a data type
with constraints (restrictions on the allowed set of values).

Domains are useful for abstracting common constraints on fields
into a single location for maintenance. For example, several tables might
contain email address columns, all requiring the same
`Squeal.PostgreSQL.Definition.Table.Constraint.check` constraint
to verify the address syntax. Define a domain rather than setting up
each table's constraint individually.

>>> :{
let
  createPositive :: Definition (Public '[]) (Public '["positive" ::: 'Typedef 'PGfloat4])
  createPositive = createDomain #positive real (#value .> 0)
in printSQL createPositive
:}
CREATE DOMAIN "positive" AS real CHECK (("value" > (0.0 :: float4)));
-}
createDomain
  :: (Has sch db schema, KnownSymbol dom)
  => QualifiedAlias sch dom -- ^ domain alias
  -> (forall null. TypeExpression db (null ty)) -- ^ underlying type
  -> (forall tab. Condition 'Ungrouped '[] '[] db '[] '[tab ::: '["value" ::: 'Null ty]])
    -- ^ constraint on type
  -> Definition db (Alter sch (Create dom ('Typedef ty) schema) db)
createDomain dom ty condition =
  UnsafeDefinition $ "CREATE DOMAIN" <+> renderSQL dom
    <+> "AS" <+> renderTypeExpression ty
    <+> "CHECK" <+> parenthesized (renderSQL condition) <> ";"

{- | Range types are data types representing a range of values
of some element type (called the range's subtype).
The subtype must have a total order so that it is well-defined
whether element values are within, before, or after a range of values.

Range types are useful because they represent many element values
in a single range value, and because concepts such as overlapping ranges
can be expressed clearly.
The use of time and date ranges for scheduling purposes
is the clearest example; but price ranges,
measurement ranges from an instrument, and so forth can also be useful.

>>> :{
let
  createSmallIntRange :: Definition (Public '[]) (Public '["int2range" ::: 'Typedef ('PGrange 'PGint2)])
  createSmallIntRange = createTypeRange #int2range int2
in printSQL createSmallIntRange
:}
CREATE TYPE "int2range" AS RANGE (subtype = int2);
-}
createTypeRange
  :: (Has sch db schema, KnownSymbol range)
  => QualifiedAlias sch range -- ^ range alias
  -> (forall null. TypeExpression db (null ty)) -- ^ underlying type
  -> Definition db (Alter sch (Create range ('Typedef ('PGrange ty)) schema) db)
createTypeRange range ty = UnsafeDefinition $
  "CREATE" <+> "TYPE" <+> renderSQL range <+> "AS" <+> "RANGE" <+>
  parenthesized ("subtype" <+> "=" <+> renderTypeExpression ty) <> ";"

-- | Drop a type.
--
-- >>> data Schwarma = Beef | Lamb | Chicken deriving GHC.Generic
-- >>> instance SOP.Generic Schwarma
-- >>> instance SOP.HasDatatypeInfo Schwarma
-- >>> printSQL (dropType #schwarma :: Definition '["public" ::: '["schwarma" ::: 'Typedef (PG (Enumerated Schwarma))]] (Public '[]))
-- DROP TYPE "schwarma";
dropType
  :: (Has sch db schema, KnownSymbol td)
  => QualifiedAlias sch td
  -- ^ name of the user defined type
  -> Definition db (Alter sch (DropSchemum td 'Typedef schema) db)
dropType tydef = UnsafeDefinition $ "DROP TYPE" <+> renderSQL tydef <> ";"

-- | Drop a type if it exists.
dropTypeIfExists
  :: (Has sch db schema, KnownSymbol td)
  => QualifiedAlias sch td
  -- ^ name of the user defined type
  -> Definition db (Alter sch (DropSchemumIfExists td 'Typedef schema) db)
dropTypeIfExists tydef = UnsafeDefinition $
  "DROP TYPE IF EXISTS" <+> renderSQL tydef <> ";"

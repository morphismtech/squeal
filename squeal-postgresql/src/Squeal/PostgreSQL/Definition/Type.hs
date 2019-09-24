{-|
Module: Squeal.PostgreSQL.Definition.Type
Description: Create and drop type definitions
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

Create and drop type definitions.
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
  ( createTypeEnum
  , createTypeEnumFrom
  , createTypeComposite
  , createTypeCompositeFrom
  , createTypeRange
  , createDomain
  , dropType
  , dropTypeIfExists
  ) where

import Data.ByteString
import Data.Monoid
import GHC.TypeLits
import Prelude hiding ((.), id)

import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression.Logic
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.PG
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL
-- >>> import qualified GHC.Generics as GHC
-- >>> import qualified Generics.SOP as SOP

-- | Enumerated types are created using the `createTypeEnum` command, for example
--
-- >>> printSQL $ (createTypeEnum #mood (label @"sad" :* label @"ok" :* label @"happy") :: Definition (Public '[]) '["public" ::: '["mood" ::: 'Typedef ('PGenum '["sad","ok","happy"])]])
-- CREATE TYPE "mood" AS ENUM ('sad', 'ok', 'happy');
createTypeEnum
  :: (KnownSymbol enum, Has sch schemas schema, SOP.All KnownSymbol labels)
  => QualifiedAlias sch enum
  -- ^ name of the user defined enumerated type
  -> NP PGlabel labels
  -- ^ labels of the enumerated type
  -> Definition schemas (Alter sch (Create enum ('Typedef ('PGenum labels)) schema) schemas)
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
  :: forall hask sch enum schemas schema.
  ( SOP.Generic hask
  , SOP.All KnownSymbol (LabelsPG hask)
  , KnownSymbol enum
  , Has sch schemas schema )
  => QualifiedAlias sch enum
  -- ^ name of the user defined enumerated type
  -> Definition schemas (Alter sch (Create enum ('Typedef (PG (Enumerated hask))) schema) schemas)
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
  :: (KnownSymbol ty, Has sch schemas schema, SOP.SListI fields)
  => QualifiedAlias sch ty
  -- ^ name of the user defined composite type
  -> NP (Aliased (TypeExpression schemas)) fields
  -- ^ list of attribute names and data types
  -> Definition schemas (Alter sch (Create ty ('Typedef ('PGcomposite fields)) schema) schemas)
createTypeComposite ty fields = UnsafeDefinition $
  "CREATE" <+> "TYPE" <+> renderSQL ty <+> "AS" <+> parenthesized
  (renderCommaSeparated renderField fields) <> ";"
  where
    renderField :: Aliased (TypeExpression schemas) x -> ByteString
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
  :: forall hask sch ty schemas schema.
  ( SOP.All (FieldTyped schemas) (RowPG hask)
  , KnownSymbol ty
  , Has sch schemas schema )
  => QualifiedAlias sch ty
  -- ^ name of the user defined composite type
  -> Definition schemas (Alter sch (Create ty ( 'Typedef (PG (Composite hask))) schema) schemas)
createTypeCompositeFrom ty = createTypeComposite ty
  (SOP.hcpure (SOP.Proxy :: SOP.Proxy (FieldTyped schemas)) fieldtype
    :: NP (Aliased (TypeExpression schemas)) (RowPG hask))

{-|
`createDomain` creates a new domain. A domain is essentially a data type
with constraints (restrictions on the allowed set of values).

Domains are useful for abstracting common constraints on fields
into a single location for maintenance. For example, several tables might
contain email address columns, all requiring the same `check` constraint
to verify the address syntax. Define a domain rather than setting up
each table's constraint individually.

>>> :{
let
  createPositive :: Definition (Public '[]) (Public '["positive" ::: 'Typedef 'PGfloat4])
  createPositive = createDomain #positive real (#value .> 0 .&& (#value & isNotNull))
in printSQL createPositive
:}
CREATE DOMAIN "positive" AS real CHECK ((("value" > 0) AND "value" IS NOT NULL));
-}
createDomain
  :: (Has sch schemas schema, KnownSymbol dom)
  => QualifiedAlias sch dom
  -> (forall null. TypeExpression schemas (null ty))
  -> (forall tab. Condition '[] '[] 'Ungrouped schemas '[] '[tab ::: '["value" ::: 'Null ty]])
  -> Definition schemas (Alter sch (Create dom ('Typedef ty) schema) schemas)
createDomain dom ty condition =
  UnsafeDefinition $ "CREATE DOMAIN" <+> renderSQL dom
    <+> "AS" <+> renderTypeExpression ty
    <+> "CHECK" <+> parenthesized (renderSQL condition) <> ";"

{- |
>>> :{
let
  createSmallIntRange :: Definition (Public '[]) (Public '["int2range" ::: 'Typedef ('PGrange 'PGint2)])
  createSmallIntRange = createTypeRange #int2range int2
in printSQL createSmallIntRange
:}
CREATE TYPE "int2range" AS RANGE (subtype = int2);
-}
createTypeRange
  :: (Has sch schemas schema, KnownSymbol range)
  => QualifiedAlias sch range
  -> (forall null. TypeExpression schemas (null ty))
  -> Definition schemas (Alter sch (Create range ('Typedef ('PGrange ty)) schema) schemas)
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
  :: (Has sch schemas schema, KnownSymbol td)
  => QualifiedAlias sch td
  -- ^ name of the user defined type
  -> Definition schemas (Alter sch (DropSchemum td 'Typedef schema) schemas)
dropType tydef = UnsafeDefinition $ "DROP" <+> "TYPE" <+> renderSQL tydef <> ";"

dropTypeIfExists
  :: (Has sch schemas schema, KnownSymbol td)
  => QualifiedAlias sch td
  -- ^ name of the user defined type
  -> Definition schemas (Alter sch (DropSchemumIfExists td 'Typedef schema) schemas)
dropTypeIfExists tydef = UnsafeDefinition $ "DROP" <+> "TYPE" <+> renderSQL tydef <> ";"

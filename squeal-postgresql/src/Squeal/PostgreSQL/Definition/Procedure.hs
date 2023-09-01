{-|
Module: Squeal.PostgreSQL.Definition.Procedure
Description: create and drop procedures
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

create and drop procedures
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
  , DataKinds
  , PolyKinds
  , TypeOperators
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Definition.Procedure
  ( -- * Create
    createProcedure
  , createOrReplaceProcedure
    -- * Drop
  , dropProcedure
  , dropProcedureIfExists
    -- * Procedure Definition
  , ProcedureDefinition(..)
  , languageSqlManipulation
  ) where

import Control.DeepSeq
import Data.ByteString
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- | Create a procedure.

>>> type Proc = 'Procedure '[ 'NotNull 'PGint4 ]
>>> type Thing = 'Table ('[] :=> '[ "id" ::: 'NoDef :=> 'NotNull 'PGint4 ])
>>> :{
let
  definition :: Definition (Public '["things" ::: Thing ]) (Public '["things" ::: Thing, "proc" ::: Proc])
  definition = createProcedure #proc (one int4) 
             . languageSqlManipulation
             $ [deleteFrom_ #things (#id .== param @1)]
in printSQL definition
:}
CREATE PROCEDURE "proc" (int4) language sql as $$ DELETE FROM "things" AS "things" WHERE ("id" = ($1 :: int4)); $$;
-}
createProcedure
  :: ( Has sch db schema
     , KnownSymbol pro
     , SOP.SListI args )
  => QualifiedAlias sch pro -- ^ procedure alias
  -> NP (TypeExpression db) args -- ^ arguments
  -> ProcedureDefinition db args -- ^ procedure definition
  -> Definition db (Alter sch (Create pro ('Procedure args) schema) db)
createProcedure pro args prodef = UnsafeDefinition $
  "CREATE" <+> "PROCEDURE" <+> renderSQL pro
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> renderSQL prodef <> ";"

{- | Create or replace a procedure.
It is not possible to change the name or argument types
of a procedure this way.

>>> type Proc = 'Procedure '[ 'NotNull 'PGint4 ]
>>> type Thing = 'Table ('[] :=> '[ "id" ::: 'NoDef :=> 'NotNull 'PGint4 ])
>>> :{
let
  definition :: Definition (Public '["things" ::: Thing ]) (Public '["things" ::: Thing, "proc" ::: Proc])
  definition = createOrReplaceProcedure #proc (one int4) 
             . languageSqlManipulation
             $ [deleteFrom_ #things (#id .== param @1)]
in printSQL definition
:}
CREATE OR REPLACE PROCEDURE "proc" (int4) language sql as $$ DELETE FROM "things" AS "things" WHERE ("id" = ($1 :: int4)); $$;
-}
createOrReplaceProcedure
  :: ( Has sch db schema
     , KnownSymbol pro
     , SOP.SListI args )
  => QualifiedAlias sch pro -- ^ procedure alias
  -> NP (TypeExpression db) args -- ^ arguments
  -> ProcedureDefinition db args -- ^ procedure definition
  -> Definition db (Alter sch (CreateOrReplace pro ('Procedure args) schema) db)
createOrReplaceProcedure pro args prodef = UnsafeDefinition $
  "CREATE" <+> "OR" <+> "REPLACE" <+> "PROCEDURE" <+> renderSQL pro
    <+> parenthesized (renderCommaSeparated renderSQL args)
    <+> renderSQL prodef <> ";"

-- | Use a parameterized `Manipulation` as a procedure body
languageSqlManipulation
  :: [Manipulation '[] db args '[]]
  -- ^ procedure body
  -> ProcedureDefinition db args
languageSqlManipulation mnps = UnsafeProcedureDefinition $
  "language sql as" <+> "$$" <+> Prelude.foldr (<+>) "" (Prelude.map ((<> ";") . renderSQL) mnps) <> "$$"

-- | 

{- | Drop a procedure.

>>> type Proc = 'Procedure '[ 'Null 'PGint4, 'Null 'PGint4]
>>> :{
let
  definition :: Definition (Public '["proc" ::: Proc]) (Public '[])
  definition = dropProcedure #proc
in printSQL definition
:}
DROP PROCEDURE "proc";
-}
dropProcedure
  :: (Has sch db schema, KnownSymbol pro)
  => QualifiedAlias sch pro
  -- ^ procedure alias
  -> Definition db (Alter sch (DropSchemum pro 'Procedure schema) db)
dropProcedure pro = UnsafeDefinition $
  "DROP PROCEDURE" <+> renderSQL pro <> ";"

{- | Drop a procedure.

>>> type Proc = 'Procedure '[ 'Null 'PGint4, 'Null 'PGint4 ]
>>> :{
let
  definition :: Definition (Public '[]) (Public '[])
  definition = dropProcedureIfExists #proc
in printSQL definition
:}
DROP PROCEDURE IF EXISTS "proc";
-}
dropProcedureIfExists
  :: (Has sch db schema, KnownSymbol pro)
  => QualifiedAlias sch pro
  -- ^ procedure alias
  -> Definition db (Alter sch (DropSchemumIfExists pro 'Procedure schema) db)
dropProcedureIfExists pro = UnsafeDefinition $
  "DROP PROCEDURE IF EXISTS" <+> renderSQL pro <> ";"

{- | Body of a user defined procedure-}
newtype ProcedureDefinition db args = UnsafeProcedureDefinition
  { renderProcedureDefinition :: ByteString }
  deriving (Eq,Show,GHC.Generic,NFData)
instance RenderSQL (ProcedureDefinition db args) where
  renderSQL = renderProcedureDefinition

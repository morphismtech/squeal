{- |
Module: Squeal.PostgreSQL.Definition.Constraint
Description: comments
Copyright: (c) Eitan Chatav, 2020
Maintainer: eitan@morphism.tech
Stability: experimental
 
comments
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

module Squeal.PostgreSQL.Definition.Comment
  ( commentOnTable
  , commentOnType
  , commentOnView
  , commentOnFunction
  , commentOnIndex
  , commentOnColumn
  ) where

import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema
import GHC.TypeLits (KnownSymbol)
import Data.Text (Text)

commentOnTable
  :: ( KnownSymbol sch
     , KnownSymbol tab
     , Has sch db schema
     , Has tab schema ('Table table)
     )
  => QualifiedAlias sch tab
  -> Text
  -> Definition db db
commentOnTable alias comm = UnsafeDefinition $
  "COMMENT ON TABLE" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <+> ";"

commentOnType
  :: ( KnownSymbol sch
     , KnownSymbol typ
     , Has sch db schema
     , Has typ schema ('Typedef type_)
     )
  => QualifiedAlias sch typ
  -> Text
  -> Definition db db
commentOnType alias comm = UnsafeDefinition $
  "COMMENT ON TYPE" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <+> ";"

commentOnView
  :: ( KnownSymbol sch
     , KnownSymbol vie
     , Has sch db schema
     , Has vie schema ('View view)
     )
  => QualifiedAlias sch vie
  -> Text
  -> Definition db db
commentOnView alias comm = UnsafeDefinition $
  "COMMENT ON VIEW" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <+> ";"

commentOnIndex
  :: ( KnownSymbol sch
     , KnownSymbol ind
     , Has sch db schema
     , Has ind schema ('Index index)
     )
  => QualifiedAlias sch ind
  -> Text
  -> Definition db db
commentOnIndex alias comm = UnsafeDefinition $
  "COMMENT ON INDEX" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <+> ";"

commentOnFunction
  :: ( KnownSymbol sch
     , KnownSymbol fun
     , Has sch db schema
     , Has fun schema ('Function function)
     )
  => QualifiedAlias sch fun
  -> Text
  -> Definition db db
commentOnFunction alias comm = UnsafeDefinition $
  "COMMENT ON FUNCTION" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <+> ";"

commentOnColumn
  :: ( KnownSymbol sch
     , KnownSymbol tab
     , KnownSymbol col
     , Has sch db schema
     , Has tab schema ('Table '(cons, cols))
     , Has col cols '(def, nulltyp)
     )
  => QualifiedAlias sch tab
  -> Alias col 
  -> Text
  -> Definition db db
commentOnColumn table col comm = UnsafeDefinition $
  "COMMENT ON COLUMN" <+> renderSQL table <> "." <> renderSQL col <+> "IS" <+> singleQuotedText comm <+> ";"

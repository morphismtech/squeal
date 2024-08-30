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
  , DataKinds
  , PolyKinds
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
  , commentOnSchema
  ) where

import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema
import GHC.TypeLits (KnownSymbol)
import Data.Text (Text)

{-----------------------------------------
COMMENT statements
-----------------------------------------}

{- |
When a user views a table in the database (i.e. with \d+ <table>), it is useful
to be able to read a description of the table.
-}
commentOnTable
  :: ( KnownSymbol sch
     , KnownSymbol tab
     , Has sch db schema
     , Has tab schema ('Table table)
     )
  => QualifiedAlias sch tab -- ^ table
  -> Text -- ^ comment
  -> Definition db db
commentOnTable alias comm = UnsafeDefinition $
  "COMMENT ON TABLE" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <> ";"

{- |
When a user views a type in the database (i.e with \dT <type>), it is useful to
be able to read a description of the type.
-}
commentOnType
  :: ( KnownSymbol sch
     , KnownSymbol typ
     , Has sch db schema
     , Has typ schema ('Typedef type_)
     )
  => QualifiedAlias sch typ -- ^ type
  -> Text -- ^ comment
  -> Definition db db
commentOnType alias comm = UnsafeDefinition $
  "COMMENT ON TYPE" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <> ";"

{- |
When a user views a view in the database (i.e. with \dv <view>), it is useful
to be able to read a description of the view.
-}
commentOnView
  :: ( KnownSymbol sch
     , KnownSymbol vie
     , Has sch db schema
     , Has vie schema ('View view)
     )
  => QualifiedAlias sch vie -- ^ view
  -> Text -- ^ comment
  -> Definition db db
commentOnView alias comm = UnsafeDefinition $
  "COMMENT ON VIEW" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <> ";"

{- |
When a user views an index in the database (i.e. with \di+ <index>), it is
useful to be able to read a description of the index.
-}
commentOnIndex
  :: ( KnownSymbol sch
     , KnownSymbol ind
     , Has sch db schema
     , Has ind schema ('Index index)
     )
  => QualifiedAlias sch ind -- ^ index
  -> Text -- ^ comment
  -> Definition db db
commentOnIndex alias comm = UnsafeDefinition $
  "COMMENT ON INDEX" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <> ";"

{- |
When a user views a function in the database (i.e. with \df+ <function>), it is
useful to be able to read a description of the function.
-}
commentOnFunction
  :: ( KnownSymbol sch
     , KnownSymbol fun
     , Has sch db schema
     , Has fun schema ('Function function)
     )
  => QualifiedAlias sch fun -- ^ function
  -> Text -- ^ comment
  -> Definition db db
commentOnFunction alias comm = UnsafeDefinition $
  "COMMENT ON FUNCTION" <+> renderSQL alias <+> "IS" <+> singleQuotedText comm <> ";"

{- |
When a user views a table in the database (i.e. with \d+ <table>), it is useful
to be able to view descriptions of the columns in that table.
-}
commentOnColumn
  :: ( KnownSymbol sch
     , KnownSymbol tab
     , KnownSymbol col
     , Has sch db schema
     , Has tab schema ('Table '(cons, cols))
     , Has col cols '(def, nulltyp)
     )
  => QualifiedAlias sch tab -- ^ table
  -> Alias col -- ^ column
  -> Text -- ^ comment
  -> Definition db db
commentOnColumn table col comm = UnsafeDefinition $
  "COMMENT ON COLUMN" <+> renderSQL table <> "." <> renderSQL col <+> "IS"
  <+> singleQuotedText comm <> ";"

{- |
When a user views a schema in the database (i.e. with \dn+ <schema>), it is
useful to be able to read a description.
-}
commentOnSchema
  :: ( KnownSymbol sch
     , Has sch db schema
     )
  => Alias sch -- ^ schema
  -> Text -- ^ comment
  -> Definition db db
commentOnSchema schema comm = UnsafeDefinition $
  "COMMENT ON SCHEMA" <+> renderSQL schema <> "IS" <+> singleQuotedText comm <> ";"

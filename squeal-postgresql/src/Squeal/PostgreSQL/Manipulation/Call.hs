{-|
Module: Squeal.PostgreSQL.Call
Description: call statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

update statements
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

module Squeal.PostgreSQL.Manipulation.Call
  ( -- * Call
    call
  , unsafeCall
  , callN
  , unsafeCallN
  ) where

import Data.ByteString hiding (foldr)

import Generics.SOP (SListI)

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-
>>> :{
printSQL $ unsafeCall "p" true
:}
CALL p(TRUE)
-}
unsafeCall :: ByteString -> Expression grp lat with db params from x -> Manipulation with db params '[]
unsafeCall pro x = UnsafeManipulation $
  "CALL" <+> pro <> parenthesized (renderSQL x)

{- | Call a user defined procedure of one variable.

>>> type Schema = '[ "p" ::: 'Procedure '[ 'NotNull 'PGint4 ] ]
>>> :{
let
  p :: Expression 'Ungrouped '[] '[] (Public Schema) '[] '[] ('NotNull 'PGint4) -> Manipulation '[] (Public Schema) '[] '[]
  p = call #p
in
  printSQL (p 1)
:}
CALL "p"((1 :: int4))
-}
call
  :: ( Has sch db schema
     , Has pro schema ('Procedure '[x]) )
  => QualifiedAlias sch pro
  -> Expression grp lat with db params from x
  -> Manipulation with db params '[]
call = unsafeCall . renderSQL


{-
>>> :{
printSQL $ unsafeCallN "p" (true :* false)
:}
CALL p(TRUE, FALSE)
-}
unsafeCallN :: SListI xs => ByteString -> NP (Expression grp lat with db params from) xs -> Manipulation with db params '[]
unsafeCallN pro xs = UnsafeManipulation $ 
  "CALL" <+> pro <> parenthesized (renderCommaSeparated renderSQL xs)

{- | Call a user defined procedure.

>>> type Schema = '[ "p" ::: 'Procedure '[ 'NotNull 'PGint4, 'NotNull 'PGint4 ] ]
>>> :{
let
  p :: NP (Expression 'Ungrouped '[] '[] (Public Schema) '[] '[]) '[ 'NotNull 'PGint4, 'NotNull 'PGint4 ] -> Manipulation '[] (Public Schema) '[] '[]
  p = callN #p
in
  printSQL (p (1 *: 2))
:}
CALL "p"((1 :: int4), (2 :: int4))
-}
callN
  :: ( Has sch db schema
     , Has pro schema ('Procedure xs)
     , SListI xs )
  => QualifiedAlias sch pro
  -> NP (Expression grp lat with db params from) xs
  -> Manipulation with db params '[]
callN = unsafeCallN . renderSQL

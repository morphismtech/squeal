{-|
Module: Squeal.PostgreSQL.Call
Description: call statements
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

call statements
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

{- |
>>> printSQL $ unsafeCall "p" true
CALL p(TRUE)
-}
unsafeCall
  :: ByteString -- ^ procedure to call
  -> Expression 'Ungrouped '[] with db params '[] x -- ^ arguments
  -> Manipulation with db params '[]
unsafeCall pro x = UnsafeManipulation $
  "CALL" <+> pro <> parenthesized (renderSQL x)

{- | Call a user defined procedure of one variable.

>>> type Schema = '[ "p" ::: 'Procedure '[ 'NotNull 'PGint4 ] ]
>>> :{
let
  p :: Manipulation '[] (Public Schema) '[] '[]
  p = call #p 1
in
  printSQL p
:}
CALL "p"((1 :: int4))
-}
call
  :: ( Has sch db schema
     , Has pro schema ('Procedure '[x]) )
  => QualifiedAlias sch pro -- ^ procedure to call
  -> Expression 'Ungrouped '[] with db params '[] x -- ^ arguments
  -> Manipulation with db params '[]
call = unsafeCall . renderSQL


{- |
>>> printSQL $ unsafeCallN "p" (true *: false)
CALL p(TRUE, FALSE)
-}
unsafeCallN
  :: SListI xs
  => ByteString -- ^ procedure to call
  -> NP (Expression 'Ungrouped '[] with db params '[]) xs -- ^ arguments
  -> Manipulation with db params '[]
unsafeCallN pro xs = UnsafeManipulation $ 
  "CALL" <+> pro <> parenthesized (renderCommaSeparated renderSQL xs)

{- | Call a user defined procedure.

>>> type Schema = '[ "p" ::: 'Procedure '[ 'NotNull 'PGint4, 'NotNull 'PGtext ] ]
>>> :{
let
  p :: Manipulation '[] (Public Schema) '[] '[]
  p = callN #p (1 *: "hi")
in
  printSQL p
:}
CALL "p"((1 :: int4), (E'hi' :: text))
-}
callN
  :: ( Has sch db schema
     , Has pro schema ('Procedure xs)
     , SListI xs )
  => QualifiedAlias sch pro -- ^ procedure to call
  -> NP (Expression 'Ungrouped '[] with db params '[]) xs -- ^ arguments
  -> Manipulation with db params '[]
callN = unsafeCallN . renderSQL

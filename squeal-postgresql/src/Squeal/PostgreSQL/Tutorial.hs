{-|
Module: Squeal.PostgreSQL.Tutorail
Description: Squeal tutorial module
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

== `P` records

A `P` record is an ad-hoc, anonymous record constructed
as a tuple of `P`s, a datatype from `Generics.SOP.Record`, like so:

>>> type Rec = (P ("foo" ::: Double), P ("bar" ::: Char))

Let's see what the image of `Rec` is under the `RowPG` type family.

>>> :kind! RowPG Rec
RowPG Rec :: [(Symbol, NullityType)]
= '[ '("foo", 'NotNull 'PGfloat8), "bar" ::: 'NotNull ('PGchar 1)]

`P` records are ad-hoc, anonymous records so unlike Haskell records
you don't have to define them outside of their use site, for instance
in the row type parameter of a `Query_` type family.

>>> :{
let
  query :: Query_ schemas () (P ("foo" ::: Double))
  query = values_ (1 `as` #foo)
in printSQL query
:}
SELECT * FROM (VALUES (1)) AS t ("foo")

You can even mix `P` records with normal Haskell records, which is very
useful for creating entity types, Haskell records with associated ids.

>>> data Row = Row {foo :: Double, bar :: Char} deriving (GHC.Generic)
-}

module Squeal.PostgreSQL.Tutorial where

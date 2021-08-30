{-|
Module: Squeal.PostgreSQL.LTree
Description: ltree
Copyright: (c) Eitan Chatav, 2020
Maintainer: eitan@morphism.tech
Stability: experimental

This module implements a data type ltree for representing
labels of data stored in a hierarchical tree-like structure.
-}

{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
#-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Squeal.PostgreSQL.LTree
  ( -- * Definition
    createLTree
    -- * Types
  , LTree(..), LQuery(..), LTxtQuery(..)
  , PGltree, PGlquery, PGltxtquery
  , ltree, lquery, ltxtquery
    -- * Functions
  , subltree, subpath, subpathEnd
  , nlevel, indexLTree, indexOffset
  , text2ltree, ltree2text, lca
    -- * Operators
  , (%~), (~%), (%?), (?%), (%@), (@%)
  , (@>%), (%<@), (<@%), (%@>)
  , (&~), (~&), (&?), (?&), (&@), (@&)
  , (?@>), (?<@), (?~), (?@)
  ) where

import Control.Exception
import Control.Monad.Reader
import Data.String
import Data.Text
import GHC.Generics
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Render

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Generics.SOP as SOP
import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding

-- | Postgres ltree type
type PGltree = 'UnsafePGType "ltree"
-- | Postgres lquery type
type PGlquery = 'UnsafePGType "lquery"
-- | Postgres ltxtquery type
type PGltxtquery = 'UnsafePGType "ltxtquery"

-- | Loads ltree extension into the current database.
createLTree :: Definition db db
createLTree = UnsafeDefinition "CREATE EXTENSION \"ltree\";"

-- | Postgres ltree type expression
ltree :: TypeExpression db (null PGltree)
ltree = UnsafeTypeExpression "ltree"

-- | Postgres lquery type expression
lquery :: TypeExpression db (null PGlquery)
lquery = UnsafeTypeExpression "lquery"

-- | Postgres ltxtquery type expression
ltxtquery :: TypeExpression db (null PGltxtquery)
ltxtquery = UnsafeTypeExpression "ltxtquery"

instance PGTyped db PGltree where pgtype = ltree
instance PGTyped db PGlquery where pgtype = lquery
instance PGTyped db PGltxtquery where pgtype = ltxtquery

instance OidOf db PGltree where
  oidOf = oidLtreeLookup "oid" "ltree"
instance OidOf db PGlquery where
  oidOf = oidLtreeLookup "oid" "lquery"
instance OidOf db PGltxtquery where
  oidOf = oidLtreeLookup "oid" "ltxtquery"
instance OidOfArray db PGltree where
  oidOfArray = oidLtreeLookup "typarray" "ltree"
instance OidOfArray db PGlquery where
  oidOfArray = oidLtreeLookup "typarray" "lquery"
instance OidOfArray db PGltxtquery where
  oidOfArray = oidLtreeLookup "typarray" "ltxtquery"

oidLtreeLookup
  :: String
  -> String
  -> ReaderT (SOP.K LibPQ.Connection db) IO LibPQ.Oid
oidLtreeLookup tyOrArr name = ReaderT $ \(SOP.K conn) -> do
  resultMaybe <- LibPQ.execParams conn q [] LibPQ.Binary
  case resultMaybe of
    Nothing -> throwIO $ ConnectionException oidErr
    Just result -> do
      numRows <- LibPQ.ntuples result
      when (numRows /= 1) $ throwIO $ RowsException oidErr 1 numRows
      valueMaybe <- LibPQ.getvalue result 0 0
      case valueMaybe of
        Nothing -> throwIO $ ConnectionException oidErr
        Just value -> case Decoding.valueParser Decoding.int value of
          Left err -> throwIO $ DecodingException oidErr err
          Right oid' -> return $ LibPQ.Oid oid'
  where
    oidErr = "oidOf " <> fromString (name <> tyOrArr)
    q = "SELECT " <> fromString tyOrArr
      <> " FROM pg_type WHERE typname = \'"
      <> fromString name <> "\';"

{- |
A label is a sequence of alphanumeric characters and underscores
(for example, in C locale the characters A-Za-z0-9_ are allowed).
Labels must be less than 256 bytes long.

@
Examples: 42, Personal_Services
@

A label path is a sequence of zero or more labels separated by dots,
for example L1.L2.L3, representing a path from the root of a
hierarchical tree to a particular node. The length of a label path
must be less than 65Kb, but keeping it under 2Kb is preferable.
In practice this is not a major limitation; for example,
the longest label path in the DMOZ catalogue
(http://www.dmoz.org) is about 240 bytes.

@
Example: Top.Countries.Europe.Russia
@

ltree stores a label path.
-}
newtype LTree = UnsafeLTree {getLTree :: Text}
  deriving stock (Eq,Ord,Show,Read,Generic)
-- | `PGltree`
instance IsPG LTree where type PG LTree = PGltree
instance FromPG LTree where
  fromPG = UnsafeLTree <$> devalue Decoding.text_strict
instance ToPG db LTree where
  toPG = pure . Encoding.text_strict . getLTree
instance Inline LTree where
  inline
    = UnsafeExpression
    . parenthesized
    . (<> " :: ltree")
    . escapeQuotedText
    . getLTree

{- |
lquery represents a regular-expression-like pattern for matching ltree values.
A simple word matches that label within a path.
A star symbol (*) matches zero or more labels. For example:

@
foo         Match the exact label path foo
*.foo.*     Match any label path containing the label foo
*.foo       Match any label path whose last label is foo
@

Star symbols can also be quantified to restrict how many labels they can match:

@
*{n}        Match exactly n labels
*{n,}       Match at least n labels
*{n,m}      Match at least n but not more than m labels
*{,m}       Match at most m labels — same as  *{0,m}
@

There are several modifiers that can be put at the end of a non-star label
in lquery to make it match more than just the exact match:

@
\@           Match case-insensitively, for example a@ matches A
*           Match any label with this prefix, for example foo* matches foobar
%           Match initial underscore-separated words
@

The behavior of % is a bit complicated.
It tries to match words rather than the entire label.
For example foo_bar% matches foo_bar_baz but not foo_barbaz.
If combined with *, prefix matching applies to each word separately,
for example foo_bar%* matches foo1_bar2_baz but not foo1_br2_baz.

Also, you can write several possibly-modified labels separated with
| (OR) to match any of those labels,
and you can put ! (NOT) at the start to match any label
that doesn't match any of the alternatives.

Here's an annotated example of lquery:

@
Top.*{0,2}.sport*@.!football|tennis.Russ*|Spain
1.  2.     3.      4.               5.
@

This query will match any label path that:

1. begins with the label Top
2. and next has zero to two labels before
3. a label beginning with the case-insensitive prefix sport
4. then a label not matching football nor tennis
5. and then ends with a label beginning with Russ or exactly matching Spain.
-}
newtype LQuery = UnsafeLQuery {getLQuery :: Text}
  deriving stock (Eq,Ord,Show,Read,Generic)
-- | `PGlquery`
instance IsPG LQuery where type PG LQuery = PGlquery
instance FromPG LQuery where
  fromPG = UnsafeLQuery <$> devalue Decoding.text_strict
instance ToPG db LQuery where
  toPG = pure . Encoding.text_strict . getLQuery
instance Inline LQuery where
  inline
    = UnsafeExpression
    . parenthesized
    . (<> " :: lquery")
    . escapeQuotedText
    . getLQuery

{- |
ltxtquery represents a full-text-search-like pattern for matching ltree values.
An ltxtquery value contains words,
possibly with the modifiers @, *, % at the end;
the modifiers have the same meanings as in lquery.
Words can be combined with & (AND), | (OR), ! (NOT), and parentheses.
The key difference from lquery is that ltxtquery matches words
without regard to their position in the label path.

Here's an example ltxtquery:

@
Europe & Russia*@ & !Transportation
@

This will match paths that contain the label Europe and any label
beginning with Russia (case-insensitive), but not paths containing
the label Transportation. The location of these words within the
path is not important. Also, when % is used, the word can be matched
to any underscore-separated word within a label, regardless of position.

Note: ltxtquery allows whitespace between symbols, but ltree and lquery do not.
-}
newtype LTxtQuery = UnsafeLTxtQuery {getLTxtQuery :: Text}
  deriving stock (Eq,Ord,Show,Read,Generic)
-- | `PGltxtquery`
instance IsPG LTxtQuery where type PG LTxtQuery = PGltxtquery
instance FromPG LTxtQuery where
  fromPG = UnsafeLTxtQuery <$> devalue Decoding.text_strict
instance ToPG db LTxtQuery where
  toPG = pure . Encoding.text_strict . getLTxtQuery
instance Inline LTxtQuery where
  inline
    = UnsafeExpression
    . parenthesized
    . (<> " :: ltxtquery")
    . escapeQuotedText
    . getLTxtQuery

instance IsString
  (Expression grp lat with db params from (null PGltree)) where
    fromString
      = UnsafeExpression
      . parenthesized
      . (<> " :: ltree")
      . escapeQuotedString
instance IsString
  (Expression grp lat with db params from (null PGlquery)) where
    fromString
      = UnsafeExpression
      . parenthesized
      . (<> " :: lquery")
      . escapeQuotedString
instance IsString
  (Expression grp lat with db params from (null PGltxtquery)) where
    fromString
      = UnsafeExpression
      . parenthesized
      . (<> " :: ltxtquery")
      . escapeQuotedString

-- | Returns subpath of ltree from position start to position end-1 (counting from 0).
subltree :: '[null PGltree, null 'PGint4, null 'PGint4] ---> null PGltree
subltree = unsafeFunctionN "subltree"

-- | Returns subpath of ltree starting at position offset, with length len.
-- If offset is negative, subpath starts that far from the end of the path.
-- If len is negative, leaves that many labels off the end of the path.
subpath :: '[null PGltree, null 'PGint4, null 'PGint4] ---> null PGltree
subpath = unsafeFunctionN "subpath"

-- | Returns subpath of ltree starting at position offset,
-- extending to end of path. If offset is negative,
-- subpath starts that far from the end of the path.
subpathEnd :: '[null PGltree, null 'PGint4] ---> null PGltree
subpathEnd = unsafeFunctionN "subpath"

-- | Returns number of labels in path.
nlevel :: null PGltree --> null 'PGint4
nlevel = unsafeFunction "nlevel"

-- | Returns position of first occurrence of b in a, or -1 if not found.
indexLTree :: '[null PGltree, null PGltree] ---> null 'PGint4
indexLTree = unsafeFunctionN "index"

-- | Returns position of first occurrence of b in a, or -1 if not found.
-- The search starts at position offset;
-- negative offset means start -offset labels from the end of the path.
indexOffset :: '[null PGltree, null PGltree, null 'PGint4] ---> null 'PGint4
indexOffset = unsafeFunctionN "index"

-- | Casts text to ltree.
text2ltree :: null 'PGtext --> null PGltree
text2ltree = unsafeFunction "text2ltree"

-- | Casts ltree to text.
ltree2text :: null PGltree --> null 'PGtext
ltree2text = unsafeFunction "ltree2text"

-- | Computes longest common ancestor of paths in array.
lca :: null ('PGvararray ('NotNull PGltree)) --> null PGltree
lca = unsafeFunction "lca"

{- |
`(@>)` Is left argument an ancestor of right (or equal)?

`(<@)` Is left argument a descendant of right (or equal)?
-}
instance PGSubset PGltree

-- | Does ltree match lquery?
(%~) :: Operator (null0 PGltree) (null1 PGlquery) ('Null 'PGbool)
(%~) = unsafeBinaryOp "~"
infix 4 %~

-- | Does ltree match lquery?
(~%) :: Operator (null1 PGlquery) (null0 PGltree) ('Null 'PGbool)
(~%) = unsafeBinaryOp "~"
infix 4 ~%

-- | Does ltree match any lquery in array?
(%?) :: Operator
  (null0 PGltree) (null1 ('PGvararray ('NotNull PGlquery))) ('Null 'PGbool)
(%?) = unsafeBinaryOp "?"
infix 4 %?

-- | Does ltree match any lquery in array?
(?%) :: Operator
  (null0 ('PGvararray ('NotNull PGlquery))) (null1 PGltree) ('Null 'PGbool)
(?%) = unsafeBinaryOp "?"
infix 4 ?%

-- | Does ltree match ltxtquery?
(%@) :: Operator (null0 PGltree) (null1 PGltxtquery) ('Null 'PGbool)
(%@) = unsafeBinaryOp "@"
infix 4 %@

-- | Does ltree match ltxtquery?
(@%) :: Operator (null0  PGltxtquery) (null1 PGltree) ('Null 'PGbool)
(@%) = unsafeBinaryOp "@"
infix 4 @%

-- | `(<>)` Concatenates ltree paths.
instance Semigroup
  (Expression grp lat with db params from (null PGltree)) where
    (<>) = unsafeBinaryOp "||"
instance Monoid
  (Expression grp lat with db params from (null PGltree)) where
    mempty = fromString ""
    mappend = (<>)

-- | Does array contain an ancestor of ltree?
(@>%) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltree) ('Null 'PGbool)
(@>%) = unsafeBinaryOp "@>"
infix 4 @>%

-- | Does array contain an ancestor of ltree?
(%<@) :: Operator
  (null0 PGltree) (null1 ('PGvararray ('NotNull PGltree))) ('Null 'PGbool)
(%<@) = unsafeBinaryOp "<@"
infix 4 %<@

-- | Does array contain a descendant of ltree?
(<@%) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltree) ('Null 'PGbool)
(<@%) = unsafeBinaryOp "<@"
infix 4 <@%

-- | Does array contain a descendant of ltree?
(%@>) :: Operator
  (null0 PGltree) (null1 ('PGvararray ('NotNull PGltree))) ('Null 'PGbool)
(%@>) = unsafeBinaryOp "@>"
infix 4 %@>

-- | Does array contain any path matching lquery?
(&~) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGlquery) ('Null 'PGbool)
(&~) = unsafeBinaryOp "~"
infix 4 &~

-- | Does array contain any path matching lquery?
(~&) :: Operator
  (null0 PGlquery) (null1 ('PGvararray ('NotNull PGltree))) ('Null 'PGbool)
(~&) = unsafeBinaryOp "~"
infix 4 ~&

-- | Does ltree array contain any path matching any lquery?
(&?) :: Operator
  (null0 ('PGvararray ('NotNull PGltree)))
  (null1 ('PGvararray ('NotNull PGlquery)))
  ('Null 'PGbool)
(&?) = unsafeBinaryOp "?"
infix 4 &?

-- | Does ltree array contain any path matching any lquery?
(?&) :: Operator
  (null0 ('PGvararray ('NotNull PGlquery)))
  (null1 ('PGvararray ('NotNull PGltree)))
  ('Null 'PGbool)
(?&) = unsafeBinaryOp "?"
infix 4 ?&

-- | Does array contain any path matching ltxtquery?
(&@) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltxtquery) ('Null 'PGbool)
(&@) = unsafeBinaryOp "@"
infix 4 &@

-- | Does array contain any path matching ltxtquery?
(@&) :: Operator
  (null0 PGltxtquery) (null1 ('PGvararray ('NotNull PGltree))) ('Null 'PGbool)
(@&) = unsafeBinaryOp "@"
infix 4 @&

-- | Returns first array entry that is an ancestor of ltree, or NULL if none.
(?@>) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltree) ('Null PGltree)
(?@>) = unsafeBinaryOp "?@>"
infix 4 ?@>

-- | Returns first array entry that is a descendant of ltree, or NULL if none.
(?<@) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltree) ('Null PGltree)
(?<@) = unsafeBinaryOp "?<@"
infix 4 ?<@

-- | Returns first array entry that matches lquery, or NULL if none.
(?~) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGlquery) ('Null PGltree)
(?~) = unsafeBinaryOp "?~"
infix 4 ?~

-- | Returns first array entry that matches ltxtquery, or NULL if none.
(?@) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltxtquery) ('Null PGltree)
(?@) = unsafeBinaryOp "?@"
infix 4 ?@

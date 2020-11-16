{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , OverloadedStrings
  , TypeOperators
  , TypeSynonymInstances
#-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Squeal.PostgreSQL.LTree where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Render
import Data.String

type PGltree = 'UnsafePGType "ltree"
type PGlquery = 'UnsafePGType "lquery"

instance PGSubset PGltree

instance IsString
  (Expression grp lat with db params from (null PGltree)) where
    fromString
      = UnsafeExpression
      . parenthesized
      . (<> " :: ltree")
      . escapeQuotedString

instance Semigroup
  (Expression grp lat with db params from (null PGltree)) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression grp lat with db params from (null PGltree)) where
    mempty = fromString ""
    mappend = (<>)

-- subltree ( ltree, start integer, end integer ) → ltree

-- Returns subpath of ltree from position start to position end-1 (counting from 0).

-- subltree('Top.Child1.Child2', 1, 2) → Child1

subltree :: '[null PGltree, null 'PGint4, null 'PGint4] ---> null PGltree
subltree = unsafeFunctionN "subltree"

-- subpath ( ltree, offset integer, len integer ) → ltree

-- Returns subpath of ltree starting at position offset, with length len. If offset is negative, subpath starts that far from the end of the path. If len is negative, leaves that many labels off the end of the path.

-- subpath('Top.Child1.Child2', 0, 2) → Top.Child1

subpath :: '[null PGltree, null 'PGint4, null 'PGint4] ---> null PGltree
subpath = unsafeFunctionN "subpath"

-- subpath ( ltree, offset integer ) → ltree

-- Returns subpath of ltree starting at position offset, extending to end of path. If offset is negative, subpath starts that far from the end of the path.

-- subpath('Top.Child1.Child2', 1) → Child1.Child2

subpathEnd :: '[null PGltree, null 'PGint4] ---> null PGltree
subpathEnd = unsafeFunctionN "subpath"

-- nlevel ( ltree ) → integer

-- Returns number of labels in path.

-- nlevel('Top.Child1.Child2') → 3

nlevel :: null PGltree --> null 'PGint4
nlevel = unsafeFunction "nlevel"

-- index ( a ltree, b ltree ) → integer

-- Returns position of first occurrence of b in a, or -1 if not found.

-- index('0.1.2.3.5.4.5.6.8.5.6.8', '5.6') → 6

index :: '[null PGltree, null PGltree] ---> null 'PGint4
index = unsafeFunctionN "index"

-- index ( a ltree, b ltree, offset integer ) → integer

-- Returns position of first occurrence of b in a, or -1 if not found. The search starts at position offset; negative offset means start -offset labels from the end of the path.

-- index('0.1.2.3.5.4.5.6.8.5.6.8', '5.6', -4) → 9

indexOffset :: '[null PGltree, null PGltree, null 'PGint4] ---> null 'PGint4
indexOffset = unsafeFunctionN "index"

-- text2ltree ( text ) → ltree

-- Casts text to ltree.

text2ltree :: null 'PGtext --> null PGltree
text2ltree = unsafeFunction "text2ltree"

-- ltree2text ( ltree ) → text

-- Casts ltree to text.

ltree2text :: null PGltree --> null 'PGtext
ltree2text = unsafeFunction "ltree2text"

-- lca ( ltree[] ) → ltree

-- Computes longest common ancestor of paths in array.

-- lca(array['1.2.3'::ltree,'1.2.3.4']) → 1.2

lca :: null ('PGvararray ('NotNull PGltree)) --> null PGltree
lca = unsafeFunction "lca"

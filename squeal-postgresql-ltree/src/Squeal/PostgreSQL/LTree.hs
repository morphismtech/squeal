{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
#-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Squeal.PostgreSQL.LTree where

import Data.String
import Data.Text
import GHC.Generics
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Render

import qualified PostgreSQL.Binary.Decoding as Decoding
import qualified PostgreSQL.Binary.Encoding as Encoding

type PGltree = 'UnsafePGType "ltree"
type PGlquery = 'UnsafePGType "lquery"
type PGltxtquery = 'UnsafePGType "ltxtquery"

createLTree :: Definition db db
createLTree = UnsafeDefinition "CREATE EXTENSION ltree;"

ltree :: TypeExpression db (null PGltree)
ltree = UnsafeTypeExpression "ltree"

lquery :: TypeExpression db (null PGlquery)
lquery = UnsafeTypeExpression "lquery"

ltxtquery :: TypeExpression db (null PGltxtquery)
ltxtquery = UnsafeTypeExpression "ltxtquery"

instance PGTyped db PGltree where pgtype = ltree
instance PGTyped db PGlquery where pgtype = lquery
instance PGTyped db PGltxtquery where pgtype = ltxtquery

newtype LTree = UnsafeLTree {getLTree :: Text}
  deriving stock (Eq,Ord,Show,Read,Generic)
  deriving newtype IsString
instance IsPG LTree where type PG LTree = PGltree
instance FromPG LTree where
  fromPG = UnsafeLTree <$> devalue Decoding.text_strict
instance ToPG db LTree where
  toPG = pure . Encoding.text_strict . getLTree
instance Inline LTree where
  inline = fromString . unpack . getLTree

newtype LQuery = UnsafeLQuery {getLQuery :: Text}
  deriving stock (Eq,Ord,Show,Read,Generic)
  deriving newtype IsString
instance IsPG LQuery where type PG LQuery = PGltree
instance FromPG LQuery where
  fromPG = UnsafeLQuery <$> devalue Decoding.text_strict
instance ToPG db LQuery where toPG = pure . Encoding.text_strict . getLQuery
instance Inline LQuery where inline = fromString . unpack . getLQuery

newtype LTxtQuery = UnsafeLTxtQuery {getLTxtQuery :: Text}
  deriving stock (Eq,Ord,Show,Read,Generic)
  deriving newtype IsString
instance IsPG LTxtQuery where type PG LTxtQuery = PGltree
instance FromPG LTxtQuery where
  fromPG = UnsafeLTxtQuery <$> devalue Decoding.text_strict
instance ToPG db LTxtQuery where
  toPG = pure . Encoding.text_strict . getLTxtQuery
instance Inline LTxtQuery where
  inline = fromString . unpack . getLTxtQuery

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

-- ltree @> ltree → boolean

-- Is left argument an ancestor of right (or equal)?

-- ltree <@ ltree → boolean

-- Is left argument a descendant of right (or equal)?

instance PGSubset PGltree

-- ltree ~ lquery → boolean

-- lquery ~ ltree → boolean

-- Does ltree match lquery?

(.~) :: Operator (null0 PGltree) (null1 PGlquery) ('Null 'PGbool)
(.~) = unsafeBinaryOp "~"

(~.) :: Operator (null1 PGlquery) (null0 PGltree) ('Null 'PGbool)
(~.) = unsafeBinaryOp "~"

-- ltree ? lquery[] → boolean

-- lquery[] ? ltree → boolean

-- Does ltree match any lquery in array?

(.?) :: Operator
  (null0 PGltree) (null1 ('PGvararray (null2 PGlquery))) ('Null 'PGbool)
(.?) = unsafeBinaryOp "?"

(?.) :: Operator
  (null0 ('PGvararray (null1 PGlquery))) (null2 PGltree) ('Null 'PGbool)
(?.) = unsafeBinaryOp "?"

-- ltree @ ltxtquery → boolean

-- ltxtquery @ ltree → boolean

-- Does ltree match ltxtquery?

(.@) :: Operator (null PGltree) (null PGltxtquery) ('Null 'PGbool)
(.@) = unsafeBinaryOp "@"

(@.) :: Operator (null  PGltxtquery) (null PGltree) ('Null 'PGbool)
(@.) = unsafeBinaryOp "@"

-- ltree || ltree → ltree

-- Concatenates ltree paths.

-- ltree || text → ltree

-- text || ltree → ltree

-- Converts text to ltree and concatenates.

-- ltree[] @> ltree → boolean

-- ltree <@ ltree[] → boolean

-- Does array contain an ancestor of ltree?

(@>.) :: Operator
  (null0 ('PGvararray (null1 PGltree))) (null2 PGltree) ('Null 'PGbool)
(@>.) = unsafeBinaryOp "@>"

(.<@) :: Operator
  (null0 PGltree) (null1 ('PGvararray (null2 PGltree))) ('Null 'PGbool)
(.<@) = unsafeBinaryOp "<@"

-- ltree[] <@ ltree → boolean

-- ltree @> ltree[] → boolean

-- Does array contain a descendant of ltree?

(<@.) :: Operator
  (null0 ('PGvararray (null1 PGltree))) (null2 PGltree) ('Null 'PGbool)
(<@.) = unsafeBinaryOp "<@"

(.@>) :: Operator
  (null0 PGltree) (null1 ('PGvararray (null2 PGltree))) ('Null 'PGbool)
(.@>) = unsafeBinaryOp "@>"

-- ltree[] ~ lquery → boolean

-- lquery ~ ltree[] → boolean

-- Does array contain any path matching lquery?

(..~) :: Operator
  (null0 ('PGvararray (null1 PGltree))) (null2 PGlquery) ('Null 'PGbool)
(..~) = unsafeBinaryOp "~"

(~..) :: Operator
  (null0 PGlquery) (null1 ('PGvararray (null2 PGltree))) ('Null 'PGbool)
(~..) = unsafeBinaryOp "~"

-- ltree[] ? lquery[] → boolean

-- lquery[] ? ltree[] → boolean

-- Does ltree array contain any path matching any lquery?

(..?) :: Operator
  (null0 ('PGvararray (null1 PGltree)))
  (null2 ('PGvararray (null3 PGlquery)))
  ('Null 'PGbool)
(..?) = unsafeBinaryOp "?"

(?..) :: Operator
  (null0 ('PGvararray (null1 PGlquery)))
  (null2 ('PGvararray (null3 PGltree)))
  ('Null 'PGbool)
(?..) = unsafeBinaryOp "?"

-- ltree[] @ ltxtquery → boolean

-- ltxtquery @ ltree[] → boolean

-- Does array contain any path matching ltxtquery?

(..@) :: Operator
  (null0 ('PGvararray (null1 PGltree))) (null2 PGltxtquery) ('Null 'PGbool)
(..@) = unsafeBinaryOp "@"

(@..) :: Operator
  (null0 PGltxtquery) (null1 ('PGvararray (null2 PGltree))) ('Null 'PGbool)
(@..) = unsafeBinaryOp "@"

-- ltree[] ?@> ltree → ltree

-- Returns first array entry that is an ancestor of ltree, or NULL if none.

(?@>) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltree) ('Null PGltree)
(?@>) = unsafeBinaryOp "?@>"

-- ltree[] ?<@ ltree → ltree

-- Returns first array entry that is a descendant of ltree, or NULL if none.

(?<@) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltree) ('Null PGltree)
(?<@) = unsafeBinaryOp "?<@"

-- ltree[] ?~ lquery → ltree

-- Returns first array entry that matches lquery, or NULL if none.

(?~) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGlquery) ('Null PGltree)
(?~) = unsafeBinaryOp "?~"

-- ltree[] ?@ ltxtquery → ltree

-- Returns first array entry that matches ltxtquery, or NULL if none.

(?@) :: Operator
  (null0 ('PGvararray ('NotNull PGltree))) (null1 PGltxtquery) ('Null PGltree)
(?@) = unsafeBinaryOp "?@"

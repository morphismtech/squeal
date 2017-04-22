{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MagicHash
  , MultiParamTypeClasses
  , OverloadedLabels
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
#-}

module Squeel.PostgreSQL.Query where

import Data.Boolean
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Vinyl
import GHC.OverloadedLabels
import GHC.TypeLits

import qualified Data.ByteString.Char8 as Char8

import Squeel.PostgreSQL.Type

newtype Expression ps xs y =
  UnsafeExpression { renderExpression :: ByteString }

param1 :: Expression (p1:ps) cols p1
param1 = UnsafeExpression "$1"
param2 :: Expression (p1:p2:ps) cols p2
param2 = UnsafeExpression "$2"
param3 :: Expression (p1:p2:p3:ps) cols p3
param3 = UnsafeExpression "$3"
param4 :: Expression (p1:p2:p3:p4:ps) cols p4
param4 = UnsafeExpression "$4"
param5 :: Expression (p1:p2:p3:p4:p5:ps) cols p5
param5 = UnsafeExpression "$5"

class HasField label xs x where
  fieldName :: Proxy label -> Proxy xs -> Proxy x -> ByteString
instance {-# OVERLAPPING #-} KnownSymbol label
  => HasField label ('(label, x) ': xs) x where
    fieldName _ _ _ = Char8.pack (symbolVal (Proxy @label))
instance {-# OVERLAPPABLE #-} HasField label xs y
  => HasField label (x ': xs) y where
  fieldName _ _ _ = fieldName (Proxy @label) (Proxy @xs) (Proxy @y)

instance HasField label xs x => IsLabel label (Expression ps xs x) where
  fromLabel _ = UnsafeExpression $
    fieldName (Proxy @label) (Proxy @xs) (Proxy @x)

data Alias label = Alias
data Aliased obj labeled where
  As :: obj x -> Alias label -> Aliased obj '(label, x)
instance IsLabel label (Alias label) where fromLabel _ = Alias
instance IsLabel label (obj x)
  => IsLabel label (Aliased obj '(label, x)) where
    fromLabel p = fromLabel p `As` fromLabel p

alias :: Aliased obj '(label, x) -> Alias label
alias _ = Alias

unalias :: Aliased obj '(label, x) -> obj x
unalias (obj `As` _) = obj

renderAliased
  :: KnownSymbol label
  => (obj x -> ByteString)
  -> Aliased obj '(label, x)
  -> ByteString
renderAliased render (obj `As` label) =
  render obj <> " AS " <> Char8.pack (symbolVal label)

class AllAliased xs where
  renderAllAliased
    :: (forall x. obj x -> ByteString)
    -> Rec (Aliased obj) xs
    -> ByteString
instance KnownSymbol label => AllAliased '[ '(label, x)] where
  renderAllAliased render
    (aliased :& RNil :: Rec (Aliased obj) '[ '(label, x)]) =
      renderAliased render aliased
instance (KnownSymbol label, AllAliased (x' ': xs))
  => AllAliased ('(label, x) ': (x' ': xs)) where
    renderAllAliased render
      (x :& xs :: Rec (Aliased obj) ('(label, x) ': x' : xs)) =
        renderAliased render x <> ", " <> renderAllAliased render xs

instance IsString (Expression ps xs 'PGText) where
  fromString str = UnsafeExpression $ "\'" <> fromString str <> "\'"

instance Num (Expression ps xs 'PGInt4) where
  fromInteger n = UnsafeExpression . fromString $ show n
  x + y = UnsafeExpression $ renderExpression x <> "+" <> renderExpression y
  x * y = UnsafeExpression $ renderExpression x <> "*" <> renderExpression y
  UnsafeExpression x - UnsafeExpression y = UnsafeExpression $ x <> "+" <> y
  abs (UnsafeExpression x) = UnsafeExpression $ "@" <> x
  signum (UnsafeExpression x) = UnsafeExpression $ "sign(" <> x <> ")"

instance Boolean (Expression ps xs 'PGBool) where
  true = UnsafeExpression "TRUE"
  false = UnsafeExpression "FALSE"
  notB x = UnsafeExpression $ "NOT " <> renderExpression x
  x &&* y = UnsafeExpression $
    renderExpression x <> " AND " <> renderExpression y
  x ||* y = UnsafeExpression $
    renderExpression x <> " OR " <> renderExpression y

type instance BooleanOf (Expression ps xs y) = Expression ps xs 'PGBool

instance IfB (Expression ps xs y) where
  ifB predicate then_ else_ = UnsafeExpression $ mconcat
    [ "CASE"
    , "\n  WHEN ",renderExpression predicate
    , "\n  THEN ",renderExpression then_
    , "\n  ELSE ",renderExpression else_
    , "\nEND"
    ]

instance EqB (Expression ps xs y) where
  x ==* y = UnsafeExpression $ renderExpression x <> "=" <> renderExpression y
  x /=* y = UnsafeExpression $ renderExpression x <> "<>" <> renderExpression y

instance OrdB (Expression ps xs y) where
  x >* y = UnsafeExpression $ renderExpression x <> ">" <> renderExpression y
  x >=* y = UnsafeExpression $ renderExpression x <> ">=" <> renderExpression y
  x <* y = UnsafeExpression $ renderExpression x <> "<" <> renderExpression y
  x <=* y = UnsafeExpression $ renderExpression x <> "<=" <> renderExpression y

newtype Projection ps xs ys = UnsafeProjection
  { renderProjection :: ByteString }

star :: Projection ps xs xs
star = UnsafeProjection "*"

project
  :: AllAliased ys
  => Rec (Aliased (Expression ps xs)) ys
  -> Projection ps xs ys
project = UnsafeProjection . renderAllAliased renderExpression

newtype Relation ps xss ys = UnsafeRelation
  { renderRelation :: ByteString }

instance HasField label xss xs
  => IsLabel label (Relation ps xss xs) where
    fromLabel _ = UnsafeRelation $
      fieldName (Proxy @label) (Proxy @xss) (Proxy @xs)

data Selection ps xss xs ys = Selection
  { projection :: Projection ps xs ys
  , relation :: Relation ps xss xs
  , restriction :: Maybe (Expression ps xs 'PGBool)
  }

renderSelection :: Selection ps xss xs ys -> ByteString
renderSelection (Selection xs ys wh) =
  renderProjection xs <> " FROM " <> renderRelation ys
    <> maybe "" ((" WHERE " <>) . renderExpression) wh

from :: Projection ps xs ys -> Relation ps xss xs -> Selection ps xss xs ys
ys `from` xs = Selection ys xs Nothing

where_
  :: Selection ps xss xs ys
  -> Expression ps xs 'PGBool
  -> Selection ps xss xs ys
ys `where_` condition = ys
  { restriction = case restriction ys of
      Nothing -> Just condition
      Just conditions -> Just (condition &&* conditions)
  }

newtype Query ps xss yss zs = UnsafeQuery { renderQuery :: ByteString }
newtype PreparedQuery ps xss yss zs =
  UnsafePreparedQuery { renderPreparedQuery :: ByteString }

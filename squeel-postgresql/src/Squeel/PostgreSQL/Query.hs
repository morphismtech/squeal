{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
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

class HasField label xs x where
  fieldName :: Proxy label -> Proxy xs -> Proxy x -> ByteString
instance {-# OVERLAPPING #-} KnownSymbol label
  => HasField label ('(label, x) ': xs) x where
    fieldName _ _ _ = Char8.pack (symbolVal (Proxy @label))
instance HasField label xs y
  => HasField label (x ': xs) y where
  fieldName _ _ _ = fieldName (Proxy @label) (Proxy @xs) (Proxy @y)

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

newtype Expression ps (xs :: [(Symbol,PGType)]) y =
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

instance HasField label xs x => IsLabel label (Expression ps xs x) where
  fromLabel _ = UnsafeExpression $
    fieldName (Proxy @label) (Proxy @xs) (Proxy @x)

instance IsString (Expression ps xs 'PGText) where
  fromString str = UnsafeExpression $ "\'" <> fromString str <> "\'"

instance Num (Expression ps xs 'PGInt2) where
  fromInteger n = UnsafeExpression . fromString $ show n
  x + y = UnsafeExpression $ renderExpression x <> "+" <> renderExpression y
  x * y = UnsafeExpression $ renderExpression x <> "*" <> renderExpression y
  UnsafeExpression x - UnsafeExpression y = UnsafeExpression $ x <> "+" <> y
  abs (UnsafeExpression x) = UnsafeExpression $ "@" <> x
  signum (UnsafeExpression x) = UnsafeExpression $ "sign(" <> x <> ")"

instance Num (Expression ps xs 'PGInt4) where
  fromInteger n = UnsafeExpression . fromString $ show n
  x + y = UnsafeExpression $ renderExpression x <> "+" <> renderExpression y
  x * y = UnsafeExpression $ renderExpression x <> "*" <> renderExpression y
  UnsafeExpression x - UnsafeExpression y = UnsafeExpression $ x <> "+" <> y
  abs (UnsafeExpression x) = UnsafeExpression $ "@" <> x
  signum (UnsafeExpression x) = UnsafeExpression $ "sign(" <> x <> ")"

instance Num (Expression ps xs 'PGInt8) where
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
    [ "CASE WHEN ",renderExpression predicate
    , " THEN ",renderExpression then_
    , " ELSE ",renderExpression else_
    , " END"
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

data Tabulation ps xss xs = UnsafeTabulation
  { renderTabulation :: ByteString }

data Relation ps xss xs = Relation
  { tabulation :: Tabulation ps xss xs
  , restriction :: Maybe (Expression ps xs 'PGBool)
  , limitation :: Maybe (Expression ps '[] 'PGInt8)
  , offsetting :: Maybe (Expression ps '[] 'PGInt8)
  }

renderRelation :: Relation ps xss xs -> ByteString
renderRelation (Relation tab wh lim off) =
  renderTabulation tab
    <> maybe "" ((" WHERE " <>) . renderExpression) wh
    <> maybe "" ((" LIMIT " <>) . renderExpression) lim
    <> maybe "" ((" OFFSET " <>) . renderExpression) off

where_
  :: Relation ps xss xs
  -> Expression ps xs 'PGBool
  -> Relation ps xss xs
ys `where_` condition = ys
  { restriction = case restriction ys of
      Nothing -> Just condition
      Just conditions -> Just (conditions &&* condition)
  }

limit
  :: Relation ps xss xs
  -> Expression ps '[] 'PGInt8
  -> Relation ps xss xs
ys `limit` n = ys
  { limitation = case limitation ys of
      Nothing -> Just n
      Just n' -> Just (n' `minB` n)
  }

offset
  :: Relation ps xss xs
  -> Expression ps '[] 'PGInt8
  -> Relation ps xss xs
ys `offset` n = ys
  { offsetting = case offsetting ys of
      Nothing -> Just n
      Just n' -> Just (n' + n)
  }

instance HasField label xss xs
  => IsLabel label (Relation ps xss xs) where
    fromLabel _ = Relation
      { tabulation = UnsafeTabulation $
          fieldName (Proxy @label) (Proxy @xss) (Proxy @xs)
      , restriction = Nothing
      , limitation = Nothing
      , offsetting = Nothing
      }

newtype Selection ps xss ys = UnsafeSelection
  { renderSelection :: ByteString }

from :: Projection ps xs ys -> Relation ps xss xs -> Selection ps xss ys
ys `from` xs = UnsafeSelection $
  renderProjection ys <> " FROM " <> renderRelation xs

subselect :: Selection ps xss ys -> Relation ps xss ys
subselect selection = Relation
  { tabulation = UnsafeTabulation $ "SELECT " <> renderSelection selection
  , restriction = Nothing
  , limitation = Nothing
  , offsetting = Nothing
  }

newtype Query ps xss yss zs = UnsafeQuery { renderQuery :: ByteString }
newtype PreparedQuery ps xss yss zs =
  UnsafePreparedQuery { renderPreparedQuery :: ByteString }

select :: Selection ps xss ys -> Query ps xss xss ys
select selection = UnsafeQuery $ "SELECT " <> renderSelection selection <> ";"

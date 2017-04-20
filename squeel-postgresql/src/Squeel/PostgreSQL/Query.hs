{-# LANGUAGE
    DataKinds
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
-- import Data.Vinyl.Functor
import GHC.TypeLits

-- import qualified Data.ByteString as ByteString
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

class HasField ident xs x where
  fieldName :: Proxy ident -> Proxy xs -> Proxy x -> ByteString
instance KnownSymbol name => HasField '[name] ((name ':= x) ': xs) x where
  fieldName _ _ _ = Char8.pack (symbolVal (Proxy @name))
instance HasField ident xs x => HasField ident (y ': xs) x where
  fieldName _ _ _ = fieldName (Proxy @ident) (Proxy @xs) (Proxy @x)
instance (KnownSymbol name, HasField names xs x)
  => HasField (name ': names) ((name ':= xs) ': xss) x where
    fieldName _ _ _ = Char8.pack (symbolVal (Proxy @name))
      <> "." <> fieldName (Proxy @names) (Proxy @xs) (Proxy @x)

column_
  :: forall ident xs x ps
   . HasField ident xs x
  => Proxy ident
  -> Expression ps xs x
column_ (_ :: Proxy ident) = UnsafeExpression $
  fieldName (Proxy @ident) (Proxy @xs) (Proxy @x)

data Identified ident x = (:=) ident x
data Aliased obj named where AS :: obj x -> Aliased obj (name ':= x)

-- class KnownAlias named obj where
--   type Alias named
--   renderAliased :: (obj x -> ByteString) -> Aliased obj named

unalias :: Aliased obj (name ':= x) -> obj x
unalias (AS obj) = obj

column
  :: forall ident xs x ps
   . HasField ident xs x
  => Aliased (Expression ps xs) (ident ':= x)
column = AS $ column_ (Proxy @ident)

renderAliased
  :: KnownSymbol name
  => (obj x -> ByteString)
  -> Aliased obj (name ':= x)
  -> ByteString
renderAliased render (AS obj :: Aliased obj (name ':= x)) =
  render obj <> " AS " <> Char8.pack (symbolVal (Proxy @name))

class AllAliased xs where
  renderAllAliased
    :: (forall x. obj x -> ByteString)
    -> Rec (Aliased obj) xs
    -> ByteString
instance KnownSymbol name => AllAliased '[name ':= x] where
  renderAllAliased render
    (aliased :& RNil :: Rec (Aliased obj) '[name ':= x]) =
      renderAliased render aliased
instance (KnownSymbol name, AllAliased xs)
  => AllAliased ((name ':= x) ': xs) where
    renderAllAliased render
      (x :& xs :: Rec (Aliased obj) ((name ':= x) ': xs)) =
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

table_
  :: forall ident xss xs ps
   . HasField ident xss xs
  => Proxy ident
  -> Relation ps xss xs
table_ (_ :: Proxy ident) = UnsafeRelation $
  fieldName (Proxy @ident) (Proxy @xss) (Proxy @xs)

table
  :: forall ident xss xs ps
   . HasField ident xss xs
  => Aliased (Relation ps xss) (ident ':= xs)
table = AS $ table_ (Proxy @ident)

newtype Query ps xss yss zs = UnsafeQuery { renderQuery :: ByteString }
newtype PreparedQuery ps xss yss zs =
  UnsafePreparedQuery { renderPreparedQuery :: ByteString }

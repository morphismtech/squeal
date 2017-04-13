{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
#-}

module Squeel.PostgreSQL.Query2 where

import Data.Boolean
import Data.ByteString
import Data.Monoid
import Data.Proxy
import Data.String
import GHC.TypeLits

import qualified Data.ByteString.Char8 as Char8

import Squeel.PostgreSQL.Type

newtype Expression ps xs x =
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

field
  :: forall ident xs x ps
   . HasField ident xs x
  => Proxy ident
  -> Expression ps xs x
field (_ :: Proxy ident) = UnsafeExpression $
  fieldName (Proxy @ident) (Proxy @xs) (Proxy @x)

data Aliased obj named where
  AS :: obj x -> Aliased obj (name ':= x)

unalias :: Aliased obj (name ':= x) -> obj x
unalias (AS obj) = obj

fieldAs
  :: forall ident xs x ps
   . HasField ident xs x
  => Aliased (Expression ps xs) (ident ':= x)
fieldAs = AS $ field (Proxy @ident)

instance IsString (Expression ps xs 'PGText) where
  fromString str = UnsafeExpression $ "\'" <> fromString str <> "\'"

instance Num (Expression ps xs 'PGInt4) where
  fromInteger n = UnsafeExpression . fromString $ show n
  UnsafeExpression x + UnsafeExpression y = UnsafeExpression $ x <> "+" <> y
  UnsafeExpression x * UnsafeExpression y = UnsafeExpression $ x <> "*" <> y
  UnsafeExpression x - UnsafeExpression y = UnsafeExpression $ x <> "+" <> y
  abs (UnsafeExpression x) = UnsafeExpression $ "@" <> x
  signum (UnsafeExpression x) = UnsafeExpression $ "sign(" <> x <> ")"

instance Boolean (Expression ps xs 'PGBool) where
  true = UnsafeExpression "TRUE"
  false = UnsafeExpression "FALSE"
  notB x = UnsafeExpression $ " NOT " <> renderExpression x
  x &&* y = UnsafeExpression $
    renderExpression x <> " AND " <> renderExpression y
  x ||* y = UnsafeExpression $
    renderExpression x <> " OR " <> renderExpression y

type instance BooleanOf (Expression ps xs x) = Expression ps xs 'PGBool

instance IfB (Expression ps xs x) where
  ifB predicate then_ else_ = UnsafeExpression $ mconcat
    [ "CASE\n"
    , "\n  WHEN ",renderExpression predicate
    , "\n  THEN ",renderExpression then_
    , "\n  ELSE ",renderExpression else_
    , "\nEND"
    ]

instance EqB (Expression ps xs x) where
  x ==* y = UnsafeExpression $ renderExpression x <> "=" <> renderExpression y
  x /=* y = UnsafeExpression $ renderExpression x <> "<>" <> renderExpression y

instance OrdB (Expression ps xs x) where
  x >* y = UnsafeExpression $ renderExpression x <> ">" <> renderExpression y
  x >=* y = UnsafeExpression $ renderExpression x <> ">=" <> renderExpression y
  x <* y = UnsafeExpression $ renderExpression x <> "<" <> renderExpression y
  x <=* y = UnsafeExpression $ renderExpression x <> "<=" <> renderExpression y

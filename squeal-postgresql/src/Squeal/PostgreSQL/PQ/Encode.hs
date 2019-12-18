{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.PQ.Encode
  ( EncodeParams (..)
  , genericParams
  , nilParams
  , (.*)
  , (*.)
  , ToParam (..)
  , ToNullParam (..)
  , ToField (..)
  , ToFixArray (..)
  ) where

import Data.Functor.Contravariant
import Data.Kind
import PostgreSQL.Binary.Encoding

import qualified Generics.SOP as SOP
import qualified Generics.SOP.Record as SOP

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Schema

class ToParam pg x where toParam :: x -> Encoding

class ToNullParam ty x where toNullParam :: x -> Maybe Encoding
instance ToParam pg x => ToNullParam ('NotNull pg) x where
  toNullParam = return . toParam @pg
instance ToParam pg x => ToNullParam ('Null pg) (Maybe x) where
  toNullParam = fmap (toParam @pg)

class ToField field x where toField :: SOP.P x -> Maybe Encoding
instance ToNullParam ty x => ToField (fld ::: ty) (fld ::: x) where
  toField (SOP.P x) = toNullParam @ty x

class ToFixArray dims ty x where toFixArray :: x -> Array
instance ToNullParam ty x => ToFixArray '[] ty x where
  toFixArray = maybe nullArray encodingArray . toNullParam @ty
instance
  ( SOP.IsProductType tuple xs
  , Length xs ~ dim
  , SOP.All ((~) x) xs
  , ToFixArray dims ty x )
  => ToFixArray (dim ': dims) ty tuple where
    toFixArray
      = dimensionArray foldlN (toFixArray @dims @ty @x)
      . SOP.unZ . SOP.unSOP . SOP.from
foldlN
  :: SOP.All ((~) x) xs
  => (z -> x -> z) -> z -> NP SOP.I xs -> z
foldlN f z = \case
  Nil -> z
  SOP.I x :* xs -> let z' = f z x in seq z' $ foldlN f z' xs

newtype EncodeParams (tys :: [NullType]) (x :: Type) = EncodeParams
  { runEncodeParams :: x -> NP (SOP.K (Maybe Encoding)) tys }
instance Contravariant (EncodeParams tys) where
  contramap f (EncodeParams g) = EncodeParams (g . f)
genericParams ::
  ( SOP.IsProductType x xs
  , SOP.AllZip ToNullParam tys xs
  ) => EncodeParams tys x
genericParams = EncodeParams
  $ trans_NP_flip (SOP.Proxy @ToNullParam) encode_
  . SOP.unZ . SOP.unSOP . SOP.from
encode_
  :: forall ty x. ToNullParam ty x
  => SOP.I x -> SOP.K (Maybe Encoding) ty
encode_ = SOP.K . toNullParam @ty . SOP.unI
trans_NP_flip
  :: SOP.AllZip c ys xs
  => proxy c
  -> (forall y x . c y x => f x -> g y)
  -> NP f xs -> NP g ys
trans_NP_flip _ _t Nil = Nil
trans_NP_flip p  t (x :* xs) = t x :* trans_NP_flip p t xs

nilParams :: EncodeParams '[] x
nilParams = EncodeParams $ return Nil

(.*)
  :: forall x0 ty x tys. ToNullParam ty x0
  => (x -> x0)
  -> EncodeParams tys x
  -> EncodeParams (ty ': tys) x
f .* EncodeParams params = EncodeParams $ \x ->
  SOP.K (toNullParam @ty (f x)) :* params x
infixr 5 .*

(*.)
  :: forall x x0 ty0 x1 ty1
   . (ToNullParam ty0 x0, ToNullParam ty1 x1)
  => (x -> x0)
  -> (x -> x1)
  -> EncodeParams '[ty0, ty1] x
f *. g = f .* g .* nilParams
infixl 9 *.

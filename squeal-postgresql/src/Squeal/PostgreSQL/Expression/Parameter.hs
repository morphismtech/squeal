{-|
Module: Squeal.PostgreSQL.Expression.Parameter
Description: Parameters
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Parameters, out-of-line data values
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Expression.Parameter
  ( HasParameter (parameter)
  , param
  , params
  , insertParamsInto
  , insertParamsInto_
  ) where

import Data.ByteString (ByteString)
import Generics.SOP (NP(..), hmap, SListI)
import GHC.TypeLits

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Expression.Type
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{- | A `HasParameter` constraint is used to indicate a value that is
supplied externally to a SQL statement.
`Squeal.PostgreSQL.PQ.manipulateParams`,
`Squeal.PostgreSQL.PQ.queryParams` and
`Squeal.PostgreSQL.PQ.traversePrepared` support specifying data values
separately from the SQL command string, in which case `param`s are used to
refer to the out-of-line data values.
-}
class KnownNat n => HasParameter
  (n :: Nat)
  (params :: [NullityType])
  (ty :: NullityType)
  | n params -> ty where
    -- | `parameter` takes a `Nat` using type application and a `TypeExpression`.
    --
    -- >>> let expr = parameter @1 int4 :: Expression outer '[] grp schemas '[ 'Null 'PGint4] from ('Null 'PGint4)
    -- >>> printSQL expr
    -- ($1 :: int4)
    parameter
      :: TypeExpression schemas ty
      -> Expression outer commons grp schemas params from ty
    parameter ty = UnsafeExpression $ parenthesized $
      "$" <> renderNat @n <+> "::"
        <+> renderSQL ty
instance {-# OVERLAPPING #-} HasParameter 1 (ty1:tys) ty1
instance {-# OVERLAPPABLE #-} (KnownNat n, HasParameter (n-1) params ty)
  => HasParameter n (ty' : params) ty

-- | `param` takes a `Nat` using type application and for basic types,
-- infers a `TypeExpression`.
--
-- >>> let expr = param @1 :: Expression outer commons grp schemas '[ 'Null 'PGint4] from ('Null 'PGint4)
-- >>> printSQL expr
-- ($1 :: int4)
param
  :: forall n outer commons schemas params from grp ty
   . (PGTyped schemas ty, HasParameter n params ty)
  => Expression outer commons grp schemas params from ty -- ^ param
param = parameter @n (pgtype @schemas)

class Insertable
  (schemas :: SchemasType)
  (params :: [NullityType])
  (row :: RowType)
  (columns :: ColumnsType)
  | columns -> params
  , columns -> row where
  params ::
    NP (Aliased (Optional (
      Expression '[] commons 'Ungrouped schemas params '[]
        ) ) ) columns
instance Insertable schemas '[] '[] '[] where params = Nil
instance
  ( KnownSymbol col
  , PGTyped schemas ty
  , SListI columns
  , Insertable schemas params row columns ) =>
    Insertable schemas (ty ': params) (col ::: ty ': row)
      (col ::: 'NoDef :=> ty ': columns) where
        params =
          Set (param @1) `as` (Alias @col)
          :* hmap (overValue paramsPlus1) (params @schemas)
instance
  ( KnownSymbol col
  , PGTyped schemas ty
  , SListI columns
  , Insertable schemas params row columns ) =>
    Insertable schemas params row (col ::: 'Def :=> ty ': columns) where
      params = Default `as` (Alias @col) :* (params @schemas)
overValue
  :: ( forall o c g s f x
      . Expression o c g s p0 f x -> Expression o c g s p1 f x )
  -> Aliased (Optional (
        Expression '[] commons 'Ungrouped schemas p0 '[]
      ) ) column
  -> Aliased (Optional (
        Expression '[] commons 'Ungrouped schemas p1 '[]
      ) ) column
overValue f = \case
  Set expr `As` alias -> Set (f expr) `as` alias
  Default `As` alias -> Default `as` alias

paramsPlus1
  :: Expression outer commons grp schemas params from ty
  -> Expression outer commons grp schemas (param ': params) from ty
paramsPlus1 = UnsafeExpression . paramsPlus1_ . renderSQL
  where
    paramsPlus1_ :: ByteString -> ByteString
    paramsPlus1_ = undefined -- replace $n with $n+1

insertParamsInto
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , row0 ~ TableToRow table
     , SListI columns
     , SListI returning
     , Insertable schemas params row columns )
  => QualifiedAlias sch tab
  -> ConflictClause tab commons schemas params table
  -> ReturningClause commons schemas params '[tab ::: row0] returning
  -> Manipulation commons schemas params returning
insertParamsInto tab = insertInto tab (Values_ params)

insertParamsInto_
  :: ( Has sch schemas schema
     , Has tab schema ('Table table)
     , columns ~ TableToColumns table
     , row0 ~ TableToRow table
     , SListI columns
     , Insertable schemas params row columns )
  => QualifiedAlias sch tab
  -> Manipulation commons schemas params '[]
insertParamsInto_ tab = insertInto_ tab (Values_ params)

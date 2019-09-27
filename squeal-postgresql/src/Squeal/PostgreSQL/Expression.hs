{-|
Module: Squeal.PostgreSQL.Expression
Description: Squeal expressions
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Squeal expressions are the atoms used to build statements.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MagicHash
  , OverloadedStrings
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
  , RankNTypes
#-}

module Squeal.PostgreSQL.Expression
  ( -- * Expression
    Expression (..)
  , Expr
  , type (-->)
  , FunctionDB
  , unsafeFunction
  , function
  , unsafeLeftOp
  , leftOp
  , unsafeRightOp
  , rightOp
  , Operator
  , OperatorDB
  , unsafeBinaryOp
  , binaryOp
  , FunctionVar
  , unsafeFunctionVar
  , type (--->)
  , FunctionNDB
  , unsafeFunctionN
  , functionN
  , PGSubset (..)
  , PGIntersect (..)
    -- * Re-export
  , (&)
  , K (..)
  , unK
  ) where

import Control.Category
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Semigroup hiding (All)
import Data.String
import Generics.SOP hiding (All, from)
import GHC.OverloadedLabels
import GHC.TypeLits
import Numeric
import Prelude hiding (id, (.))

import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Alias
import Squeal.PostgreSQL.List
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

{-----------------------------------------
column expressions
-----------------------------------------}

{- | `Expression`s are used in a variety of contexts,
such as in the target `Squeal.PostgreSQL.Query.List` of the
`Squeal.PostgreSQL.Query.select` command,
as new column values in `Squeal.PostgreSQL.Manipulation.insertRow` or
`Squeal.PostgreSQL.Manipulation.update`,
or in search `Squeal.PostgreSQL.Logic.Condition`s in a number of commands.

The expression syntax allows the calculation of
values from primitive expression using arithmetic, logical,
and other operations.

The type parameters of `Expression` are

* @outer :: @ `FromType`, the @from@ clauses of any outer queries in which
  the `Expression` is a correlated subquery expression;
* @commons :: @ `FromType`, the `Squeal.PostgreSQL.Query.CommonTableExpression`s
  that are in scope for the `Expression`;
* @grp :: @ `Grouping`, the `Grouping` of the @from@ clause which may limit
  which columns may be referenced by alias;
* @schemas :: @ `SchemasType`, the schemas of your database that are in
  scope for the `Expression`;
* @from :: @ `FromType`, the @from@ clause which the `Expression` may use
  to reference columns by alias;
* @ty :: @ `NullityType`, the type of the `Expression`.
-}
newtype Expression
  (outer :: FromType)
  (commons :: FromType)
  (grp :: Grouping)
  (schemas :: SchemasType)
  (params :: [NullityType])
  (from :: FromType)
  (ty :: NullityType)
    = UnsafeExpression { renderExpression :: ByteString }
    deriving (GHC.Generic,Show,Eq,Ord,NFData)
instance RenderSQL (Expression outer commons grp schemas params from ty) where
  renderSQL = renderExpression

-- | An `Expr` is a closed `Expression`.
-- It is a F@RankNType@ but don't be scared.
-- Think of it as an expression which sees no
-- namespaces, so you can't use parameters
-- or alias references. It can be used as
-- a simple piece of more complex `Expression`s.
type Expr x
  = forall outer commons grp schemas params from
  . Expression outer commons grp schemas params from x
    -- ^ cannot reference aliases

-- | A @RankNType@ for binary operators.
type Operator x1 x2 y
  =  forall outer commons grp schemas params from
  .  Expression outer commons grp schemas params from x1
     -- ^ left input
  -> Expression outer commons grp schemas params from x2
     -- ^ right input
  -> Expression outer commons grp schemas params from y
     -- ^ output

type OperatorDB schemas x1 x2 y
  =  forall outer commons grp params from
  .  Expression outer commons grp schemas params from x1
     -- ^ left input
  -> Expression outer commons grp schemas params from x2
     -- ^ right input
  -> Expression outer commons grp schemas params from y
     -- ^ output

-- | A @RankNType@ for functions with a single argument.
-- These could be either function calls or unary operators.
-- This is a subtype of the usual Haskell function type `Prelude.->`,
-- indeed a subcategory as it is closed under the usual
-- `Prelude..` and `Prelude.id`.
type (-->) x y
  =  forall outer commons grp schemas params from
  .  Expression outer commons grp schemas params from x
     -- ^ input
  -> Expression outer commons grp schemas params from y
     -- ^ output

type FunctionDB schemas x y
  =  forall outer commons grp params from
  .  Expression outer commons grp schemas params from x
     -- ^ input
  -> Expression outer commons grp schemas params from y
     -- ^ output

{- | A @RankNType@ for functions with a fixed-length list of heterogeneous arguments.
Use the `*:` operator to end your argument lists, like so.

>>> printSQL (unsafeFunctionN "fun" (true :* false :* localTime *: true))
fun(TRUE, FALSE, LOCALTIME, TRUE)
-}
type (--->) xs y
  =  forall outer commons grp schemas params from
  .  NP (Expression outer commons grp schemas params from) xs
     -- ^ inputs
  -> Expression outer commons grp schemas params from y
     -- ^ output

type FunctionNDB schemas xs y
  =  forall outer commons grp params from
  .  NP (Expression outer commons grp schemas params from) xs
     -- ^ inputs
  -> Expression outer commons grp schemas params from y
     -- ^ output

{- | A @RankNType@ for functions with a variable-length list of
homogeneous arguments and at least 1 more argument.
-}
type FunctionVar x0 x1 y
  =  forall outer commons grp schemas params from
  .  [Expression outer commons grp schemas params from x0]
     -- ^ inputs
  -> Expression outer commons grp schemas params from x1
     -- ^ must have at least 1 input
  -> Expression outer commons grp schemas params from y
     -- ^ output

{- | >>> printSQL (unsafeFunctionVar "greatest" [true, null_] false)
greatest(TRUE, NULL, FALSE)
-}
unsafeFunctionVar :: ByteString -> FunctionVar x0 x1 y
unsafeFunctionVar fun xs x = UnsafeExpression $ fun <> parenthesized
  (commaSeparated (renderSQL <$> xs) <> ", " <> renderSQL x)

instance (HasUnique tab (Join outer from) row, Has col row ty)
  => IsLabel col (Expression outer commons 'Ungrouped schemas params from ty) where
    fromLabel = UnsafeExpression $ renderSQL (Alias @col)
instance (HasUnique tab (Join outer from) row, Has col row ty, tys ~ '[ty])
  => IsLabel col (NP (Expression outer commons 'Ungrouped schemas params from) tys) where
    fromLabel = fromLabel @col :* Nil
instance (HasUnique tab (Join outer from) row, Has col row ty, column ~ (col ::: ty))
  => IsLabel col
    (Aliased (Expression outer commons 'Ungrouped schemas params from) column) where
    fromLabel = fromLabel @col `As` Alias
instance (HasUnique tab (Join outer from) row, Has col row ty, columns ~ '[col ::: ty])
  => IsLabel col
    (NP (Aliased (Expression outer commons 'Ungrouped schemas params from)) columns) where
    fromLabel = fromLabel @col :* Nil

instance (Has tab (Join outer from) row, Has col row ty)
  => IsQualified tab col (Expression outer commons 'Ungrouped schemas params from ty) where
    tab ! col = UnsafeExpression $
      renderSQL tab <> "." <> renderSQL col
instance (Has tab (Join outer from) row, Has col row ty, tys ~ '[ty])
  => IsQualified tab col (NP (Expression outer commons 'Ungrouped schemas params from) tys) where
    tab ! col = tab ! col :* Nil
instance (Has tab (Join outer from) row, Has col row ty, column ~ (col ::: ty))
  => IsQualified tab col
    (Aliased (Expression outer commons 'Ungrouped schemas params from) column) where
    tab ! col = tab ! col `As` col
instance (Has tab (Join outer from) row, Has col row ty, columns ~ '[col ::: ty])
  => IsQualified tab col
    (NP (Aliased (Expression outer commons 'Ungrouped schemas params from)) columns) where
    tab ! col = tab ! col :* Nil

instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  ) => IsLabel col
    (Expression outer commons ('Grouped bys) schemas params from ty) where
      fromLabel = UnsafeExpression $ renderSQL (Alias @col)
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , tys ~ '[ty]
  ) => IsLabel col
    (NP (Expression outer commons ('Grouped bys) schemas params from) tys) where
      fromLabel = fromLabel @col :* Nil
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , column ~ (col ::: ty)
  ) => IsLabel col
    (Aliased (Expression outer commons ('Grouped bys) schemas params from) column) where
      fromLabel = fromLabel @col `As` Alias
instance
  ( HasUnique tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , columns ~ '[col ::: ty]
  ) => IsLabel col
    (NP (Aliased (Expression outer commons ('Grouped bys) schemas params from)) columns) where
      fromLabel = fromLabel @col :* Nil

instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  ) => IsQualified tab col
    (Expression outer commons ('Grouped bys) schemas params from ty) where
      tab ! col = UnsafeExpression $
        renderSQL tab <> "." <> renderSQL col
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , tys ~ '[ty]
  ) => IsQualified tab col
    (NP (Expression outer commons ('Grouped bys) schemas params from) tys) where
      tab ! col = tab ! col :* Nil
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , column ~ (col ::: ty)
  ) => IsQualified tab col
    (Aliased (Expression outer commons ('Grouped bys) schemas params from) column) where
      tab ! col = tab ! col `As` col
instance
  ( Has tab (Join outer from) row
  , Has col row ty
  , GroupedBy tab col bys
  , columns ~ '[col ::: ty]
  ) => IsQualified tab col
    (NP (Aliased (Expression outer commons ('Grouped bys) schemas params from)) columns) where
      tab ! col = tab ! col :* Nil

instance (KnownSymbol label, label `In` labels) => IsPGlabel label
  (Expression outer commons grp schemas params from (null ('PGenum labels))) where
  label = UnsafeExpression $ renderSQL (PGlabel @label)

-- | >>> printSQL $ unsafeBinaryOp "OR" true false
-- (TRUE OR FALSE)
unsafeBinaryOp :: ByteString -> Operator ty0 ty1 ty2
unsafeBinaryOp op x y = UnsafeExpression $ parenthesized $
  renderSQL x <+> op <+> renderSQL y

binaryOp
  :: forall op sch schemas schema x y z.
    ( Has sch schemas schema
    , Has op schema ('Operator ('BinaryOp x y z)) )
  => OperatorDB schemas x y z
binaryOp = unsafeBinaryOp $ renderSymbol @op

-- | >>> printSQL $ unsafeLeftOp "NOT" true
-- (NOT TRUE)
unsafeLeftOp :: ByteString -> x --> y
unsafeLeftOp op x = UnsafeExpression $ parenthesized $ op <+> renderSQL x

leftOp
  :: forall op sch schemas schema x y.
    ( Has sch schemas schema
    , Has op schema ('Operator ('LeftOp x y)) )
  => FunctionDB schemas x y
leftOp = unsafeLeftOp $ renderSymbol @op

-- | >>> printSQL $ true & unsafeRightOp "IS NOT TRUE"
-- (TRUE IS NOT TRUE)
unsafeRightOp :: ByteString -> x --> y
unsafeRightOp op x = UnsafeExpression $ parenthesized $ renderSQL x <+> op

rightOp
  :: forall op sch schemas schema x y.
    ( Has sch schemas schema
    , Has op schema ('Operator ('RightOp x y)) )
  => FunctionDB schemas x y
rightOp = unsafeRightOp $ renderSymbol @op

-- | >>> printSQL $ unsafeFunction "f" true
-- f(TRUE)
unsafeFunction :: ByteString -> x --> y
unsafeFunction fun x = UnsafeExpression $
  fun <> parenthesized (renderSQL x)

function
  :: (Has sch schemas schema, Has fun schema ('Function ('[x] :=> 'Returns y)))
  => QualifiedAlias sch fun
  -> FunctionDB schemas x y
function = unsafeFunction . renderSQL

-- | >>> printSQL $ unsafeFunctionN "f" (currentTime :* localTimestamp :* false *: literal 'a')
-- f(CURRENT_TIME, LOCALTIMESTAMP, FALSE, E'a')
unsafeFunctionN :: SListI xs => ByteString -> xs ---> y
unsafeFunctionN fun xs = UnsafeExpression $
  fun <> parenthesized (renderCommaSeparated renderSQL xs)

functionN
  :: ( Has sch schemas schema
     , Has fun schema ('Function (xs :=> 'Returns y))
     , SListI xs )
  => QualifiedAlias sch fun
  -> FunctionNDB schemas xs y
functionN = unsafeFunctionN . renderSQL

instance ty `In` PGNum
  => Num (Expression outer commons grp schemas params from (null ty)) where
    (+) = unsafeBinaryOp "+"
    (-) = unsafeBinaryOp "-"
    (*) = unsafeBinaryOp "*"
    abs = unsafeFunction "abs"
    signum = unsafeFunction "sign"
    fromInteger
      = UnsafeExpression
      . fromString
      . show

instance (ty `In` PGNum, ty `In` PGFloating) => Fractional
  (Expression outer commons grp schemas params from (null ty)) where
    (/) = unsafeBinaryOp "/"
    fromRational
      = UnsafeExpression
      . fromString
      . ($ "")
      . showFFloat Nothing
      . fromRat @Double

instance (ty `In` PGNum, ty `In` PGFloating) => Floating
  (Expression outer commons grp schemas params from (null ty)) where
    pi = UnsafeExpression "pi()"
    exp = unsafeFunction "exp"
    log = unsafeFunction "ln"
    sqrt = unsafeFunction "sqrt"
    b ** x = UnsafeExpression $
      "power(" <> renderSQL b <> ", " <> renderSQL x <> ")"
    logBase b y = log y / log b
    sin = unsafeFunction "sin"
    cos = unsafeFunction "cos"
    tan = unsafeFunction "tan"
    asin = unsafeFunction "asin"
    acos = unsafeFunction "acos"
    atan = unsafeFunction "atan"
    sinh x = (exp x - exp (-x)) / 2
    cosh x = (exp x + exp (-x)) / 2
    tanh x = sinh x / cosh x
    asinh x = log (x + sqrt (x*x + 1))
    acosh x = log (x + sqrt (x*x - 1))
    atanh x = log ((1 + x) / (1 - x)) / 2

-- | Contained by operators
class PGSubset ty where
  (@>) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
  (@>) = unsafeBinaryOp "@>"
  (<@) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
  (<@) = unsafeBinaryOp "<@"
instance PGSubset 'PGjsonb
instance PGSubset 'PGtsquery
instance PGSubset ('PGvararray ty)
instance PGSubset ('PGrange ty)

class PGIntersect ty where
  (@&&) :: Operator (null0 ty) (null1 ty) ('Null 'PGbool)
  (@&&) = unsafeBinaryOp "&&"
instance PGIntersect ('PGvararray ty)
instance PGIntersect ('PGrange ty)

instance IsString
  (Expression outer commons grp schemas params from (null 'PGtext)) where
    fromString str = UnsafeExpression $
      "E\'" <> fromString (escape =<< str) <> "\'"
instance IsString
  (Expression outer commons grp schemas params from (null 'PGtsvector)) where
    fromString str = UnsafeExpression . parenthesized . (<> " :: tsvector") $
      "E\'" <> fromString (escape =<< str) <> "\'"
instance IsString
  (Expression outer commons grp schemas params from (null 'PGtsquery)) where
    fromString str = UnsafeExpression . parenthesized . (<> " :: tsquery") $
      "E\'" <> fromString (escape =<< str) <> "\'"

instance Semigroup
  (Expression outer commons grp schemas params from (null ('PGvararray ty))) where
    (<>) = unsafeBinaryOp "||"
instance Semigroup
  (Expression outer commons grp schemas params from (null 'PGjsonb)) where
    (<>) = unsafeBinaryOp "||"
instance Semigroup
  (Expression outer commons grp schemas params from (null 'PGtext)) where
    (<>) = unsafeBinaryOp "||"
instance Semigroup
  (Expression outer commons grp schemas params from (null 'PGtsvector)) where
    (<>) = unsafeBinaryOp "||"

instance Monoid
  (Expression outer commons grp schemas params from (null 'PGtext)) where
    mempty = fromString ""
    mappend = (<>)
instance Monoid
  (Expression outer commons grp schemas params from (null 'PGtsvector)) where
    mempty = fromString ""
    mappend = (<>)

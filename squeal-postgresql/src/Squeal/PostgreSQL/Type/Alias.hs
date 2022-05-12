{-|
Module: Squeal.PostgreSQL.Type.Alias
Description: aliases
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

This module embeds Postgres's alias system in Haskell in
a typesafe fashion. Thanks to GHC's @OverloadedLabels@ extension,
Squeal can reference aliases by prepending with a @#@.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DeriveAnyClass
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilyDependencies
  , TypeInType
  , TypeOperators
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module Squeal.PostgreSQL.Type.Alias
  ( -- * Aliases
    (:::)
  , Alias (..)
  , IsLabel (..)
  , Aliased (As)
  , Aliasable (as)
  , renderAliased
  , mapAliased
  , Has
  , HasUnique
  , HasErr
  , HasAll
  , HasIn
    -- * Qualified Aliases
  , QualifiedAlias (..)
  , IsQualified (..)
    -- * Grouping
  , Grouping (..)
  , GroupedBy
    -- * Error reporting
  , LookupFailedError
  , PrettyPrintHaystack
  , PrettyPrintInfo(..)
  , MismatchError
  , LookupFailedError'
  , DefaultPrettyPrinter
  , MismatchError'
  ) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.String (fromString)
import GHC.Exts (Any, Constraint)
import GHC.OverloadedLabels
import GHC.TypeLits

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Squeal.PostgreSQL.Type.List
import Squeal.PostgreSQL.Render

-- $setup
-- >>> import Squeal.PostgreSQL

-- | The alias operator `:::` is like a promoted version of `As`,
-- a type level pair between an alias and some type.
type (:::) (alias :: Symbol) ty = '(alias,ty)
infixr 6 :::


-- | `Grouping` is an auxiliary namespace, created by
-- @GROUP BY@ clauses (`Squeal.PostgreSQL.Query.groupBy`), and used
-- for typesafe aggregation
data Grouping
  = Ungrouped -- ^ no aggregation permitted
  | Grouped [(Symbol,Symbol)] -- ^ aggregation required for any column which is not grouped

{- | A `GroupedBy` constraint indicates that a table qualified column is
a member of the auxiliary namespace created by @GROUP BY@ clauses and thus,
may be called in an output `Squeal.PostgreSQL.Expression.Expression` without aggregating.
-}
class (KnownSymbol table, KnownSymbol column)
  => GroupedBy table column bys where
instance {-# OVERLAPPING #-} (KnownSymbol table, KnownSymbol column)
  => GroupedBy table column ('(table,column) ': bys)
instance {-# OVERLAPPABLE #-}
  ( KnownSymbol table
  , KnownSymbol column
  , GroupedBy table column bys
  ) => GroupedBy table column (tabcol ': bys)

-- | `Alias`es are proxies for a type level string or `Symbol`
-- and have an `IsLabel` instance so that with @-XOverloadedLabels@
--
-- >>> :set -XOverloadedLabels
-- >>> #foobar :: Alias "foobar"
-- Alias
data Alias (alias :: Symbol) = Alias
  deriving (Eq,GHC.Generic,Ord,Show,NFData)
instance alias1 ~ alias2 => IsLabel alias1 (Alias alias2) where
  fromLabel = Alias
instance aliases ~ '[alias] => IsLabel alias (NP Alias aliases) where
  fromLabel = fromLabel SOP.:* Nil
-- | >>> printSQL (#jimbob :: Alias "jimbob")
-- "jimbob"
instance KnownSymbol alias => RenderSQL (Alias alias) where
  renderSQL = doubleQuoted . fromString . symbolVal

-- >>> printSQL (#jimbob :* #kandi :: NP Alias '["jimbob", "kandi"])
-- "jimbob", "kandi"
instance SOP.All KnownSymbol aliases => RenderSQL (NP Alias aliases) where
  renderSQL
    = commaSeparated
    . SOP.hcollapse
    . SOP.hcmap (SOP.Proxy @KnownSymbol) (SOP.K . renderSQL)

-- | The `As` operator is used to name an expression. `As` is like a demoted
-- version of `:::`.
--
-- >>> Just "hello" `As` #hi :: Aliased Maybe ("hi" ::: String)
-- As (Just "hello") Alias
data Aliased expression aliased where
  As
    :: KnownSymbol alias
    => expression ty
    -> Alias alias
    -> Aliased expression (alias ::: ty)
deriving instance Show (expression ty)
  => Show (Aliased expression (alias ::: ty))
deriving instance Eq (expression ty)
  => Eq (Aliased expression (alias ::: ty))
deriving instance Ord (expression ty)
  => Ord (Aliased expression (alias ::: ty))
instance (alias0 ~ alias1, alias0 ~ alias2, KnownSymbol alias2)
  => IsLabel alias0 (Aliased Alias (alias1 ::: alias2)) where
    fromLabel = fromLabel @alias2 `As` fromLabel @alias1

-- | The `Aliasable` class provides a way to scrap your `Nil`s
-- in an `NP` list of `Aliased` expressions.
class KnownSymbol alias => Aliasable alias expression aliased
  | aliased -> expression
  , aliased -> alias
  where as :: expression -> Alias alias -> aliased
instance (KnownSymbol alias, aliased ~ (alias ::: ty)) => Aliasable alias
  (expression ty)
  (Aliased expression aliased)
    where
      as = As
instance (KnownSymbol alias, tys ~ '[alias ::: ty]) => Aliasable alias
  (expression ty)
  (NP (Aliased expression) tys)
    where
      expression `as` alias = expression `As` alias SOP.:* Nil

-- | >>> let renderMaybe = fromString . maybe "Nothing" (const "Just")
-- >>> renderAliased renderMaybe (Just (3::Int) `As` #an_int)
-- "Just AS \"an_int\""
renderAliased
  :: (forall ty. expression ty -> ByteString)
  -> Aliased expression aliased
  -> ByteString
renderAliased render (expression `As` alias) =
  render expression <> " AS " <> renderSQL alias

-- | Map a function over an `Aliased` expression.
mapAliased
  :: (expr x -> expr y)
  -> Aliased expr (alias ::: x)
  -> Aliased expr (alias ::: y)
mapAliased f (x `As` alias) = f x `As` alias

-- | @HasUnique alias fields field@ is a constraint that proves that
-- @fields@ is a singleton of @alias ::: field@.
type HasUnique alias fields field = fields ~ '[alias ::: field]

-- | @Has alias fields field@ is a constraint that proves that
-- @fields@ has a field of @alias ::: field@, inferring @field@
-- from @alias@ and @fields@.
class (KnownSymbol alias) => Has (alias :: Symbol) (fields :: [(Symbol, kind)]) (field :: kind) | alias fields -> field
-- having these instances forces 'Has' to inspect 'alias' and 'fields' and thereby fail before delegating to
-- 'HasErr', which means 'Has' shows up in error messages instead of 'HasErr'
instance {-# OVERLAPPING #-} (KnownSymbol alias, HasErr (alias ::: field0 ': fields) alias (alias ::: field0 ': fields) field1)
  => Has alias (alias ::: field0 ': fields) field1
instance {-# OVERLAPPABLE #-} (KnownSymbol alias, HasErr (field' ': fields) alias (field' ': fields) field)
  => Has alias (field' ': fields) field
instance (KnownSymbol alias, HasErr '[] alias '[] field)
  => Has alias '[] field

{- | 'HasErr' is like `Has` except it also retains the original
list of fields being searched, so that error messages are more
useful.
-}
class KnownSymbol alias =>
  HasErr (allFields :: [(Symbol, kind)]) (alias :: Symbol) (fields :: [(Symbol,kind)]) (field :: kind)
  | alias fields -> field where
instance {-# OVERLAPPING #-} (KnownSymbol alias, field0 ~ field1, MismatchError alias allFields field0 field1)
  => HasErr allFields alias (alias ::: field0 ': fields) field1
instance {-# OVERLAPPABLE #-} (KnownSymbol alias, HasErr allFields alias fields field)
  => HasErr allFields alias (field' ': fields) field
instance ( KnownSymbol alias
         , LookupFailedError alias allFields -- report a nicer error
         , field ~ Any -- required to satisfy the fundep
         ) => HasErr allFields alias '[] field

-- | @MismatchError@ reports a nicer error with more context when we successfully do a lookup but
-- find a different field than we expected. As a type family, it ensures that we only do the (expensive)
-- calculation of coming up with our pretty printing information when we actually have a mismatch
type family MismatchError (alias :: Symbol) (fields :: [(Symbol, kind)]) (found :: kind) (expected :: kind) :: Constraint where
  MismatchError _ _ found found = ()
  MismatchError alias fields found expected = MismatchError' (MismatchError' () (DefaultPrettyPrinter fields) alias fields found expected) (PrettyPrintHaystack fields) alias fields found expected

-- | @MismatchError'@ is the workhorse behind @MismatchError@, but taking an additional type as the first argument. We can put another type error
-- in there which will only show if @MismatchError'@ is stuck; this allows us to fall back to @DefaultPrettyPrinter@ when a @PrettyPrintHaystack@ instance
-- is missing
type family MismatchError' (err :: Constraint) (ppInfo :: PrettyPrintInfo) (alias :: Symbol) (fields :: [(Symbol, kind)]) (found :: kind) (expected :: kind) :: Constraint where
  MismatchError' _ ('PrettyPrintInfo needleName haystackName _) alias fields found expected = TypeError
    (     'Text "Type mismatch when looking up " ':<>: needleName ':<>: 'Text " named " ':<>: 'ShowType alias
    ':$$: 'Text "in " ':<>: haystackName ':<>: 'Text ":"
    -- we don't use a pretty haystack because we want to show the values
    ':$$: 'ShowType fields
    ':$$: 'Text ""
    ':$$: 'Text "Expected: " ':<>: 'ShowType expected
    ':$$: 'Text "But found: " ':<>: 'ShowType found
    ':$$: 'Text ""
    )

-- | @LookupFailedError@ reports a nicer error when we fail to look up some @needle@ in some @haystack@
type LookupFailedError needle haystack = LookupFailedError' (LookupFailedError' () (DefaultPrettyPrinter haystack) needle haystack) (PrettyPrintHaystack haystack) needle haystack

-- | @LookupFailedError'@ is the workhorse behind @LookupFailedError@, but taking an additional type as the first argument. We can put another type error
-- in there which will only show if @LookupFailedError'@ is stuck; this allows us to fall back to @DefaultPrettyPrinter@ when a @PrettyPrintHaystack@ instance
-- is missing
type family LookupFailedError' (fallbackForUnknownKind :: Constraint) (prettyPrintInfo :: PrettyPrintInfo) (needle :: Symbol) (haystack :: [(Symbol, k)]) :: Constraint where
  LookupFailedError' _ ('PrettyPrintInfo needleName haystackName prettyHaystack) needle rawHaystack = TypeError
    (     'Text "Could not find " ':<>: needleName ':<>: 'Text " named " ':<>: 'ShowType needle
    ':$$: 'Text "in " ':<>: haystackName ':<>: 'Text ":"
    ':$$: prettyHaystack
    ':$$: 'Text ""
    ':$$: 'Text "*Raw " ':<>: haystackName ':<>: 'Text "*:"
    ':$$: 'ShowType rawHaystack
    ':$$: 'Text ""
    )

-- | @PrettyPrintInfo@ is a data type intended to be used at the type level
-- which describes how to pretty print a haystack in our custom errors. The general intention is we use @PrettyPrintHaystack@
-- to define a more specific way of pretty printing our error information for each kind that we care about
data PrettyPrintInfo = PrettyPrintInfo
  { _needleName :: ErrorMessage
  , _haystackName :: ErrorMessage
  , _haystackPrettyPrint :: ErrorMessage
  }

-- | 'PrettyPrintHaystack' allows us to use the kind of our haystack to come up
-- with nicer errors. It is implemented as an open type family for dependency reasons
type family PrettyPrintHaystack (haystack :: [(Symbol, k)]) :: PrettyPrintInfo

-- | @DefaultPrettyPrinter@ provides a default we can use for kinds that don't provide an instance of @PrettyPrintInfo@,
-- although that should generally only be accidental
type family DefaultPrettyPrinter (haystack :: [(Symbol, k)]) :: PrettyPrintInfo where
  DefaultPrettyPrinter (haystack :: [(Symbol, k)]) = 'PrettyPrintInfo
    ('Text "some kind without a PrettyPrintHaystack instance ("  ':<>: 'ShowType k ':<>: 'Text ")")
    ('Text "associative list of that kind ([(Symbol, "  ':<>: 'ShowType k ':<>: 'Text ")])")
    ('ShowType (Sort (MapFst haystack)))

{-| @HasIn fields (alias ::: field)@ is a constraint that proves that
@fields@ has a field of @alias ::: field@. It is used in @UPDATE@s to
choose which subfields to update.
-}
class HasIn fields field where
instance (Has alias fields field) => HasIn fields (alias ::: field) where

-- | `HasAll` extends `Has` to take lists of @aliases@ and @fields@ and infer
-- a list of @subfields@.
class
  ( SOP.All KnownSymbol aliases
  ) => HasAll
    (aliases :: [Symbol])
    (fields :: [(Symbol,kind)])
    (subfields :: [(Symbol,kind)])
    | aliases fields -> subfields where
instance {-# OVERLAPPING #-} HasAll '[] fields '[]
instance {-# OVERLAPPABLE #-}
  (Has alias fields field, HasAll aliases fields subfields)
  => HasAll (alias ': aliases) fields (alias ::: field ': subfields)

-- | Analagous to `IsLabel`, the constraint
-- `IsQualified` defines `!` for a column alias qualified
-- by a table alias.
class IsQualified qualifier alias expression where
  (!) :: Alias qualifier -> Alias alias -> expression
  infixl 9 !
instance IsQualified qualifier alias (Alias qualifier, Alias alias) where
  (!) = (,)

{-| `QualifiedAlias`es enables multi-schema support by allowing a reference
to a `Squeal.PostgreSQL.Type.Schema.Table`, `Squeal.PostgreSQL.Type.Schema.Typedef`
or `Squeal.PostgreSQL.Type.Schema.View` to be qualified by their schemas. By default,
a qualifier of @public@ is provided.

>>> :{
let
  alias1 :: QualifiedAlias "sch" "tab"
  alias1 = #sch ! #tab
  alias2 :: QualifiedAlias "public" "vw"
  alias2 = #vw
in printSQL alias1 >> printSQL alias2
:}
"sch"."tab"
"vw"
-}
data QualifiedAlias (qualifier :: Symbol) (alias :: Symbol) = QualifiedAlias
  deriving (Eq,GHC.Generic,Ord,Show,NFData)
instance (q ~ q', a ~ a') => IsQualified q a (QualifiedAlias q' a') where
  _ ! _ = QualifiedAlias
instance (q' ~ "public", a ~ a') => IsLabel a (QualifiedAlias q' a') where
  fromLabel = QualifiedAlias
instance (q0 ~ q1, a0 ~ a1, a1 ~ a2, KnownSymbol a2) =>
  IsQualified q0 a0 (Aliased (QualifiedAlias q1) (a1 ::: a2)) where
    _ ! _ = QualifiedAlias `As` Alias
instance (q ~ "public", a0 ~ a1, a1 ~ a2, KnownSymbol a2) =>
  IsLabel a0 (Aliased (QualifiedAlias q) (a1 ::: a2)) where
    fromLabel = QualifiedAlias `As` Alias

instance (KnownSymbol q, KnownSymbol a)
  => RenderSQL (QualifiedAlias q a) where
    renderSQL _ =
      let
        qualifier = renderSQL (Alias @q)
        alias = renderSQL (Alias @a)
      in
        if qualifier == "\"public\"" then alias else qualifier <> "." <> alias

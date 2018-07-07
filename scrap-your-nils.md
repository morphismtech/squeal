## Scrap your Nils

One of the most useful types I've come across in Haskell is the type of
"heterogeneous lists". This is the same as the [Rec](http://hackage.haskell.org/package/vinyl-0.8.1.1/docs/Data-Vinyl-Core.html)
datatype from the [vinyl](http://hackage.haskell.org/package/vinyl) library.
It's also the same as the [NP](http://hackage.haskell.org/package/generics-sop-0.3.2.0/docs/Generics-SOP-NP.html)
datatype from the [generics-sop](http://hackage.haskell.org/package/generics-sop) library.
Squeal makes heavy use of this type.

```Haskell
>>> import Generics.SOP (NP(..))
>>> :i NP
type role NP representational nominal
data NP (a :: k -> *) (b :: [k]) where
  Nil :: forall k (a :: k -> *). NP a '[]
  (:*) :: forall k (a :: k -> *) (x :: k) (xs :: [k]).
          (a x) -> (NP a xs) -> NP a (x : xs)
        -- Defined in ‘Generics.SOP.NP’
```

This type allows us to construct "product" types, where the types of the individual
"terms" are hosted at the type level.

```Haskell
>>> :set -XDataKinds
>>> import Generics.SOP (I(..))
>>> let example = I "foo" :* I pi :* Nil :: NP I '[String, Double]
>>> example
I "foo" :* I 3.141592653589793 :* Nil
```

One thing Squeal uses `NP` for is to form lists of aliases,
using GHC's `OverloadedLabels` extension, hosting the names
of the aliases themselves at the type level.

```Haskell
>>> :set -XKindSignatures -XOverloadedLabels -XFlexibleInstances -XMultiParamTypeClasses
>>> import GHC.TypeLits
>>> import GHC.OverloadedLabels
>>> data Alias (alias :: Symbol) = Alias deriving (Eq,Ord,Show)
>>> instance IsLabel label (Alias label) where fromLabel = Alias
>>> let aliases = #foo :* #bar :* Nil :: NP Alias '["foo", "bar"]
```

However, it's very ugly to have to end every list with `:* Nil`.
When I announced the release of Squeal, people rightly [complained](https://www.reddit.com/r/haskell/comments/6yr5v6/announcing_squeal_a_deep_embedding_of_sql_in/dmq8vvn)
about the syntactic noise. Luckily, there's a neat trick we can use to get rid of it.
Making an `IsLabel` instance not only for elements of our list
but also for lists of length 1, we can ask GHC to interpret
the last label as a list.

```Haskell
>>> instance IsLabel label (NP Alias '[label]) where fromLabel = Alias :* Nil
>>> let aliases' = #foo :* #bar :: NP Alias '["foo", "bar"]
```

Version 0.3.1 of Squeal enables the "Scrap your Nils" trick for
heterogeneous lists of `Alias`es, `Aliased` expressions, `PGlabel`s and `By`s
with the typeclasses `IsLabel`, `IsQualified`, `IsPGlabel`,
and the new `Aliasable` typeclass, to eliminate all need of using `Nil` in a list.

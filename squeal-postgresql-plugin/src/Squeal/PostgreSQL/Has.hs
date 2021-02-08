{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FunctionalDependencies
  , GADTs
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , QuantifiedConstraints
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Has where

class Has (s :: fld) (xs :: [(fld, k)]) (x :: k) | fld xs -> x

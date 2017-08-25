{-|
Module: Squeal.PostgreSQL
Description: Squeel export module
Copyright: (c) Eitan Chatav, 2017
Maintainer: eitan@morphism.tech
Stability: experimental

`Squeal.PostgreSQL` re-exports @squeel-postgresql@'s main modules.
-}

module Squeal.PostgreSQL
  ( module Squeal.PostgreSQL.Binary
  , module Squeal.PostgreSQL.Definition
  , module Squeal.PostgreSQL.Expression
  , module Squeal.PostgreSQL.Manipulation
  , module Squeal.PostgreSQL.PQ
  , module Squeal.PostgreSQL.Query
  , module Squeal.PostgreSQL.Schema
  ) where

import Squeal.PostgreSQL.Binary
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Query
import Squeal.PostgreSQL.Schema

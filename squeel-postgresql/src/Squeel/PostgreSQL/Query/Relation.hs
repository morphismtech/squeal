{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , RankNTypes
#-}

module Squeel.PostgreSQL.Query.Relation where

import Data.ByteString (ByteString)

import Squeel.PostgreSQL.Type

data Relation (ps::[PGType]) (xs::[Column]) (ys::[Column]) =
  Relation { unRelation :: ByteString }

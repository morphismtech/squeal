module Squeal.PostgreSQL.Type.Schema
  ( -- * Postgres Type
    PGType (..)
  ) where

-- | `PGType` is the promoted datakind of PostgreSQL types.
--
-- >>> :set -XDataKinds
-- >>> :kind 'PGbool
-- 'PGbool :: PGType
data PGType
  = PGbool -- ^ logical Boolean (true/false)
  | PGint2 -- ^ signed two-byte integer
  | PGint4 -- ^ signed four-byte integer
  | PGint8 -- ^ signed eight-byte integer
  | PGnumeric -- ^ arbitrary precision numeric type

{-# LANGUAGE
    PolyKinds
#-}

module Squeel.PostgreSQL.Query where

import Data.ByteString (ByteString)

newtype Query db0 db1 ps xs = Query { unQuery :: ByteString }
newtype PreparedQuery db0 db1 ps xs
  = PreparedQuery { unPreparedQuery :: ByteString }

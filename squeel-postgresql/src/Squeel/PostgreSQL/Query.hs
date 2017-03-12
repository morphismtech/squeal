{-# LANGUAGE
    PolyKinds
#-}

module Squeel.PostgreSQL.Query where

import Data.ByteString (ByteString)

newtype Query db0 db1 ps x = Query { unQuery :: ByteString }

{-|
Module: Squeal.PostgreSQL.Session.Oid
Description: object identifiers
Copyright: (c) Eitan Chatav, 2019
Maintainer: eitan@morphism.tech
Stability: experimental

Object identifiers are used internally by PostgreSQL as
primary keys. They are needed to correctly encode
statement parameters.
-}

{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Session.Oid
  ( -- * Oids
    LibPQ.Oid
  , OidOf (..)
  , OidOfArray (..)
  , OidOfNull (..)
  , OidOfField (..)
  ) where

import Control.Monad.Reader
import Data.String
import GHC.TypeLits
import UnliftIO (throwIO)
import PostgreSQL.Binary.Decoding (valueParser, int)

import qualified Data.ByteString as ByteString
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Generics.SOP as SOP

import Squeal.PostgreSQL.Type.Alias
import Squeal.PostgreSQL.Session.Exception
import Squeal.PostgreSQL.Type.Schema

-- $setup
-- >>> import Squeal.PostgreSQL

-- | The `LibPQ.Oid` of a `PGType`
--
-- >>> :set -XTypeApplications
-- >>> conn <- connectdb @'[] "host=localhost port=5432 dbname=exampledb user=postgres password=postgres"
-- >>> runReaderT (oidOf @'[] @'PGbool) conn
-- Oid 16
--
-- >>> finish conn
class OidOf (db :: SchemasType) (pg :: PGType) where
  oidOf :: ReaderT (SOP.K LibPQ.Connection db) IO LibPQ.Oid
-- | The `LibPQ.Oid` of an array
class OidOfArray (db :: SchemasType) (pg :: PGType) where
  oidOfArray :: ReaderT (SOP.K LibPQ.Connection db) IO LibPQ.Oid
instance OidOfArray db pg => OidOf db ('PGvararray (null pg)) where
  oidOf = oidOfArray @db @pg
instance OidOfArray db pg => OidOf db ('PGfixarray dims (null pg)) where
  oidOf = oidOfArray @db @pg
-- | The `LibPQ.Oid` of a `NullType`
class OidOfNull (db :: SchemasType) (ty :: NullType) where
  oidOfNull :: ReaderT (SOP.K LibPQ.Connection db) IO LibPQ.Oid
instance OidOf db pg => OidOfNull db (null pg) where
  oidOfNull = oidOf @db @pg
-- | The `LibPQ.Oid` of a field
class OidOfField (db :: SchemasType) (field :: (Symbol, NullType)) where
  oidOfField :: ReaderT (SOP.K LibPQ.Connection db) IO LibPQ.Oid
instance OidOfNull db ty => OidOfField db (fld ::: ty) where
  oidOfField = oidOfNull @db @ty

instance OidOf db 'PGbool where oidOf = pure $ LibPQ.Oid 16
instance OidOfArray db 'PGbool where oidOfArray = pure $ LibPQ.Oid 1000
instance OidOf db 'PGint2 where oidOf = pure $ LibPQ.Oid 21
instance OidOfArray db 'PGint2 where oidOfArray = pure $ LibPQ.Oid 1005
instance OidOf db 'PGint4 where oidOf = pure $ LibPQ.Oid 23
instance OidOfArray db 'PGint4 where oidOfArray = pure $ LibPQ.Oid 1007
instance OidOf db 'PGint8 where oidOf = pure $ LibPQ.Oid 20
instance OidOfArray db 'PGint8 where oidOfArray = pure $ LibPQ.Oid 1016
instance OidOf db 'PGnumeric where oidOf = pure $ LibPQ.Oid 1700
instance OidOfArray db 'PGnumeric where oidOfArray = pure $ LibPQ.Oid 1231
instance OidOf db 'PGfloat4 where oidOf = pure $ LibPQ.Oid 700
instance OidOfArray db 'PGfloat4 where oidOfArray = pure $ LibPQ.Oid 1021
instance OidOf db 'PGfloat8 where oidOf = pure $ LibPQ.Oid 701
instance OidOfArray db 'PGfloat8 where oidOfArray = pure $ LibPQ.Oid 1022
instance OidOf db 'PGmoney where oidOf = pure $ LibPQ.Oid 790
instance OidOfArray db 'PGmoney where oidOfArray = pure $ LibPQ.Oid 791
instance OidOf db ('PGchar n) where oidOf = pure $ LibPQ.Oid 18
instance OidOfArray db ('PGchar n) where oidOfArray = pure $ LibPQ.Oid 1002
instance OidOf db ('PGvarchar n) where oidOf = pure $ LibPQ.Oid 1043
instance OidOfArray db ('PGvarchar n) where oidOfArray = pure $ LibPQ.Oid 1015
instance OidOf db 'PGtext where oidOf = pure $ LibPQ.Oid 25
instance OidOfArray db 'PGtext where oidOfArray = pure $ LibPQ.Oid 1009
instance OidOf db 'PGbytea where oidOf = pure $ LibPQ.Oid 17
instance OidOfArray db 'PGbytea where oidOfArray = pure $ LibPQ.Oid 1001
instance OidOf db 'PGtimestamp where oidOf = pure $ LibPQ.Oid 1114
instance OidOfArray db 'PGtimestamp where oidOfArray = pure $ LibPQ.Oid 1115
instance OidOf db 'PGtimestamptz where oidOf = pure $ LibPQ.Oid 1184
instance OidOfArray db 'PGtimestamptz where oidOfArray = pure $ LibPQ.Oid 1185
instance OidOf db 'PGdate where oidOf = pure $ LibPQ.Oid 1082
instance OidOfArray db 'PGdate where oidOfArray = pure $ LibPQ.Oid 1182
instance OidOf db 'PGtime where oidOf = pure $ LibPQ.Oid 1083
instance OidOfArray db 'PGtime where oidOfArray = pure $ LibPQ.Oid 1183
instance OidOf db 'PGtimetz where oidOf = pure $ LibPQ.Oid 1266
instance OidOfArray db 'PGtimetz where oidOfArray = pure $ LibPQ.Oid 1270
instance OidOf db 'PGinterval where oidOf = pure $ LibPQ.Oid 1186
instance OidOfArray db 'PGinterval where oidOfArray = pure $ LibPQ.Oid 1187
instance OidOf db 'PGuuid where oidOf = pure $ LibPQ.Oid 2950
instance OidOfArray db 'PGuuid where oidOfArray = pure $ LibPQ.Oid 2951
instance OidOf db 'PGinet where oidOf = pure $ LibPQ.Oid 869
instance OidOfArray db 'PGinet where oidOfArray = pure $ LibPQ.Oid 1041
instance OidOf db 'PGjson where oidOf = pure $ LibPQ.Oid 114
instance OidOfArray db 'PGjson where oidOfArray = pure $ LibPQ.Oid 199
instance OidOf db 'PGjsonb where oidOf = pure $ LibPQ.Oid 3802
instance OidOfArray db 'PGjsonb where oidOfArray = pure $ LibPQ.Oid 3807
instance OidOf db 'PGtsvector where oidOf = pure $ LibPQ.Oid 3614
instance OidOfArray db 'PGtsvector where oidOfArray = pure $ LibPQ.Oid 3643
instance OidOf db 'PGtsquery where oidOf = pure $ LibPQ.Oid 3615
instance OidOfArray db 'PGtsquery where oidOfArray = pure $ LibPQ.Oid 3645
instance OidOf db 'PGoid where oidOf = pure $ LibPQ.Oid 26
instance OidOfArray db 'PGoid where oidOfArray = pure $ LibPQ.Oid 1028
instance OidOf db ('PGrange 'PGint4) where oidOf = pure $ LibPQ.Oid 3904
instance OidOfArray db ('PGrange 'PGint4) where oidOfArray = pure $ LibPQ.Oid 3905
instance OidOf db ('PGrange 'PGint8) where oidOf = pure $ LibPQ.Oid 3926
instance OidOfArray db ('PGrange 'PGint8) where oidOfArray = pure $ LibPQ.Oid 3927
instance OidOf db ('PGrange 'PGnumeric) where oidOf = pure $ LibPQ.Oid 3906
instance OidOfArray db ('PGrange 'PGnumeric) where oidOfArray = pure $ LibPQ.Oid 3907
instance OidOf db ('PGrange 'PGtimestamp) where oidOf = pure $ LibPQ.Oid 3908
instance OidOfArray db ('PGrange 'PGtimestamp) where oidOfArray = pure $ LibPQ.Oid 3909
instance OidOf db ('PGrange 'PGtimestamptz) where oidOf = pure $ LibPQ.Oid 3910
instance OidOfArray db ('PGrange 'PGtimestamptz) where oidOfArray = pure $ LibPQ.Oid 3911
instance OidOf db ('PGrange 'PGdate) where oidOf = pure $ LibPQ.Oid 3912
instance OidOfArray db ('PGrange 'PGdate) where oidOfArray = pure $ LibPQ.Oid 3913
instance
  ( UserType db ('PGcomposite row) ~ '(sch,td)
  , Has sch db schema
  , Has td schema ('Typedef ('PGcomposite row)) )
  => OidOf db ('PGcomposite row) where
    oidOf = oidOfTypedef (QualifiedAlias @sch @td)
instance
  ( UserType db ('PGcomposite row) ~ '(sch,td)
  , Has sch db schema
  , Has td schema ('Typedef ('PGcomposite row)) )
  => OidOfArray db ('PGcomposite row) where
    oidOfArray = oidOfArrayTypedef (QualifiedAlias @sch @td)
instance
  ( UserType db ('PGenum labels) ~ '(sch,td)
  , Has sch db schema
  , Has td schema ('Typedef ('PGenum labels)) )
  => OidOf db ('PGenum labels) where
    oidOf = oidOfTypedef (QualifiedAlias @sch @td)
instance
  ( UserType db ('PGenum labels) ~ '(sch,td)
  , Has sch db schema
  , Has td schema ('Typedef ('PGenum labels)) )
  => OidOfArray db ('PGenum labels) where
    oidOfArray = oidOfArrayTypedef (QualifiedAlias @sch @td)

oidOfTypedef
  :: (Has sch db schema, Has ty schema pg)
  => QualifiedAlias sch ty
  -> ReaderT (SOP.K LibPQ.Connection db) IO LibPQ.Oid
oidOfTypedef (_ :: QualifiedAlias sch ty) = ReaderT $ \(SOP.K conn) -> do
  resultMaybe <- LibPQ.execParams conn q [] LibPQ.Binary
  case resultMaybe of
    Nothing -> throwIO $ ConnectionException oidErr
    Just result -> do
      numRows <- LibPQ.ntuples result
      when (numRows /= 1) $ throwIO $ RowsException oidErr 1 numRows
      valueMaybe <- LibPQ.getvalue result 0 0
      case valueMaybe of
        Nothing -> throwIO $ ConnectionException oidErr
        Just value -> case valueParser int value of
          Left err -> throwIO $ DecodingException oidErr err
          Right oid -> return $ LibPQ.Oid oid
  where
    tyVal = symbolVal (SOP.Proxy @ty)
    schVal = symbolVal (SOP.Proxy @sch)
    oidErr = "oidOfTypedef " <> fromString (schVal <> "." <> tyVal)
    q = ByteString.intercalate " "
      [ "SELECT pg_type.oid"
      , "FROM pg_type"
      , "INNER JOIN pg_namespace"
      , "ON pg_type.typnamespace = pg_namespace.oid"
      , "WHERE pg_type.typname = "
      , "\'" <> fromString tyVal <> "\'"
      , "AND pg_namespace.nspname = "
      , "\'" <> fromString schVal <> "\'"
      , ";" ]

oidOfArrayTypedef
  :: (Has sch db schema, Has ty schema pg)
  => QualifiedAlias sch ty
  -> ReaderT (SOP.K LibPQ.Connection db) IO LibPQ.Oid
oidOfArrayTypedef (_ :: QualifiedAlias sch ty) = ReaderT $ \(SOP.K conn) -> do
  resultMaybe <- LibPQ.execParams conn q [] LibPQ.Binary
  case resultMaybe of
    Nothing -> throwIO $ ConnectionException oidErr
    Just result -> do
      numRows <- LibPQ.ntuples result
      when (numRows /= 1) $ throwIO $ RowsException oidErr 1 numRows
      valueMaybe <- LibPQ.getvalue result 0 0
      case valueMaybe of
        Nothing -> throwIO $ ConnectionException oidErr
        Just value -> case valueParser int value of
          Left err -> throwIO $ DecodingException oidErr err
          Right oid -> return $ LibPQ.Oid oid
  where
    tyVal = symbolVal (SOP.Proxy @ty)
    schVal = symbolVal (SOP.Proxy @sch)
    oidErr = "oidOfArrayTypedef " <> fromString (schVal <> "." <> tyVal)
    q = ByteString.intercalate " "
      [ "SELECT pg_type.typelem"
      , "FROM pg_type"
      , "INNER JOIN pg_namespace"
      , "ON pg_type.typnamespace = pg_namespace.oid"
      , "WHERE pg_type.typname = "
      , "\'" <> fromString tyVal <> "\'"
      , "AND pg_namespace.nspname = "
      , "\'" <> fromString schVal <> "\'"
      , ";" ]

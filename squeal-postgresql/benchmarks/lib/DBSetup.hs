{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DerivingStrategies                #-}
{-# LANGUAGE ScopedTypeVariables                #-}

module DBSetup where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C
import           Control.Monad                  ( void )
import           GHC.Generics
import           Test.QuickCheck
import           Squeal.PostgreSQL
-- Project imports
import           Schema                         ( Schemas
                                                , DeviceOS
                                                , IPLocation
                                                )
import           Queries                        ( InsertUser )


-- First create enums as they're needed in the Schema
setup :: Definition (Public '[]) Schemas
setup =
  createTypeEnumFrom @DeviceOS #device_os
    >>> createTypeCompositeFrom @IPLocation #ip_location
    >>> createTable
          #users
          (    serial8
          `as` #id
          :*   (text & notNullable)
          `as` #email
          :*   (text & notNullable)
          `as` #password
          :*   (text & nullable)
          `as` #first_name
          :*   (int2 & nullable)
          `as` #birthyear
          )
          (primaryKey #id `as` #pk_users :* unique #email `as` #email)
    >>> createTable
          #user_devices
          (    serial8
          `as` #id
          :*   notNullable int8
          `as` #user_id
          :*   (text & notNullable)
          `as` #token
          :*   (typedef #device_os & notNullable)
          `as` #os
          )
          (    primaryKey #id
          `as` #pk_user_devices
          :*   foreignKey #user_id #users #id OnDeleteCascade OnUpdateCascade
          `as` #fk_user_id
          :*   unique #token
          `as` #token
          )

-- Drop types last because tables depend on them
teardown :: Definition Schemas (Public '[])
teardown =
  dropTableCascade #user_devices
    >>> dropTableCascade #users
    >>> dropType #ip_location
    >>> dropType #device_os

-- With env vars, we could use the commented keys
data PGConfig = PGConfig
  { pgHost     :: String -- "PG_HOST"
  , pgPort     :: Int    -- "PG_PORT"
  , pgDbname   :: String -- "PG_DBNAME" 
  , pgUser     :: String -- "PG_USER"
  , pgPassword :: String -- "PG_PASSWORD" 
  }
  deriving (Generic, Show)

-- | Helper: unused now, but primarily for testing locally
makeConnStr :: PGConfig -> ByteString
makeConnStr PGConfig { pgHost = host, pgPort = portNumber, pgDbname = dbName, pgUser = user, pgPassword = pw }
  = C.pack
    $  "host="
    <> host
    <> " dbname="
    <> dbName
    <> " user="
    <> user
    <> " password="
    <> pw
    <> " port="
    <> show portNumber

connectionString :: ByteString
connectionString =
  "host=localhost dbname=exampledb user=postgres password=postgres port=5432"

performDBAction :: Definition a b -> String -> IO ()
performDBAction action message = do
  void
    $ withConnection connectionString
    $ manipulate_ (UnsafeManipulation "SET client_min_messages TO WARNING;")
    & pqThen (define action)
  putStrLn message

initDB :: IO ()
initDB =
  performDBAction setup "Initialized Schema & corresponding tables for Database"

teardownDB :: IO ()
teardownDB = performDBAction teardown "Dropped all database tables"

dbSchema :: Definition '["public" ::: '[]] (Drop "public" '["public" ::: '[]])
dbSchema = dropSchemaCascade #public

dropDBSchema :: IO ()
dropDBSchema = performDBAction dbSchema "Dropped Public schema from database"

-- | Concatenate two `ByteString`s with a space between.
(<+>) :: ByteString -> ByteString -> ByteString
infixr 7 <+>
str1 <+> str2 = str1 <> " " <> str2

-- | Drop table custom SQL statement with 'cascade'
dropTableCascade
  :: (Has sch schemas schema, Has tab schema ( 'Table table))
  => QualifiedAlias sch tab -- ^ table to remove
  -> Definition schemas (Alter sch (Drop tab schema) schemas)
dropTableCascade tab =
  UnsafeDefinition $ "DROP TABLE" <+> renderSQL tab <> " cascade;"

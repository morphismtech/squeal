{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module Schema where

import           Squeal.PostgreSQL
import           GHC.Generics
import qualified Generics.SOP                  as SOP


-- Type

data DeviceOS = Android | IOS
    deriving (Show, Read, Eq, Generic)
-- DeviceOS is converted to PG Enum type
instance SOP.Generic DeviceOS
instance SOP.HasDatatypeInfo DeviceOS

-- Defined extra types for the database
-- Operating system enum
type PGDeviceOS = PG (Enumerated DeviceOS)
type DeviceOSType = 'Typedef PGDeviceOS

-- SCHEMA

-- | Helper: Joining timestamps to every table from here
type TimestampColumns = '[
  "inserted_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
  , "updated_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
  ]

-- Users

type UsersColumns = '[
    "id"   :::   'Def :=> 'NotNull 'PGint8
    , "email" ::: 'NoDef :=> 'NotNull 'PGtext
    , "password" ::: 'NoDef :=> 'NotNull 'PGtext
    , "first_name" ::: 'NoDef :=> 'Null 'PGtext
    , "birthyear" ::: 'NoDef :=> 'Null 'PGint2
    ]

type UsersConstraints = '[
    "pk_users" ::: 'PrimaryKey '["id"]
    , "email" ::: 'Unique '["email"]
    ]

type UsersTable = 'Table (UsersConstraints :=> Join UsersColumns TimestampColumns)

-- User devices
type UserDevicesColumns = '[
  "id" ::: 'Def :=> 'NotNull 'PGint8 -- ID as PK because user might have many same OS devices
  , "user_id" ::: 'NoDef :=> 'NotNull 'PGint8
  , "token" ::: 'NoDef :=> 'NotNull 'PGtext
  , "os" ::: 'NoDef :=> 'NotNull PGDeviceOS
  ]

type UserDevicesConstraints = '[
  "pk_user_devices" ::: 'PrimaryKey '["id"]
  , "fk_user_id" ::: 'ForeignKey '["user_id"] "users" '["id"]
  , "token" ::: 'Unique '["token"]
  ]

type UserDevicesTable = 'Table (UserDevicesConstraints :=> Join UserDevicesColumns TimestampColumns)

-- Schema
-- Make sure to put types before tables, otherwise won't compile
type Schema = '[
  -- Enum types:
    "device_os" ::: DeviceOSType
  -- Composite types:
  , "users" ::: UsersTable
  , "user_devices" ::: UserDevicesTable
  ]

type Schemas = '["public" ::: Schema]
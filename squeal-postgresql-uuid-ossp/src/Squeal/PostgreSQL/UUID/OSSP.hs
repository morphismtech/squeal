{-|
Module: Squeal.PostgreSQL.UUID.OSSP
Description: uuid-ossp
Copyright: (c) Eitan Chatav, 2020
Maintainer: eitan@morphism.tech
Stability: experimental

This module provides functions to generate universally
unique identifiers (UUIDs) using one of several standard algorithms.
There are also functions to produce certain special UUID constants.
-}

{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , TypeOperators
#-}

module Squeal.PostgreSQL.UUID.OSSP
  ( -- * Definition
    createUuidOssp
    -- * Generation
  , uuidGenerateV1
  , uuidGenerateV1mc
  , uuidGenerateV3
  , uuidGenerateV4
  , uuidGenerateV5
    -- * Constants
  , uuidNil
  , uuidNSUrl
  , uuidNSDns
  , uuidNSOid
  , uuidNSX500
  ) where

import Squeal.PostgreSQL

-- | Loads ltree extension into the current database.
createUuidOssp :: Definition db db
createUuidOssp = UnsafeDefinition "CREATE EXTENSION \"uuid-ossp\";"

-- | This function generates a version 1 UUID.
-- This involves the MAC address of the computer and a time stamp.
-- Note that UUIDs of this kind reveal the identity of the computer
-- that created the identifier and the time at which it did so,
-- which might make it unsuitable for certain security-sensitive applications.
uuidGenerateV1 :: Expr (null 'PGuuid)
uuidGenerateV1 = UnsafeExpression "uuid_generate_v1()"

-- | This function generates a version 1 UUID but uses a random multicast
-- MAC address instead of the real MAC address of the computer.
uuidGenerateV1mc :: Expr (null 'PGuuid)
uuidGenerateV1mc = UnsafeExpression "uuid_generate_v1mc()"

{- | This function generates a version 3 UUID in the given namespace
using the specified input name. The namespace should be one of the
special constants produced by the uuidNS* functions.
(It could be any UUID in theory.)
The name is an identifier in the selected namespace.
For example:
@
uuidGenerateV3 (uuidNSUrl *: "http://www.postgresql.org")
@

The name parameter will be MD5-hashed,
so the cleartext cannot be derived from the generated UUID.
The generation of UUIDs by this method has no random or
environment-dependent element and is therefore reproducible.
-}
uuidGenerateV3 :: '[null 'PGuuid, null 'PGtext] ---> null 'PGuuid
uuidGenerateV3 = unsafeFunctionN "uuid_generate_v3"

{- | This function generates a version 4 UUID,
which is derived entirely from random numbers.
-}
uuidGenerateV4 :: Expr (null 'PGuuid)
uuidGenerateV4 = UnsafeExpression "uuid_generate_v4()"

{- | This function generates a version 5 UUID,
which works like a version 3 UUID except that
SHA-1 is used as a hashing method.
Version 5 should be preferred over version 3 because
SHA-1 is thought to be more secure than MD5.
-}
uuidGenerateV5 :: '[null 'PGuuid, null 'PGtext] ---> null 'PGuuid
uuidGenerateV5 = unsafeFunctionN "uuid_generate_v5"

-- | A "nil" UUID constant, which does not occur as a real UUID.
uuidNil :: Expr (null 'PGuuid)
uuidNil = UnsafeExpression "uuid_nil()"

-- | Constant designating the DNS namespace for UUIDs.
uuidNSDns :: Expr (null 'PGuuid)
uuidNSDns = UnsafeExpression "uuid_ns_dns()"

-- | Constant designating the URL namespace for UUIDs.
uuidNSUrl :: Expr (null 'PGuuid)
uuidNSUrl = UnsafeExpression "uuid_ns_url()"

-- | Constant designating the ISO object identifier (OID) namespace for UUIDs.
-- (This pertains to ASN.1 OIDs,
-- which are unrelated to the OIDs used in PostgreSQL.)
uuidNSOid :: Expr (null 'PGuuid)
uuidNSOid = UnsafeExpression "uuid_ns_oid()"

-- | Constant designating the X.500 distinguished
-- name (DN) namespace for UUIDs.
uuidNSX500 :: Expr (null 'PGuuid)
uuidNSX500 = UnsafeExpression "uuid_ns_x500()"

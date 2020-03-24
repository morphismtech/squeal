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

module Queries where

import           Squeal.PostgreSQL
import           GHC.Generics                   ( Generic )
import qualified Generics.SOP                  as SOP
-- Need below for deriving instances
import           Control.DeepSeq
import           Data.Text                      ( Text )
import           Data.Int                       ( Int16
                                                , Int64
                                                )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , PrintableString(..)
                                                , listOf
                                                , arbitraryPrintableChar
                                                )
import           Generic.Random                 ( genericArbitrarySingle )
-- Import Orphan instances
import           Test.QuickCheck.Instances      ( )
-- Project imports
import           Schema

import           Debug.Trace

-- Types

type UserId = Int64
-- Insert user
data InsertUser = InsertUser
  { userEmail     :: Text
  , userPassword  :: Text
  , userFirstName :: Maybe Text
  , userBirthyear :: Maybe Int16
  }
  deriving (Show, Eq, Generic, NFData)
instance SOP.Generic InsertUser
instance SOP.HasDatatypeInfo InsertUser
-- Arbitrary instances for producing values with quickcheck
instance Arbitrary InsertUser where
  arbitrary = genericArbitrarySingle

sampleInsertUser :: InsertUser
sampleInsertUser = InsertUser { userEmail     = "mark@gmail.com"
                              , userPassword  = "MySecretPassword"
                              , userFirstName = Just "Mark"
                              , userBirthyear = Just 1980
                              }

data APIDBUser_ = APIDBUser_
  { userId     :: UserId
  , email      :: Text
  , first_name :: Maybe Text
  , birthyear  :: Maybe Int16
  }
  deriving (Show, Eq, Generic, NFData)
instance SOP.Generic APIDBUser_
instance SOP.HasDatatypeInfo APIDBUser_
-- Arbitrary instances for producing values with quickcheck
instance Arbitrary APIDBUser_ where
  arbitrary = genericArbitrarySingle

data Row3 a b c = Row4
  { col1 :: a
  , col2 :: b
  , col3 :: c
  }
  deriving stock Generic
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- (UserId, Token, OS)
type DeviceDetailsRow = Row3 UserId Text (Enumerated DeviceOS)

-- -- Queries

createUserSession :: InsertUser -> PQ Schemas Schemas IO APIDBUser_
createUserSession insertUser =
  getRow 0 =<< manipulateParams createUser insertUser

createUser :: Manipulation_ Schemas InsertUser APIDBUser_
createUser = insertInto
  #users
  (Values_
    (    Default
    `as` #id
    :*   Set (param @1)
    `as` #email
    :*   Set (param @2)
    `as` #password
    :*   Set (param @3)
    `as` #first_name
    :*   Set (param @4 & cast int2)
    `as` #birthyear
    )
  )
  OnConflictDoRaise
  (Returning_
    (    #id
    `as` #userId
    :*   #email
    `as` #email
    :*   #first_name
    `as` #first_name
    :*   #birthyear
    `as` #birthyear
    )
  )

userDetails :: Query_ Schemas (Only UserId) APIDBUser_
userDetails = select_
  (    #id
  `as` #userId
  :*   #email
  `as` #email
  :*   #first_name
  `as` #first_name
  :*   #birthyear
  `as` #birthyear
  )
  (from (table #users) & where_ (#id .== (param @1 & cast int8)))

insertDeviceDetails :: Manipulation_ Schemas DeviceDetailsRow ()
insertDeviceDetails = insertInto
  #user_devices
  (Values_
    (    Default
    `as` #id
    :*   Set (param @1)
    `as` #user_id
    :*   Set (param @2)
    `as` #token
    :*   Set (parameter @3 (typedef #device_os))
    `as` #os
    )
  )
  OnConflictDoRaise
  (Returning_ Nil)

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
import           GHC.Generics            hiding ( from )
import qualified Generics.SOP                  as SOP
import           Data.Text                      ( Text )
import           Data.Int                       ( Int16
                                                , Int64
                                                )
import           Data.Time                      ( UTCTime(..)
                                                , fromGregorian
                                                , secondsToDiffTime
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


-- Types

type UserId = Int64
-- Insert user
data InsertUser = InsertUser
  { userEmail     :: Text
  , userPassword  :: Text
  , userFirstName :: Maybe Text
  , userBirthyear :: Maybe Int16
  , timeNow       :: UTCTime
  }
  deriving (Show, Generic)
instance SOP.Generic InsertUser
instance SOP.HasDatatypeInfo InsertUser
-- Arbitrary instances for producing values with quickcheck
instance Arbitrary InsertUser where
  arbitrary = genericArbitrarySingle

utcTime :: UTCTime
utcTime = UTCTime (fromGregorian 2019 7 4) (secondsToDiffTime 5800)

sampleInsertUser :: InsertUser
sampleInsertUser = InsertUser { userEmail     = "mark@gmail.com"
                              , userPassword  = "MySecretPassword"
                              , userFirstName = Just "Mark"
                              , userBirthyear = Just 1980
                              , timeNow       = utcTime
                              }

data APIDBUser_ = APIDBUser_
  { userId     :: UserId
  , email      :: Text
  , first_name :: Maybe Text
  , birthyear  :: Maybe Int16
  }
  deriving (Show, Generic)
instance SOP.Generic APIDBUser_
instance SOP.HasDatatypeInfo APIDBUser_
-- Arbitrary instances for producing values with quickcheck
instance Arbitrary APIDBUser_ where
  arbitrary = genericArbitrarySingle

data Row4 a b c d = Row4
  { col1 :: a
  , col2 :: b
  , col3 :: c
  , col4 :: d
  }
  deriving stock Generic
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- (UserId, Token, OS)
type DeviceDetailsRow = Row4 UserId Text (Enumerated DeviceOS) UTCTime

-- Queries

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
    :*   Set (param @5)
    `as` #inserted_at
    :*   Set (param @5)
    `as` #updated_at
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
    :*   Set (param @4)
    `as` #inserted_at
    :*   Set (param @4)
    `as` #updated_at
    )
  )
  OnConflictDoRaise
  (Returning_ Nil)

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell #-}


module Types.Status where

import Data.Aeson
import GHC.Generics
import Data.Data
import Data.SafeCopy
import Data.Text (Text)


newtype ErrorMessage = ErrorMessage Text
                     deriving (Show, Generic, Typeable, Data)
instance ToJSON ErrorMessage
instance FromJSON ErrorMessage
$(deriveSafeCopy 0 'base ''ErrorMessage)

newtype Status a = Status { status :: Either ErrorMessage a }
              deriving (Show, Generic, Typeable, Data)
instance (ToJSON a) => ToJSON (Status a)
instance (FromJSON a) => FromJSON (Status a)
$(deriveSafeCopy 0 'base ''Status)

data Done = Done
          | Failed
          deriving (Show, Generic, Typeable, Data)
instance ToJSON Done
instance FromJSON Done
$(deriveSafeCopy 0 'base ''Done)

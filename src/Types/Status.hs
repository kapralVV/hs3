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

data Status a = Done a
              | Failed ErrorMessage
              deriving (Show, Generic, Typeable, Data)
instance (ToJSON a) => ToJSON (Status a)
instance (FromJSON a) => FromJSON (Status a)
$(deriveSafeCopy 0 'base ''Status)

instance Functor Status where
  fmap f (Done x)   = Done (f x)
  fmap _ (Failed y) = Failed y

maybeToStatus :: Maybe a -> Status a
maybeToStatus Nothing  = Failed $ ErrorMessage "Not Found"
maybeToStatus (Just a) = Done a

statusToBool :: Status a -> Bool
statusToBool (Done _)   = True
statusToBool (Failed _) = False

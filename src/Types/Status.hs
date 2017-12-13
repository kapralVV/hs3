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

instance Applicative Status where
  pure           = Done
  Done f <*> r   = fmap f r
  Failed e <*> _ = Failed e

instance  Monad Status where
  return = pure
  Done r >>= k   = k r
  Failed e >>= _ = Failed e

maybeToStatus :: Maybe a -> Status a
maybeToStatus Nothing  = Failed $ ErrorMessage "Not Found"
maybeToStatus (Just a) = Done a

statusToBool :: Status a -> Bool
statusToBool (Done _)   = True
statusToBool (Failed _) = False

statusToBool' :: Status a -> Status Bool
statusToBool' (Done _)   = Done True
statusToBool' (Failed x) = Failed x

fromStatus :: Status a -> a
fromStatus (Done a)   = a
fromStatus (Failed _) = error "fromStatus: Failed"

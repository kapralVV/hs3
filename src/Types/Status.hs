{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ConstraintKinds            #-}



module Types.Status where

import Data.Aeson
import GHC.Generics
import Data.Data
import Data.SafeCopy
import Data.Text (Text)
import Control.Applicative (Applicative, pure, (<*>))


data ErrorMessage = ErrorMessage Text
                  | NameExists
                  | NotFound
                  | NotAFile
                  | NotALink
                  | NotADirectory
                  | NotAllowed
                  deriving (Show,Eq, Generic, Typeable, Data)
instance ToJSON ErrorMessage
instance FromJSON ErrorMessage
$(deriveSafeCopy 0 'base ''ErrorMessage)

data Status a = Done a
              | Failed ErrorMessage
              deriving (Show, Eq, Generic, Typeable, Data)
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
maybeToStatus Nothing  = Failed NotFound
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

errorMessages :: [Status a] -> [ErrorMessage]
errorMessages xs = [a | Failed a <- xs]

whenDone :: forall t (m :: * -> *) a.
            Monad m =>
            Status t -> (t -> m (Status a)) -> m (Status a)
whenDone x m = case x of
                 Done y   -> m y
                 Failed e -> return $ Failed e

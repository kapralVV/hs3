{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE InstanceSigs               #-}

module Types.Status where

import Data.Aeson
import GHC.Generics
import Data.Data
import Data.Text (Text)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Control.DeepSeq

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

instance NFData ErrorMessage


data Status a = Done a
              | Failed ErrorMessage
              deriving (Show, Eq, Generic, Typeable, Data)
instance (ToJSON a) => ToJSON (Status a)
instance (FromJSON a) => FromJSON (Status a)

instance NFData a => NFData (Status a)

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

statusToMaybe :: Status a -> Maybe a
statusToMaybe (Failed _) = Nothing
statusToMaybe (Done a)   = Just a

statusToBool :: Status a -> Bool
statusToBool (Done _)   = True
statusToBool (Failed _) = False

statusToBool' :: Status Bool -> Bool
statusToBool' (Done True)   = True
statusToBool' _             = False

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

newtype (Monad m) => StatusT m a = StatusT { runStatusT :: m (Status a) }

instance (Functor m, Monad m) => Functor (StatusT m) where
  fmap f = StatusT . fmap (fmap f) . runStatusT

instance (Monad m) => Monad (StatusT m) where
  return = StatusT . return . Done
  x >>= f = StatusT $ do
    statusValue <- runStatusT x
    case statusValue of
      Failed e -> return $ Failed e
      Done value   -> runStatusT $ f value

instance (Functor m, Monad m) => Applicative (StatusT m) where
  pure = StatusT . return . Done
  (<*>)  = ap

instance (Functor m, Monad m) => Alternative (StatusT m) where
  empty   = StatusT . return $ Failed NotFound
  x <|> y = StatusT $ do statusValue <- runStatusT x
                         case statusValue of
                           Failed _ -> runStatusT y
                           Done   _ -> return statusValue

instance (Functor m, Monad m) => MonadPlus (StatusT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans StatusT where
  lift :: Monad m => m a -> StatusT m a
  lift = StatusT . (liftM Done)

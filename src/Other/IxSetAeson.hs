module Other.IxSetAeson where

import Data.Typeable
import Data.Aeson
import qualified Data.IxSet as IX

instance (FromJSON a, Ord a, Typeable a, IX.Indexable a) => FromJSON (IX.IxSet a) where
  parseJSON = fmap IX.fromList . parseJSON

instance (ToJSON a, Ord a) => ToJSON (IX.IxSet a) where
  toJSON = toJSON . IX.toList


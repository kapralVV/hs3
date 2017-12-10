{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings          #-}

module Storage.Generic where

import Data.Typeable
import Control.Applicative
import qualified Data.IxSet                 as IX
import Types.Status

queryBy :: (Ord a, Typeable a, Typeable k, IX.Indexable a) => k -> IX.IxSet a -> Status a
queryBy key = maybeToStatus . IX.getOne . IX.getEQ key

doesNameExist
  :: (Ord a, Typeable k, Typeable a, Typeable k1, IX.Indexable a)
  => k -> Status (IX.IxSet a) -> Status [k1] -> Bool
doesNameExist objectName_ allObjects_ childObjects_ =
  statusToBool (queryBy objectName_ <$> (liftA2 (IX.@*) allObjects_ childObjects_))



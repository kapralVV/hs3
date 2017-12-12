{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings          #-}

module Storage.Generic where

import Data.Typeable
import Control.Applicative
import qualified Data.IxSet                 as IX
import Types.Status

queryBy :: (Ord a, Typeable a, Typeable k, IX.Indexable a) => k -> IX.IxSet a -> Status a
queryBy key = maybeToStatus . IX.getOne . IX.getEQ key

queryIxSetFromList :: (Ord a, Typeable a, Typeable k, IX.Indexable a)
                   => Status (IX.IxSet a)
                   -> Status [k]
                   -> Status (IX.IxSet a)
queryIxSetFromList all' listIds = liftA2 (IX.@+) all' listIds



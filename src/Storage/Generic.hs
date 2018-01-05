{-# LANGUAGE TypeFamilies #-}

module Storage.Generic where

import Data.Typeable
import qualified Data.IxSet                 as IX
import Types.Status

queryBy :: (Ord a, Typeable a, Typeable k, IX.Indexable a) => k -> IX.IxSet a -> Status a
queryBy key = maybeToStatus . IX.getOne . IX.getEQ key

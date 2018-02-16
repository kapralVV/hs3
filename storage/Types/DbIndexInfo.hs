{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Types.DbIndexInfo where

import Data.Data
import Control.Applicative (liftA2)


data DbIndexInfo a = DbIndexInfo { maxIndex :: a
                                 , holes :: [a]
                                 }
                   deriving (Show, Eq, Ord, Data, Typeable)

haveHoles :: DbIndexInfo a -> Bool
haveHoles = not . null . holes

getIndexE :: Enum r => DbIndexInfo r -> Either r r
getIndexE dbIndexInfo | haveHoles dbIndexInfo = Left . head $ holes dbIndexInfo
                      | otherwise = Right . succ $ maxIndex dbIndexInfo

getMaxIndex :: Enum r => DbIndexInfo r -> r
getMaxIndex = either id id . getIndexE

updateIndexInfoE :: Enum r => DbIndexInfo r -> Either r r -> DbIndexInfo r
updateIndexInfoE dbIndexInfo (Left _) =  DbIndexInfo { maxIndex = maxIndex dbIndexInfo
                                                     , holes = drop 1 $ holes dbIndexInfo
                                                     }
updateIndexInfoE dbIndexInfo (Right maxIndex_) = DbIndexInfo { maxIndex = maxIndex_
                                                             , holes = holes dbIndexInfo
                                                             }

updateIndexInfo :: Enum r => DbIndexInfo r -> DbIndexInfo r
updateIndexInfo = liftA2 ($) updateIndexInfoE getIndexE

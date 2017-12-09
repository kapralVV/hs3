{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Types.AcidDB where

import Data.Data
import GHC.Generics
import Data.SafeCopy
import Data.IxSet (IxSet, empty)
import Control.Applicative (liftA2)

import Types.FileSystem

data DbIndexInfo a = DbIndexInfo { maxIndex :: a
                                 , holes :: [a]
                                 }
                   deriving (Show, Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''DbIndexInfo)

data AcidDB = AcidDB { buckets :: (DbIndexInfo BucketId, IxSet Bucket)
                     , objects :: (DbIndexInfo ObjectId, IxSet Object)
                     , files   :: (DbIndexInfo FileId, IxSet FileData)
                     }
            deriving (Show, Generic, Typeable, Data)
$(deriveSafeCopy 0 'base ''AcidDB)




initAcidDB :: AcidDB
initAcidDB = AcidDB { buckets = (DbIndexInfo { maxIndex = BucketId 0
                                             , holes = []
                                             }
                                , empty
                                )
                    , objects = (DbIndexInfo { maxIndex = ObjectId 0
                                             , holes = []
                                             }
                                , empty
                                )
                    , files   = (DbIndexInfo { maxIndex = FileId 0
                                             , holes = []
                                             }
                                , empty
                                )
                    }

--- updating the index.

haveHoles :: DbIndexInfo a -> Bool
haveHoles = not . null . holes

getMaxIndexE :: Enum r => DbIndexInfo r -> Either r r
getMaxIndexE dbIndexInfo | haveHoles dbIndexInfo = Left . head $ holes dbIndexInfo
                         | otherwise = Right . succ $ maxIndex dbIndexInfo

getMaxIndex :: Enum r => DbIndexInfo r -> r
getMaxIndex = either id id . getMaxIndexE

updateIndexInfoE :: Enum r => DbIndexInfo r -> Either r r -> DbIndexInfo r
updateIndexInfoE dbIndexInfo (Left maxIndex_) =  DbIndexInfo { maxIndex = maxIndex_
                                                             , holes = drop 1 $ holes dbIndexInfo
                                                             }
updateIndexInfoE dbIndexInfo (Right maxIndex_) = DbIndexInfo { maxIndex = maxIndex_
                                                             , holes = holes dbIndexInfo
                                                             }

updateIndexInfo :: Enum r => DbIndexInfo r -> DbIndexInfo r
updateIndexInfo = liftA2 ($) updateIndexInfoE getMaxIndexE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Types.AcidDB where

import Data.Data
import GHC.Generics
import Data.SafeCopy
import Data.IxSet (IxSet, empty)


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

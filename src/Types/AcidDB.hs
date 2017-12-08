{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Types.AcidDB where

import Data.Data
import GHC.Generics
import Data.SafeCopy
import Data.IxSet (IxSet, empty)


import Types.FileSystem

data AcidDB = AcidDB { buckets :: ([BucketId], IxSet Bucket)
                     , objects :: ([ObjectId], IxSet Object)
                     , files   :: ([FileId], IxSet FileData)
                     }
            deriving (Show, Generic, Typeable, Data)
$(deriveSafeCopy 0 'base ''AcidDB)


initAcidDB :: AcidDB
initAcidDB = AcidDB { buckets = ([BucketId 0], empty)
                    , objects = ([ObjectId 0], empty)
                    , files   = ([FileId 0], empty)
                    }

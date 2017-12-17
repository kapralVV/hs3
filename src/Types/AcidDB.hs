{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Types.AcidDB where

import Data.Data
import GHC.Generics
import Data.SafeCopy
import Data.IxSet (IxSet, empty)

import Types.FileSystem
import Types.DbIndexInfo

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


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
import Types.Status


$(deriveSafeCopy 0 'base ''BucketId)
$(deriveSafeCopy 0 'base ''ObjectId)
$(deriveSafeCopy 0 'base ''FileId)
$(deriveSafeCopy 0 'base ''ObjectName)
$(deriveSafeCopy 0 'base ''Object)
$(deriveSafeCopy 0 'base ''ObjectType)
$(deriveSafeCopy 0 'base ''FileData)
$(deriveSafeCopy 0 'base ''BucketName)
$(deriveSafeCopy 0 'base ''Bucket)

$(deriveSafeCopy 0 'base ''ErrorMessage)
$(deriveSafeCopy 0 'base ''Status)

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


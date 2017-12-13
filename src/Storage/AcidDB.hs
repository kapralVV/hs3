{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings          #-}

module Storage.AcidDB where

import Data.Acid
import Types.AcidDB

import Storage.Bucket
import Storage.FileData
import Storage.Object

makeAcidic ''AcidDB [ 'createBucket
                    , 'updateBucket
                    , 'queryAllBuckets
                    , 'queryBucketByName
                    , 'queryBucketById
                    , 'queryBChildObjects
                    , 'createFileData
                    , 'deleteFileData
                    , 'queryAllFiles
                    , 'queryFile
                    , 'queryFileData
                    , 'queryFileMd5
                    , 'queryFileOwners
                    , 'queryAllObjects
                    , 'queryObjectById
                    , 'queryObjectByName
                    , 'queryObjectType
                    , 'queryChildObjects
                    , 'findChildObjects
                    , 'createObject
                    ]

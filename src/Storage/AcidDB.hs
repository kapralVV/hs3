{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings          #-}

module Storage.AcidDB where

import Data.Acid
import Types.AcidDB

import Storage.DbIndexInfo
import Storage.Bucket
import Storage.FileData
import Storage.Object

makeAcidic ''AcidDB [ 'queryBucketIndex
                    , 'queryObjectIndex
                    , 'queryFileDataIndex
                    , 'createBucket
                    , 'updateBucket
                    , 'queryAllBuckets
                    , 'queryBucketByName
                    , 'queryBucketById
                    , 'createFileData
                    , 'deleteFileData
                    , 'queryAllFiles
                    , 'queryFile
                    , 'queryFileData
                    , 'queryFileMd5
                    , 'queryParentFObjectId
                    , 'queryParentFObject
                    , 'queryAllObjects
                    , 'queryObjectById
                    , 'queryObjectByName
                    , 'queryObjectType
                    , 'queryObjectName
                    , 'queryChildObjects
                    , 'queryChidFiles
                    , 'createObject
                    ]

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

import Types.FileSystemJson

makeAcidic ''AcidDB [ 'queryBucketIndex
                    , 'queryObjectIndex
                    , 'queryFileDataIndex
                    , 'createBucket
                    , 'updateBucket
                    , 'deleteBucketGeneric
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
                    , 'createFileObject
                    , 'createDirectoryObject
                    , 'createLinkObject
                    , 'addFileDataToFile
                    , 'deleteObjectGeneric
                    , 'queryAllBucketsForJson
                    ]

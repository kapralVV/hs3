{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings          #-}

module Storage.AcidDB where

import Data.Acid
import Types.AcidDB

import Storage.Bucket
import Storage.FileData

makeAcidic ''AcidDB [ 'createBucket
                    , 'queryAllBuckets
                    , 'queryBucketByName
                    , 'queryBucketById
                    , 'createFileData
                    , 'deleteFileData
                    , 'queryAllFiles
                    , 'queryFile
                    , 'queryFileData
                    , 'queryFileMd5
                    , 'queryFileOwners
                    ]

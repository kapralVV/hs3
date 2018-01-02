{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Types.FileSystemJson where

import Data.Typeable
import Data.Data
import Data.SafeCopy
import Data.Acid
import GHC.Generics
import qualified Data.IxSet               as IX
import qualified Data.Set                 as DS
import Control.Monad.Reader

import qualified Data.Aeson as A

import Types.FileSystem
import Types.AcidDB
import Types.Status
import Storage.Bucket
import Storage.FileData
import Storage.Object

data BucketJson = BucketJson { bucket       :: Bucket
                             , childObjects :: DS.Set ObjectJson
                             } deriving (Show, Eq, Ord,  Data, Typeable, Generic)
instance A.ToJSON BucketJson
instance A.FromJSON BucketJson



data ObjectJson = ObjectJson { object   :: Object
                             , typeJson :: TypeJson
                             } deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON ObjectJson
instance A.FromJSON ObjectJson

data TypeJson = FileJson      (DS.Set FileId)
              | DirectoryJson (DS.Set ObjectJson)
              | LinkJson      { link :: ObjectJson }
              deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON TypeJson
instance A.FromJSON TypeJson

$(deriveSafeCopy 0 'base ''TypeJson)
$(deriveSafeCopy 0 'base ''BucketJson)
$(deriveSafeCopy 0 'base ''ObjectJson)

objectToJson :: AcidDB -> Object -> ObjectJson
objectToJson db Object{..} =
  ObjectJson
  { object = fromStatus $ queryObjectById' objectId db
  , typeJson =
      case objectType of
        File      -> FileJson . DS.map fileId $ IX.toSet (queryAllFiles' db IX.@= objectId)
        Directory -> DirectoryJson . DS.map (objectToJson db) $ IX.toSet (queryAllObjects' db IX.@= (Just objectId))
        Link oId  -> LinkJson . objectToJson db . fromStatus $ queryObjectById' oId db
  }

bucketToJson :: AcidDB -> Bucket -> BucketJson
bucketToJson db Bucket{..} =
  BucketJson { bucket = fromStatus $ queryBucketById' bucketId db
             , childObjects = DS.map (objectToJson db) $ IX.toSet (queryAllObjects' db IX.@= bucketId)
             }

allBucketsJson :: AcidDB -> DS.Set BucketJson
allBucketsJson db = DS.map (bucketToJson db) . IX.toSet . fromStatus $ queryAllBuckets' db

queryAllBucketsForJson :: Query AcidDB (DS.Set BucketJson)
queryAllBucketsForJson = allBucketsJson `fmap` ask

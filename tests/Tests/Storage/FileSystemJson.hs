{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Tests.Storage.FileSystemJson where

import Data.Typeable
import Data.Data
import Data.Acid
import Data.Acid.Advanced
import GHC.Generics
import qualified Data.IxSet               as IX
import qualified Data.Set                 as DS
import Control.Monad
import Control.Applicative
import qualified Data.Aeson as A

import Types.FileSystem
import Types.AcidDB
import Types.Status
import Storage.AcidDB

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

objectToJson :: AcidState AcidDB -> Object -> StatusT IO ObjectJson
objectToJson db object@Object{..} =
  ObjectJson
  <$> pure object
  <*>  case objectType of
         File      -> fmap (FileJson . DS.fromList) . StatusT . query' db $ QueryAllFileIdsOfObject objectId
         Directory -> join . fmap (fmap (DirectoryJson . DS.fromList) . mapM (objectToJson db) . IX.toList)
                      . StatusT . query' db $ QueryChildObjects parentBucketId (Just objectId)
         Link oId  -> join . fmap (fmap LinkJson . (objectToJson db))
                      . StatusT . query' db $ QueryObjectById oId

bucketToJson :: AcidState AcidDB -> Bucket -> StatusT IO BucketJson
bucketToJson db bucket@Bucket{..} = do
  allObjects <- StatusT $ query' db QueryAllObjects
  bucketObjects <- fmap DS.fromList . mapM (objectToJson db) . IX.toList $ IX.getEQ bucketId allObjects
  return $ BucketJson bucket bucketObjects

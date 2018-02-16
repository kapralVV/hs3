{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Storage.MainStorage where

import Data.Acid
import Data.Acid.Advanced

import Types.FileSystem
import Types.Status
import Types.AcidDB
import Storage.Object
import qualified Data.IxSet  as IX
import Storage.AcidDB

deleteObject :: AcidState AcidDB -> ObjectId -> IO (Status ())
deleteObject db oId = do
  sOId <- query' db $ QueryObjectById oId
  case sOId of
    Failed e   -> return $ Failed e
    Done object
      | isFile'' object -> do
          childrenFiles <- query' db (QueryChidFiles oId)
          whenDone childrenFiles $ \childrenFiles' -> do
            let childrenFileIds = map fileId $ IX.toList childrenFiles'
            groupUpdates db $ map DeleteFileData childrenFileIds
            update' db $ DeleteObjectGeneric oId
      | isLink'' object -> update' db $ DeleteObjectGeneric oId
      | isDirectory'' object -> do
          childrenObjects <- query' db (QueryChildObjects (parentBucketId object) (Just oId))
          whenDone childrenObjects $ \childrenObjects' -> do
            let childrenObjectIds = map objectId $ IX.toList childrenObjects'
            mapM_ (deleteObject db) childrenObjectIds
            update' db $ DeleteObjectGeneric oId
      | otherwise -> return . Failed $ ErrorMessage "This case is not possible until you add new ObjectType"

deleteBucket :: AcidState AcidDB -> BucketId -> IO (Status ())
deleteBucket db bId = do
  createCheckpoint db
  xs <- query' db $ QueryEverythingIdsInBucket bId
  whenDone xs $ \((objectIds, fileIds)) -> do
    groupUpdates db $ map DeleteFileData fileIds
    groupUpdates db $ map DeleteObjectGeneric objectIds
    update' db $ DeleteBucketGeneric bId

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
import Control.Monad

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
            void $ mapM_ (deleteObject db) (strictList childrenObjectIds)
            update' db $ DeleteObjectGeneric oId
      | otherwise -> return . Failed $ ErrorMessage "This case is not possible until you add new ObjectType"

strictList :: forall t. [t] -> [t]
strictList xs = if all p xs then xs else []
  where p x = x `seq` True

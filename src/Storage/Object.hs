{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}

module Storage.Object where

import Data.Acid
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.IxSet                 as IX
import qualified Data.Set                   as DS
import qualified Data.ByteString.Lazy       as DBL
import Control.Applicative

import Types.FileSystem
import Types.AcidDB
import Types.Status
import Storage.Generic
import Storage.FileData
import Storage.Bucket

queryAllObjects' :: AcidDB -> Status (IX.IxSet Object)
queryAllObjects' = Done . snd . objects

queryAllObjects :: Query AcidDB (Status (IX.IxSet Object))
queryAllObjects = queryAllObjects' `fmap` ask

queryObjectBy' :: Typeable k =>  k -> AcidDB -> Status Object
queryObjectBy' key = queryBy key . snd . objects

queryObjectBy :: (Typeable k, MonadReader AcidDB f) => k -> f (Status Object)
queryObjectBy key = queryObjectBy' key `fmap` ask

queryObjectById' :: ObjectId -> AcidDB -> Status Object
queryObjectById' = queryObjectBy'

queryObjectById :: ObjectId -> Query AcidDB (Status Object)
queryObjectById = queryObjectBy

queryObjectByName' :: ObjectName -> AcidDB -> Status Object
queryObjectByName' = queryObjectBy'

queryObjectByName :: ObjectName -> Query AcidDB (Status Object)
queryObjectByName = queryObjectBy

queryObjectType' :: ObjectId -> AcidDB -> Status ObjectType
queryObjectType' key = fmap objectType . queryObjectById' key

queryObjectType :: ObjectId -> Query AcidDB (Status ObjectType)
queryObjectType key = queryObjectType' key `fmap` ask

queryObjectName' :: ObjectId -> AcidDB -> Status ObjectName
queryObjectName' key = fmap objectName . queryObjectById' key

queryObjectName :: ObjectId -> Query AcidDB (Status ObjectName)
queryObjectName key = queryObjectName' key `fmap` ask

queryChildObjectIds' :: ObjectId -> AcidDB -> Status (DS.Set ObjectId)
queryChildObjectIds' key db =
  case queryObjectType' key db of
    Failed _                       -> Failed $ ErrorMessage "Object is not found"
    Done (Directory childObjects') -> Done childObjects'
    _                              -> Failed $ ErrorMessage "Is not a directory"

queryChildObjectIds :: ObjectId -> Query AcidDB (Status (DS.Set ObjectId))
queryChildObjectIds key = queryChildObjectIds' key `fmap` ask

queryChildObjects' :: ObjectId -> AcidDB -> Status (IX.IxSet Object)
queryChildObjects' oid = liftA2 queryIxSetByKeys queryAllObjects' (queryChildObjectIds' oid)

queryChildObjects ::  ObjectId -> Query AcidDB (Status (IX.IxSet Object))
queryChildObjects oid = queryChildObjects' oid `fmap` ask

queryBChildObjects' :: BucketId -> AcidDB -> Status (IX.IxSet Object)
queryBChildObjects' bid = liftA2 queryIxSetByKeys queryAllObjects' (queryBChildObjectIds' bid)

queryBChildObjects :: BucketId -> Query AcidDB (Status (IX.IxSet Object))
queryBChildObjects bid = queryBChildObjects' bid `fmap` ask


addObjectChilds' :: ObjectId -> Object -> Object
addObjectChilds' oid object@Object{..} =
  object { objectType = objectType
           { childObjects =  DS.insert oid (childObjects objectType)  }
         }
         

createObject :: ObjectName
                 -> BucketId
                 -> (Maybe ObjectId)
                 -> ObjectType
                 -> Update AcidDB (Status ObjectId)
createObject objectName_ parentBucketId_ parentObjectId_ objectType_ = do
  acidDb <- get
  
  let dbIndexInfo = fst $ objects acidDb
  let maxIndex_   = getMaxIndex dbIndexInfo
  let newObject = Object { objectId       = maxIndex_
                         , objectName     = objectName_
                         , parentBucketId = parentBucketId_
                         , parentObjectId = parentObjectId_
                         , objectType     = objectType_
                         }

  case parentObjectId_ of
    Nothing -> do
      let childObjects = queryBChildObjects' parentBucketId_ acidDb
      case childObjects of
        Failed e -> return $ Failed e
        Done childObjects_ -> do
          if statusToBool $ queryBy objectName_ childObjects_
            then return . Failed $ ErrorMessage "Object-name exists"
            else do
            let updatedBucket = fromStatus . fmap (addBucketChilds' maxIndex_) $ queryBucketById' parentBucketId_ acidDb
            let updatedAcidDB =
                  acidDb { objects = (\(_, objectSet) ->
                                        (updateIndexInfo dbIndexInfo
                                        , IX.insert newObject objectSet
                                        )
                                     )
                                     $ objects acidDb
                         , buckets = (\(i , bucketSet) ->
                                        ( i
                                        , IX.updateIx parentBucketId_ updatedBucket bucketSet
                                        )
                                     )
                                     $ buckets acidDb
                         }
            put updatedAcidDB
            return $ Done maxIndex_
    Just parentObjectId' -> do
      let childObjects = queryChildObjects' parentObjectId' acidDb
      case childObjects of
        Failed e -> return $ Failed e
        Done childObjects_ -> do
          if statusToBool $ queryBy objectName_ childObjects_
            then return . Failed $ ErrorMessage "Object-name exists"
            else do
            let updatedParentObject = fromStatus . fmap (addObjectChilds' maxIndex_) $ queryObjectById' parentObjectId' acidDb
            let updatedAcidDB =
                  acidDb { objects = (\(_, objectSet) ->
                                        (updateIndexInfo dbIndexInfo
                                        , IX.insert newObject objectSet
                                        )
                                     )
                                     $ objects acidDb
                         }

            let updatedAcidDB' =
                  acidDb { objects = (\(i, objectSet) ->
                                        (i
                                        , IX.updateIx parentObjectId' updatedParentObject objectSet
                                        )
                                     )
                                     $ objects updatedAcidDB
                         }
            put updatedAcidDB'
            return $ Done maxIndex_


  

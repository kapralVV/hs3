{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

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

queryChildObjects' :: ObjectId -> AcidDB -> Status (DS.Set ObjectId)
queryChildObjects' key db =
  case queryObjectType' key db of
    Failed _                       -> Failed $ ErrorMessage "Object is not found"
    Done (Directory childObjects') -> Done childObjects'
    _                              -> Failed $ ErrorMessage "Is not a directory"

queryChildObjects :: ObjectId -> Query AcidDB (Status (DS.Set ObjectId))
queryChildObjects key = queryChildObjects' key `fmap` ask

findChildObjects :: BucketId
                 -> (Maybe ObjectId)
                 -> Query AcidDB (Status (IX.IxSet Object))
findChildObjects parentBucketId_ parentObjectId_ = do
  acidDb <- ask
  case parentObjectId_ of
    Just parentObjectId' -> do
      let childObjects_ = fmap DS.toList $ queryChildObjects' parentObjectId' acidDb
      let allObjects_ = queryAllObjects' acidDb
      return $ queryIxSetFromList allObjects_ childObjects_
    Nothing              -> do
      let childObjects_ = fmap DS.toList $ queryBChildObjects' parentBucketId_ acidDb
      let allObjects_ = queryAllObjects' acidDb
      return $ queryIxSetFromList allObjects_ childObjects_

createObject :: ObjectName
                 -> BucketId
                 -> (Maybe ObjectId)
                 -> ObjectType
                 -> Update AcidDB (Status ObjectId)
createObject objectName_ parentBucketId_ parentObjectId_ objectType_ = do
  childObjects' <- liftQuery $ findChildObjects parentBucketId_ parentObjectId_
  acidDb <- get
  case childObjects' of
    Failed e -> return $ Failed e
    Done childObjects'' -> do
      if statusToBool $ queryBy objectName_ childObjects''
        then return . Failed $ ErrorMessage "Object-name exists"
        else do
        let dbIndexInfo = fst $ objects acidDb
        let maxIndex_   = getMaxIndex dbIndexInfo
        let newObject = Object { objectId       = maxIndex_
                               , objectName     = objectName_
                               , parentBucketId = parentBucketId_
                               , parentObjectId = parentObjectId_
                               , objectType     = objectType_
                               }
-- TODO Write childObjects update for parent Object
        let updatedAcidDB =
              acidDb { objects = (\(_, objectSet) ->
                                    (updateIndexInfo dbIndexInfo
                                    , IX.insert newObject objectSet
                                    )
                                 )
                                 $ objects acidDb
                     }
        put updatedAcidDB
        return $ Done maxIndex_

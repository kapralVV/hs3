{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Storage.Object where

import Data.Acid
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.IxSet                 as IX
import qualified Data.ByteString.Lazy       as DBL
import Control.Applicative
import Control.Monad (mapM)
import Data.Time.Clock

import Types.FileSystem
import Types.DbIndexInfo
import Types.AcidDB
import Types.Status
import Storage.Generic
import Storage.FileData

queryAllObjects' :: AcidDB -> IX.IxSet Object
queryAllObjects' = snd . objects

queryAllObjects :: Query AcidDB (Status (IX.IxSet Object))
queryAllObjects = (Done . queryAllObjects') `fmap` ask

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

queryChildObjects'' :: BucketId -> Maybe ObjectId -> AcidDB -> IX.IxSet Object
queryChildObjects'' bid pob acidDb = queryAllObjects' acidDb IX.@= bid IX.@= pob

queryChildObjects' :: BucketId -> Maybe ObjectId -> AcidDB -> Status (IX.IxSet Object)
queryChildObjects' bid pob@(Just x) acidDb =
  case isDirectory' x acidDb of
    Done True    -> Done $  queryChildObjects'' bid pob acidDb
    Done False   -> Failed NotADirectory
    Failed e     -> Failed e
queryChildObjects' bid Nothing acidDb = Done $ queryChildObjects'' bid Nothing acidDb

queryChildObjects :: BucketId -> Maybe ObjectId -> Query AcidDB (Status (IX.IxSet Object))
queryChildObjects bid pod = queryChildObjects' bid pod `fmap` ask

queryParentFObject' :: FileId -> AcidDB -> Status Object
queryParentFObject' key acidDb = queryParentFObjectId' key acidDb >>= flip queryObjectById' acidDb

queryParentFObject :: FileId -> Query AcidDB (Status Object)
queryParentFObject key = queryParentFObject' key `fmap` ask

queryChidFiles' :: ObjectId -> AcidDB -> Status (IX.IxSet FileData)
queryChidFiles' key acidDb = case queryObjectType' key acidDb of
                               (Done File) -> Done . IX.getEQ key $ queryAllFiles' acidDb
                               (Failed e)  -> Failed e
                               _           -> Failed NotAFile

queryChidFiles :: ObjectId -> Query AcidDB (Status (IX.IxSet FileData))
queryChidFiles key = queryChidFiles' key `fmap` ask

isFile', isDirectory', isLink' :: ObjectId -> AcidDB -> Status Bool
isFile'      key   = fmap (== File)      . queryObjectType' key
isDirectory' key   = fmap (== Directory) . queryObjectType' key
isLink' key acidDb = case queryObjectType' key acidDb of
                       (Done (Link _)) -> Done True
                       (Failed e)      -> Failed e
                       _               -> Failed NotALink

isFile, isDirectory, isLink :: ObjectId -> Query AcidDB (Status Bool)
isFile key      = isFile' key `fmap` ask
isDirectory key = isDirectory' key `fmap` ask
isLink key      = isLink' key `fmap` ask


createObject :: ObjectName
                 -> BucketId
                 -> Maybe ObjectId
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

  let childObjects = queryChildObjects' parentBucketId_ parentObjectId_ acidDb
  case childObjects of
    Failed e -> return $ Failed e
    Done childObjects_ ->
      if statusToBool $ queryBy objectName_ childObjects_
        then return $ Failed NameExists
        else do
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

createFileObject, createDirectoryObject
  :: ObjectName
  -> BucketId
  -> Maybe ObjectId
  -> Update AcidDB (Status ObjectId)
createFileObject name bId pId = createObject name bId pId File

addFileDataToFile :: ObjectId
                  -> UTCTime
                  -> DBL.ByteString
                  -> Update AcidDB (Status FileId)
addFileDataToFile oId time fData = do
  statusIsFile <- liftQuery $ isFile oId
  case statusIsFile of
    Done True  -> createFileData oId time fData
    Done False -> return $ Failed NotAFile
    Failed e   -> return $ Failed e

createDirectoryObject name bId pId = createObject name bId pId Directory

createLinkObject
  :: ObjectName
  -> BucketId
  -> Maybe ObjectId
  -> ObjectId      -- linked object
  -> Update AcidDB (Status ObjectId)
createLinkObject name bId pId oId = createObject name bId pId (Link oId)

-- Generic function does not check whether object has children
-- and do not remove them recursively
deleteObjectGeneric :: ObjectId -> Update AcidDB (Status ())
deleteObjectGeneric oId = do
  acidDb <- get
  if statusToBool $ queryObjectById' oId acidDb then do
    let updatedAcidDB =
          acidDb { objects = (\(dbIndexInfo, objectsSet) ->
                                 ( DbIndexInfo { maxIndex = maxIndex dbIndexInfo
                                               , holes = oId : holes dbIndexInfo
                                               }
                                 , IX.deleteIx oId objectsSet
                                 )
                             )
                             $ objects acidDb
                 }
    put updatedAcidDB

    return $ Done ()
    else
    return $ Failed NotFound

-- TODO : Move to higher level 
{-
deleteObject :: ObjectId -> Update AcidDB (Status ())
deleteObject oId = do
  acidDb <- get
  let statusObject = queryObjectById' oId acidDb
  case statusObject of
    Failed e   -> return $ Failed e
    Done object@Object{..} | statusToBool (isFile' objectId acidDb) -> do
                               deleteFileDataErrors <- (queryChidFiles' objectId acidDb
                                                         >>= mapM deleteFileData . map fileId . IX.toList)
                               if not $ null deleteFileDataErrors then
                                 return $ Failed "Cannot remove all FileData set to File-Object"
                                 else deleteObjectGeneric objectId
-}

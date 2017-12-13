{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards  #-}

module Storage.Bucket where

import Data.Acid
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.IxSet                 as IX
import qualified Data.Set                   as DS

import Types.FileSystem
import Types.AcidDB
import Types.Status
import Storage.Generic

createBucket :: BucketName -> Update AcidDB (Status BucketId)
createBucket bucketName_ = do
  acidDb <- get
  if statusToBool $ queryBucketByName' bucketName_ acidDb then
    return . Failed $ ErrorMessage "Bucket exists"
    else do
    let dbIndexInfo = fst $ buckets acidDb
    let maxIndex_ = getMaxIndex dbIndexInfo
    let newBucket = Bucket { bucketId = maxIndex_
                           , bucketName = bucketName_
                           , childBObjects = DS.empty
                           }
    let updatedAcidDB =
          acidDb { buckets = (\(_ , bucketSet) ->
                                 ( updateIndexInfo dbIndexInfo
                                 , IX.insert newBucket bucketSet
                                 )
                             )
                             $ buckets acidDb
                 }
    put updatedAcidDB
    return $ Done maxIndex_

updateBucket :: Bucket -> Update AcidDB (Status ())
updateBucket bucket@Bucket{..} = do
  acidDb <- get
  if statusToBool $ queryBucketById' bucketId acidDb
    then do
    let updatedAcidDB =
          acidDb { buckets = (\(i , bucketSet) ->
                                 ( i
                                 , IX.updateIx bucketId bucket bucketSet
                                 )
                             )
                             $ buckets acidDb
                 }
    put updatedAcidDB
    return $ Done ()
    else return . Failed $ ErrorMessage "Bucket does not exist"

addBucketChilds' :: ObjectId -> Bucket -> Bucket
addBucketChilds' oid bucket@Bucket{..} =
  bucket { childBObjects = DS.insert oid childBObjects}

queryAllBuckets' :: AcidDB -> Status (IX.IxSet Bucket)
queryAllBuckets' = Done . snd . buckets

queryAllBuckets :: Query AcidDB (Status (IX.IxSet Bucket))
queryAllBuckets = queryAllBuckets' `fmap` ask

queryBucketBy' :: Typeable k => k -> AcidDB -> Status Bucket
queryBucketBy' key = queryBy key . snd . buckets

queryBucketBy :: (Typeable k, MonadReader AcidDB f) => k -> f (Status Bucket)
queryBucketBy key = queryBucketBy' key `fmap` ask

queryBucketByName' :: BucketName -> AcidDB -> Status Bucket
queryBucketByName' = queryBucketBy'

queryBucketByName :: BucketName -> Query AcidDB (Status Bucket)
queryBucketByName = queryBucketBy

queryBucketById' :: BucketId -> AcidDB -> Status Bucket
queryBucketById' = queryBucketBy'

queryBucketById :: BucketId -> Query AcidDB (Status Bucket)
queryBucketById = queryBucketBy

queryBChildObjectIds' :: BucketId -> AcidDB -> Status (DS.Set ObjectId)
queryBChildObjectIds' key = fmap childBObjects . queryBucketById' key

queryBChildObjectIds :: BucketId -> Query AcidDB (Status (DS.Set ObjectId))
queryBChildObjectIds key = queryBChildObjectIds' key `fmap` ask

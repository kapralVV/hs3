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

import Types.FileSystem
import Types.DbIndexInfo
import Types.AcidDB
import Types.Status
import Storage.Generic

createBucket :: BucketName -> Update AcidDB (Status BucketId)
createBucket bucketName_ = do
  acidDb <- get
  if statusToBool $ queryBucketByName' bucketName_ acidDb then
    return $ Failed NameExists
    else do
    let dbIndexInfo = fst $ buckets acidDb
    let maxIndex_ = getMaxIndex dbIndexInfo
    let newBucket = Bucket { bucketId = maxIndex_
                           , bucketName = bucketName_
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
    else return $ Failed NotFound

-- it does not check if child objects exist
deleteBucketGeneric :: BucketId -> Update AcidDB (Status ())
deleteBucketGeneric bId = do
  acidDb <- get
  if statusToBool $ queryBucketById' bId acidDb then do
    let updatedAcidDB =
          acidDb { buckets = (\(dbIndexInfo, bucketSet) ->
                               ( DbIndexInfo { maxIndex = maxIndex dbIndexInfo
                                             , holes = bId : holes dbIndexInfo
                                             }
                               , IX.deleteIx bId bucketSet
                               )
                           )
                           $ buckets acidDb
                 }
    put updatedAcidDB
    return $ Done ()

    else
    return $ Failed NotFound

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

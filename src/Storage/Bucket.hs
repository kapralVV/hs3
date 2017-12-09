{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

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
  if (statusToBool . queryBy bucketName_ . snd . buckets $ acidDb) then do
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
    put $ updatedAcidDB
    return $ Done maxIndex_

queryAllBuckets :: Query AcidDB (Status (IX.IxSet Bucket))
queryAllBuckets = fmap (Done . snd . buckets) ask

queryBucketBy :: (Typeable k, MonadReader AcidDB f) => k -> f (Status Bucket)
queryBucketBy key = (queryBy key . snd . buckets) `fmap` ask

queryBucketByName :: BucketName -> Query AcidDB (Status Bucket)
queryBucketByName key = queryBucketBy key

queryBucketById :: BucketId -> Query AcidDB (Status Bucket)
queryBucketById key = queryBucketBy key

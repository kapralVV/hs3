{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Client.Bucket where


import Types.FileSystem
import API.Bucket
import Data.Proxy
import Servant.API
import Servant.Client

getBuckets :: ClientM [Bucket]
getBucketbyId :: BucketId -> ClientM Bucket
getBucketbyName :: BucketName -> ClientM Bucket
getBucketChildrens :: BucketId -> ClientM [Object]
createBucket :: BucketName -> ClientM BucketId
updateBucket :: Bucket -> ClientM ()
deleteBucket :: BucketId -> ClientM ()

bucketApi :: Proxy BucketAPI
bucketApi = Proxy

getBuckets
  :<|> getBucketbyId
  :<|> getBucketbyName
  :<|> getBucketChildrens
  :<|> createBucket
  :<|> updateBucket
  :<|> deleteBucket = client bucketApi

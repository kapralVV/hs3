{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Bucket where

import API.Bucket
import Types.Status
import Types.AcidDB
import Storage.AcidDB
import Storage.MainStorage
import Server.StatusToHandler

import qualified Data.IxSet                 as IX
import Data.Acid
import Data.Acid.Advanced
import Servant.Server
import Servant.API
import Control.Monad.Trans


serverBucket :: AcidState AcidDB -> Server BucketAPI
serverBucket db =
  ( (runStatusT . fmap IX.toList . StatusT $ query' db QueryAllBuckets) >>= statusToHandler )
  :<|> ( \bId -> (query' db $ QueryBucketById bId) >>= statusToHandler)
  :<|> ( \bName -> (query' db $ QueryBucketByName bName) >>= statusToHandler )
  :<|> ( \bId -> (runStatusT . fmap IX.toList . StatusT .  query' db $ QueryChildObjects bId Nothing) >>= statusToHandler )
  :<|> ( \bName -> (update' db $ CreateBucket bName) >>= statusToHandler )
  :<|> ( \bucket -> (update' db $ UpdateBucket bucket) >>= statusToHandler )
  :<|> ( \bId -> (liftIO $ deleteBucket db bId) >>= statusToHandler )

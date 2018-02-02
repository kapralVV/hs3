{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servers.Bucket where

import API.Bucket
import Types.Status
import Types.AcidDB
import Storage.AcidDB
import Storage.MainStorage

import qualified Data.IxSet                 as IX
import Data.Acid
import Data.Acid.Advanced
import Servant.Server
import Servant.API
import Control.Monad.IO.Class (liftIO)


serverBucket :: AcidState AcidDB -> Server BucketAPI
serverBucket db = ( runStatusT . fmap IX.toList . StatusT $ query' db QueryAllBuckets )
                  :<|> ( \bId ->  query' db $ QueryBucketById bId)
                  :<|> ( \bName -> query' db $ QueryBucketByName bName)
                  :<|> ( \bName -> update' db $ CreateBucket bName)
                  :<|> ( \bucket -> update' db $ UpdateBucket bucket)
                  :<|> ( \bId -> liftIO $ deleteBucket db bId)

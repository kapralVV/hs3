{-# LANGUAGE OverloadedStrings #-}

module Servers.FileData where

import API.FileData
import Types.Status
import Types.AcidDB
import Types.FileSystem
import Storage.AcidDB
import Servers.StatusToHandler

import Data.Acid
import Data.Acid.Advanced
import Servant.Server
import Servant.API

serverFileData :: AcidState AcidDB -> Server FileAPI
serverFileData db =
  ( \fId -> (runStatusT . fmap fileDataToJson . StatusT . query' db $ QueryFile fId ) >>= statusToHandler )
  :<|> ( \fId -> (query' db $ QueryFileData fId ) >>= statusToHandler )
  :<|> ( \fId -> (update' db $ DeleteFileData fId) >>= statusToHandler )


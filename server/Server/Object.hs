{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Object where

import API.Object
import Types.Status
import Types.FileSystem
import Types.AcidDB
import Storage.AcidDB
import Storage.MainStorage
import Server.StatusToHandler

import qualified Data.IxSet                 as IX
import Data.Acid
import Data.Acid.Advanced
import Servant.Server
import Servant.API
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)


serverObject :: AcidState AcidDB -> Server ObjectAPI
serverObject db =
  ( \oId ->  (query' db $ QueryObjectById oId) >>= statusToHandler )
  :<|> ( \oId -> do
           oType <- query' db $ QueryObjectType oId
           case oType of
             Done Directory -> (runStatusT . fmap (ObjectDirChild . IX.toList) . StatusT .  query' db $ QueryChildObjectsByOid oId) >>= statusToHandler
             Done File      -> (runStatusT . fmap (ObjectFileChild . map fileDataToJson . IX.toList) . StatusT .  query' db $ QueryChidFiles oId) >>= statusToHandler
             Done (Link nOid) -> (runStatusT . fmap ObjectLinkChild . StatusT . query' db $ QueryObjectById nOid) >>= statusToHandler
             Failed e        -> (return $ Failed e) >>= statusToHandler
       )
  :<|> ( \CreateObjectInfo{..} -> case c_objectType of
           File -> (update' db $ CreateFileObject c_objectName c_bucketId c_parentObjectId) >>= statusToHandler
           Directory -> (update' db $ CreateDirectoryObject c_objectName c_bucketId c_parentObjectId) >>= statusToHandler
           Link nId -> (update' db $ CreateLinkObject c_objectName c_bucketId c_parentObjectId nId) >>= statusToHandler
       )
  :<|> ( \oId -> \bytestring ->
             liftIO getCurrentTime >>= ( \time -> 
             update' db $ AddFileDataToFile oId time bytestring) >>= statusToHandler
       )
  :<|> ( \oId -> (liftIO $ deleteObject db oId) >>= statusToHandler )

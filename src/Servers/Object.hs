{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Servers.Object where

import API.Object
import Types.Status
import Types.FileSystem
import Types.AcidDB
import Storage.AcidDB
import Storage.MainStorage

import qualified Data.IxSet                 as IX
import Data.Acid
import Data.Acid.Advanced
import Servant.Server
import Servant.API
import Control.Monad.IO.Class (liftIO)


serverObject :: AcidState AcidDB -> Server ObjectAPI
serverObject db =
  ( \oId ->  query' db $ QueryObjectById oId )
  :<|> ( \bId -> runStatusT . fmap IX.toList . StatusT .  query' db $ QueryChildObjects bId Nothing)
  :<|> ( \oId -> do
           oType <- query' db $ QueryObjectType oId
           case oType of
             Done Directory -> runStatusT . fmap (ObjectDirChild . IX.toList) . StatusT .  query' db $ QueryChildObjectsByOid oId
             Done File      -> runStatusT . fmap (ObjectFileChild . map fileDataToJson . IX.toList) . StatusT .  query' db $ QueryChidFiles oId
             Done (Link nOid) -> runStatusT . fmap ObjectLinkChild . StatusT . query' db $ QueryObjectById nOid
             Failed e        -> return $ Failed e
       )
  :<|> ( \CreateObjectInfo{..} -> update' db $ CreateObject c_objectName c_bucketId c_parentObjectId c_objectType )
  :<|> ( \oId -> liftIO $ deleteObject db oId)

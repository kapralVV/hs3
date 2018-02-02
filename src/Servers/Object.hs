{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servers.Object where

import API.Object
import Types.Status
import Types.FileSystem (fileDataToJson)
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
serverObject db = ( \oId ->  query' db $ QueryObjectById oId)
                  :<|> ( \oId -> runStatusT . fmap IX.toList . StatusT . query' db $ QueryChildObjectsByOid oId)
                  :<|> ( \bId -> runStatusT . fmap IX.toList . StatusT .  query' db $ QueryChildObjects bId Nothing)
                  :<|> ( \oId -> runStatusT . fmap (map fileDataToJson . IX.toList) . StatusT . query' db $ QueryChidFiles oId)
                  -- :<|> ( \oName -> update' db $ CreateObject oName )
                  --  :<|> ( \object -> update' db $ UpdateObject object)
                  :<|> ( \oId -> liftIO $ deleteObject db oId)

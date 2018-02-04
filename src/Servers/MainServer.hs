{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servers.MainServer where

import API.MainAPI
import Servers.Bucket
import Servers.Object
import Servers.FileData
import Types.AcidDB

import Data.Acid
import Servant.Server
import Servant.API
import Data.Proxy

serverMain :: AcidState AcidDB -> Server MainAPI
serverMain db = serverBucket db
                :<|> serverObject db
                :<|> serverFileData db


api :: Proxy MainAPI
api = Proxy

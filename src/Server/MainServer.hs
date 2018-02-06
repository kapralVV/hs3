{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.MainServer where

import API.MainAPI
import Server.Bucket
import Server.Object
import Server.FileData
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

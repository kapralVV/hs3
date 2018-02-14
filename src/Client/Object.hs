{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Client.Object where


import Types.FileSystem
import API.Object

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Servant.API
import Servant.Client

getObjectById     :: ObjectId -> ClientM Object
getObjectChildren :: ObjectId -> ClientM ObjectChildren
createObject      :: CreateObjectInfo -> ClientM ObjectId
putData           :: ObjectId -> ByteString -> ClientM FileId
deleteObject      :: ObjectId -> ClientM ()

objectApi :: Proxy ObjectAPI
objectApi = Proxy


getObjectById
  :<|> getObjectChildren
  :<|> createObject
  :<|> putData
  :<|> deleteObject = client objectApi

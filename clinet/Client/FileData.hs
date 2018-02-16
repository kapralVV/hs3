{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Client.FileData where

import Types.FileSystem
import API.FileData

import Data.Proxy
import Data.ByteString.Lazy (ByteString)
import Servant.API
import Servant.Client

getFileDataInfo :: FileId -> ClientM FileDataJson
fetchData       :: FileId -> ClientM ByteString
deleteFileData  :: FileId -> ClientM ()

fileAPI :: Proxy FileAPI
fileAPI = Proxy

getFileDataInfo
  :<|> fetchData
  :<|> deleteFileData = client fileAPI

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.FileData where

import Types.FileSystem
import Data.ByteString.Lazy (ByteString)
import Servant.API

type FileAPI =
  "api" :> "file" :> Capture "file Id" FileId :> Get '[JSON] FileDataJson
-- GET /api/file/<ID> -> show file by ID

  :<|> "api" :> "file" :> Capture "file Id" FileId :> "data" :> Get '[OctetStream] ByteString
-- GET /api/file/<ID>/data -> show file data by ID

  :<|> "api" :> "file" :> Capture "object Id" ObjectId :> Get '[JSON] [FileDataJson]
-- GET /api/file/<ObjectId> -> show all files of specific object

  :<|> "api" :> "file" :> Capture "object Id" ObjectId :> ReqBody '[OctetStream] ByteString :> Put '[JSON] FileId
-- PUT /api/file/<ObjectId> -> create new file under provided object

  :<|> "api" :> "file" :> Capture "file Id" FileId :> Delete '[JSON] ()
-- DELETE /api/file/<ID> -> remove file

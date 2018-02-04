{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Object where

import Types.FileSystem
import Data.ByteString.Lazy (ByteString)
import Servant.API

type ObjectAPI =
  "api" :> "object" :> Capture "object Id" ObjectId :> Get '[JSON] Object
-- GET /api/object/<ID> -> show object by ID

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> "children" :> Get '[JSON] ObjectChildren
-- GET /api/object/<ObjectId>/children -> show object's children

  :<|> "api" :> "object" :> ReqBody '[JSON] CreateObjectInfo :> Put '[JSON] ObjectId
--  PUT /api/object -> create new object

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> ReqBody '[OctetStream] ByteString :> Put '[JSON] FileId
-- PUT /api/object/<ObjectId> -> create new file under provided object

--  :<|> "api" :> "object" :> ReqBody '[JSON] Object :> Post '[JSON] ()
-- POST /api/object -> update object

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> Delete '[JSON] ()
-- DELETE /api/object/<ID> -> remove object

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Object where

import Types.FileSystem
import Servant.API

type ObjectAPI =
  "api" :> "object" :> Capture "object Id" ObjectId :> Get '[JSON] Object
-- GET /api/object/<ID> -> show object by ID

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> "children" :> Get '[JSON] [Object]
-- GET /api/object/<ID>/children -> show object's children

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> "files" :> Get '[JSON] [FileDataJson]
-- GET /api/object/<ObjectId>/files -> show all files of specific object

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> "link" :> Get '[JSON] Object
-- GET /api/object/<ObjectId>/link -> show the linked object to current one

  :<|> "api" :> "object" :> ReqBody '[JSON] ObjectName :> Put '[JSON] ObjectId
-- PUT /api/object -> create new object

  :<|> "api" :> "object" :> ReqBody '[JSON] Object :> Post '[JSON] ()
-- POST /api/object -> update object

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> Delete '[JSON] ()
-- DELETE /api/object/<ID> -> remove object

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Object where

import Types.FileSystem
import Types.Status
import Servant.API

type ObjectAPI =
  "api" :> "object" :> Capture "object Id" ObjectId :> Get '[JSON] (Status Object)
-- GET /api/object/<ID> -> show object by ID

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> "children" :> Get '[JSON] (Status [Object])
-- GET /api/object/<ID>/children -> show object's children

  :<|> "api" :> "bucket" :> Capture "bucket Id" BucketId :> "children" :> Get '[JSON] (Status [Object])
-- GET /api/bucket/<BucketID>/children -> show bucket children

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> "files" :> Get '[JSON] (Status [FileDataJson])
-- GET /api/object/<ObjectId>/files -> show all files of specific object

--  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> "link" :> Get '[JSON] (Status Object)
-- GET /api/object/<ObjectId>/link -> show the linked object to current one

--  :<|> "api" :> "object" :> ReqBody '[JSON] ObjectName :> Put '[JSON] (Status ObjectId)
-- PUT /api/object -> create new object

--  :<|> "api" :> "object" :> ReqBody '[JSON] Object :> Post '[JSON] (Status ())
-- POST /api/object -> update object

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> Delete '[JSON] (Status ())
-- DELETE /api/object/<ID> -> remove object

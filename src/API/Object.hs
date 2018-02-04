{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Object where

import Types.FileSystem
import Types.Status
import Servant.API

type ObjectAPI =
  "api" :> "object" :> Capture "object Id" ObjectId :> Get '[JSON] (Status Object)
-- GET /api/object/<ID> -> show object by ID

  :<|> "api" :> "bucket" :> Capture "bucket Id" BucketId :> "children" :> Get '[JSON] (Status [Object])
-- GET /api/bucket/<BucketID>/children -> show bucket children

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> "children" :> Get '[JSON] (Status ObjectChildren)
-- GET /api/object/<ObjectId>/children -> show object's children

  :<|> "api" :> "object" :> ReqBody '[JSON] CreateObjectInfo :> Put '[JSON] (Status ObjectId)
--  PUT /api/object -> create new object

--  :<|> "api" :> "object" :> ReqBody '[JSON] Object :> Post '[JSON] (Status ())
-- POST /api/object -> update object

  :<|> "api" :> "object" :> Capture "object Id" ObjectId :> Delete '[JSON] (Status ())
-- DELETE /api/object/<ID> -> remove object

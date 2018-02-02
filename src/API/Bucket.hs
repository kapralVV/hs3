{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Bucket where

import Types.FileSystem
import Types.Status
import Servant.API

type BucketAPI =
  "api" :> "bucket" :> Get '[JSON] (Status [Bucket])
-- GET /api/bucket -> show all buckets

  :<|> "api" :> "bucket" :> Capture "bucket Id" BucketId :> Get '[JSON] (Status Bucket)
-- GET /api/bucket/<ID> -> show bucket by ID

  :<|> "api" :> "bucket" :> Capture "bucket name" BucketName :> Get '[JSON] (Status Bucket)
-- GET /api/bucket/<NAME> -> show bucket by Name

  :<|> "api" :> "bucket" :> ReqBody '[JSON] BucketName :> Put '[JSON] (Status BucketId)
-- PUT /api/bucket -> create new bucket

  :<|> "api" :> "bucket" :> ReqBody '[JSON] Bucket :> Post '[JSON] (Status () )
-- POST /api/bucket -> update bucket

  :<|> "api" :> "bucket" :> Capture "bucket Id" BucketId :> Delete '[JSON] (Status () )
-- DELETE /api/bucket/<ID> -> remove bucket

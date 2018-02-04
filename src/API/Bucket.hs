{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Bucket where

import Types.FileSystem
import Servant.API

type BucketAPI =
  "api" :> "bucket" :> Get '[JSON] [Bucket]
-- GET /api/bucket -> show all buckets

  :<|> "api" :> "bucket" :> Capture "bucket Id" BucketId :> Get '[JSON] Bucket
-- GET /api/bucket/<ID> -> show bucket by ID

  :<|> "api" :> "bucket" :> Capture "bucket name" BucketName :> Get '[JSON] Bucket
-- GET /api/bucket/<NAME> -> show bucket by Name

  :<|> "api" :> "bucket" :> Capture "bucket Id" BucketId :> "children" :> Get '[JSON] [Object]
-- GET /api/bucket/<BucketID>/children -> show bucket children

  :<|> "api" :> "bucket" :> ReqBody '[JSON] BucketName :> Put '[JSON] BucketId
-- PUT /api/bucket -> create new bucket

  :<|> "api" :> "bucket" :> ReqBody '[JSON] Bucket :> Post '[JSON] ()
-- POST /api/bucket -> update bucket

  :<|> "api" :> "bucket" :> Capture "bucket Id" BucketId :> Delete '[JSON] ()
-- DELETE /api/bucket/<ID> -> remove bucket

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.MainAPI where

import API.Bucket
import API.FileData
import API.Object
import Servant.API

type MainAPI = BucketAPI
               :<|> ObjectAPI
               :<|> FileAPI

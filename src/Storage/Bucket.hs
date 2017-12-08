{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings          #-}

module Storage.Bucket where

import Data.Acid
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.IxSet                 as IX
import qualified Data.Set                   as DS


import Types.FileSystem
import Types.AcidDB
import Types.Status
import Data.Text (Text)

createBucket :: Text -> Update AcidDB (Status BucketId)
createBucket bucketName_ = do
  acidDb <- get
  let dbIndexInfo = fst $ buckets acidDb
  let haveHoles = not . null $ holes dbIndexInfo
  
  let maxIndex_ | haveHoles = head $ holes dbIndexInfo
                | otherwise = succ $ maxIndex dbIndexInfo

  let newBucket = Bucket { bucketId = maxIndex_
                         , bucketName = bucketName_
                         , childBObjects = DS.empty
                         }
  let updatedAcidDB =
        acidDb { buckets = (\(dbIndexInfo_, buckets_) ->
                               ( if haveHoles then
                                   (DbIndexInfo { maxIndex = maxIndex_
                                                , holes = drop 1 $ holes dbIndexInfo_
                                                }
                                   )
                                 else 
                                   (DbIndexInfo { maxIndex = maxIndex_
                                                , holes = holes dbIndexInfo_
                                                }
                                   )
                               , IX.insert newBucket buckets_
                               )
                           )
                           $ buckets acidDb
               }

  put $ updatedAcidDB
  return (Status $ Right maxIndex_)



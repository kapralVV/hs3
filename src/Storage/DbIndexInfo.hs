module Storage.DbIndexInfo where

import Data.Acid
import Control.Monad.Reader

import Types.FileSystem
import Types.DbIndexInfo
import Types.AcidDB

queryBucketIndex' :: AcidDB -> DbIndexInfo BucketId
queryBucketIndex' = fst . buckets

queryBucketIndex :: Query AcidDB (DbIndexInfo BucketId)
queryBucketIndex = queryBucketIndex' `fmap` ask

queryObjectIndex' :: AcidDB -> DbIndexInfo ObjectId
queryObjectIndex' = fst . objects

queryObjectIndex :: Query AcidDB (DbIndexInfo ObjectId)
queryObjectIndex = queryObjectIndex' `fmap` ask

queryFileDataIndex' :: AcidDB -> DbIndexInfo FileId
queryFileDataIndex' = fst . files

queryFileDataIndex :: Query AcidDB (DbIndexInfo FileId)
queryFileDataIndex = queryFileDataIndex' `fmap` ask

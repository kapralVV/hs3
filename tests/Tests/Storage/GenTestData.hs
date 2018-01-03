module Tests.Storage.GenTestData where

import Types.FileSystem
import Storage.AcidDB
import Storage.Object
import Types.Status
import Types.AcidDB

import Test.QuickCheck
import Data.Acid
import Data.Acid.Advanced
import qualified Data.IxSet as IX
import Control.Applicative
import Data.Text (Text)
import Test.QuickCheck.Instances ()


instance Arbitrary BucketName where
  arbitrary = pure BucketName <*> (arbitrary :: Gen Text)

instance Arbitrary ObjectName where
  arbitrary = pure ObjectName <*> (arbitrary :: Gen Text)

genBucketId :: AcidState AcidDB -> IO BucketId
genBucketId db = (fmap (oneof . map (return . bucketId) . IX.toList . fromStatus)
                   $ query' db QueryAllBuckets) >>= generate

genDirectoryObjId :: AcidState AcidDB -> IO (BucketId, Maybe ObjectId)
genDirectoryObjId db = (fmap (oneof . map (return . liftA2 (,) parentBucketId (Just . objectId))
                               . filter isDirectory'' . IX.toList . fromStatus)
                        $ query' db QueryAllObjects) >>= generate

genObjectIdPred :: AcidState AcidDB -> (Object -> Bool) -> IO ObjectId
genObjectIdPred db p = (fmap (oneof . map (return . objectId) . filter p . IX.toList . fromStatus)
                       $ query' db QueryAllObjects) >>= generate

genObjectId :: AcidState AcidDB -> IO ObjectId
genObjectId db = genObjectIdPred db (const True)

genFileObjId :: AcidState AcidDB -> IO ObjectId
genFileObjId db = genObjectIdPred db isFile''
  
genLinkObjId :: AcidState AcidDB -> IO ObjectId
genLinkObjId db = genObjectIdPred db isLink''

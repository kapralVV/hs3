module Tests.Storage.ArbitraryInstances where

import Types.FileSystem
import Storage.AcidDB
import Storage.Object
import Types.Status

import Data.Typeable

import Test.QuickCheck

import Types.AcidDB
import Data.Acid
import Data.Acid.Advanced
import qualified Data.IxSet as IX
import Control.Applicative
import Data.Text (Text)
import Test.QuickCheck.Instances


instance (Ord a, Arbitrary a, Typeable a, IX.Indexable a) => Arbitrary (IX.IxSet a) where
  arbitrary = do
    k <- choose (0,5)
    fmap IX.fromList (vectorOf k arbitrary)

instance Arbitrary ObjectId where
  arbitrary = fmap ObjectId $ choose (1,1000)

instance Arbitrary FileId where
  arbitrary = fmap FileId $ choose (1,1000)

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

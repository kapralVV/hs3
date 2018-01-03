{-# LANGUAGE OverloadedStrings #-}

module Tests.Storage.Object where

import Types.FileSystem
import Types.DbIndexInfo
import Types.AcidDB
import Types.Status
import Storage.AcidDB
import Tests.Storage.ArbitraryInstances

import Data.Time
import Data.Acid
import Data.Acid.Advanced
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Applicative (liftA2)
import qualified Data.IxSet  as IX
import qualified Data.Set    as DS

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as DBL
import Other.IxSetAeson

import Tests.Generic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances


prop_createDirObjectInRoot :: AcidState AcidDB -> ObjectName -> Property
prop_createDirObjectInRoot db name = monadicIO $ do
  bId <- run $ genBucketId db
  oId <- run . update' db $ CreateDirectoryObject name bId Nothing
  assert (statusToBool oId || (Failed NameExists) == oId)

prop_createDirsInDirs :: AcidState AcidDB -> ObjectName -> Property
prop_createDirsInDirs db name = monadicIO $ do
  (bId,dId) <- run $ genDirectoryObjId db
  oId <- run . update' db $ CreateDirectoryObject name bId dId
  assert (statusToBool oId || (Failed NameExists) == oId)

prop_createEmptyFilesInDirs :: AcidState AcidDB -> ObjectName -> Property
prop_createEmptyFilesInDirs db name = monadicIO $ do
  (bId,dId) <- run $ genDirectoryObjId db
  oId  <- run . update' db $ CreateFileObject name bId dId
  assert (statusToBool oId || (Failed NameExists) == oId)

prop_addFileDataToFile :: AcidState AcidDB
                       -> UTCTime
                       -> DBL.ByteString
                       -> Property
prop_addFileDataToFile db time bstring = monadicIO $ do
  oId <- run $ genFileObjId db
  fId <- run . update' db $ AddFileDataToFile oId time bstring
  assert $ statusToBool fId

prop_createLinkObject :: AcidState AcidDB -> ObjectName -> Property
prop_createLinkObject db name = monadicIO $ do
  (bId,dId) <- run $ genDirectoryObjId db
  fId       <- run $ genObjectId db
  oId  <- run . update' db $ CreateLinkObject name bId dId fId
  assert $ or [statusToBool oId, (Failed NameExists) == oId, Failed NotAllowed == oId]

prop_deleteObjects :: AcidState AcidDB -> Property
prop_deleteObjects db = monadicIO $ do
  oId    <- run $ genObjectId db
  status <- run . update' db $ DeleteObject oId
  assert $ statusToBool status
  

objectTests :: Spec
objectTests = do
  beforeAll (openLocalStateFrom "test-state" initAcidDB) $ do
    afterAll (liftA2 (>>) createCheckpoint closeAcidState) $ do
      describe "Test Object: " $ do

        it "Run auto generation of Dir objects under bucket root" $
          \db -> property $ prop_createDirObjectInRoot db

        it "Run auto generation of Dir objects under dir objects" $
          \db -> property $ prop_createDirsInDirs db

        modifyMaxSuccess (const 500) $ it "Run auto generation of File objects in dir objects" $
          \db -> property $ prop_createEmptyFilesInDirs db

        -- (BucketId 1000) is not possible in test-state (but can be)
        it "Creating Object under non-existing bucket should fail" $ do
          \db -> update' db (CreateFileObject (ObjectName "failed") (BucketId 1000) Nothing)
                 `shouldReturn` Failed NotFound

        -- (ObjectId 1000) is not possible in test-state (but can be)
        it "Creating  Object under non-existing Dir Object should fail" $ do
         \db -> update' db ( CreateFileObject (ObjectName "failed") (BucketId 1) (Just $ ObjectId 10000))
                `shouldReturn` Failed NotFound

        it "Creating FileObject under FileObject should fail" $ do
          \db -> (genFileObjId db >>= (update' db . CreateFileObject (ObjectName "failed") (BucketId 1) . Just))
                 `shouldReturn` Failed NotADirectory

        modifyMaxSuccess (const 200) $ it "Run auto generation of Link objects" $
          \db -> property $ prop_createLinkObject db

        it "Creating FileObject under LinkObject should fail" $ do
          \db -> (genLinkObjId db >>= (update' db . CreateFileObject (ObjectName "failed") (BucketId 1) . Just))
                `shouldReturn` Failed NotADirectory

        -- it "Run auto deleting objects" $
        --   \db -> property $ prop_deleteObjects db

        it "Creating Link for object is located in other bucket should not be allowed" $ do
          \db -> (update' db $ CreateLinkObject (ObjectName "LINK") (BucketId 1) Nothing (ObjectId 100))
                 `shouldReturn` Failed NotAllowed

        it "Run auto generating FileData" $
          \db -> property $ prop_addFileDataToFile db

        it "Query all Objects and generate json output" $
          \db -> (query' db QueryAllBucketsForJson >>= DBL.putStr . encodePretty)
            `shouldReturn` ()

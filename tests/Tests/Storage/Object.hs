{-# LANGUAGE OverloadedStrings #-}

module Tests.Storage.Object where

import Types.FileSystem
import Types.DbIndexInfo
import Types.AcidDB
import Types.Status
import Storage.AcidDB
import Tests.Storage.ArbitraryInstances

import Data.Acid
import Data.Acid.Advanced
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.IxSet  as IX
import qualified Data.Set    as DS

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as DBL
import Other.IxSetAeson


import Tests.Generic
import Test.Hspec


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
  oId <- run . update' db $ CreateFileObject name bId dId
  assert (statusToBool oId || (Failed NameExists) == oId)

objectTests :: Spec
objectTests = do
  beforeAll (openLocalStateFrom "test-state" initAcidDB) $ do
    afterAll closeAcidState $ do
      describe "Test Object: " $ do

        it "Run auto generation of Dir objects under bucket root" $
          \db -> property $ prop_createDirObjectInRoot db

        it "Run auto generation of Dir objects under dir objects" $
          \db -> property $ prop_createDirsInDirs db

        it "Run auto generation of File objects in dir objects" $
          \db -> property $ prop_createEmptyFilesInDirs db

        -- (BucketId 1000) is not possible in test-state (but can be)
        it "Creating Object under non-existing bucket should fail" $ do
          \db -> update' db (CreateFileObject (ObjectName "failed") (BucketId 1000) Nothing)
                 `shouldReturn` Failed NotFound

        -- (ObjectId 1000) is not possible in test-state (but can be)
        it "Creating  Object under non-existing Dir Object should fail" $ do
         \db -> update' db ( CreateFileObject (ObjectName "failed") (BucketId 1) (Just $ ObjectId 1000))
                `shouldReturn` Failed NotFound

        it "Creating FileObject under FileObject should fail" $ do
          \db -> (genFileObjId db >>= (update' db . CreateFileObject (ObjectName "failed") (BucketId 1) . Just))
                 `shouldReturn` Failed NotADirectory

        it "Query all Objects and generate json output" $
          \db -> (query' db QueryAllObjects >>= DBL.putStr . encodePretty)
            `shouldReturn` ()

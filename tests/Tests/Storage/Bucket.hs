{-# LANGUAGE OverloadedStrings #-}

module Tests.Storage.Bucket where

import Types.FileSystem
import Types.DbIndexInfo
import Types.AcidDB
import Types.Status
import Storage.AcidDB
import Tests.Storage.GenTestData()

import Data.Acid
import Data.Acid.Advanced
import Data.List.Unique (allUnique)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.IxSet  as IX
import qualified Data.Set    as DS

import Tests.Generic
import Test.Hspec
import Test.Hspec.QuickCheck


prop_createBucket :: AcidState AcidDB -> BucketName -> Property
prop_createBucket db name = monadicIO $ do
  bId <- run . update' db $ CreateBucket name
  assert (statusToBool bId || (Failed NameExists) == bId)

bucketTests :: Spec
bucketTests = do
  beforeAll (openLocalStateFrom "test-state" initAcidDB) $ do
    afterAll closeAcidState $ do
      describe "Test Buckets: " $ do

        it "Create bucket manually" $ do
          \db -> update' db (CreateBucket (BucketName "test")) `shouldReturn` Done (BucketId 1)

        it "Query the Bucket by Id" $ do
          \db -> query' db (QueryBucketById (BucketId 1))
                 `shouldReturn`
                 Done (Bucket { bucketId = BucketId 1
                              , bucketName = BucketName "test"
                              }
                      )

        it "It's not possible to create bucket with the same name" $ do
          \db -> update' db (CreateBucket (BucketName "test")) `shouldReturn` Failed NameExists

        modifyMaxSuccess (const 10) $ it "Run auto generation of buckets" $
          \db -> property $ prop_createBucket db

        it "Check if all bucketNames are unique" $ do
          \db -> (query' db QueryAllBuckets >>= return . fmap (allUnique . map bucketName . IX.toList ) )
                 `shouldReturn` Done True

        it "Check if all bucketId are unique" $ do
          \db -> (query' db QueryAllBuckets >>= return . fmap (allUnique . map bucketId . IX.toList) )
                 `shouldReturn` Done True

        it "Check the maximum index. maximum BucketId should equal maxIndex" $ do
          \db -> (fmap (DS.findMax . DS.map bucketId . IX.toSet . fromStatus) $ query' db QueryAllBuckets)
                 `compareIoActions`
                 (fmap maxIndex $ query' db QueryBucketIndex)

        it "Delete bucket with index '1' from storage" $ do
          \db -> update' db (DeleteBucketGeneric (BucketId 1)) `shouldReturn` Done ()

        it "Check index <holes> to find the index of just deleted Bucket" $ do
          \db -> fmap holes (query' db QueryBucketIndex) `shouldReturn` [BucketId 1]

        it "Add new Bucket again. It should have '1' index" $ do
          \db -> update' db (CreateBucket (BucketName "New Bucket")) `shouldReturn` Done (BucketId 1)

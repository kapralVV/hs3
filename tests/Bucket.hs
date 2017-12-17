{-# LANGUAGE OverloadedStrings #-}
module Bucket where

import Types.FileSystem
import Types.AcidDB
import Types.Status
import Storage.AcidDB
import ArbitraryInstances()

import Data.Acid
import Data.Acid.Advanced
import Data.List.Unique (allUnique)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.IxSet  as IX

import Test.Hspec


prop_createBucket :: AcidState AcidDB -> BucketName -> Property
prop_createBucket db name = monadicIO $ do
  bId <- run . update' db $ CreateBucket name
  assert (statusToBool bId || (Failed $ ErrorMessage "Bucket exists") == bId)

bucketTests :: Spec
bucketTests = do
  beforeAll (openLocalStateFrom "test-state" initAcidDB) $ do
    afterAll closeAcidState $ do
      describe "Test Buckets in storage" $ do

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
          \db -> update' db (CreateBucket (BucketName "test")) `shouldReturn` Failed (ErrorMessage ("Bucket exists"))

        it "Run auto generation of buckets" $ do
          \db -> do
            property $ prop_createBucket db

        it "Check if all bucketNames are unique" $ do
          \db -> (query' db QueryAllBuckets >>= return . fmap (allUnique . map bucketName . IX.toList ) )
                 `shouldReturn` (Done True)

        it "Check if all bucketId are unique" $ do
          \db -> (query' db QueryAllBuckets >>= return . fmap (allUnique . map bucketId . IX.toList) )
                 `shouldReturn` (Done True)

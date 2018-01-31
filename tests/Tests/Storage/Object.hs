{-# LANGUAGE OverloadedStrings #-}

module Tests.Storage.Object where

import Types.FileSystem
import Types.AcidDB
import Types.Status
import Types.DbIndexInfo
import Storage.AcidDB
import Storage.MainStorage
import Tests.Storage.GenTestData
import Tests.Storage.FileSystemJson
import Other.IxSetAeson ()

import Data.Time
import Data.Acid
import Data.Acid.Advanced
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Applicative (liftA2)
import Control.Monad.Trans

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as DBL

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()


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
  oId <- run $ genObjectId db
  fId <- run . update' db $ AddFileDataToFile oId time bstring
  assert (statusToBool fId || (Failed NotAFile) == fId)

prop_createLinkObject :: AcidState AcidDB -> ObjectName -> Property
prop_createLinkObject db name = monadicIO $ do
  (bId,dId) <- run $ genDirectoryObjId db
  fId       <- run $ genObjectId db
  oId  <- run . update' db $ CreateLinkObject name bId dId fId
  assert $ or [statusToBool oId, (Failed NameExists) == oId, Failed NotAllowed == oId]

prop_deleteObjects :: AcidState AcidDB -> Property
prop_deleteObjects db = monadicIO $ do
  oId    <- run $ genObjectId db
  status <- run $ deleteObject db oId
  assert $ statusToBool status

showBucketJson :: AcidState AcidDB -> BucketId -> StatusT IO ()
showBucketJson db bId = do
  bucket' <- StatusT . query' db $ QueryBucketById bId
  json <- bucketToJson db bucket'
  lift . DBL.putStr $ encodePretty json

testObject :: Object
testObject = Object { objectId = ObjectId 4
                     , objectName = ObjectName "Directory"
                     , parentBucketId = BucketId 1
                     , parentObjectId = Just $ ObjectId 1
                     , objectType = Directory
                     }

objectTests :: Spec
objectTests = do
  beforeAll (openLocalStateFrom "test-state" initAcidDB) $ do
    afterAll (liftA2 (>>) createCheckpoint closeAcidState) $ do
      describe "Test Object: " $ do

        it "Create Dir object under bucket root manually" $ do
          \db -> (update' db $ CreateDirectoryObject (ObjectName "Directory") (BucketId 1) Nothing)
                 `shouldReturn` Done (ObjectId 1)

        it "Creating any object with the same ObjectName should fail" $ do
          \db -> (update' db $ CreateFileObject (ObjectName "Directory") (BucketId 1) Nothing)
                 `shouldReturn` Failed NameExists

        it "Create File object under Directory manually" $ do
          \db -> (update' db $ CreateFileObject (ObjectName "File") (BucketId 1) (Just $ ObjectId 1) )
                 `shouldReturn` Done (ObjectId 2)

        it "Create Link object under Directory to File manually" $ do
          \db -> (update' db $ CreateLinkObject (ObjectName "Link") (BucketId 1) (Just $ ObjectId 1) (ObjectId 2) )
                 `shouldReturn` Done (ObjectId 3)

        it "Creating any object with the same ObjectName should NOT fail if it's located in other place" $ do
          \db -> (update' db $ CreateDirectoryObject (ObjectName "Directory") (BucketId 1) (Just $ ObjectId 1) )
                 `shouldReturn` Done (ObjectId 4)

        it "Bucket can have the same ObjectName in a different locations, querying it should work" $ do
          \db -> (query' db $ QueryObjectByName (BucketId 1) (Just $ ObjectId 1) (ObjectName "Directory") ) 
                 `shouldReturn` Done (testObject)

        it "Using <PATH> to find the Object" $ do
          \db -> (query' db $ FollowNames (BucketName "New Bucket") [(ObjectName "Directory"), (ObjectName "Directory")])
                  `shouldReturn` Done (testObject)

        it "Create FileData manually" $ do
          \db -> (getCurrentTime >>= \time -> (update' db $ AddFileDataToFile (ObjectId 2) time "TestData"))
                 `shouldReturn` Done (FileId 1)

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

        it "Creating Link for Directory to itself should fail" $ do
          \db -> (update' db $ CreateLinkObject (ObjectName "Link") (BucketId 1) (Just $ ObjectId 1) (ObjectId 1))
                 `shouldReturn` Failed NotAllowed

        it "Creating Link to object in another bucket should fail" $ do
          \db -> (update' db $ CreateLinkObject (ObjectName "Link") (BucketId 2) Nothing (ObjectId 1))
                 `shouldReturn` Failed NotAllowed

        modifyMaxSuccess (const 200) $ it "Run auto generation of Link objects" $
          \db -> property $ prop_createLinkObject db

        it "Creating FileObject under LinkObject should fail" $ do
          \db -> (genLinkObjId db >>= (update' db . CreateFileObject (ObjectName "failed") (BucketId 1) . Just))
                `shouldReturn` Failed NotADirectory

        it "Creating FileData for non-File object should fail" $ do
          \db -> ( getCurrentTime >>= (\time -> update' db $ AddFileDataToFile (ObjectId 1) time "Test data")
                 ) `shouldReturn` Failed NotAFile

        modifyMaxSuccess (const 1000) $ it "Run auto generation of FileData" $
          \db -> property $ prop_addFileDataToFile db

        it "Delete the Link Object manually" $ do
          \db -> deleteObject db (ObjectId 3)
                 `shouldReturn` Done ()

        it "Delete the File Object manually" $ do
          \db -> deleteObject db (ObjectId 2)
                 `shouldReturn` Done ()

        it "Show Childrens of Directory" $ do
          \db -> ((query' db $ QueryChildObjects (BucketId 1) (Just $ ObjectId 1)) >>= DBL.putStr . encodePretty)
                 `shouldReturn` ()

        it "Delete the Directory Object manually" $ do
          \db -> deleteObject db (ObjectId 1)
                 `shouldReturn` Done ()

        modifyMaxSuccess (const 20) $ it "Run auto deleting objects" $
          \db -> property $ prop_deleteObjects db

        it "Remove Bucket manually" $ do
          \db -> deleteBucket db (BucketId 2)
                 `shouldReturn` Done ()

        it "DbIndex <holes> should not be empty after removing Objects and Bucket" $ do
          \db -> ( sequence [ fmap (not . null . holes) $ query' db QueryBucketIndex
                            , fmap (not . null . holes) $ query' db QueryObjectIndex
                            , fmap (not . null . holes) $ query' db QueryFileDataIndex
                            ] >>= return . and
                 ) `shouldReturn` True

        it "Query and show DBIndex" $ do
          \db -> ( query' db QueryBucketIndex >>= putStrLn . show
                   >>
                   query' db QueryObjectIndex >>= putStrLn . show
                   >>
                   query' db QueryFileDataIndex >>= putStrLn . show
                 )
                 `shouldReturn` ()

        it "Query Objects from Bucket 3 and generate json output" $ do
          \db -> runStatusT (showBucketJson db $ BucketId 3) `shouldReturn` Done ()

        it "Querying Objects from (Bucket 2) should fail as it's removed" $ do
          \db -> runStatusT (showBucketJson db $ BucketId 2) `shouldReturn` Failed NotFound

        modifyMaxSuccess (const 500) $ it "Run auto-generation of Files again to check if <holes> are used" $
          \db -> property $ prop_createEmptyFilesInDirs db

        modifyMaxSuccess (const 500) $ it "Run auto-generation FileData again to check if <holes> are used" $
          \db -> property $ prop_addFileDataToFile db

        it "DbIndex <holes> should BE empty after new auto-generation Objects and FileDatas" $ do
          \db -> ( sequence [ fmap (null . holes) $ query' db QueryObjectIndex
                            , fmap (null . holes) $ query' db QueryFileDataIndex
                            ] >>= return . and
                 ) `shouldReturn` True

        it "Query and show DBIndex again" $ do
          \db -> ( query' db QueryBucketIndex >>= putStrLn . show
                   >>
                   query' db QueryObjectIndex >>= putStrLn . show
                   >>
                   query' db QueryFileDataIndex >>= putStrLn . show
                 )
                 `shouldReturn` ()

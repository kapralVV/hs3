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
import Data.List.Unique (allUnique)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.IxSet  as IX
import qualified Data.Set    as DS

import Tests.Generic
import Test.Hspec


prop_createObjectInRoot :: AcidState AcidDB -> ObjectName -> ObjectType -> Property
prop_createObjectInRoot db name oType = monadicIO $ do
  bId <- run $ genBucketId db
  oId <- run . update' db $ CreateObject name bId Nothing oType
  assert (statusToBool oId || (Failed $ ErrorMessage "Object-name exists") == oId)

objectTests :: Spec
objectTests = do
  beforeAll (openLocalStateFrom "test-state" initAcidDB) $ do
    afterAll closeAcidState $ do
      describe "Test Object:" $ do

        it "Run auto generation of objects" $
          \db -> property $ prop_createObjectInRoot db



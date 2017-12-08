module Tests.ArbitraryInstances where

import Types.FileSystem

import Data.Typeable

import Test.QuickCheck
import Data.Time
import qualified Data.IxSet as IX
import Data.Text.Arbitrary
import Control.Applicative

-- stolen from here :
-- https://gist.github.com/agrafix/2b48ec069693e3ab851e

instance Arbitrary UTCTime where
  arbitrary = do
    randomDay <- choose (1, 28) :: Gen Int
    randomMonth <- choose (1, 12) :: Gen Int
    randomYear <- choose (1970, 2017) :: Gen Integer
    randomTime <- choose (0, 86401) :: Gen Int
    return $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)

--

instance (Ord a, Arbitrary a, Typeable a, IX.Indexable a) => Arbitrary (IX.IxSet a) where
  arbitrary = do
    k <- choose (0,5)
    fmap IX.fromList (vectorOf k arbitrary)

instance Arbitrary BucketId where
  arbitrary = fmap BucketId $ choose (1,1000)
  
instance Arbitrary ObjectId where
  arbitrary = fmap ObjectId $ choose (1,1000)

instance Arbitrary FileId where
  arbitrary = fmap FileId $ choose (1,1000)

instance Arbitrary ObjectType where
  arbitrary = oneof [ liftA Link arbitrary 
                    , liftA Directory arbitrary
                    , liftA File arbitrary
                    ]


instance Arbitrary Object where
  arbitrary = pure Object
              <*> (arbitrary :: Gen ObjectId)
              <*> (arbitrary :: Gen Text)
              <*> (arbitrary :: Gen BucketId)
              <*> (arbitrary :: Gen (Maybe ObjectId))
              <*> (arbitrary :: Gen ObjectType)


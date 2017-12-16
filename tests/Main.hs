import Types.FileSystem
import Types.AcidDB
import Storage.AcidDB
import Storage.Object
import Types.Status

import Data.Acid
import Data.Acid.Advanced
import Test.QuickCheck
import Test.QuickCheck.Monadic


main = do
  db <- openLocalStateFrom "test-state" initAcidDB
  quickCheck $ whenFail (closeAcidState db) (prop_createBucket db)
  
prop_createBucket :: AcidState AcidDB -> BucketName -> Property
prop_createBucket db name = monadicIO $ do
  bId <- run . update' db $ CreateBucket name
  assert (True == statusToBool bId)

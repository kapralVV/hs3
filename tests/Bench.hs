
import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Data.Acid
import Data.Acid.Advanced
import Control.Applicative (liftA2)
import Data.Time (UTCTime)
import Data.ByteString.Lazy (ByteString)

import Types.FileSystem
import Types.AcidDB
import Storage.AcidDB
import Storage.MainStorage
import Tests.Storage.GenTestData

main :: IO ()
main = do
  db <- openLocalStateFrom "test-state" initAcidDB
  defaultMain
    [ bench "CreateDirs" $
      perRunEnv (genDirectoryObjId db) $ \ ~(bId,dId) -> do
        name <- generate (arbitrary :: Gen ObjectName)
        update' db $ CreateDirectoryObject name bId dId
    , bench "CreateFiles" $
      perRunEnv (genDirectoryObjId db) $ \ ~(bId,dId) -> do
        name <- generate (arbitrary :: Gen ObjectName)
        update' db $ CreateFileObject name bId dId
    , bench "DeleteObject" $
      perRunEnv (genObjectId db) $ \ ~oid -> do
        deleteObject db oid
    , bench "CreateFileData" $
      perRunEnv (genObjectId db) $ \ ~oId -> do
        time <- generate (arbitrary :: Gen UTCTime)
        bstring <- generate (arbitrary :: Gen ByteString)
        update' db $ AddFileDataToFile oId time bstring
    ]
  liftA2 (>>) createCheckpoint closeAcidState db

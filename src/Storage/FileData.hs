{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.FileData where

import Data.Acid
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.IxSet                 as IX
import qualified Data.Set                   as DS
import Data.ByteString.Lazy
import Data.Digest.Pure.MD5

import Types.FileSystem
import Types.AcidDB
import Types.Status
import Storage.Generic

createFileData :: ObjectId -> ByteString -> Update AcidDB (Status FileId)
createFileData objectId_ fileData_ = do
  acidDb <- get
  let dbIndexInfo = fst $ files acidDb
  let maxIndex_   = getMaxIndex dbIndexInfo
  let newFile     = FileData { fileId = maxIndex_
                             , fileData = fileData_
                             , ownedbyObjects = DS.singleton objectId_
                             , fileMd5sum = show $ md5 fileData_
                             }
  let updatedAcidDB =
        acidDb { files = (\(_ , filesSet) ->
                             ( updateIndexInfo dbIndexInfo
                             , IX.insert newFile filesSet
                             )
                         )
                         $ files acidDb
               }
  put updatedAcidDB
  return $ Done maxIndex_


deleteFileData :: FileId -> Update AcidDB (Status ())
deleteFileData fileId_ = do
  acidDb <- get
  if statusToBool . queryBy fileId_ . snd . files $ acidDb then do
    let updatedAcidDB =
          acidDb { files = (\(dbIndexInfo, filesSet) ->
                               ( DbIndexInfo { maxIndex = maxIndex dbIndexInfo
                                             , holes = fileId_ : holes dbIndexInfo
                                             }
                               , IX.deleteIx fileId_ filesSet
                               )
                           )
                           $ files acidDb
                 }
    put updatedAcidDB
    return $ Done ()

    else
    return . Failed $ ErrorMessage "File data not Found"

  
queryAllFiles :: Query AcidDB (Status (IX.IxSet FileData))
queryAllFiles = fmap (Done . snd . files) ask

queryFile :: FileId -> Query AcidDB (Status FileData)
queryFile key = (queryBy key . snd . files) `fmap` ask

queryFileData :: FileId -> Query AcidDB (Status ByteString)
queryFileData key = fmap fileData `fmap` queryFile key

queryFileMd5 :: FileId -> Query AcidDB (Status String)
queryFileMd5 key = fmap fileMd5sum `fmap` queryFile key

queryFileOwners :: FileId -> Query AcidDB (Status (DS.Set ObjectId))
queryFileOwners key = fmap ownedbyObjects `fmap` queryFile key

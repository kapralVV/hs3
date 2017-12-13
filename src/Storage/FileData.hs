{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.FileData where

import Data.Acid
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.IxSet                 as IX
import qualified Data.ByteString.Lazy       as DBL
import Data.Digest.Pure.MD5
import Data.Time.Clock

import Types.FileSystem
import Types.AcidDB
import Types.Status
import Storage.Generic

createFileData :: ObjectId -> UTCTime -> DBL.ByteString -> Update AcidDB (Status FileId)
createFileData objectId_ time fileData_ = do
  acidDb <- get
  let dbIndexInfo = fst $ files acidDb
  let maxIndex_   = getMaxIndex dbIndexInfo
  let newFile     = FileData { fileId = maxIndex_
                             , fileData = fileData_
                             , parentFObjectId = objectId_
                             , fileMd5sum = show $ md5 fileData_
                             , fileSize = DBL.length fileData_
                             , createTime = time
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

  
queryAllFiles' :: AcidDB -> IX.IxSet FileData
queryAllFiles' = snd . files

queryAllFiles :: Query AcidDB (Status (IX.IxSet FileData))
queryAllFiles = (Done . queryAllFiles') `fmap` ask

queryFile' :: FileId -> AcidDB -> Status FileData
queryFile' key = queryBy key . queryAllFiles'

queryFile :: FileId -> Query AcidDB (Status FileData)
queryFile key = queryFile' key `fmap` ask

queryFileData' :: FileId -> AcidDB -> Status DBL.ByteString
queryFileData' key = fmap fileData . queryFile' key

queryFileData :: FileId -> Query AcidDB (Status DBL.ByteString)
queryFileData key = queryFileData' key `fmap` ask

queryFileMd5' :: FileId -> AcidDB -> Status String
queryFileMd5' key = fmap fileMd5sum . queryFile' key

queryFileMd5 :: FileId -> Query AcidDB (Status String)
queryFileMd5 key = queryFileMd5' key `fmap` ask

queryParentFObjectId' :: FileId -> AcidDB -> Status ObjectId
queryParentFObjectId' key = fmap parentFObjectId . queryFile' key

queryParentFObjectId :: FileId -> Query AcidDB (Status ObjectId)
queryParentFObjectId key = queryParentFObjectId' key `fmap` ask


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE RecordWildCards    #-}

module Types.FileSystem where


import Data.Typeable
import Data.Data
import GHC.Generics
import Control.DeepSeq

import Data.SafeCopy
import qualified Data.Aeson as A
import Web.HttpApiData
import qualified Data.IxSet as IX
import Data.Text

import Data.ByteString.Lazy
import Data.Time.Clock
import Data.Int
import Control.Applicative


---------------- BucketId ----------------

newtype BucketId = BucketId Int
                 deriving (Show, Eq, Ord, Data, Typeable, Generic, Enum, FromHttpApiData)
instance A.ToJSON BucketId
instance A.FromJSON BucketId

$(deriveSafeCopy 0 'base ''BucketId)

instance NFData BucketId

---------------- ObjectId ----------------

newtype ObjectId = ObjectId Int
                 deriving (Show, Eq, Ord, Data, Typeable, Generic, Enum, FromHttpApiData)
instance A.ToJSON ObjectId
instance A.FromJSON ObjectId

$(deriveSafeCopy 0 'base ''ObjectId)

instance NFData ObjectId

---------------- FileId ------------------

newtype FileId = FileId Int
               deriving (Show, Eq, Ord, Data, Typeable, Generic, Enum, FromHttpApiData)
instance A.ToJSON FileId
instance A.FromJSON FileId

$(deriveSafeCopy 0 'base ''FileId)

instance NFData FileId

---------------- ObjectType --------------

data ObjectType = File
                | Directory
                | Link  { linkedObject :: ObjectId}
                deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON ObjectType
instance A.FromJSON ObjectType


---------------- ObjectName -------------

newtype ObjectName = ObjectName Text
                 deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON ObjectName
instance A.FromJSON ObjectName

$(deriveSafeCopy 0 'base ''ObjectName)

---------------- Object ------------------

data Object = Object { objectId :: ObjectId
                     , objectName :: ObjectName
                     , parentBucketId :: BucketId
                     , parentObjectId :: Maybe ObjectId
                     , objectType :: ObjectType
--                     , objectMetaData :: ObjectMetaData
                     } deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON Object
instance A.FromJSON Object

instance IX.Indexable Object where
  empty = IX.ixSet
    [ IX.ixFun (\x -> [objectId x])
    , IX.ixFun (\x -> [objectName x])
    , IX.ixFun (\x -> [parentBucketId x])
    , IX.ixFun (\x -> [parentObjectId x])
    ]

$(deriveSafeCopy 0 'base ''Object)
$(deriveSafeCopy 0 'base ''ObjectType)

---------------- FileData  ------------------

data FileData = FileData { fileId :: FileId
                         , fileData :: ByteString
                         , parentFObjectId :: ObjectId
                         , fileMd5sum :: String
                         , fileSize :: Int64
                         , createTime :: UTCTime
                         } deriving (Show, Eq, Ord, Data, Typeable)
instance IX.Indexable FileData where
  empty = IX.ixSet
    [ IX.ixFun (\x -> [fileId x])
    , IX.ixFun (\x -> [parentFObjectId x])
    ]

$(deriveSafeCopy 0 'base ''FileData)

data FileDataJson = FileDataJson { _fileId :: FileId
                                 , _parentFObjectId :: ObjectId
                                 , _fileMd5sum :: String
                                 , _fileSize :: Int64
                                 , _createTime :: UTCTime
                                 } deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON FileDataJson
instance A.FromJSON FileDataJson

fileDataToJson :: FileData -> FileDataJson
fileDataToJson = FileDataJson
                 <$> fileId
                 <*> parentFObjectId
                 <*> fileMd5sum
                 <*> fileSize
                 <*> createTime

-- data ObjectMetaData = undefined

---------------- ObjectChildren  --------------

data ObjectChildren = ObjectDirChild  [Object]
                    | ObjectFileChild [FileDataJson]
                    | ObjectLinkChild Object
                    deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON ObjectChildren
instance A.FromJSON ObjectChildren

---------------- CreateObjectInfo ------------_

data CreateObjectInfo = CreateObjectInfo
                        { c_objectName :: ObjectName
                        , c_bucketId :: BucketId
                        , c_parentObjectId :: Maybe ObjectId
                        , c_objectType :: ObjectType
                        } deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON CreateObjectInfo
instance A.FromJSON CreateObjectInfo

---------------- BucketName  ------------------

newtype BucketName = BucketName Text
                 deriving (Show, Eq, Ord, Data, Typeable, Generic, FromHttpApiData)
instance A.ToJSON BucketName
instance A.FromJSON BucketName

$(deriveSafeCopy 0 'base ''BucketName)

---------------- Bucket  ------------------

data Bucket = Bucket { bucketId :: BucketId
                     , bucketName :: BucketName
--                     , bucketOptions :: [BucketOptions]
                     } deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON Bucket
instance A.FromJSON Bucket

instance IX.Indexable Bucket where
  empty = IX.ixSet
    [ IX.ixFun (\x -> [bucketId x])
    , IX.ixFun (\x -> [bucketName x])
    ]

$(deriveSafeCopy 0 'base ''Bucket)
              
-- data BucketOptions = undefined


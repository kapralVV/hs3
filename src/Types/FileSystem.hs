{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE RecordWildCards    #-}

module Types.FileSystem where


-- import Other.IxSetAeson

import Data.Typeable
import Data.Data
import GHC.Generics

-- import Data.Acid
import Data.SafeCopy
import qualified Data.Aeson as A
import qualified Data.IxSet as IX
import qualified Data.Map.Strict as DMS
import qualified Data.Set as DS
import Data.Text

import Data.ByteString.Lazy
import Data.Time.Clock
import Data.Int



---------------- BucketId ----------------

newtype BucketId = BucketId Int
                 deriving (Show, Eq, Ord, Data, Typeable, Generic, Enum)
instance A.ToJSON BucketId
instance A.FromJSON BucketId

$(deriveSafeCopy 0 'base ''BucketId)

---------------- ObjectId ----------------

newtype ObjectId = ObjectId Int
                 deriving (Show, Eq, Ord, Data, Typeable, Generic, Enum)
instance A.ToJSON ObjectId
instance A.FromJSON ObjectId

$(deriveSafeCopy 0 'base ''ObjectId)

---------------- FileId ------------------

newtype FileId = FileId Int
               deriving (Show, Eq, Ord, Data, Typeable, Generic, Enum)
instance A.ToJSON FileId
instance A.FromJSON FileId

$(deriveSafeCopy 0 'base ''FileId)

---------------- ObjectType --------------

data ObjectType = File      { fileVersions :: DMS.Map UTCTime FileId }
                | Directory { childObjects :: DS.Set ObjectId }
                | Link      { linkedObject :: ObjectId}
                deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON ObjectType
instance A.FromJSON ObjectType


---------------- ObjectName -------------

newtype ObjectName = ObjectName Text
                 deriving (Show, Eq, Ord, Data, Typeable, Generic, Monoid)
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
    [ IX.ixGen (IX.Proxy :: IX.Proxy ObjectId)
    , IX.ixGen (IX.Proxy :: IX.Proxy ObjectName)
    ]

$(deriveSafeCopy 0 'base ''Object)
$(deriveSafeCopy 0 'base ''ObjectType)

---------------- FileData  ------------------

data FileData = FileData { fileId :: FileId
                         , fileData :: ByteString
                         , ownedbyObjects :: DS.Set ObjectId
                         , fileMd5sum :: String
                         , fileSize :: Int64
                         } deriving (Show, Eq, Ord, Data, Typeable)
instance IX.Indexable FileData where
  empty = IX.ixSet
    [ IX.ixGen (IX.Proxy :: IX.Proxy FileId)]

$(deriveSafeCopy 0 'base ''FileData)

-- data ObjectMetaData = undefined

---------------- BucketName  ------------------

newtype BucketName = BucketName Text
                 deriving (Show, Eq, Ord, Data, Typeable, Generic, Monoid)
instance A.ToJSON BucketName
instance A.FromJSON BucketName

$(deriveSafeCopy 0 'base ''BucketName)

---------------- Bucket  ------------------

data Bucket = Bucket { bucketId :: BucketId
                     , bucketName :: BucketName
                     , childBObjects :: DS.Set ObjectId
--                     , bucketOptions :: [BucketOptions]
                     } deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance A.ToJSON Bucket
instance A.FromJSON Bucket

instance IX.Indexable Bucket where
  empty = IX.ixSet
    [ IX.ixGen (IX.Proxy :: IX.Proxy BucketId)
    , IX.ixGen (IX.Proxy :: IX.Proxy BucketName)
    ]

$(deriveSafeCopy 0 'base ''Bucket)
              
-- data BucketOptions = undefined


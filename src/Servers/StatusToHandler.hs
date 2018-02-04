{-# LANGUAGE OverloadedStrings #-}

module Servers.StatusToHandler where

import Types.Status
import Servant.Server
import Control.Monad.Error.Class
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)

errorMessageToServantErr :: ErrorMessage -> ServantErr
errorMessageToServantErr NameExists = err409
errorMessageToServantErr NotFound = err404
errorMessageToServantErr NotAFile = err400 { errBody = "Not a File" }
errorMessageToServantErr NotALink = err400 { errBody = "Not a Link" }
errorMessageToServantErr NotADirectory = err400 { errBody = "Not a Directory" }
errorMessageToServantErr NotAllowed = err406
errorMessageToServantErr (ErrorMessage x) = err400 { errBody = fromStrict $ encodeUtf8 x }


statusToHandler :: Status a -> Handler a
statusToHandler (Done x)   = return x
statusToHandler (Failed e) = throwError (errorMessageToServantErr e)

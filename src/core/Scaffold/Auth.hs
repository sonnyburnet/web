{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Scaffold.Auth (User (..), withBasicAuth, checkBasicAuth) where

import qualified Data.Text as T
import Servant (BasicAuthData(..), err401, throwError)
import Servant.Auth.Server (FromJWT (decodeJWT), FromBasicAuthData (..), BasicAuthCfg, AuthResult (..), ToJWT (..))
import Scaffold.Transport.Response
import KatipController (KatipController, KatipLoggerLocIO)
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import Katip
import Katip.Core

newtype User = User { email :: T.Text } deriving Show

instance FromJWT User where
  decodeJWT _ = undefined

instance ToJWT User where
  encodeJWT _ = undefined

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData basicAuthData authChecker = authChecker basicAuthData

withBasicAuth :: AuthResult User -> (User -> KatipController (Response a)) -> KatipController (Response a)
withBasicAuth (Authenticated user) runApi = runApi user
withBasicAuth _ _ = throwError err401

checkBasicAuth :: KatipLoggerLocIO ->  M.Map T.Text User -> BasicAuthData -> IO (AuthResult User)
checkBasicAuth log storage auth_data = do
  log getLoc InfoS $ ls $ show (basicAuthPassword auth_data, basicAuthUsername auth_data)
  let result = T.decodeUtf8 (basicAuthPassword auth_data) `M.lookup` storage
  return $ case result of
    Just email -> Authenticated $ User $ T.decodeUtf8 (basicAuthUsername auth_data)
    Nothing -> Indefinite
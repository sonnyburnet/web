{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Scaffold.Auth (User (..), withBasicAuth, checkBasicAuth) where

import qualified Data.Text as T
import Servant (BasicAuthData(..))
import Servant.Auth.Server (FromJWT (decodeJWT), FromBasicAuthData (..), BasicAuthCfg, AuthResult (..), ToJWT (..))
import Scaffold.Transport.Response
import KatipController (KatipController, KatipLoggerLocIO)
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import Katip
import Katip.Core
import Crypto.JWT

newtype User = User { email :: T.Text } deriving Show

instance FromJWT User where
  decodeJWT _ = undefined

instance ToJWT User where
  encodeJWT _ = emptyClaimsSet

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData basicAuthData authChecker = authChecker basicAuthData

withBasicAuth :: AuthResult User -> (User -> KatipController (Response a)) -> KatipController (Response a)
withBasicAuth (Authenticated user) runApi = runApi user
withBasicAuth _ _ = return $ Error $ asError @T.Text "only for authorized personal"

checkBasicAuth :: KatipLoggerLocIO ->  M.Map T.Text User -> BasicAuthData -> IO (AuthResult User)
checkBasicAuth log storage auth_data = do
  log getLoc InfoS $ ls $ show (basicAuthPassword auth_data, basicAuthUsername auth_data)
  return $  maybe Indefinite (const (Authenticated (User mempty))) $ T.decodeUtf8 (basicAuthPassword auth_data) `M.lookup` storage
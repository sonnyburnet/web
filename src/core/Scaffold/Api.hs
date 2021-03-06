{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api
       ( ApplicationApi (..)
       , HttpApi (..)
       , FileApi (..)
       , AdminApi (..)
     --  , WebsocketApi (..)
       , api
       , swaggerHttpApi
       ) where

import Scaffold.Api.Api
import Scaffold.Transport.Version

import BuildInfo
import Servant.API
import Servant.API.Generic
import Data.Proxy
import Servant.Swagger
import Data.Swagger
import Control.Lens
import Control.Lens.Iso.Extended
import Servant.Swagger.RawM ()
import Servant.Auth.Swagger ()
import Servant.Ip

newtype ApplicationApi route =
        ApplicationApi {
         _applicationApiHttp
         :: route
         :- ToServant HttpWrapperApi AsApi
       } deriving stock Generic

newtype HttpWrapperApi route =
        HttpWrapperApi
        { _httpWrapperApiApi
          :: route
          :- Description "http api: "
          :> "api"
          :> Capture "version" APIVersion
          :> HeaderIP
          :> ToServant HttpApi AsApi
        } deriving stock Generic

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

swaggerHttpApi :: String -> Int -> Version -> Swagger
swaggerHttpApi hs port ver =
  toSwagger (genericApi (Proxy @HttpWrapperApi))
  & schemes ?~ [Http, Https]
  & host ?~ Host hs (Just (fromIntegral port))
  & info.description ?~ "Scaffold server api"^.stext
  & info.version .~ show ver^.stext
  & info.contact ?~ Contact Nothing Nothing (Just ("fclaw007@gmail.com"^.stext))
  & info.title .~ "Scaffold. Tag (" <> $gitTag <> "). Commit (" <> $gitCommit <> ")"
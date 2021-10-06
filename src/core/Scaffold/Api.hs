{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Scaffold.Api
       ( ApplicationApi (..)
       , HttpApi (..)
       , FileApi (..)
     --  , WebsocketApi (..)
       , api
       , swaggerHttpApi
       ) where

import Scaffold.Api.Api

import BuildInfo
import Servant.API
import Servant.API.Generic
import Data.Proxy
import Servant.Swagger
import Data.Swagger
import Control.Lens
import Control.Lens.Iso.Extended
import Servant.Swagger.RawM ()

data ApplicationApi route =
     ApplicationApi
     { _applicationApiHttp
       :: route
       :- ToServant HttpWrapperApi AsApi
     } deriving stock Generic

newtype HttpWrapperApi route =
        HttpWrapperApi
        { _httpWrapperApiApi
          :: route
          :- Description "http api: "
          :> "api"
          :> "v1"
          :> ToServant HttpApi AsApi
        } deriving stock Generic

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

swaggerHttpApi :: String -> Int -> Version -> Swagger
swaggerHttpApi hs port ver =
  toSwagger (genericApi (Proxy :: Proxy HttpWrapperApi))
  & schemes ?~ [Http, Https]
  & host ?~ Host hs (Just (fromIntegral port))
  & info.description ?~ "Scaffold server api"^.stext
  & info.version .~ show ver^.stext
  & info.contact ?~ (Contact Nothing Nothing (Just ("fclaw007@gmail.com"^.stext)))
  & info.title .~ "Scaffold: global bank for intellectual capital. Tag (" <> $gitTag <> "). Commit (" <> $gitCommit <> ")"
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Scaffold.Api.Api
       ( HttpApi (..)
       , module File
       , module Protected
       ) where

import Scaffold.Api.File as File
import Scaffold.Api.Protected as Protected
import Scaffold.Auth

import Servant.API.Generic
import Servant.API
import Servant.Swagger.Tags
import qualified Servant.Auth.Server as SA

data HttpApi route =
     HttpApi
     {_httpApiFile
       :: route
       :- Tags "File"
       :> "file"
       :> ToServant FileApi AsApi
     , _httpApiAdmin
       :: route
       :- Tags "Admin"
       :> SA.Auth '[SA.BasicAuth] User
       :> "admin"
       :> ToServant AdminApi AsApi
     } deriving stock Generic
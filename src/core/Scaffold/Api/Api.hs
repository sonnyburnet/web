{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Scaffold.Api.Api
       ( HttpApi (..)
       , module Scaffold.Api.File
       ) where

import Scaffold.Api.File

import Servant.API.Generic
import Servant.API
import Servant.Swagger.Tags

data HttpApi route =
     HttpApi
     {_httpApiFile
       :: route
       :- Tags "File"
       :> "file"
       :> ToServant FileApi AsApi
     } deriving stock Generic
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Scaffold.Api.Http.Test (spec_api) where

import Scaffold.Api.File

import Test.Hspec
import Data.Proxy
import Servant.API.Generic
import Servant.Swagger.Test

spec_api :: Spec
spec_api =
  describe "Swagger spec for API v1" $ do
   context "ToJSON matches ToSchema (FileApi)" $
     validateEveryToJSON (genericApi (Proxy :: Proxy FileApi))
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.Protected (AdminApi (..)) where

import Scaffold.Transport.Response ( Response )

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended ( JSON, Get, type (:>) )

newtype AdminApi route =
        AdminApi {
          _adminApiTest
          :: route
          :- "test"
          :> Get '[JSON] (Response ())
        } deriving stock Generic
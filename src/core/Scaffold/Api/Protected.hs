{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.Protected (AdminApi (..)) where

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended ( Get, type (:>), JSON )
import Scaffold.Transport.Response ( Response )
import Data.Time (UTCTime)

newtype AdminApi route =
        AdminApi {
          _adminApiTest
          :: route
          :- "test"
          :> Get '[JSON] (Response UTCTime)
        } deriving stock Generic
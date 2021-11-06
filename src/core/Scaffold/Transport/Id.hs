{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Scaffold.Transport.Id (Id(..)) where

import Data.Int
import Data.Aeson
import Data.Binary
import Data.Hashable
import Data.Swagger
import GHC.Generics
import Servant.API
import Test.QuickCheck    (Arbitrary)
import TextShow
import Database.Transaction (ParamsShow (..))
import GHC.Types
import Data.Default.Class

-- | Type for ids that is shared across all the projects.
-- user id is int64, so it's encrypted as "text" in json,
-- otherwise js code may fail to work with it.
newtype Id (a :: Symbol) = Id Int64
  deriving newtype (Show, Eq)
  deriving newtype Arbitrary
  deriving newtype Binary
  deriving newtype Hashable
  deriving newtype Read
  deriving newtype Num
  deriving newtype Enum
  deriving newtype Real
  deriving newtype Integral
  deriving newtype TextShow
  deriving stock Generic
  deriving stock Ord
  deriving anyclass ToParamSchema
  deriving newtype FromHttpApiData
  deriving newtype ParamsShow
  deriving newtype ToJSON
  deriving newtype FromJSON

instance ToSchema (Id a)

instance Default (Id a)
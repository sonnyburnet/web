{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Scaffold.Transport.Payload (Payload(..), valueToPayload) where

import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Proxy
import Data.Swagger
import GHC.Generics
import Test.QuickCheck.Arbitrary

-- | Swagger friendly wrapper over any JSON object
newtype Payload = Payload { getPayload :: Object }
  deriving stock Eq
  deriving stock Show
  deriving stock Generic
  deriving newtype ToJSON
  deriving newtype FromJSON

instance Arbitrary Payload where arbitrary = pure $ Payload HM.empty

instance ToSchema Payload where
  declareNamedSchema _ =
    pure $
    NamedSchema (Just "Payload") $
    toSchema (Proxy :: Proxy Object)

valueToPayload :: Value -> Payload
valueToPayload (Object o) = Payload o
valueToPayload v = Payload $ HM.singleton "value" v

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Scaffold.Transport.Version (APIVersion) where

import Data.Swagger
import GHC.Generics
import Web.HttpApiData
import qualified Data.Text as T
import Control.Lens

data APIVersion = APIVersion Int deriving Generic

instance FromHttpApiData APIVersion where
  parseUrlPiece version =
    case "v" `T.stripPrefix` version of
      Just "" -> Left "url: /api/v{1}/..."
      Just v -> Right $ APIVersion $ read @Int $ T.unpack v
      Nothing -> Left "url: /api/v{1}/..."

instance ToParamSchema APIVersion where
  toParamSchema _ = mempty & type_ ?~ SwaggerString
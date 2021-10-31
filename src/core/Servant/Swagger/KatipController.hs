{-# LANGUAGE TypeFamilies #-}

module Servant.Swagger.KatipController (swaggerSchemaUIServerT) where

import KatipController

import Servant.Swagger.UI.Core
import Data.ByteString                (ByteString)
import qualified Data.Text as T
import Network.Wai.Application.Static (embeddedSettings, staticApp)
import Servant.Swagger.UI hiding (swaggerSchemaUIServerT)
import Servant
import Data.Swagger

swaggerSchemaUIServerT
  :: (ServerT api KatipController ~
      KatipController Swagger)
  => Swagger
  -> ServerT (SwaggerSchemaUI' dir api) KatipController
swaggerSchemaUIServerT =
  swaggerSchemaUIServerTImpl'
  swaggerUiIndexTemplate
  swaggerUiFiles

swaggerSchemaUIServerTImpl'
  :: (ServerT api KatipController ~ KatipController Swagger)
  => T.Text
  -> [(FilePath, ByteString)]
  -> Swagger
  -> ServerT (SwaggerSchemaUI' dir api) KatipController
swaggerSchemaUIServerTImpl' indexTemplate files server =
  return server :<|>
  return (SwaggerUiHtml indexTemplate) :<|>
  return (SwaggerUiHtml indexTemplate) :<|>
  Tagged (staticApp (embeddedSettings files))
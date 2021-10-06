{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Scaffold.Controller.File.Delete (controller) where

import Scaffold.Transport.Response
import Scaffold.Transport.Id
import Scaffold.Statement.File as File

import Katip
import KatipController
import Data.Aeson.Unit
import Control.Lens
import Database.Transaction
import Data.Int
import Data.Coerce
import Control.Lens.Iso.Extended
import Data.Bool
import BuildInfo

controller :: Id "file" -> KatipController (Response Unit)
controller id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @(Id "file") @Int64 id)^.stext <> "} not found"
  $(logTM) DebugS (logStr (show id))
  runTelegram $location id
  isOk <- katipTransaction hasql $ statement File.delete id
  return $ bool (Error (asError notFound)) (Ok Unit) isOk
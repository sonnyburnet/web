{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Scaffold.Controller.Controller (controller) where

import Scaffold.Api
-- controllers
import qualified Scaffold.Controller.File.Upload as File.Upload
import qualified Scaffold.Controller.File.Download as File.Download
import qualified Scaffold.Controller.File.Delete as File.Delete
import qualified Scaffold.Controller.File.Patch as File.Patch
import Servant.RawM.Server ()
import Scaffold.Auth
import Scaffold.Transport.Response

import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic
import Servant.Ip
import Control.Monad.Time
import BuildInfo
import Data.Functor

controller :: ApplicationApi (AsServerT KatipController)
controller = ApplicationApi { _applicationApiHttp = \_ ip -> toServant $ httpApi ip  }

httpApi :: Maybe IP4 -> HttpApi (AsServerT KatipController)
httpApi _ =
  HttpApi {
    _httpApiFile =
    toServant file
  , _httpApiAdmin =
    (`withBasicAuth` toServant . admin) }

file :: FileApi (AsServerT KatipController)
file =
  FileApi
  { _fileApiUpload = \bucket files ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "upload"])
    (File.Upload.controller bucket files)
  , _fileApiPatch = \fid file ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "patch"])
    (File.Patch.controller fid file)
  , _fileApiDelete =
      flip logExceptionM ErrorS
    . katipAddNamespace
     (Namespace ["file", "delete"])
    . File.Delete.controller
  , _fileApiDownload = \option fid w h ->
     flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["file", "download"])
     (File.Download.controller option fid w h) }

admin :: User -> AdminApi (AsServerT KatipController)
admin _ = AdminApi {
  _adminApiTest = do
    ct <- currentTime
    runTelegram $location $ show [1, 2]
    runTelegram $location (show ct) $> Ok ct }
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Scaffold.Application (Cfg (..), AppMonad (..), run) where

import Scaffold.Api
import qualified Scaffold.Controller.Controller as Controller
import qualified Scaffold.Config as Cfg
import qualified Scaffold.Transport.Response as Response
import Scaffold.Transport.Error

import KatipController
import Servant.Swagger.KatipController ()
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Katip
import qualified Network.Wai.Handler.Warp      as Warp
import Servant
import Servant.API.Generic
import Control.Lens
import Servant.Swagger.UI ()
import Servant.Auth.Server
import Control.Concurrent.Async
import Network.Wai
import Control.Lens.Iso.Extended
import qualified Middleware
import Control.Monad.RWS.Strict as RWS
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Network.Wai.Middleware.Cors
import Data.Generics.Product.Fields
import Servant.Multipart
import Network.Wai.Parse
import Network.HTTP.Types.Status
import TextShow
import BuildInfo
import qualified Network.Wai.Handler.WarpTLS as Warp
import System.Directory
import System.FilePath.Posix
import Control.Concurrent.Lifted
import Pretty
import Data.Either.Combinators
import Control.Exception
import Data.String.Conv
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Method
import Data.Coerce
import Data.Bool
import qualified Data.Text as T
import Data.Aeson
import Network.HTTP.Types.Header.Extended

data Cfg =
     Cfg
     { cfgHost         :: !String
     , cfgSwaggerPort  :: !Int
     , cfgServerPort   :: !Int
     , cfgCors          :: !Cfg.Cors
     , cfgServerError   :: !Cfg.ServerError
     }

newtype AppMonad a = AppMonad { runAppMonad :: RWS.RWST KatipEnv KatipLogger KatipState IO a }
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad
  deriving newtype MonadIO
  deriving newtype (MonadReader KatipEnv)
  deriving newtype (MonadState KatipState)
  deriving newtype (MonadWriter KatipLogger)
  deriving newtype (MonadRWS KatipEnv KatipLogger KatipState)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype MonadCatch
  deriving newtype MonadThrow

run :: Cfg -> KatipContextT AppMonad ()
run Cfg {..} = katipAddNamespace (Namespace ["application"]) $ do
  telegram_service <- fmap (^.telegram) ask
  let runTelegram l msg = void $ fork $ liftIO $ send telegram_service l ((mkPretty ("At module " <> $location) msg)^.stext)
  logger <- katipAddNamespace (Namespace ["application"]) askLoggerIO

  version_e <- liftIO getVersion
  runTelegram logger $ "server version " <> show version_e
  whenLeft version_e $ \e -> throwM $ ErrorCall e
  let Right ver = version_e

  runTelegram logger $ "server run on port " <> show cfgServerPort

  $(logTM) DebugS $ ls $ "server run on port " <> showt cfgServerPort
  configKatipEnv <- lift ask
  let initCfg = do
        configEnv <- getLogEnv
        configCtx <- getKatipContext
        configNm <-  getKatipNamespace
        return $ Config {..}
  cfg <- initCfg
  let withSwagger :: Proxy a -> Proxy a
      withSwagger _ = Proxy
  let context :: Proxy '[CookieSettings]
      context = Proxy
  let server = hoistServerWithContext (withSwagger api) context (runKatipController cfg (KatipControllerState 0)) (toServant Controller.controller)
        -- swaggerSchemaUIServerT' (swaggerHttpApi cfgHost cfgSwaggerPort ver))
  excep <-katipAddNamespace (Namespace ["exception"]) askLoggerIO
  ctx_logger <-katipAddNamespace (Namespace ["context"]) askLoggerIO
  req_logger <- katipAddNamespace (Namespace ["request"]) askLoggerIO
  let settings =
        Warp.defaultSettings
        & Warp.setPort cfgServerPort
        & Warp.setOnException (logUncaughtException excep runTelegram)
        & Warp.setOnExceptionResponse (`mk500Response` (coerce cfgServerError))
        & Warp.setServerName ("edgenode api server, revision " <> $gitCommit)
        & Warp.setLogger (logRequest req_logger runTelegram)
  let multipartOpts =
        (defaultMultipartOptions (Proxy :: Proxy Tmp))
        { generalOptions = clearMaxRequestNumFiles defaultParseRequestBodyOptions }
  let mkCtx = defaultCookieSettings :. EmptyContext
  let runServer = serveWithContext (withSwagger api) mkCtx server
  mware_logger <- katipAddNamespace (Namespace ["middleware"]) askLoggerIO

  path <- liftIO getCurrentDirectory
  let tls_settings = (Warp.tlsSettings (path </> "tls/certificate") (path </> "tls/key")) { Warp.onInsecure = Warp.AllowInsecure }

  servAsync <- liftIO $ async $ Warp.runTLS tls_settings settings (middleware cfgCors mware_logger runServer)
  mail_logger <- katipAddNamespace (Namespace ["mail"]) askLoggerIO
  liftIO (void (waitAnyCancel [servAsync])) `logExceptionM` ErrorS

middleware :: Cfg.Cors -> KatipLoggerIO -> Application -> Application
middleware cors log app = mkCors cors $ Middleware.logger log app

logUncaughtException :: KatipLoggerIO -> (KatipLoggerIO -> String -> IO ()) -> Maybe Request -> SomeException -> IO ()
logUncaughtException log runTelegram req e = when (Warp.defaultShouldDisplayException e) $ maybe without within req
  where without = do
          runTelegram log $ "before request being handled" <> show e
          log ErrorS (logStr ("before request being handled" <> show e))
        within r = do
          runTelegram log $ "\"" <> toS (requestMethod r) <> " " <> toS (rawPathInfo r) <> " " <> toS (show (httpVersion r)) <> "500 - " <> show e
          log ErrorS (logStr ("\"" <> toS (requestMethod r) <> " " <> toS (rawPathInfo r) <> " " <> toS (show (httpVersion r)) <> "500 - " <> show e))

mk500Response :: SomeException -> Bool -> Response
mk500Response error = bool
  (responseLBS status200
   [(H.hContentType, "application/json; charset=utf-8"),
    (hAccessControlAllowOrigin, "*")] $
   encode @(Response.Response ()) $
   (Response.Error (asError @T.Text (showt error))))
  (responseLBS status500
   [(H.hContentType, "text/plain; charset=utf-8"),
    (hAccessControlAllowOrigin, "*")] (showt error^.textbsl))

logRequest :: KatipLoggerIO -> (KatipLoggerIO -> String -> IO ()) -> Request -> Status -> Maybe Integer -> IO ()
logRequest log runTelegram req _ _ = log InfoS (logStr (show req)) >> runTelegram log (mkPretty mempty req)

deriving instance Generic CorsResourcePolicy

mkCors :: Cfg.Cors -> Middleware
mkCors cfg_cors =
  cors $ const $ pure $
    simpleCorsResourcePolicy
    & field @"corsOrigins" .~ fmap ((, True) . map toS) (Cfg.corsOrigins cfg_cors)
    & field @"corsRequestHeaders" .~ [hAuthorization, hContentType, hOrigin, hAccessControlAllowOrigin]
    & field @"corsMethods" .~ simpleMethods <> [methodPut, methodPatch, methodDelete, methodOptions]
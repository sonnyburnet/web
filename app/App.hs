{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module App (main) where

import qualified Scaffold.Application as App
import Scaffold.Config
import Scaffold.Auth (User (User))

import KatipController
import BuildInfo (gitCommit)
import Pretty
import qualified Database.Migration as Migration
import Control.Lens hiding (Wrapped, Unwrapped)
import Data.Monoid.Colorful (hGetTerm)
import Katip
import qualified Hasql.Connection as HasqlConn
import Control.Lens.Iso.Extended
import Control.Monad
import Data.Default.Class
import Control.Monad.RWS.Strict (evalRWST)
import qualified Network.HTTP.Client.TLS as Http
import Network.HTTP.Client
       ( ManagerSettings
         ( managerConnCount
         , managerResponseTimeout)
       , responseTimeoutMicro)
import Options.Generic
import Data.Maybe
import System.FilePath.Posix
import Control.Monad.IO.Class
import qualified Data.Pool as Pool
import Control.Exception
import qualified Network.Minio as Minio
import Data.String
import System.IO
import qualified Web.Telegram
import Logo
import Data.Time.Clock.System
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Cmd w =
     Cmd
     { cfgPath :: w ::: FilePath <?> "config file path"
     , localhost :: w ::: Maybe String <?> "override db host if needed, used along with port"
     , localport :: w ::: Maybe Int <?> "override db port if needed"
     , pathToKatip :: w ::: Maybe FilePath <?> "path to katip log"
     , pathToJwk :: w ::: Maybe FilePath <?> "path to jwk"
     , minioHost :: w ::: Maybe String <?> "minio host"
     , minioPort :: w ::: Maybe String <?> "minio port"
     , swaggerHost :: w ::: Maybe String <?> "swagger host"
     , swaggerPort :: w ::: Maybe Int <?> "swagger port"
     , serverPort :: w ::: Maybe Int <?> "server port"
     ,cfgAdminStoragePath :: w ::: FilePath <?> "admin storage"
     } deriving stock Generic

instance ParseRecord (Cmd Wrapped)
deriving instance Show (Cmd Unwrapped)

main :: IO ()
main = do
  Cmd {..} <- unwrapRecord "scaffold"
  rawCfg <- Scaffold.Config.load cfgPath
  let cfg =
        rawCfg
        & db.host %~ (`fromMaybe` localhost)
        & db.port %~ (`fromMaybe` localport)
        & katip.path %~ (\path -> maybe path (</> path) pathToKatip)
        & Scaffold.Config.minio.host %~ (`fromMaybe` minioHost)
        & Scaffold.Config.minio.port %~ (`fromMaybe` minioPort)
        & swagger.host %~ (`fromMaybe` swaggerHost)
        & swagger.port %~ (`fromMaybe` swaggerPort)
        & serverConnection.port %~ (`fromMaybe` serverPort)
  pPrint cfg
  haskellSay "Welcome to Scaffolding!!!"

  term <- hGetTerm stdout
  hSetBuffering stdout NoBuffering

  hasqlpool <- Pool.createPool
    (HasqlConn.acquire (mkRawConn (cfg^.db)) >>=
      either (throwIO . ErrorCall . maybe "hasql connection error" (^.from textbs.from stext)) pure)
    HasqlConn.release
    (cfg^.hasql.poolN)
    (cfg^.hasql.tm)
    (cfg^.hasql.resPerStripe)

  std <- mkHandleScribeWithFormatter
          jsonFormat
          ColorIfTerminal
          stdout
          (permitItem (cfg^.katip.severity.from stringify))
          (cfg^.katip.verbosity.from stringify)
  tm <- fmap systemSeconds getSystemTime
  let katipFilePath = cfg^.katip.path <> "/" <> show tm <> ".log"
  fileHdl <- openFile katipFilePath AppendMode

  mapM_ (`hSetEncoding` utf8) [stdout, stderr, fileHdl]

  file <- mkHandleScribe
          (ColorLog True)
          fileHdl
          (permitItem (cfg^.katip.severity.from stringify))
          (cfg^.katip.verbosity.from stringify)
  let mkNm = Namespace [("<" ++ $(gitCommit) ++ ">")^.stext]
  init_env <- initLogEnv mkNm (cfg^.katip.Scaffold.Config.env.isoEnv.stext.coerced)
  let env = do
        env' <- registerScribe "stdout" std defaultScribeSettings init_env
        registerScribe "file" file defaultScribeSettings env'

  admin_storage <- withFile cfgAdminStoragePath ReadMode $ \h -> do
    content <- T.hGetContents h
    let xs = flip foldMap (T.splitOn "," content) $ \x ->
              case T.splitOn ":" x of
                [pass, email] -> [(pass, User email)]
                _ -> []
    return $ Map.fromList xs

  let appCfg =
        App.Cfg
        (cfg^.swagger.host.coerced)
        (cfg^.swagger.port)
        (cfg^.serverConnection.port)
        (cfg^.cors)
        (cfg^.serverError)
        admin_storage
  let runApp le =
        runKatipContextT le (mempty :: LogContexts) mempty $ do
          logger <- katipAddNamespace (Namespace ["db", "migration"]) askLoggerIO
          liftIO $ Migration.run hasqlpool logger
          App.run appCfg
  manager <- Http.newTlsManagerWith Http.tlsManagerSettings
    { managerConnCount = 1
    , managerResponseTimeout =
      responseTimeoutMicro (5 * 10^6) }

  minioEnv <-  flip Minio.mkMinioConn manager $
    Minio.setCreds
    (Minio.Credentials
     (cfg^.Scaffold.Config.minio.accessKey)
     (cfg^.Scaffold.Config.minio.secretKey))
    (fromString (cfg^.Scaffold.Config.minio.host <> ":" <> cfg^.Scaffold.Config.minio.port))

  telegram <- Web.Telegram.mkService manager (cfg^.Scaffold.Config.telegram)

  let katipMinio = Minio minioEnv (cfg^.Scaffold.Config.minio.Scaffold.Config.bucketPrefix)
  let katipEnv = KatipEnv term hasqlpool manager (cfg^.service.coerced) katipMinio telegram

  bracket env closeScribes (void . (\x -> evalRWST (App.runAppMonad x) katipEnv def) . runApp)

mkRawConn :: Db -> HasqlConn.Settings
mkRawConn x = HasqlConn.settings (x^.host.stext.textbs) (x^.port.to fromIntegral) (x^.Scaffold.Config.user.stext.textbs) (x^.pass.stext.textbs) (x^.database.stext.textbs)
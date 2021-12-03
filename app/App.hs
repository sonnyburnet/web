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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module App (main) where

import qualified Scaffold.Application as App
import Scaffold.Config
import Scaffold.Auth (User (User))

import KatipController
import BuildInfo (gitCommit)
import Pretty
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
import qualified Data.Pool as Pool
import Control.Exception
import qualified Network.Minio as Minio
import Data.String
import System.IO
import qualified Web.Telegram
import Data.Time.Clock.System
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable (for_)
import Data.Char (toLower, isUpper)
import qualified Text.Read.Lex as L
import GHC.Read
import Text.ParserCombinators.ReadPrec (pfail)

data PrintCfg = Y | N deriving stock (Generic)

instance Show PrintCfg where
  show Y = "y"
  show N = "n"

instance Read PrintCfg where
  readPrec =
    parens
    (do L.Ident s <- lexP
        case s of
          "y" -> return Y
          "n" -> return N
          _   -> pfail)

instance ParseField PrintCfg

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
     , cfgAdminStoragePath :: w ::: FilePath <?> "admin storage"
     , migration :: w ::: FilePath <?> "migration path"
     , printCfg :: w ::: Maybe PrintCfg  <?> "whether config be printed"
     } deriving stock Generic

deriving instance Show (Cmd Unwrapped)

instance ParseRecord (Cmd Wrapped) where
  parseRecord =
    parseRecordWithModifiers
    defaultModifiers {
      fieldNameModifier = toSnake }

{-|
   Convert CamelCased or mixedCases 'String' to a 'String' with underscores,
   the \"snake\" 'String'.
   It splits an input value to chunks by 'isUpper' predicate,
   then adds underscores to each element except the first.
   Finally concats the result and convers it downcase.
-}
toSnake :: String -> String
toSnake = map toLower . concat . underscores . splitR isUpper
  where
    underscores [] = []
    underscores (h:t) = h : map ('_':) t
    splitR _ [] = []
    splitR p s =
      let go m s' =
            case break p s' of
              (b', [])     -> [ m:b' ]
              (b', x:xs) -> ( m:b' ) : go x xs
      in case break p s of
        (b,  [])    -> [ b ]
        ([], h:t) -> go h t
        (b,  h:t) -> b : go h t

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
  for_ printCfg $ \case Y -> pPrint cfg; N -> pure ()

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
    return $ Map.fromList $
      flip foldMap (T.splitOn "," content) $ \x ->
        case T.splitOn ":" x of
          [pass, email] -> [(pass, User email)]
          _ -> []

  let appCfg =
        App.Cfg
        (cfg^.swagger.host.coerced)
        (cfg^.swagger.port)
        (cfg^.serverConnection.port)
        (cfg^.cors)
        (cfg^.serverError)
        admin_storage
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

  let runApp le = runKatipContextT le (mempty @LogContexts) mempty $ App.run appCfg
  bracket env closeScribes $ void . (\x -> evalRWST (App.runAppMonad x) katipEnv def) . runApp

mkRawConn :: Db -> HasqlConn.Settings
mkRawConn x = HasqlConn.settings (x^.host.stext.textbs) (x^.port.to fromIntegral) (x^.Scaffold.Config.user.stext.textbs) (x^.pass.stext.textbs) (x^.database.stext.textbs)
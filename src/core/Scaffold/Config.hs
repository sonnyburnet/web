{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Scaffold.Config
       ( Config
       , Katip (..)
       , Db
       , Service (..)
       , ApiKeys (..)
       , Swagger (..)
       , Telegram (..)
       , Env (..)
       , Cors (..)
       , Smtp (..)
       , ServerError (..)
       , db
       , pass
       , port
       , database
       , host
       , user
       , poolN
       , tm
       , hasql
       , resPerStripe
       , katip
         -- * load config
       , load
       , path
       , verbosity
       , severity
       , env
       , service
       , accessKey
       , secretKey
       , minio
       , bucketPrefix
       , swagger
       , bot
       , chat
       , telegram
       , serverConnection
       , cors
       , origins
       , smtp
       , serverError
         -- * Iso
       , isoEnv
       )
       where

import Data.Aeson
import Data.Aeson.TH.Extended
import Control.Lens
import Control.Exception
import Data.Yaml
import Data.Time.Clock
import qualified Data.Text as T
import TH.Mk
import Data.String.Conv
import GHC.Generics


data Db = Db
     { dbHost :: !String
     , dbPort :: !Int
     , dbUser :: !String
     , dbPass :: !String
     , dbDatabase :: !String
     } deriving Show

data Swagger = Swagger { swaggerHost :: String, swaggerPort :: Int }
  deriving Show

data HasqlSettings =
     HasqlSettings
     { hasqlSettingsPoolN :: !Int
     , hasqlSettingsTm :: !NominalDiffTime
     , hasqlSettingsResPerStripe :: !Int
     } deriving Show

data Env = Prod | Dev deriving (Show, Eq)

mkEnumConvertor ''Env

instance FromJSON Env where parseJSON = withText "Scaffold.Config:Env" (pure . toEnv . toS)

data Katip =
     Katip
     { katipPath :: !FilePath
     , katipSeverity :: !String
     , katipVerbosity :: !String
     , katipEnv :: !Env
     } deriving Show

newtype ApiKeys = ApiKeys [(String, String)]
  deriving Show

newtype Service = Service { serviceApiKeys :: ApiKeys }
  deriving Show

data Minio =
     Minio
     { minioAccessKey :: !T.Text
     , minioSecretKey :: !T.Text
     , minioBucketPrefix :: !T.Text
     , minioHost :: !String
     , minioPort :: !String
     } deriving Show

data Telegram =
     Telegram
     { telegramBot  :: !T.Text
     , telegramChat :: !T.Text
     , telegramHost :: !T.Text
     , telegramEnv  :: !Env
     } deriving Show

newtype ServerConnection = ServerConnection { serverConnectionPort :: Int }
  deriving Show

newtype Cors = Cors { corsOrigins :: (Maybe [T.Text]) } deriving Show

newtype ServerError = ServerError { serverErrorMk500 :: Bool }
  deriving stock Generic
  deriving newtype FromJSON
  deriving stock Show

data Smtp =
     Smtp
     { smtpServer         :: !T.Text
     , smtpLogin          :: !T.Text
     , smtpPassword       :: !T.Text
     , smtpEmail          :: !T.Text
     , smtpDefaultSubject :: !T.Text
     } deriving Show

data Config =
     Config
     { configDb               :: !Db
     , configSwagger          :: !Swagger
     , configHasql            :: !HasqlSettings
     , configKatip            :: !Katip
     , configService          :: !Service
     , configMinio            :: !Minio
     , configTelegram         :: !Telegram
     , configServerConnection :: !ServerConnection
     , configCors             :: !Cors
     , configSmtp             :: !Smtp
     , configServerError      :: !ServerError
     } deriving Show

makeFields ''Config
makeFields ''Db
makeFields ''HasqlSettings
makeFields ''Katip
makeFields ''Minio
makeFields ''Swagger
makeFields ''Telegram
makeFields ''ServerConnection
makeFields ''Cors
makeFields ''Smtp

-- Load program configuration from file (server.yaml), or
-- raise YamlException and terminate program.
load :: FilePath -> IO Config
load path = decodeFileEither path >>= either throwIO pure

deriveFromJSON defaultOptions ''Db
deriveFromJSON defaultOptions ''Config
deriveFromJSON defaultOptions ''HasqlSettings
deriveFromJSON defaultOptions ''Katip
deriveFromJSON defaultOptions ''ApiKeys
deriveFromJSON defaultOptions ''Service
deriveFromJSON defaultOptions ''Minio
deriveFromJSON defaultOptions ''Swagger
deriveFromJSON defaultOptions ''Telegram
deriveFromJSON defaultOptions ''ServerConnection
deriveFromJSON defaultOptions ''Cors
deriveFromJSON defaultOptions ''Smtp
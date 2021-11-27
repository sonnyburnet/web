{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Scaffold.Controller.File.Patch (controller) where

import Scaffold.Transport.Response
import Scaffold.Transport.Id
import Scaffold.Statement.File as File
import Scaffold.Transport.Model.File

import KatipController
import Data.Aeson.Unit
import Servant.Multipart.File
import Control.Lens
import Database.Transaction
import Katip
import Data.Int
import Data.Either.Combinators
import Data.Coerce
import Control.Lens.Iso.Extended
import Data.Traversable
import Control.Monad
import Network.Minio hiding (Bucket)
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import BuildInfo

controller :: Id "file" -> File -> KatipController (Response Unit)
controller id file = do
  runTelegram $location (id, file)
  $(logTM) DebugS (logStr (show (id, file)))
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @(Id "file") @Int64 id)^.stext <> "} not found"
  resp <- fmap (maybeToRight (asError notFound)) $
    katipTransaction hasql $ statement File.getHashWithBucket id
  let patch (hash, bucket) = do
        Minio {..} <- fmap (^.katipEnv.minio) ask
        void $ katipTransaction hasql $
          statement
          File.patch
          ( Name (UnicodeText (fileName file))
          , Mime (UnicodeText (fileMime file))
          , hash)
        minioRes <- liftIO $ runMinioWith minioConn $
          fPutObject
          (minioBucketPrefix <> "." <> coerce @Bucket bucket)
          (coerce hash)
          (filePath file)
          defaultPutObjectOptions
        whenLeft minioRes $ \e -> do
          $(logTM) ErrorS (logStr (show e))
          throwError undefined
        return Unit
  fromEither <$> for resp patch
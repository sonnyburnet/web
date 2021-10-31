{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Scaffold.Controller.File.Upload (controller) where

import Scaffold.Transport.Response
import Scaffold.Statement.File as File
import Scaffold.Transport.Id
import Scaffold.Model.File

import Servant.Multipart.File
import KatipController
import Control.Lens
import Network.Minio
import Control.Monad.IO.Class
import Control.Lens.Iso.Extended
import Katip
import Control.Monad
import Hash
import Database.Transaction
import Data.Time.Clock
import Data.Traversable
import Data.Coerce
import Data.Either
import BuildInfo
import qualified Data.Text as T
import System.Timeout
import Data.Foldable

controller :: T.Text -> Files -> KatipController (Response [Id "file"])
controller bucket x = do
  runTelegram $location (bucket, x)
  $(logTM) DebugS (logStr (show (bucket, x)))
  Minio {..} <- fmap (^.katipEnv.minio) ask
  es <- for (coerce x) $ \File {..} -> do
    tm <- liftIO getCurrentTime
    let hash = mkHash (fileName <> fileMime <> (show tm^.stext))
    minioResult <- liftIO $ timeout (5 * 10 ^ 6) $ runMinioWith minioConn $ do
      let newBucket = minioBucketPrefix <> "." <> bucket
      exist <- bucketExists newBucket
      unless exist $
        makeBucket
        (minioBucketPrefix <> "." <> bucket)
        Nothing
      fPutObject newBucket hash filePath defaultPutObjectOptions
    $(logTM) DebugS (logStr (show minioResult))
    let tpl =
          ( Hash (UnicodeText hash)
          , Name (UnicodeText fileName)
          , Mime (UnicodeText fileMime)
          , bucket)
    return $ maybe  (Left (MErrIO (userError "minio server didn't respond"))) (fmap (const tpl)) minioResult
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let (error_xs, success_xs) = partitionEithers es
  ids <- katipTransaction hasql $ statement File.save success_xs
  for_ error_xs $ runTelegram $location
  return $ Warnings ids (map (asError . (\e -> show e^.stext)) error_xs)
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Scaffold.Controller.File.Upload (controller) where

import Scaffold.Transport.Response
import Scaffold.Statement.File as File
import Scaffold.Transport.Id
import Scaffold.Transport.Model.File

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
import System.Directory
import System.FilePath

controller :: T.Text -> Files -> KatipController (Response [Id "file"])
controller bucket x = do
  runTelegram $location (bucket, x)
  $(logTM) DebugS (logStr (show (bucket, x)))
  Minio {..} <- fmap (^.katipEnv.minio) ask
  es <- for (coerce x) $ \file@File {..} -> do
    tm <- liftIO getCurrentTime
    let hash = mkHash (fileName <> fileMime <> (show tm^.stext))
    tmp <- liftIO getTemporaryDirectory
    let new_file_path = tmp </> T.unpack (mkHash file)
    liftIO $ copyFile filePath new_file_path
    runTelegram $location file { filePath = new_file_path }
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
  return $ case ids of
    [] -> Errors $ map (asError . (\e -> show e^.stext)) error_xs
    _ -> Warnings ids (map (asError . (\e -> show e^.stext)) error_xs)
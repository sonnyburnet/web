{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Scaffold.Controller.File.Download (controller, Option) where

import Scaffold.Transport.Id
import Scaffold.Statement.File as File
import Scaffold.Model.File
import Scaffold.Transport.Error
import qualified Scaffold.Transport.Response as Response

import Katip
import KatipController
import Network.Wai
import qualified Data.ByteString as B
import Network.HTTP.Types
import Database.Transaction
import Control.Lens
import Data.Traversable
import Network.Minio
import Control.Monad.IO.Class
import Conduit
import qualified Data.Text as T
import Data.Either.Combinators
import Data.Coerce
import Control.Lens.Iso.Extended
import Data.Int
import Control.Monad
import Data.Bifunctor
import Data.Aeson
import Data.Time.Clock
import Data.Text.Encoding
import qualified Data.ByteString.Base64 as B64
import TH.Mk
import Network.HTTP.Types.Header
import Hash
import System.Process (readProcess)
import Data.Foldable
import BuildInfo
import qualified Network.HTTP.Types as H
import Data.String.Conv

data Option = Embedded | Raw deriving Show

mkEnumConvertor ''Option
mkParamSchemaEnum ''Option [|isoOption.stext.to String|]
mkFromHttpApiDataEnum ''Option [|from stext.from isoOption.to Right|]

imageMimeTypes = ["image/apng", "image/bmp", "image/gif", "image/x-icon", "image/jpeg", "image/png", "image/svg+xml", "image/tiff", "image/webp"]

controller :: Option -> Id "file" -> Maybe Int -> Maybe Int -> KatipController Application
controller option id width_m height_m = do
  runTelegram $location (option, id, width_m, height_m)
  $(logTM) DebugS (logStr (show (option, id, width_m, height_m)))
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @(Id "file") @Int64 id)^.stext <> "} not found"
  meta <- fmap (maybeToRight (asError notFound)) $
    katipTransaction hasql $ statement File.getMeta id
  minioResp <- fmap join $ for meta $ \x -> do
    Minio {..} <- fmap (^.katipEnv.minio) ask
    let bucket = minioBucketPrefix <> "." <> x^._4.coerced
    r <- liftIO $ runMinioWith minioConn $ do
      o <- getObject bucket (x^._1.coerced) defaultGetObjectOptions
      let size = oiSize (gorObjectInfo o)
      tm <- fmap show $ liftIO getCurrentTime
      path <- runConduit $
        gorObjectStream o .|
        sinkSystemTempFile
        (x^._1.coerced.from stext <> "_" <> tm)
      when (x^._3.coerced @Mime @_ @T.Text @_ `elem` imageMimeTypes) $ do
        let size = do
              h <- height_m
              w <- width_m
              pure $ show h ++ "x" ++ show w
        for_ size $ \s -> liftIO $ readProcess "convert" [path, "-resize", s, path] mempty
      payload <- liftIO $ B.readFile path
      return (payload, size, x^._2.coerced @Name @_ @T.Text @_, x^._3.coerced @Mime @_ @T.Text @_)
    return $ first (asError . (\e -> show e^.stext)) r
  runTelegram $location (second (^._3) minioResp)
  $(logTM) DebugS (logStr (show (second (^._3) minioResp)))
  return $ \req resp -> case option of Embedded -> embedded req resp minioResp; Raw -> raw req resp minioResp

embedded
  :: Request
  -> (Response -> IO ResponseReceived)
  -> Either Error (B.ByteString, Int64, T.Text, T.Text)
  -> IO ResponseReceived
embedded req resp _ | requestMethod req /= methodGet =
  resp $
  responseLBS status200 [(H.hContentType, "application/json; charset=utf-8")] $
  encode @(Response.Response ()) $
  (Response.Error (asError @T.Text ("only " <> toS methodGet <> " allowed")))
embedded _ resp minioResp = resp $
  case minioResp of
    Right minio ->
      responseLBS status200 [(hContentType, (minio^._4.textbs))] $
        encode (Response.Ok $ decodeUtf8 $ B64.encode (minio^._1))
    Left e -> responseLBS status200 [] $ encode $ (Response.Error e :: Response.Response ())

raw
  :: Request
  -> (Response -> IO ResponseReceived)
  -> Either Error (B.ByteString, Int64, T.Text, T.Text)
  -> IO ResponseReceived
raw req resp _ | requestMethod req /= methodGet = resp $ responseLBS status405 [] mempty
raw _ resp (Right minio) = resp $
  responseLBS status200
  [ (hContentType, (minio^._4.textbs))
  , (hContentDisposition,
    "attachment;filename=" <>
    (mkHash (minio^._3)^.textbs))] $ (minio^._1.from bytesLazy)
raw _ resp _ = resp $ responseLBS status404 [] "image not found"
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Middleware (logger) where

import Network.Wai
import KatipController
import System.CPUTime
import Katip
import Text.Printf
import Control.Lens.Iso.Extended
import Control.Lens

logger :: KatipLoggerIO -> Middleware
logger log app req runResp = do
  start <- getCPUTime
  app req $ \resp -> do
    x <- runResp resp
    end <- getCPUTime
    let mills :: Double
        mills =
         fromIntegral
         (end - start)
         * 1e-12
    let duration :: String
        duration =
         printf
         "duration: %.2f sec"
         mills
    let message =
         "response.http.status: " <>
         show (responseStatus resp) <>
         ", request.http.rawPathInfo: " <>
         (rawPathInfo req^.from textbs.from stext) <>
         ", " <> duration
    log InfoS (ls message)
    return x
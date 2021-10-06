{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Web.Telegram (Service (..), mkService) where

import Scaffold.Config (Telegram (..), Env (..))

import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import Data.String.Conv
import Control.Monad
import Katip
import Control.Monad.Catch
import qualified Data.ByteString.Lazy as BL
import Data.Foldable

newtype Service = Service { send :: (Severity -> LogStr -> IO ()) -> T.Text -> IO () }

mkService :: HTTP.Manager -> Telegram -> IO Service
mkService mgr Telegram {..} = do
  let url = telegramHost <> telegramBot <> "/sendMessage"
  req <- HTTP.parseRequest $ T.unpack url
  let send logger msg = catch @IO @HTTP.HttpException (
        when (telegramEnv == Dev) $ do
          for_ (splitByteString (toS msg)) $ \chunk -> do
            void $ logger DebugS (ls ("telegram req: " <> chunk))
            response <- flip HTTP.httpLbs mgr $
              HTTP.urlEncodedBody
              [ ("chat_id", toS ("@" <> telegramChat))
              , ("text", "`" <> toS chunk <> "`")
              , ("parse_mode", "markdown")
              ] req { HTTP.method = "POST" }
            let response_status = HTTP.statusCode $ HTTP.responseStatus response
            let response_body = toS $ HTTP.responseBody response
            let log_msg =
                  "telegram response with status " <>
                  show response_status <> ": " <> response_body
            void $ logger DebugS (ls ("telegram resp: " <> log_msg))) $
          \e -> void $ logger ErrorS $ ls (show e)
  return Service {..}

splitByteString :: BL.ByteString -> [BL.ByteString]
splitByteString = reverse . split []
  where split xs source | BL.length source < 4096 = source : xs
        split xs old = let (x, new) = BL.splitAt 4096 old in split (x:xs) new
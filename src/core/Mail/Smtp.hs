{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

-- | This is a fairly simple module that just connects the smtp-mail package
-- with the tls package. Hopefully this will make it easier to connect to, say,
-- GMail.
--
-- This is just this SO answer, http://stackoverflow.com/a/13634590/34864,
-- wrapped in a module and packaged up.

module Mail.Smtp
    ( sendMailTls
    , sendMailTlsDefPort
    , ciphers
    , tlsParams
    ) where

import Control.Monad (unless, forM_)
import qualified Crypto.Random.AESCtr as RNG
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Mail.Mime
import Network.Mail.SMTP
import Network.Socket
import Network.TLS
import Network.TLS.Extra
import System.IO
import Text.Printf
import System.Timeout
import Control.Exception
import Data.String.Conv

ciphers :: [Cipher]
ciphers = ciphersuite_default

tlsParams :: String ->  B.ByteString -> ClientParams
tlsParams h p =
  (defaultParamsClient h p)
  { clientSupported =
    (clientSupported (defaultParamsClient h p))
    { supportedCiphers = ciphers }
  , clientHooks =
    (clientHooks (defaultParamsClient h p))
    { onServerCertificate = \_ _ _ _ -> pure [] }
  }

write :: Handle -> String -> IO ()
write h = hPrintf h "%s\r\n"
        -- printf ">>> %s\n" cmd
        -- hFlush stdout

waitFor :: Handle -> String -> IO ()
waitFor h str = do
        ln <- hGetLine h
      --  putStrLn $ "<<< " <> ln
        unless (str `isPrefixOf` ln) (waitFor h str)
        hFlush stdout

class Writeable a where
        toCommand :: a -> BL.ByteString
        toDebug   :: a -> String

instance Writeable String where
        toCommand = BL.fromChunks
                  . (:[])
                  . TE.encodeUtf8
                  . T.pack
                  . printf "%s\r\n"
        toDebug = id

instance Writeable B.ByteString where
        toCommand = BL.fromChunks . (:["\r\n"])
        toDebug   = T.unpack . TE.decodeUtf8

instance Writeable BCL.ByteString where
        toCommand = (<> crlf)
        toDebug   = toDebug . mconcat . BCL.toChunks

tlsWrite :: Writeable a => Context -> a -> IO ()
tlsWrite ctx cmd = do
        sendData ctx $ toCommand cmd
        contextFlush ctx
        -- printf ">>> %s\n" $ toDebug cmd
        -- hFlush stdout

crlf :: BL.ByteString
crlf = BCL.pack "\r\n"

-- TODO: Add timeout
tlsWaitFor :: Context -> T.Text -> IO ()
tlsWaitFor ctx str = do
        lns <- T.lines . TE.decodeUtf8 <$> recvData ctx
        -- forM_ lns $ printf . T.unpack . ("<<< " <>) . (<> "\n")
        -- hFlush stdout
        case filter (T.isPrefixOf str) lns of
            [] -> tlsWaitFor ctx str
            _  -> return ()

tlsWriteWait :: Writeable a => Context -> a -> T.Text -> IO ()
tlsWriteWait ctx cmd waitFor = do
  resp <- timeout (10 ^ 6) (tlsWrite ctx cmd >> tlsWaitFor ctx waitFor)
  case resp of
    Just _ -> pure ()
    Nothing -> throwIO $ ErrorCall ("error occured while sending mail at command \"" <> toS (toCommand cmd) <> "\"")

sendMailTlsDefPort :: String -> UserName -> Password -> Mail -> IO ()
sendMailTlsDefPort host = sendMailTls host 587

b64 :: String -> B.ByteString
b64 = B64.encode . B.pack . map (toEnum . fromEnum)

-- TODO: option to pass in TLSParams
-- TODO: run in EitherT.
writeAddress :: Context -> T.Text -> Address -> IO ()
writeAddress ctx cmd Address{..} = tlsWriteWait ctx (T.unpack $ cmd <> ":<" <> addressEmail <> ">") "250"

sendMailTls :: String -> Int -> UserName -> Password -> Mail -> IO ()
sendMailTls host port user passwd mail = do
  g <- RNG.makeSystem
  let pn = show port
  let hints = defaultHints { addrSocketType = Stream }
  addr <- head <$> getAddrInfo (Just hints) (Just host) (Just pn)
  s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect s $ addrAddress addr
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering

  write h "EHLO edgenode.org"
  waitFor h "250-STARTTLS"
  write h "STARTTLS"
  waitFor h "220"

  ctx <- contextNew h (tlsParams host mempty)
  let sendLine = tlsWrite ctx

  handshake ctx

  tlsWriteWait ctx ("EHLO edgenode.org" :: String) "250"
  tlsWriteWait ctx ("AUTH LOGIN" :: String) "334"
  tlsWriteWait ctx (b64 user) "334"
  tlsWriteWait ctx (b64 passwd) "235"

  mailbs <- renderMail' mail
  writeAddress ctx "MAIL FROM" $ mailFrom mail
  let rcpts = concatMap (\f -> f mail) [mailTo, mailCc, mailBcc]
  forM_ rcpts $ writeAddress ctx "RCPT TO"
  tlsWriteWait ctx ("DATA" :: String) "354"

  mapM_ sendLine . concatMap BCL.toChunks $ split mailbs
  mapM_ sendLine $ BCL.toChunks dot
  tlsWaitFor ctx "250"
  bye ctx

  where
    split = map (padDot . stripCR) . BCL.split '\n'
    -- remove \r at the end of a line
    stripCR s = if cr `BL.isSuffixOf` s then BL.init s else s
    -- duplicate . at the start of a line
    padDot s = if dot `BL.isPrefixOf` s then dot <> s else s
    cr = BCL.pack "\r"
    dot = BCL.pack "."
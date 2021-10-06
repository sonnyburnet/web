{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Ip (HeaderIP, module Network.IP.Addr) where

import Data.Proxy
import Data.Textual
import Data.Typeable
import Data.Monoid
import Servant.API
import Servant.Client.Core (HasClient(..), Client(..))
import Servant.Server
import Servant.Server.Internal
import Servant.Swagger
import Network.IP.Addr
import Network.Wai
import Network.Socket
import Control.Applicative

-- | Parses API from @X-REAL-IP@ or @X-FORWARD-FOR@ headers.
data HeaderIP deriving Typeable

instance (HasServer api context) => HasServer (HeaderIP :> api) context where
  type ServerT (HeaderIP :> api) m = Maybe IP4 -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy:: Proxy api) pc nt . s

  route Proxy context subserver = route (Proxy :: Proxy api) context $
      subserver `addHeaderCheck` withRequest headerCheck
    where
      headerCheck :: Request -> DelayedIO (Maybe IP4)
      headerCheck req =
        pure $ ((maybeParsed . parseAscii) =<<
          (getFirst (mconcat
            [ First $ y `lookup` requestHeaders req
              | y <- ["x-real-ip", "x-forward-for"]
            ]))) <|> (getRemoteAddress (remoteHost req))
      getRemoteAddress (SockAddrInet _ addr) = Just $ IP4 addr
      getRemoteAddress _ = Nothing

instance (HasSwagger api) => HasSwagger (HeaderIP :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)

instance HasClient m api => HasClient m (HeaderIP :> api) where
  type Client m (HeaderIP :> api) = Client m api
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)
{-# LANGUAGE OverloadedStrings     #-}

module Network.HTTP.Types.Header.Extended (module X, hAccessControlAllowOrigin, hXSetBearer) where

import Network.HTTP.Types.Header as X

hAccessControlAllowOrigin :: HeaderName
hAccessControlAllowOrigin = "Access-Control-Allow-Origin"

hXSetBearer :: HeaderName
hXSetBearer = "X-Set-Bearer"
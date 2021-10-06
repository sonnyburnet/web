{-# LANGUAGE FlexibleInstances #-}

module Servant.Swagger.RawM () where

import Servant.Swagger
import Servant.RawM
import Control.Lens
import Data.Swagger.Lens

instance HasSwagger RawM where toSwagger _ = mempty & paths . at "/" ?~ mempty
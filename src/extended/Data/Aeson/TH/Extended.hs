module Data.Aeson.TH.Extended (underscoreOptions, module Data.Aeson.TH) where

import Data.Aeson.TH
import Data.Char (toLower)
    
underscoreOptions :: Options
underscoreOptions = defaultOptions { fieldLabelModifier = tail, constructorTagModifier = map toLower }
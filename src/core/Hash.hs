{-# LANGUAGE PackageImports #-}

module Hash (mkHash) where

import Control.Lens.Iso.Extended
import Control.Lens
import "hashing" Crypto.Hash
import Data.Text

mkHash :: Show a => a -> Text
mkHash x = show (hash (show x^.stext.textbs) :: SHA256)^.stext
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Default.Class.Extended (module Data.Default.Class) where

import Data.Default.Class
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Proto3.Suite.Types
import qualified Data.Vector.Extended as V

instance Default LT.Text where
  def = LT.empty

instance Default B.ByteString where
  def = B.empty

instance Default BL.ByteString where
  def = BL.empty

instance Default a => Default (Enumerated a) where
  def = Enumerated $ Right def

instance Default a => Default (V.Vector a) where def = V.replicate 10 def
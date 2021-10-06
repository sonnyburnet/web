module Data.Text.Extended (UnicodeText(..), module T) where

import qualified Data.Text as T
import Test.QuickCheck.Extended
import Database.Transaction
import Control.Lens
import Control.Lens.Iso.Extended
                
-- | Wrapper for text in order to generate valid unicode string
newtype UnicodeText = UnicodeText T.Text

instance Arbitrary UnicodeText where arbitrary = UnicodeText <$> genText

instance ParamsShow UnicodeText where render = (^.coerced.from stext)
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications #-}

module BuildInfo where

import Data.String (fromString)
import Language.Haskell.TH (Exp(..), Lit(..))
import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Syntax
       (lift, loc_module, qLocation)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Data.Maybe
import Data.Yaml
import qualified Data.Text as T
import Data.Bifunctor
import Data.Coerce

gitCommit :: ExpQ
gitCommit = lift $ unsafePerformIO $ (head.lines) `fmap` readProcess "git" ["log", "-1", "--format=%h"] mempty

gitTag :: ExpQ
gitTag = lift $ unsafePerformIO $ fromMaybe "-" . listToMaybe . lines <$> readProcess "git" ["tag", "--list", "--sort=-creatordate"] ""

location :: ExpQ
location = [| fromString $((LitE . StringL . loc_module) `fmap` qLocation) |]

newtype Version = Version T.Text

instance Show Version where
  show (Version v) = "v" <> T.unpack v

instance FromJSON Version where
  parseJSON = withObject "Version" $ \o -> fmap (coerce @T.Text @Version . fromMaybe "-") $ o .:? "version"

getVersion ::  IO (Either String Version)
getVersion = first prettyPrintParseException <$> decodeFileEither @Version "package.yaml"

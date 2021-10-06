{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Lens.Extended
  ( module Control.Lens,
    module Control.Lens.Extras,
    mkLensesWithSuffix
  ) where

import           Control.Lens
import           Control.Lens.Extras
import           Language.Haskell.TH

-- by default to create lenses from data we need to define all fields with underscore
-- to incorporate lenses creation in existing data use this function
mkLensesWithSuffix :: String -> Name -> DecsQ
mkLensesWithSuffix suffix = makeLensesWith $ lensRules & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name ++ suffix)]

{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.QuickCheck.Extended
       ( module Test.QuickCheck
       , genText 
       , genTextN
      ) where

import TH.Mk
import qualified Data.Text as T
import Test.QuickCheck
import Data.Aeson.Unit
import Generic.Random
import GHC.Generics
import Protobuf.Scalar
import Prelude hiding (String, Bool, Double, Float)
import Data.Time.Transport

-- | Generate arbitrary text.
genText :: Gen T.Text
genText = T.pack . getPrintableString <$> arbitrary

-- | Get string of the bounded size.
genTextN :: Int -> Gen T.Text
genTextN n = T.pack . take (n - 1) . getPrintableString <$> arbitrary

instance Arbitrary Unit where arbitrary = pure (toEnum 0)

newtype GenericArbitraryU a = GenericArbitraryU a deriving Generic

instance ( GArbitrary UnsizedOpts a
         , GUniformWeight a
         , Generic a) => 
         Arbitrary (GenericArbitraryU a) where
  arbitrary = GenericArbitraryU <$> genericArbitraryU

mkArbitrary ''String
mkArbitrary ''Bool
mkArbitrary ''Double
mkArbitrary ''Float
mkArbitrary ''Int64
mkArbitrary ''Int32
mkArbitrary ''Time
mkArbitrary ''UInt64
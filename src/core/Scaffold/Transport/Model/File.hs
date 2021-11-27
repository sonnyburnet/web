{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scaffold.Transport.Model.File (Hash (..), Name (..), Mime (..), UnicodeText (..), Bucket (..)) where

import Test.QuickCheck.Extended
import Database.Transaction
import Data.Text.Extended

newtype Hash = Hash UnicodeText
  deriving newtype (Arbitrary, ParamsShow)

newtype Name = Name UnicodeText
  deriving newtype (Arbitrary, ParamsShow)

newtype Mime = Mime UnicodeText
  deriving newtype (Arbitrary, ParamsShow)

newtype Bucket = Bucket UnicodeText
  deriving newtype (Arbitrary, ParamsShow)
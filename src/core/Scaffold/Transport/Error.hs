{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Scaffold.Transport.Error
      ( Error (..)
      , AsError(..)
      , Located(..)
      , addMeta
      , appendMetas
      , appendMeta
      ) where

import Scaffold.Transport.Payload

import Control.Exception
import Control.Lens hiding ((.=))
import Data.Aeson.Extended hiding (Error)
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics

-- | Generic error. Keeps information about the problem and
-- additional metadata.
data Error = Error
  { errorMessage :: T.Text -- ^ Human readable message.
  , errorMeta :: Maybe Payload -- ^ Message metadata.
  } deriving stock Show
    deriving stock Generic

instance Exception Error

-- | How to convert concrete error into generic one.
class AsError e where
  asError :: e -> Error

instance AsError Error where asError = id

-- | Add metadata to the existing error.
addMeta :: ToJSON a => T.Text -> a -> Error -> Error
addMeta name t Error {..} =
  case errorMeta of
    Nothing -> Error { errorMeta = Just $ Payload $ HM.singleton name (toJSON t), ..}
    Just (Payload v) -> Error { errorMeta = Just $ Payload $ HM.insert name (toJSON t) v, ..}

-- | Append metadata to the list of arrays.
appendMeta :: ToJSON a => T.Text -> a -> Error -> Error
appendMeta name t Error{..} =
  case errorMeta of
    Nothing -> Error { errorMeta = Just $ Payload $ HM.singleton name (toJSON (t:[])), ..}
    Just (Payload v) -> Error { errorMeta = Just $ Payload $ HM.alter (\case
      Nothing -> Just $ toJSON (t:[])
      Just (Array as) -> Just $ Array $ V.cons (toJSON t) as
      Just other -> Just $ toJSON $ toJSON t:other:[]) name v
    , ..}

appendMetas :: ToJSON a => T.Text -> [a] -> Error -> Error
appendMetas name ts Error{..} = case errorMeta of
  Nothing -> Error { errorMeta = Just $ Payload $ HM.singleton name (toJSON ts), ..}
  Just (Payload v) -> Error { errorMeta = Just $ Payload $ HM.alter (\case
    Nothing -> Just $ toJSON ts
    Just (Array as) -> Just $ Array $ V.map toJSON (V.fromList ts) <> as
    Just other -> Just $ toJSON $ other:(map toJSON ts) ) name v
    , ..}

instance ToJSON Error where
  toJSON Error{..} =
     object
       ( "message" .= errorMessage
       : case errorMeta of
           Nothing -> mempty
           Just x -> ["meta" .= x])

instance FromJSON Error where
  parseJSON = withObject "error" $ \o -> do
    errorMessage <- o .: "message"
    errorMeta <- o .: "meta"
    pure Error{..}

instance AsError T.Text where
  asError t = Error t Nothing

instance ToSchema Error where
  declareNamedSchema _ = do
    tSchema <- declareSchemaRef (Proxy @T.Text)
    oSchema <- declareSchemaRef (Proxy @Object)
    pure $ NamedSchema (Just $ "Error") $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ [("message", tSchema), ("meta", oSchema)]
         & required .~ ["message"]

-- | Error with corresponding locations.
data Located at err
  = Located
  { locations :: [at]
  , value :: err
  } deriving stock Functor

instance (ToJSON l, AsError v) => AsError (Located l v) where
  asError (Located l v) = appendMetas "source" l $ asError v

instance Bifunctor Located where
  first f (Located a b) = Located (f <$> a) b
  second g (Located a b) = Located a (g b)
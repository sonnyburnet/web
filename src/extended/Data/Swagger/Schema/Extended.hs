{-# LANGUAGE TemplateHaskell #-}
module Data.Swagger.Schema.Extended 
       ( schemaOptions
       , schemaOptionsDef
       , deriveToSchema
       , deriveToSchemaDef
       , module Data.Swagger.Schema
       ) where

import           Data.Aeson (defaultOptions)
import           Data.Aeson.Extended (aesonOptions) 
import           Data.Swagger.Schema
import           Language.Haskell.TH

schemaOptionsDef :: SchemaOptions
schemaOptionsDef = fromAesonOptions defaultOptions

schemaOptions :: String -> SchemaOptions
schemaOptions = fromAesonOptions . aesonOptions

deriveToSchema :: Name -> Q [Dec]
deriveToSchema name =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema = 
       genericDeclareNamedSchema (schemaOptions $sname)
  |]
  where sname = return (LitE (StringL (nameBase name)))


deriveToSchemaDef :: Name -> Q [Dec]
deriveToSchemaDef name =
  [d|
    instance ToSchema $(conT name) where
      declareNamedSchema = 
       genericDeclareNamedSchema schemaOptionsDef
  |]
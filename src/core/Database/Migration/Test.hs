{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Test (migrate) where

import Hasql.Session
import TH.Mk
import Data.String.Interpolate
import Data.Foldable
import Data.ByteString

$mkMigrationTest

migrate :: Session ()
migrate = sql $ exts <> fold list
  where 
    exts =
     [i|create extension postgres_fdw;
        create extension hstore;
        create extension ltree;
        create extension pg_trgm;|]
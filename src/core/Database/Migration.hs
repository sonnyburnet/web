{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Migration (run) where

import Data.Word
import Data.Bool (bool)
import Data.Time.Clock ()
import Katip
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Foldable
import qualified Database.Migration.Batch as Batch
import Data.Maybe
import Control.Monad
import Control.Applicative
import KatipController
import qualified Hasql.Statement
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Prelude hiding (init)
import Control.Monad.IO.Class
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Database.Transaction
import Hasql.TH

run :: Pool.Pool Hasql.Connection -> KatipLoggerIO -> IO ()
run cm logger = void $ transaction cm logger $ statement checkDBMeta () >>= traverse_ (bool initDb (roll Nothing))

checkDBMeta :: Hasql.Statement.Statement () (Maybe Bool)
checkDBMeta =
  [maybeStatement|
     select exists (select 1
     from information_schema.tables
     where table_schema = 'public'
           and table_name = 'db_meta') :: bool|]

initDb :: SessionR ()
initDb = statement mkDbMeta () >> roll (Just 1)

mkDbMeta ::  Hasql.Statement.Statement () ()
mkDbMeta = Hasql.Statement.Statement sql HE.noParams HD.noResult False
  where
    sql =
     [uncheckedSql|
       create table public.db_meta (
        version int4 not null,
        created timestamptz not
        null default now(),
        modified timestamptz)|]

roll :: Maybe Word32 -> SessionR ()
roll v =
  do
    v' <- (<|> v) `fmap` statement getVersion ()
    let batch = Batch.Version `fmap` v
    for_ (v' <|> v) $ \i -> do
      logger <- ask
      liftIO $ logger InfoS (logStr ("migration will be start from version " <> i^.stringify))
      next <- lift $ Batch.rollMigrations (Batch.Version i) logger
      for_ (next <|> batch) $ \ident -> do
        let new = ident^.coerced
        if new /= i then
          do statement (setVersion (maybe (New new) (const (Init new)) v)) None
             liftIO $ logger InfoS (logStr ("migration finished at version " <> show ident))
        else liftIO $ logger InfoS (logStr ("no migration found" :: String))
      when (isNothing next) $ liftIO $ logger InfoS (logStr ("no migration found" :: String))

getVersion :: Hasql.Statement.Statement () (Maybe Word32)
getVersion = rmap (fmap fromIntegral) [maybeStatement|select (version :: int4) from db_meta|]

data SetVersion = Init Word32 | New Word32 | None

instance ParamsShow SetVersion where
  render (Init x) = show x
  render (New x) = show x
  render None = mempty

setVersion ::  SetVersion -> Hasql.Statement.Statement SetVersion ()
setVersion (Init v) = lmap (const (v^.integral)) [resultlessStatement|insert into public.db_meta (version, created) values ($1 :: int4, now())|]
setVersion (New v) = lmap (const (v^.integral)) [resultlessStatement|update public.db_meta set version = $1 :: int4, modified = now()|]
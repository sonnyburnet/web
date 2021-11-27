module Database.Migration (run) where

import qualified Hasql.Migration as Hasql
import qualified Hasql.Transaction as Hasql ()
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Control.Monad.Writer.Lazy as W
import qualified Hasql.Pool as HasqlPool
import Control.Exception
import Data.Traversable
import KatipController (KatipLoggerIO)
import Katip
import Data.Foldable

run :: FilePath -> KatipLoggerIO -> HasqlPool.Settings -> IO ()
run path logger settings = do
  cmds <- Hasql.loadMigrationsFromDirectory path
  logger DebugS $ ls $ show cmds
  let goMigrationV2 = W.runWriterT $ do
        migr_result <- for (Hasql.MigrationInitialization : cmds) $
          W.lift . Hasql.runMigration
        for_ migr_result $ W.tell . ls . show
        schemas <- W.lift Hasql.getMigrations
        W.tell $ foldMap (ls . show) schemas
  let go pool = do
        result_e <- HasqlPool.use pool $
          Hasql.transaction
           Hasql.ReadCommitted
           Hasql.Write $
          goMigrationV2
        case result_e of
          Right (_, ws) -> logger DebugS ws
          Left e -> do logger ErrorS $ ls (show e); error $ show e
  bracket (HasqlPool.acquire settings) go HasqlPool.release
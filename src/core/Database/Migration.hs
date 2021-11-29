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
  cmds <- (Hasql.MigrationInitialization :) <$>
    Hasql.loadMigrationsFromDirectory path
  let goMigrationV2 = W.runWriterT $ do
        schemas <- W.lift Hasql.getMigrations
        W.tell $ foldMap (ls . show) schemas
        -- validation
        validation <- fmap sequence $
          for (tail cmds) $
            W.lift .
            Hasql.runMigration .
            Hasql.MigrationValidation
        for_ validation $ \es -> error $ show es

        migr_result <- fmap sequence $ for cmds $
          W.lift . Hasql.runMigration
        for_ migr_result $ \es -> error $ show es
  let go pool = do
        result_e <- HasqlPool.use pool $
          Hasql.transaction
           Hasql.ReadCommitted
           Hasql.Write $
          goMigrationV2
        for_ result_e $ \(_, ws) ->
          logger DebugS $ ls (show cmds) <> ws
  bracket (HasqlPool.acquire settings) go HasqlPool.release
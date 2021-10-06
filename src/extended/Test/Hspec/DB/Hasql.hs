{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Test.Hspec.DB.Hasql (TestDBHasql, itHasql, describeHasql, session) where

import Control.Exception (bracketOnError, finally)
import Control.Monad
import qualified Database.Postgres.Temp as Temp
import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Hasql
import Test.Hspec as HSpec
import Control.Lens
import Control.Applicative ((<|>))
import Data.Either
import Control.Monad.IO.Class
import Data.List

data TestDBHasql = 
     TestDBHasql
     { tempDB :: !Temp.DB
       -- ^ handle for temporary @postgres@ process
     , conn   :: !Hasql.Connection
       -- ^ connections to the temporary @postgres@
     }

-- | Start a temporary postgres process and create a connections to it.
setupDBHasql
  :: [Hasql.Session ()]
  -> Maybe (Hasql.Session ())
     -- ^ Data population.
  -> IO TestDBHasql
setupDBHasql migrations mpopulate = do
  putStrLn $ "temp db config: " <> Temp.prettyPrintConfig (Temp.autoExplainConfig 0)
  bracketOnError
      (Temp.startConfig (Temp.autoExplainConfig 0) >>= \case
        Left e -> error $ "Error during db initialization: " <> show e
        Right x -> pure x
      )
      Temp.stop
    $ \tempDB -> do
        let connStr = Temp.toConnectionString tempDB
        Hasql.acquire connStr >>= \case
          Left e -> error $ "Error connecting to db" <> show e
          Right conn ->
            do
              es <- fmap lefts $ traverse (`Hasql.run` conn) migrations
              let migr = 
                    case es of 
                      [] -> Right ()
                      es -> Left $ intercalate "," $ map show es        
              pop <- traverse (`Hasql.run` conn) mpopulate
              case Just migr^?_Just._Left <|> pop^?_Just._Left.to show of 
                Just e -> liftIO (print $ "error while setup db: " <> e) >> error mempty
                Nothing -> pure TestDBHasql {..}
             
-- | Tear down DB Hasql.
tearDownDBHasql :: TestDBHasql -> IO ()
tearDownDBHasql TestDBHasql {..} = Hasql.release conn `finally` void (Temp.stop tempDB)

-- | Run session helper, throws a error, in session
-- errors out.
runSession :: Hasql.Session a -> TestDBHasql -> IO a
runSession action db = (action `Hasql.run` conn db) >>= either (error . show) return

-- | Wrapper for hasql test.
itHasql :: String -> Hasql.Session a -> SpecWith TestDBHasql
itHasql str action = str `HSpec.it` (void . runSession action)

-- | Run a hasql session bases test. Test takes a function that
-- can run the `Session` in on the current connection.
session :: String -> ((forall a . Hasql.Session a -> IO (Either Hasql.QueryError a)) -> IO a) -> SpecWith TestDBHasql
session name f = HSpec.it name (void . test)
  where test db = f (`Hasql.run` conn db)

-- | Hasql test.
describeHasql
  :: [Hasql.Session ()] -- ^ Database initialization.
  -> Maybe (Hasql.Session ()) -- ^ Database population.
  -> String -- ^ Test name.
  -> SpecWith TestDBHasql -- ^ Test itself.
  -> Spec
describeHasql migrations mpopulate str =
  beforeAll (setupDBHasql migrations mpopulate) . afterAll tearDownDBHasql . HSpec.describe str
module Concurrent (throttle) where

import Control.Concurrent
import Control.Concurrent.MSemN
import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad

-- | Limit the number of @tasks@ started per second. @throttle@ will run all
--   actions concurrently but only starting a certain number per second. It
--   will wait for all tasks and return the results in a list.
throttle
  :: Int     -- ^ number of tasks per second (TPS)
  -> [IO a]  -- ^ the tasks to run concurrently but limited by TPS
  -> IO [a]  -- ^ the tasks results
throttle tps tasks = do
  sem <- new tps
  let runTask task = fmap snd $ wait sem 1 >> Thread.forkIO task
  let timeResetWorker = forever $ threadDelay 1000000 >> signalF sem (\i -> (tps-i, ()))
  let runAllTasks = mapM runTask tasks
  mapM Thread.result =<< sequence =<< bracket (forkIO timeResetWorker) killThread (const runAllTasks)
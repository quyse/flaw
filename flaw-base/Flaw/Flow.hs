{-|
Module: Flaw.Flow
Description: Different helpful functions for dealing with synchronized flows of operations.
License: MIT
-}

module Flaw.Flow
  ( forkFlow
  , forkFlowOS
  , Flow()
  , newFlow
  , newFlowOS
  , newMultiFlow
  , newMultiFlowOS
  , asyncRunInFlow
  , runInFlow
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import Flaw.Book

-- | Fork a thread.
forkFlow :: IO () -> IO ((), IO ())
forkFlow = forkFlowInternal forkFinally

-- | Fork an OS thread.
forkFlowOS :: IO () -> IO ((), IO ())
forkFlowOS = forkFlowInternal $ \action andThen -> mask $ \restore -> forkOS $ try (restore action) >>= andThen

forkFlowInternal :: (IO () -> (Either SomeException () -> IO ()) -> IO ThreadId) -> IO () -> IO ((), IO ())
forkFlowInternal f work = do
  stoppedVar <- newEmptyMVar
  threadId <- f work $ \_ -> putMVar stoppedVar ()
  let
    stop = do
      killThread threadId
      takeMVar stoppedVar
  return ((), stop)

-- | Operation flow.
newtype Flow = Flow (TQueue (IO ()))

-- | Create operation flow, i.e. single stream of operations running in a separate
-- thread, and booked into Flaw.Book.
newFlow :: IO (Flow, IO ())
newFlow = newMultiFlow 1

-- | Create operation flow in a bound thread.
newFlowOS :: IO (Flow, IO ())
newFlowOS = newMultiFlowOS 1

-- | Create operation multiflow, i.e. multiple threads, booked into Flaw.Book, and using a single queue of operations.
newMultiFlow :: Int -> IO (Flow, IO ())
newMultiFlow threadsCount = newFlowInternal threadsCount forkFlow

-- | Create operation multiflow using bounded threads.
newMultiFlowOS :: Int -> IO (Flow, IO ())
newMultiFlowOS threadsCount = newFlowInternal threadsCount forkFlowOS

newFlowInternal :: Int -> (IO () -> IO ((), IO ())) -> IO (Flow, IO ())
newFlowInternal threadsCount f = withSpecialBook $ \bk -> do
  queue <- newTQueueIO
  forM_ [1..threadsCount] $ \_i -> book bk $ f $ runOperations queue
  return $ Flow queue

asyncRunInFlow :: Flow -> IO () -> STM ()
asyncRunInFlow (Flow queue) operation = writeTQueue queue $ do
  operation
  runOperations queue

runInFlow :: Flow -> IO a -> IO a
runInFlow flow operation = do
  resultVar <- newEmptyMVar
  atomically $ asyncRunInFlow flow $ putMVar resultVar =<< try operation
  r <- takeMVar resultVar
  case r of
    Right a -> return a
    Left e -> throwIO (e :: SomeException)

runOperations :: TQueue (IO ()) -> IO ()
runOperations queue = join $ atomically $ readTQueue queue

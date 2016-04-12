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
	, asyncRunInFlow
	, runInFlow
	, exitFlow
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

-- | Fork a thread.
{-# INLINE forkFlow #-}
forkFlow :: IO () -> IO ((), IO ())
forkFlow = forkFlowInternal forkFinally

-- | Fork an OS thread.
{-# INLINE forkFlowOS #-}
forkFlowOS :: IO () -> IO ((), IO ())
forkFlowOS = forkFlowInternal $ \action andThen -> mask $ \restore -> forkOS $ try (restore action) >>= andThen

forkFlowInternal :: (IO () -> (Either SomeException () -> IO ()) -> IO ThreadId) -> IO () -> IO ((), IO ())
forkFlowInternal f work = do
	stoppedVar <- newEmptyMVar
	threadId <- f work $ \_ -> putMVar stoppedVar ()
	let stop = do
		killThread threadId
		takeMVar stoppedVar
	return ((), stop)

-- | Operation flow.
newtype Flow = Flow (TQueue (IO ()))

-- | Create operation flow, i.e. single stream of operations running in a separate
-- thread, and booked into Flaw.Book.
{-# INLINE newFlow #-}
newFlow :: IO (Flow, IO ())
newFlow = newFlowInternal forkFlow

-- | Create operation flow in a bound thread.
{-# INLINE newFlowOS #-}
newFlowOS :: IO (Flow, IO ())
newFlowOS = newFlowInternal forkFlowOS

newFlowInternal :: (IO () -> IO ((), IO ())) -> IO (Flow, IO ())
newFlowInternal f = do
	queue <- newTQueueIO
	((), stop) <- f $ runOperations queue
	return (Flow queue, stop)

{-# INLINE asyncRunInFlow #-}
asyncRunInFlow :: Flow -> IO () -> STM ()
asyncRunInFlow (Flow queue) operation = writeTQueue queue $ do
	operation
	runOperations queue

{-# INLINE runInFlow #-}
runInFlow :: Flow -> IO a -> IO a
runInFlow flow operation = do
	resultVar <- newEmptyMVar
	atomically $ asyncRunInFlow flow $ putMVar resultVar =<< try operation
	r <- takeMVar resultVar
	case r of
		Right a -> return a
		Left e -> throwIO (e :: SomeException)

-- | Graceful shutdown of flow.
{-# INLINE exitFlow #-}
exitFlow :: Flow -> IO ()
exitFlow (Flow queue) = do
	dummyVar <- newEmptyMVar
	atomically $ writeTQueue queue $ putMVar dummyVar ()
	takeMVar dummyVar

runOperations :: TQueue (IO ()) -> IO ()
runOperations queue = join $ atomically $ readTQueue queue

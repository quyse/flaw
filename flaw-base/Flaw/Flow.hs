{-|
Module: Flaw.Flow
Description: Different helpful functions for dealing with synchronized flows of operations.
License: MIT
-}

module Flaw.Flow
	( forkFlow
	, Flow()
	, newFlow
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
forkFlow work = do
	stoppedVar <- newEmptyMVar
	threadId <- forkFinally work $ \_ -> putMVar stoppedVar ()
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
newFlow = do
	queue <- newTQueueIO
	((), stop) <- forkFlow $ runOperations queue
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
	atomically $ asyncRunInFlow flow $ putMVar resultVar =<< handle (return . Left) (fmap Right operation)
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

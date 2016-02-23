{-|
Module: Flaw.Mess.HashMapCache
Description: Simple Mess in-memory cache using hash map.
License: MIT
-}

module Flaw.Mess.HashMapCache
	( HashMapCache(..)
	, newHashMapCache
	) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.HashMap.Strict as HM

import Flaw.Mess
import Flaw.Mess.Dispatcher

newtype HashMapCache = HashMapCache (TVar (HM.HashMap ExpressionHash (TVar TaskResult)))

newHashMapCache :: IO HashMapCache
newHashMapCache = liftM HashMapCache $ newTVarIO HM.empty

instance DispatcherCache HashMapCache where
	dispatchCache (HashMapCache cacheVar) eh = do
		cache <- readTVar cacheVar
		case HM.lookup eh cache of
			Just taskVar -> return (False, taskVar)
			Nothing -> do
				taskVar <- newTVar TaskInProgress
				writeTVar cacheVar $ HM.insert eh taskVar cache
				return (True, taskVar)

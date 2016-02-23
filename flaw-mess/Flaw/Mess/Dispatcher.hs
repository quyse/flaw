{-|
Module: Flaw.Mess.Dispatcher
Description: Mess cache dispatcher.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}

module Flaw.Mess.Dispatcher
	( Dispatcher()
	, TaskResult(..)
	, DispatcherCache(..)
	, SomeDispatcherCache(..)
	, DispatcherLogger(..)
	, SomeDispatcherLogger(..)
	, newDispatcher
	, DispatchM
	, MessM
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Reader

import Flaw.Book
import Flaw.Flow
import Flaw.Mess

data Dispatcher = Dispatcher
	{ dispatcherBook :: Book
	, dispatcherCache :: SomeDispatcherCache
	, dispatcherLogger :: SomeDispatcherLogger
	, dispatcherRootValue :: Value DispatchM
	, dispatcherCurrentTask :: Maybe HashedExpression
	}

data TaskResult
	= TaskInProgress
	| TaskSucceeded (Value DispatchM)
	| TaskFailed SomeException

class DispatcherCache c where
	-- | Try to get task result from cache, or create new task result var.
	-- Returns True if new var was created (so task should be calculated).
	dispatchCache :: c -> ExpressionHash -> STM (Bool, TVar TaskResult)

data SomeDispatcherCache where
	SomeDispatcherCache :: DispatcherCache c => c -> SomeDispatcherCache

class DispatcherLogger l where
	-- | Log start of calculation.
	dispatchLogStart :: l -> HashedExpression -> IO ()
	-- | Log asking for task (dependency).
	dispatchLogRequest :: l
		-> Maybe HashedExpression -- ^ Asking task.
		-> HashedExpression -- ^ Target task.
		-> IO ()
	-- | Log success of calculation.
	dispatchLogSuccess :: l -> HashedExpression -> IO ()
	-- | Log error in calculation.
	dispatchLogError :: l -> HashedExpression -> SomeException -> IO ()

data SomeDispatcherLogger where
	SomeDispatcherLogger :: DispatcherLogger l => l -> SomeDispatcherLogger

newDispatcher :: (DispatcherCache c, DispatcherLogger l, IsValue r DispatchM) => c -> l -> r -> IO (Dispatcher, IO ())
newDispatcher cache logger root = withSpecialBook $ \bk -> do
	return Dispatcher
		{ dispatcherBook = bk
		, dispatcherCache = SomeDispatcherCache cache
		, dispatcherLogger = SomeDispatcherLogger logger
		, dispatcherRootValue = toValue root
		, dispatcherCurrentTask = Nothing
		}

type DispatchM = ReaderT Dispatcher IO

type MessM = MessT DispatchM

newtype DispatchFailed = DispatchFailed [SomeException] deriving Show

instance Exception DispatchFailed

instance DispatchMonad DispatchM where
	dispatchRootValue = liftM dispatcherRootValue ask
	dispatchCalc tasks = ReaderT $ \dispatcher@Dispatcher
		{ dispatcherBook = bk
		, dispatcherCache = SomeDispatcherCache cache
		, dispatcherLogger = SomeDispatcherLogger logger
		, dispatcherCurrentTask = parentTask
		} -> do
		-- schedule execution of tasks
		taskVars <- forM tasks $ \(he, mv) -> do
			let eh = hashedExpressionHash he
			dispatchLogRequest logger parentTask he
			join $ atomically $ do
				(isNewTaskVar, taskVar) <- dispatchCache cache eh
				if isNewTaskVar then return $ do
					-- log start
					dispatchLogStart logger he

					-- error handling function
					let handleError err = do
						atomically $ writeTVar taskVar $ TaskFailed err
						dispatchLogError logger he err

					-- run computation in a separate thread
					book bk $ forkFlow $ handle handleError $ do
						v <- runReaderT mv dispatcher
							{ dispatcherCurrentTask = Just he
							}

						-- write result
						atomically $ writeTVar taskVar $ TaskSucceeded v
						-- log success
						dispatchLogSuccess logger he

					return taskVar

				else return $ return taskVar

		-- wait for tasks to be done
		taskResults <- atomically $ forM taskVars $ \taskVar -> do
			maybeTaskResult <- readTVar taskVar
			case maybeTaskResult of
				TaskInProgress -> retry
				TaskSucceeded taskResult -> return $ Right taskResult
				TaskFailed err -> return $ Left err

		-- if there're some failed tasks, throw an exception
		let foldResults taskResult (results, errors) = case taskResult of
			Right result -> (result : results, errors)
			Left err -> ([], err : errors)
		let (results, errors) = foldr foldResults ([], []) taskResults
		case errors of
			[] -> return results
			_ -> throwIO $ DispatchFailed errors

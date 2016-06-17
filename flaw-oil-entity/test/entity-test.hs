{-|
Module: Main
Description: Tests for Oil entities.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
	( main
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Int
import Data.IORef
import Data.Word
import System.Exit
import System.IO.Unsafe

import Flaw.Book
import Flaw.Flow
import Flaw.Oil.ClientRepo
import Flaw.Oil.ServerRepo
import Flaw.Oil.Entity
import Flaw.Oil.Entity.Basic
import Flaw.Oil.Repo

{-# NOINLINE testFailedRef #-}
testFailedRef :: IORef Bool
testFailedRef = unsafePerformIO $ newIORef False

failTest :: IO ()
failTest = writeIORef testFailedRef True

verify :: Eq a => (a -> Bool) -> IO a -> IO ()
verify f m = do
	r <- m
	unless (f r) failTest

mustThrow :: (Eq e, Exception e) => e -> IO a -> IO ()
mustThrow e io = handle (\ee -> unless (e == ee) failTest) $ do
	void io
	failTest

data TestException
	= WaitForSyncTimedOut
	deriving Show

instance Exception TestException

data Client = Client
	{ clientEntityManager :: !EntityManager
	, clientSync :: !(IO ())
	, clientWaitAndSync :: !(IO ())
	}

session :: String -> (IO Client -> IO ()) -> IO ()
session sessionName f = handle (\e -> print ("session failed" :: String, sessionName, e :: SomeException) >> failTest) $ withBook $ \bk -> do
	let manifest = defaultManifest
	serverRepo <- book bk $ openServerRepo ":memory:"

	f $ do
		clientRepo <- book bk $ openClientRepo ":memory:"
		syncScheduledVar <- newTVarIO False
		entityManager <- book bk $ newEntityManager clientRepo $ atomically $ writeTVar syncScheduledVar True

		-- register deserializators
		registerBasicEntityDeserializators entityManager

		let sync = do
			-- pull
			runInFlow (entityManagerFlow entityManager) $ do
				(push, crps) <- pushClientRepo clientRepo manifest
				pull <- syncServerRepo serverRepo manifest push 1
				atomically . pullEntityManager entityManager . clientRepoPullChanges =<< pullClientRepo clientRepo pull crps
			-- wait for async pull
			runInFlow (entityManagerFlow entityManager) $ return ()

		return Client
			{ clientEntityManager = entityManager
			, clientSync = sync
			, clientWaitAndSync = do
				-- wait for sync to be scheduled
				delayVar <- registerDelay 5000000
				atomically $ do
					timedOut <- readTVar delayVar
					when timedOut $ throwSTM WaitForSyncTimedOut
					syncScheduled <- readTVar syncScheduledVar
					unless syncScheduled retry
					writeTVar syncScheduledVar False
				sync
			}

main :: IO ()
main = do
	session "main" $ \newClient -> do

		c1 <- newClient

		intVar1 <- newEntityVar $ clientEntityManager c1
		verify (== 0) $ atomically $ readEntityVar intVar1
		atomically $ writeBasicEntityVar intVar1 (1 :: Word32)
		verify (== 1) $ atomically $ readEntityVar intVar1
		clientWaitAndSync c1

		let intEntityId = entityVarEntityId intVar1

		c2 <- newClient

		clientSync c2

		intVar2 <- getEntityVar (clientEntityManager c2) intEntityId :: IO (EntityVar Word32)
		verify (== 1) $ atomically $ readEntityVar intVar2

		atomically $ writeBasicEntityVar intVar2 2
		verify (== 1) $ atomically $ readEntityVar intVar1
		verify (== 2) $ atomically $ readEntityVar intVar2
		clientWaitAndSync c2
		verify (== 1) $ atomically $ readEntityVar intVar1
		verify (== 2) $ atomically $ readEntityVar intVar2
		clientSync c1
		verify (== 2) $ atomically $ readEntityVar intVar1
		verify (== 2) $ atomically $ readEntityVar intVar2
		atomically $ writeBasicEntityVar intVar1 3
		verify (== 3) $ atomically $ readEntityVar intVar1
		verify (== 2) $ atomically $ readEntityVar intVar2
		clientWaitAndSync c1
		verify (== 3) $ atomically $ readEntityVar intVar1
		verify (== 2) $ atomically $ readEntityVar intVar2
		clientSync c2
		verify (== 3) $ atomically $ readEntityVar intVar1
		verify (== 3) $ atomically $ readEntityVar intVar2

		atomically $ writeBasicEntityVar intVar1 4
		atomically $ writeBasicEntityVar intVar2 5
		verify (== 4) $ atomically $ readEntityVar intVar1
		verify (== 5) $ atomically $ readEntityVar intVar2
		clientWaitAndSync c1
		clientWaitAndSync c2
		verify (== 4) $ atomically $ readEntityVar intVar1
		verify (== 5) $ atomically $ readEntityVar intVar2
		clientWaitAndSync c1
		verify (== 5) $ atomically $ readEntityVar intVar1
		verify (== 5) $ atomically $ readEntityVar intVar2

		-- entity var pointing to Word32 entity
		ptrVar1 <- newEntityVar $ clientEntityManager c1 :: IO (EntityVar (EntityPtr Word32))
		verify (== EntityPtr nullEntityId) $ atomically $ readEntityVar ptrVar1
		atomically $ writeBasicEntityVar ptrVar1 $ EntityPtr intEntityId
		verify (== EntityPtr intEntityId) $ atomically $ readEntityVar ptrVar1

		-- wrongly typed var
		do
			ptrVar2 <- getEntityVar (clientEntityManager c1) $ entityVarEntityId ptrVar1 :: IO (EntityVar Word32)
			mustThrow EntityVarWrongTypeException $ void $ atomically $ readEntityVar ptrVar2

		-- wrongly typed EntityPtr var, first client
		ptrVar3 <- getEntityVar (clientEntityManager c1) $ entityVarEntityId ptrVar1 :: IO (EntityVar (EntityPtr Int32))
		mustThrow EntityVarWrongTypeException $ void $ atomically $ readEntityVar ptrVar3

		clientWaitAndSync c1
		clientSync c2

		-- correctly typed var, second client
		ptrVar4 <- getEntityVar (clientEntityManager c2) $ entityVarEntityId ptrVar1 :: IO (EntityVar (EntityPtr Word32))
		verify (== EntityPtr intEntityId) $ atomically $ readEntityVar ptrVar4

		-- change of type through second client
		do
			ptrVar5 <- getEntityVar (clientEntityManager c2) $ entityVarEntityId ptrVar1 :: IO (EntityVar (EntityPtr Int32))
			mustThrow EntityVarWrongTypeException $ void $ atomically $ readEntityVar ptrVar5
			atomically $ writeBasicEntityVar ptrVar5 $ EntityPtr intEntityId
			mustThrow EntityVarWrongTypeException $ void $ atomically $ readEntityVar ptrVar4
			verify (== EntityPtr intEntityId) $ atomically $ readEntityVar ptrVar5

		-- transfer change of type into first client
		clientWaitAndSync c2
		clientSync c1
		-- was correctly typed, now incorrect
		mustThrow EntityVarWrongTypeException $ void $ atomically $ readEntityVar ptrVar1
		-- was incorrectly typed, now correct
		verify (== EntityPtr intEntityId) $ atomically $ readEntityVar ptrVar3

	testFailed <- readIORef testFailedRef
	when testFailed exitFailure
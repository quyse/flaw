{-|
Module: Flaw.Oil.RemoteRepo
Description: Asynchronous abstraction for local and remote repo.
License: MIT
-}

module Flaw.Oil.RemoteRepo
	( AsyncRepo(..)
	, HttpRemoteRepo()
	, AsyncRepoNotification(..)
	, initHttpRemoteRepo
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Types as H

import Flaw.Book
import Flaw.Exception
import Flaw.Oil.ClientRepo

-- | Asynchronous eventually-consistent repo.
class AsyncRepo r where
	-- | Get channel with changes happening to repo.
	asyncRepoChangesChan :: r -> STM (TChan (B.ByteString, B.ByteString))
	-- | Get notifications channel.
	asyncRepoNotificationsChan :: r -> STM (TChan AsyncRepoNotification)
	-- | Perform change.
	sendAsyncRepoChange :: r -> B.ByteString -> B.ByteString -> STM ()

-- | Notifications async repo may send.
data AsyncRepoNotification
	-- | Update approximate lag (number of items local repo needs to pull from remote repo in order to catch up).
	= AsyncRepoLag Int64
	-- | Warning about some problems with connection to remote repo.
	-- Repo will try to reconnect, so connection may be restored.
	| AsyncRepoWarning String
	-- | Error report about problems with connection to remote repo.
	-- Repo will not try to reconnect, error is considered fatal.
	| AsyncRepoError String
	deriving Show

-- | Implementation for a local client repo, synchronized with remote server repo.
data HttpRemoteRepo = HttpRemoteRepo
	{ httpRemoteRepoClientRepo :: !ClientRepo
	, httpRemoteRepoChangesChan :: !(TChan (B.ByteString, B.ByteString))
	, httpRemoteRepoNotificationsChan :: !(TChan AsyncRepoNotification)
	, httpRemoteRepoOperationsQueue :: !(TQueue (IO ()))
	}

instance AsyncRepo HttpRemoteRepo where
	asyncRepoChangesChan HttpRemoteRepo
		{ httpRemoteRepoChangesChan = changesChan
		} = dupTChan changesChan
	asyncRepoNotificationsChan HttpRemoteRepo
		{ httpRemoteRepoNotificationsChan = notificationsChan
		} = dupTChan notificationsChan
	sendAsyncRepoChange HttpRemoteRepo
		{ httpRemoteRepoClientRepo = repo
		, httpRemoteRepoChangesChan = changesChan
		, httpRemoteRepoOperationsQueue = operationsQueue
		} key value = writeTQueue operationsQueue $ do
		clientRepoChange repo key value
		atomically $ writeTChan changesChan (key, value)

-- | Initialize remote repo with HTTP connection to server.
initHttpRemoteRepo :: H.Manager -> ClientRepo -> T.Text -> IO (HttpRemoteRepo, IO ())
initHttpRemoteRepo httpManager repo url = withSpecialBook $ \bk -> do
	-- create template request by parsing url
	templateRequest <- H.parseUrl $ T.unpack url
	-- create channels
	changesChan <- newBroadcastTChanIO
	notificationsChan <- newBroadcastTChanIO

	-- operation thread
	-- only this thread does anything with repo
	operationsQueue <- newTQueueIO
	let runOperations = join $ atomically $ readTQueue operationsQueue
	let operation io = do
		resultVar <- newEmptyMVar
		atomically $ writeTQueue operationsQueue $ do
			putMVar resultVar =<< io
			runOperations
		takeMVar resultVar
	book bk $ do
		stoppedVar <- newEmptyMVar
		void $ forkFinally runOperations $ \_ -> putMVar stoppedVar ()
		let stop = do
			atomically $ writeTQueue operationsQueue $ return ()
			takeMVar stoppedVar
		return ((), stop)

	-- helper function to pause before another try
	let throttle timeBefore = do
		let throttlePause = 5000000 -- microseconds
		timeAfter <- getCurrentTime
		let pause = max 0 $ min throttlePause $ throttlePause - (floor $ diffUTCTime timeAfter timeBefore)
		when (pause > 0) $ threadDelay $ pause

	-- helper function to do something http-related multiple times
	let tryFewTimes io = do
		let triesCount = 3 :: Int
		let trying i = do
			timeBefore <- getCurrentTime
			catch io $ \e -> do
				atomically $ writeTChan notificationsChan $ AsyncRepoWarning $ show (e :: H.HttpException)
				if i < triesCount then do
					throttle timeBefore
					trying $ i + 1
				else throwIO $ DescribeFirstException "failed to do HTTP operation"
		trying 1

	-- networking thread
	let networking = do
		-- first, get manifest from server
		manifest <- do
			let request = templateRequest
				{ H.method = H.methodGet
				, H.queryString = T.encodeUtf8 $ T.pack "manifest"
				}
			body <- tryFewTimes $ H.withResponse request httpManager $ liftM BL.fromChunks . H.brConsume . H.responseBody
			-- decodeBody
			case S.decodeLazy body of
				Right manifest -> return manifest
				Left err -> throwIO $ DescribeFirstException ("failed to decode manifest", err)

		-- sync routine
		let sync = do
			-- perform push on client repo
			(push, pushState) <- operation $ pushClientRepo repo manifest
			-- prepare request
			let request = templateRequest
				{ H.method = H.methodPost
				, H.queryString = T.encodeUtf8 $ T.pack "sync"
				, H.requestBody = H.RequestBodyLBS $ S.encodeLazy push
				}
			-- measure time before sending request, to correctly do throttling
			timeBefore <- getCurrentTime
			-- send request
			body <- tryFewTimes $ H.withResponse request httpManager $ liftM BL.fromChunks . H.brConsume . H.responseBody
			-- decode body
			case S.decodeLazy body of
				Right pull -> do
					operation $ do
						-- perform pull
						ClientRepoPullInfo
							{ clientRepoPullLag = lag
							, clientRepoPullChanges = changes
							} <- pullClientRepo repo pull pushState
						-- send notifications
						atomically $ do
							forM_ changes $ writeTChan changesChan
							writeTChan notificationsChan $ AsyncRepoLag lag
					-- make pause before another sync
					throttle timeBefore
					-- sync again
					sync
				Left err -> throwIO $ DescribeFirstException ("failed to decode pull", err)
		sync

	book bk $ do
		stoppedVar <- newEmptyMVar
		let work = catch networking $ \e -> atomically $ writeTChan notificationsChan $ AsyncRepoError $ show (e :: SomeException)
		threadId <- forkFinally work $ \_ -> putMVar stoppedVar ()
		let stop = do
			killThread threadId
			takeMVar stoppedVar
		return ((), stop)

	return HttpRemoteRepo
		{ httpRemoteRepoClientRepo = repo
		, httpRemoteRepoChangesChan = changesChan
		, httpRemoteRepoNotificationsChan = notificationsChan
		, httpRemoteRepoOperationsQueue = operationsQueue
		}

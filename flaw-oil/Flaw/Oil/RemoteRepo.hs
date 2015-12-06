{-|
Module: Flaw.Oil.RemoteRepo
Description: Remote repo abstraction.
License: MIT
-}

module Flaw.Oil.RemoteRepo
	( RemoteRepo(..)
	, HttpRemoteRepo()
	, RemoteRepoNotification(..)
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
import Flaw.Oil.Repo

-- | Remote repo.
class RemoteRepo r where
	-- | Get current revision.
	remoteRepoRevision :: r -> STM Revision
	-- | Get channel with changes coming to repo from outside.
	remoteRepoChangesChan :: r -> STM (TChan (B.ByteString, B.ByteString))
	-- | Get notifications channel.
	remoteRepoNotificationsChan :: r -> STM (TChan RemoteRepoNotification)
	-- | Perform async change.
	-- It won't be sent to changes chan.
	remoteRepoChange :: r -> B.ByteString -> B.ByteString -> STM ()

-- | Notifications remote repo may send.
data RemoteRepoNotification
	-- | Update approximate lag (number of items local repo needs to pull from remote repo in order to catch up).
	= RemoteRepoLag Int64
	-- | Warning about some problems with connection to remote repo.
	-- Repo will try to reconnect, so connection may be restored.
	| RemoteRepoWarning String
	-- | Error report about problems with connection to remote repo.
	-- Repo will not try to reconnect, error is considered fatal.
	| RemoteRepoError String
	deriving Show

-- | Implementation for a local client repo, synchronized with remote server repo.
data HttpRemoteRepo = HttpRemoteRepo
	{ httpRemoteRepoClientRepo :: !ClientRepo
	, httpRemoteRepoRevisionVar :: !(TVar Revision)
	, httpRemoteRepoChangesChan :: !(TChan (B.ByteString, B.ByteString))
	, httpRemoteRepoNotificationsChan :: !(TChan RemoteRepoNotification)
	, httpRemoteRepoOperationsQueue :: !(TQueue (IO ()))
	}

instance RemoteRepo HttpRemoteRepo where
	remoteRepoRevision HttpRemoteRepo
		{ httpRemoteRepoRevisionVar = revisionVar
		} = readTVar revisionVar
	remoteRepoChangesChan HttpRemoteRepo
		{ httpRemoteRepoChangesChan = changesChan
		} = dupTChan changesChan
	remoteRepoNotificationsChan HttpRemoteRepo
		{ httpRemoteRepoNotificationsChan = notificationsChan
		} = dupTChan notificationsChan
	remoteRepoChange HttpRemoteRepo
		{ httpRemoteRepoClientRepo = repo
		, httpRemoteRepoOperationsQueue = operationsQueue
		} key value = writeTQueue operationsQueue $ clientRepoChange repo key value

-- | Initialize remote repo with HTTP connection to server.
initHttpRemoteRepo :: H.Manager -> ClientRepo -> T.Text -> IO (HttpRemoteRepo, IO ())
initHttpRemoteRepo httpManager repo url = withSpecialBook $ \bk -> do
	-- create template request by parsing url
	templateRequest <- H.parseUrl $ T.unpack url
	-- create things
	revisionVar <- newTVarIO =<< clientRepoRevision repo
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
				atomically $ writeTChan notificationsChan $ RemoteRepoWarning $ show (e :: H.HttpException)
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
							{ clientRepoPullRevision = revision
							, clientRepoPullLag = lag
							, clientRepoPullChanges = changes
							} <- pullClientRepo repo pull pushState
						-- set revision and send notifications
						atomically $ do
							writeTVar revisionVar revision
							forM_ changes $ writeTChan changesChan
							writeTChan notificationsChan $ RemoteRepoLag lag
					-- make pause before another sync
					throttle timeBefore
					-- sync again
					sync
				Left err -> throwIO $ DescribeFirstException ("failed to decode pull", err)
		sync

	book bk $ do
		stoppedVar <- newEmptyMVar
		let work = catch networking $ \e -> atomically $ writeTChan notificationsChan $ RemoteRepoError $ show (e :: SomeException)
		threadId <- forkFinally work $ \_ -> putMVar stoppedVar ()
		let stop = do
			killThread threadId
			takeMVar stoppedVar
		return ((), stop)

	return HttpRemoteRepo
		{ httpRemoteRepoClientRepo = repo
		, httpRemoteRepoRevisionVar = revisionVar
		, httpRemoteRepoChangesChan = changesChan
		, httpRemoteRepoNotificationsChan = notificationsChan
		, httpRemoteRepoOperationsQueue = operationsQueue
		}

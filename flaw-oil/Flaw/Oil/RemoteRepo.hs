{-|
Module: Flaw.Oil.RemoteRepo
Description: Asynchronous abstraction for local and remote repo.
License: MIT
-}

module Flaw.Oil.RemoteRepo
	( AsyncRepo(..)
	, HttpRemoteRepo()
	, initHttpRemoteRepo
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Monoid
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
	-- | Get channel informing about approximate lag (how many items client repo is behind).
	asyncRepoLagChan :: r -> STM (TChan Int64)
	-- | Perform change.
	sendAsyncRepoChange :: r -> B.ByteString -> B.ByteString -> STM ()

-- | Implementation for a local client repo, synchronized with remote server repo.
data HttpRemoteRepo = HttpRemoteRepo
	{ httpRemoteRepoClientRepo :: !ClientRepo
	, httpRemoteRepoChangesChan :: !(TChan (B.ByteString, B.ByteString))
	, httpRemoteRepoLagChan :: !(TChan Int64)
	, httpRemoteRepoOperationsQueue :: !(TQueue (IO ()))
	}

instance AsyncRepo HttpRemoteRepo where
	asyncRepoChangesChan HttpRemoteRepo
		{ httpRemoteRepoChangesChan = changesChan
		} = dupTChan changesChan
	asyncRepoLagChan HttpRemoteRepo
		{ httpRemoteRepoLagChan = lagChan
		} = dupTChan lagChan
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
	lagChan <- newBroadcastTChanIO

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

	-- helper function to do something http-related multiple times
	let tryFewTimes io = do
		let triesCount = 3 :: Int
		let trying i es = do
			if i < triesCount then catch io $ \e -> trying (i + 1) (e : es)
			else throwIO $ DescribeFirstException ("failed to do HTTP operation", es :: [H.HttpException])
		trying 0 []

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
							writeTChan lagChan lag
					-- make pause before another sync if needed
					let throttlePause = 5000000 -- microseconds
					timeAfter <- getCurrentTime
					let pause = max 0 $ min throttlePause $ throttlePause - (floor $ diffUTCTime timeAfter timeBefore)
					when (pause > 0) $ threadDelay $ pause
					-- sync again
					sync
				Left err -> throwIO $ DescribeFirstException ("failed to decode pull", err)
		sync

	book bk $ do
		stoppedVar <- newEmptyMVar
		threadId <- forkFinally networking $ \_ -> putMVar stoppedVar ()
		let stop = do
			killThread threadId
			takeMVar stoppedVar
		return ((), stop)

	return HttpRemoteRepo
		{ httpRemoteRepoClientRepo = repo
		, httpRemoteRepoChangesChan = changesChan
		, httpRemoteRepoLagChan = lagChan
		, httpRemoteRepoOperationsQueue = operationsQueue
		}

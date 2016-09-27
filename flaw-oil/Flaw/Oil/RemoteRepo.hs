{-|
Module: Flaw.Oil.RemoteRepo
Description: Remote repo abstraction.
License: MIT
-}

module Flaw.Oil.RemoteRepo
	( HttpRemoteRepo()
	, initHttpRemoteRepo
	, syncHttpRemoteRepo
	, watchHttpRemoteRepo
	) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Types as H

import Flaw.Exception
import Flaw.Oil.Repo

-- | Implementation for a local client repo, synchronized with remote server repo.
data HttpRemoteRepo = HttpRemoteRepo
	{ httpRemoteRepoHttpManager :: H.Manager
	, httpRemoteRepoTemplateRequest :: !H.Request
	}

initHttpRemoteRepo :: H.Manager -> T.Text -> IO (HttpRemoteRepo, Manifest)
initHttpRemoteRepo httpManager url = do

	-- create template request by parsing url
	templateRequest <- H.parseUrlThrow $ T.unpack url

	-- get manifest
	let request = templateRequest
		{ H.method = H.methodGet
		, H.queryString = T.encodeUtf8 $ T.pack "manifest"
		}
	body <- tryFewTimes $ H.withResponse request httpManager $ fmap BL.fromChunks . H.brConsume . H.responseBody
	manifest <- case S.decodeLazy body of
		Right manifest -> return manifest
		Left err -> throwIO $ DescribeFirstException ("failed to parse manifest", err)

	return (HttpRemoteRepo
		{ httpRemoteRepoHttpManager = httpManager
		, httpRemoteRepoTemplateRequest = templateRequest
		}, manifest)

syncHttpRemoteRepo :: HttpRemoteRepo -> Push -> IO Pull
syncHttpRemoteRepo HttpRemoteRepo
	{ httpRemoteRepoHttpManager = httpManager
	, httpRemoteRepoTemplateRequest = templateRequest
	} push = do
	-- prepare request
	let request = templateRequest
		{ H.method = H.methodPost
		, H.queryString = T.encodeUtf8 $ T.pack "sync"
		, H.requestBody = H.RequestBodyLBS $ S.encodeLazy push
		}
	-- send request
	body <- tryFewTimes $ H.withResponse request httpManager $ fmap BL.fromChunks . H.brConsume . H.responseBody
	-- decode body
	case S.decodeLazy body of
		Right p -> return p
		Left err -> throwIO $ DescribeFirstException ("failed to parse sync response", err)

watchHttpRemoteRepo :: HttpRemoteRepo -> Revision -> IO Revision
watchHttpRemoteRepo HttpRemoteRepo
	{ httpRemoteRepoHttpManager = httpManager
	, httpRemoteRepoTemplateRequest = templateRequest
	} clientRevision = do
	-- prepare request
	let request = templateRequest
		{ H.method = H.methodPost
		, H.queryString = T.encodeUtf8 $ T.pack "watch"
		, H.requestBody = H.RequestBodyLBS $ S.encodeLazy clientRevision
		}
	-- send request
	body <- H.withResponse request httpManager $ fmap BL.fromChunks . H.brConsume . H.responseBody
	-- decode body
	case S.decodeLazy body of
		Right serverRevision -> return serverRevision
		Left err -> throwIO $ DescribeFirstException ("failed to parse watch response", err)

-- helper function to do something http-related multiple times
tryFewTimes :: IO a -> IO a
tryFewTimes io = do
	let triesCount = 3 :: Int

	-- helper function to pause before another try
	let throttle timeBefore = do
		let throttlePause = 5000000 -- microseconds
		timeAfter <- getCurrentTime
		let pause = max 0 $ min throttlePause $ throttlePause - floor (diffUTCTime timeAfter timeBefore)
		when (pause > 0) $ threadDelay pause

	let trying i = do
		timeBefore <- getCurrentTime
		catch io $ \(SomeException e) -> do
			if i < triesCount then do
				throttle timeBefore
				trying $ i + 1
			else throwIO $ DescribeFirstException ("failed to do HTTP operation", e)

	trying 1

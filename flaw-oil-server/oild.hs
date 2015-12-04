{-|
Module: Main
Description: Oil server.
License: MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
	( main
	) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Middleware.Gzip as W
import qualified Network.Wai.Middleware.RequestLogger as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as O

import Flaw.Book
import Flaw.Oil.Repo
import Flaw.Oil.ServerRepo

main :: IO ()
main = run =<< O.execParser parser where
	parser = O.info (O.helper <*> opts)
		(  O.fullDesc
		<> O.progDesc "Opens FILE repo and starts listening on given PORT"
		<> O.header "oild - Flaw Oil server"
		)
	opts = Options
		<$> O.strOption
			(  O.long "repo"
			<> O.short 'r'
			<> O.metavar "FILE"
			<> O.help "server repo database file"
			)
		<*> O.option O.auto
			(  O.long "port"
			<> O.short 'p'
			<> O.value 8080 <> O.showDefault
			<> O.metavar "PORT"
			<> O.help "port to listen at"
			)

data Options = Options
	{ optionsRepoFileName :: String
	, optionsPort :: Int
	}

run :: Options -> IO ()
run Options
	{ optionsRepoFileName = repoFileName
	, optionsPort = port
	} = withBook $ \bk -> do

	-- form manifest with limits
	let manifest = defaultManifest

	-- calculate rough limit for sync requests
	let syncRequestBodyLimit = manifestMaxPushItemsCount manifest *
		(manifestMaxKeySize manifest + manifestMaxValueSize manifest + 0x1000) + 0x1000

	-- open repo
	repo <- book bk $ openServerRepo $ T.pack repoFileName

	-- start repo processing thread, book a shutdown handler
	operationVar <- newEmptyMVar
	let runOperations = join $ takeMVar operationVar
	book bk $ do
		stoppedVar <- newEmptyMVar
		void $ forkFinally runOperations $ \_ -> putMVar stoppedVar ()
		let stop = do
			putMVar operationVar $ return ()
			takeMVar stoppedVar
		return ((), stop)

	-- function to perform serialized operation
	let operation io = do
		resultVar <- newEmptyMVar
		putMVar operationVar $ do
			putMVar resultVar =<< io
			runOperations
		takeMVar resultVar

	-- start web server
	Warp.run port $ W.gzip W.def $ W.logStdout $ \request respond -> do

		-- helper methods
		let respondFail status msg = respond $ W.responseLBS status
			[(H.hContentType, "text/plain; charset=utf-8")] $
			BL.fromStrict $ msg

		case W.queryString request of
			[("manifest", Nothing)] -> respond $ W.responseLBS H.status200
				[(H.hContentType, "application/x-flawoil-manifest")] $
				S.encodeLazy manifest
			[("sync", Nothing)] -> do
				-- read request
				body <- liftM (BL.take $ fromIntegral $ syncRequestBodyLimit + 1) $ W.lazyRequestBody request
				if BL.length body <= fromIntegral syncRequestBodyLimit then do
					-- deserialize push
					case S.decodeLazy body of
						Right push -> do
							-- perform sync
							let userId = 1 -- TODO: implement user auth
							pull <- operation $ syncServerRepo repo manifest push userId
							-- respond with pull
							respond $ W.responseLBS H.status200
								[(H.hContentType, "application/x-flawoil-sync")] $
								S.encodeLazy pull
						Left _ -> respondFail H.status400 "wrong push format"
				else respondFail H.status400 "too big sync request"
			_ -> respondFail H.status404 "wrong request"

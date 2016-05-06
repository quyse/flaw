{-|
Module: Flaw.Network.WebSocket.Wai
Description: WebSocket server integration with WAI.
License: MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, RankNTypes #-}

module Flaw.Network.WebSocket.Wai
	( WebSocket()
	, webSocketWaiHandler
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as W
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Middleware.Routes as W
import qualified Network.WebSockets as WS

import Flaw.Network
import Flaw.Network.BiuoSocket

newtype WebSocket = WebSocket BiuoSocket deriving Socket

webSocketWaiHandler :: (sub -> W.Request -> IO (Either W.Response (WebSocket -> IO ()))) -> W.Handler sub
webSocketWaiHandler f W.Env
	{ W.envSub = sub
	} reqData sendResponse = do
	eitherResponse <- f sub request
	sendResponse $ case eitherResponse of
		Left response -> response
		Right processSocket -> case WS.websocketsApp WS.defaultConnectionOptions (serverApp processSocket) request of
			Just response -> response
			Nothing -> W.responseLBS HT.badRequest400 [] "wrong websocket"
	where
		request = W.waiReq reqData
		serverApp processSocket pendingConnection = do
			-- accept connection
			connection <- WS.acceptRequest pendingConnection
			-- create socket
			socket@BiuoSocket
				{ biuoSocketAliveVar = aliveVar
				, biuoSocketInQueue = inQueue
				, biuoSocketOutQueue = outQueue
				} <- atomically $ newBiuoSocket 1

			-- reading loop
			void $ forkIO $ flip finally (atomically $ writeTVar aliveVar False) $
				forever $ atomically . writeTBQueue inQueue =<< WS.receiveData connection

			-- writing loop
			void $ forkIO $ forever $ do
				delayVar <- registerDelay 5000000
				-- step loops until timeout
				let step = join $ atomically $ do
					let readPing = do
						timedOut <- readTVar delayVar
						unless timedOut retry
						return $ WS.sendPing connection B.empty
					let getBytes = do
						bytes <- readTQueue outQueue
						return $ do
							WS.sendBinaryData connection bytes
							step
					readPing `orElse` getBytes
				step

			-- run a handler
			processSocket $ WebSocket socket

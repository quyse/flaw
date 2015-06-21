{-|
Module: Flaw.Game.Socket
Description: Socket processing.
License: MIT
-}

module Flaw.Game.Socket
	( SocketProcess
	, processSocket
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as B
import qualified Network.Socket as N hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

type SocketProcess = (STM (TChan B.ByteString), B.ByteString -> STM ())

processSocket :: N.Socket -> IO SocketProcess
processSocket socket = do
	-- eliminate latency
	N.setSocketOption socket N.NoDelay 1

	-- create receiving chan
	receivingChan <- newBroadcastTChanIO
	-- create sending queue
	sendingQueue <- newTQueueIO

	-- run receiving thread
	_ <- forkIO $ do
		let work = do
			bytes <- N.recv socket 4096
			if B.null bytes then return ()
			else do
				atomically $ writeTChan receivingChan bytes
				work
		finally work $ do
			-- signal sending thread to end
			atomically $ writeTQueue sendingQueue B.empty
			-- signal receivers about end
			atomically $ writeTChan receivingChan B.empty

	-- run sending thread
	_ <- forkIO $ do
		let doSend bytes = do
			sent <- N.send socket bytes
			if sent == B.length bytes then return ()
			else doSend $ B.drop sent bytes
		let loop = do
			bytes <- atomically $ readTQueue sendingQueue
			if B.null bytes then return ()
			else do
				doSend bytes
				loop
		finally loop $ N.shutdown socket N.ShutdownBoth

	return (dupTChan receivingChan, writeTQueue sendingQueue)

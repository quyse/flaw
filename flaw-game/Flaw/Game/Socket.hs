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
import qualified Data.ByteString as B
import qualified Network.Socket as N hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

type SocketProcess = (STM (TChan B.ByteString), B.ByteString -> STM ())

processSocket :: N.Socket -> IO SocketProcess
processSocket socket = do
	-- eliminate latency
	--N.setSocketOption socket N.NoDelay 1

	-- create receiving chan
	receivingChan <- newBroadcastTChanIO
	-- run receiving thread
	_ <- forkIO $ do
		let work = do
			bytes <- N.recv socket 4096
			atomically $ writeTChan receivingChan bytes
			if B.length bytes == 0 then return ()
			else work
		work

	-- create sending queue
	sendingQueue <- newTQueueIO
	-- run sending thread
	_ <- forkIO $ do
		let doSend bytes = do
			sent <- N.send socket bytes
			if sent == B.length bytes then return ()
			else doSend $ B.drop sent bytes
		let loop = do
			bytes <- atomically $ readTQueue sendingQueue
			if B.length bytes == 0 then N.shutdown socket N.ShutdownSend
			else do
				doSend bytes
				loop
		loop

	return (dupTChan receivingChan, writeTQueue sendingQueue)

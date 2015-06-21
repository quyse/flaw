{-|
Module: Flaw.Game.Socket
Description: Socket processing.
License: MIT
-}

module Flaw.Game.Socket
	( Socket(..)
	, QueueSocket(..)
	, processNetworkSocket
	, receiveBytes
	, receiveSerialize
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as B
import Data.Monoid
import Data.Serialize
import qualified Network.Socket as N hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

class Socket s where
	send :: s -> B.ByteString -> STM ()
	receive :: s -> STM B.ByteString
	unreceive :: s -> B.ByteString -> STM ()

data QueueSocket = QueueSocket (TBQueue B.ByteString) (TBQueue B.ByteString)

instance Socket QueueSocket where
	send (QueueSocket _receiveQueue sendQueue) bytes = writeTBQueue sendQueue bytes
	receive (QueueSocket receiveQueue _sendQueue) = readTBQueue receiveQueue
	unreceive (QueueSocket receiveQueue _sendQueue) bytes = unGetTBQueue receiveQueue bytes

processNetworkSocket :: N.Socket -> Int -> Int -> IO QueueSocket
processNetworkSocket socket receiveQueueSize sendQueueSize = do
	-- eliminate latency
	N.setSocketOption socket N.NoDelay 1

	-- create receiving queue
	receiveQueue <- newTBQueueIO receiveQueueSize
	-- create sending queue
	sendQueue <- newTBQueueIO sendQueueSize

	-- run receiving thread
	_ <- forkIO $ do
		let work = do
			bytes <- N.recv socket 4096
			if B.null bytes then return ()
			else do
				atomically $ writeTBQueue receiveQueue bytes
				work
		finally work $ do
			-- signal sending thread to end
			atomically $ writeTBQueue sendQueue B.empty
			-- signal receivers about end
			atomically $ writeTBQueue receiveQueue B.empty

	-- run sending thread
	_ <- forkIO $ do
		let doSend bytes = do
			sent <- N.send socket bytes
			if sent == B.length bytes then return ()
			else doSend $ B.drop sent bytes
		let loop = do
			bytes <- atomically $ readTBQueue sendQueue
			if B.null bytes then return ()
			else do
				doSend bytes
				loop
		finally loop $ N.shutdown socket N.ShutdownBoth

	return $ QueueSocket receiveQueue sendQueue

-- | Read specified amount of bytes.
receiveBytes :: Socket s => s -> Int -> STM B.ByteString
receiveBytes socket len = do
	bytes <- receive socket
	let bytesLength = B.length bytes
	-- if it's end of stream, exit
	if bytesLength == 0 then return bytes
	-- else if it's not enough bytes, read more
	else if bytesLength < len then do
		restBytes <- receiveBytes socket $ len - bytesLength
		return $ bytes <> restBytes
	-- else if it's too many bytes, put them back
	else if bytesLength > len then do
		let (neededBytes, restBytes) = B.splitAt len bytes
		unreceive socket restBytes
		return neededBytes
	else return bytes

-- | Read serializable data.
receiveSerialize :: (Socket s, Serialize a) => s -> STM (Either String a)
receiveSerialize socket = loop $ runGetPartial get where
	loop parse = do
		bytes <- receive socket
		case parse bytes of
			Fail err rest -> do
				if B.null rest then return ()
				else unreceive socket rest
				return $ Left err
			Partial nextParse -> loop nextParse
			Done result rest -> do
				if B.null rest then return ()
				else unreceive socket rest
				return $ Right result

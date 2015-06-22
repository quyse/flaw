{-|
Module: Flaw.Game.Socket
Description: Socket processing.
License: MIT
-}

module Flaw.Game.Socket
	( Socket(..)
	, BoundedQueueSocket(..)
	, BoundedReceiveQueueSocket(..)
	, processNetworkSocket
	, receiveBytes
	, receiveSerialize
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Serialize as S
import qualified Network.Socket as N hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

class Socket s where
	send :: s -> B.ByteString -> STM ()
	receive :: s -> STM B.ByteString
	unreceive :: s -> B.ByteString -> STM ()

data BoundedQueueSocket = BoundedQueueSocket (TBQueue B.ByteString) (TBQueue B.ByteString)

instance Socket BoundedQueueSocket where
	send (BoundedQueueSocket _receiveQueue sendQueue) bytes = writeTBQueue sendQueue bytes
	receive (BoundedQueueSocket receiveQueue _sendQueue) = readTBQueue receiveQueue
	unreceive (BoundedQueueSocket receiveQueue _sendQueue) bytes = unGetTBQueue receiveQueue bytes

data BoundedReceiveQueueSocket = BoundedReceiveQueueSocket (TBQueue B.ByteString) (TQueue B.ByteString)

instance Socket BoundedReceiveQueueSocket where
	send (BoundedReceiveQueueSocket _receiveQueue sendQueue) bytes = writeTQueue sendQueue bytes
	receive (BoundedReceiveQueueSocket receiveQueue _sendQueue) = readTBQueue receiveQueue
	unreceive (BoundedReceiveQueueSocket receiveQueue _sendQueue) bytes = unGetTBQueue receiveQueue bytes

processNetworkSocket :: N.Socket -> Int -> IO BoundedReceiveQueueSocket
processNetworkSocket socket receiveQueueSize = do
	-- eliminate latency
	N.setSocketOption socket N.NoDelay 1

	-- create receiving queue
	receiveQueue <- newTBQueueIO receiveQueueSize
	-- create sending queue
	sendQueue <- newTQueueIO

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
			atomically $ writeTQueue sendQueue B.empty
			-- signal receivers about end
			atomically $ writeTBQueue receiveQueue B.empty

	-- run sending thread
	_ <- forkIO $ do
		let doSend bytes = do
			sent <- N.send socket bytes
			if sent == B.length bytes then return ()
			else doSend $ B.drop sent bytes
		let loop = do
			bytes <- atomically $ readTQueue sendQueue
			if B.null bytes then return ()
			else do
				doSend bytes
				loop
		finally loop $ N.shutdown socket N.ShutdownBoth

	return $ BoundedReceiveQueueSocket receiveQueue sendQueue

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
receiveSerialize :: (Socket s, S.Serialize a) => s -> S.Get a -> STM (Either String a)
receiveSerialize socket g = loop $ S.runGetPartial g where
	loop parse = do
		bytes <- receive socket
		case parse bytes of
			S.Fail err rest -> do
				if B.null rest then return ()
				else unreceive socket rest
				return $ Left err
			S.Partial nextParse -> loop nextParse
			S.Done result rest -> do
				if B.null rest then return ()
				else unreceive socket rest
				return $ Right result

{-|
Module: Flaw.Game.Socket
Description: Socket processing.
License: MIT
-}

module Flaw.Game.Socket
	( SendSocket(..)
	, ReceiveSocket(..)
	, UnreceiveSocket(..)
	, SendReceiveSocket(..)
	, NetworkSocket
	, processNetworkSocket
	, receiveBytes
	, receiveSerialize
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Serialize as S
import qualified Network.Socket as N hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as N

class SendSocket s where
	send :: s a -> a -> STM ()

class ReceiveSocket s where
	receive :: s a -> STM a

class UnreceiveSocket s where
	unreceive :: s a -> a -> STM ()

instance SendSocket TQueue where
	send = writeTQueue

instance ReceiveSocket TQueue where
	receive = readTQueue

instance UnreceiveSocket TQueue where
	unreceive = unGetTQueue

instance SendSocket TBQueue where
	send = writeTBQueue

instance ReceiveSocket TBQueue where
	receive = readTBQueue

instance UnreceiveSocket TBQueue where
	unreceive = unGetTBQueue

data SendReceiveSocket s r a = SendReceiveSocket (s a) (r a)

instance SendSocket s => SendSocket (SendReceiveSocket s r) where
	send (SendReceiveSocket s _r) = send s

instance ReceiveSocket r => ReceiveSocket (SendReceiveSocket s r) where
	receive (SendReceiveSocket _s r) = receive r

instance UnreceiveSocket r => UnreceiveSocket (SendReceiveSocket s r) where
	unreceive (SendReceiveSocket _s r) = unreceive r

instance ReceiveSocket STM where
	receive = id

type NetworkSocket = SendReceiveSocket TQueue TBQueue B.ByteString

processNetworkSocket :: N.Socket -> Int -> IO NetworkSocket
processNetworkSocket socket receiveQueueSize = do
	-- eliminate latency
	N.setSocketOption socket N.NoDelay 1

	-- create sending queue
	sendQueue <- newTQueueIO

	-- run sending thread
	_ <- forkIO $ do
		let doSend bytes = do
			sent <- N.send socket bytes
			unless (sent == B.length bytes) $ doSend $ B.drop sent bytes
		let loop = do
			bytes <- atomically $ readTQueue sendQueue
			unless (B.null bytes) $ do
				doSend bytes
				loop
		finally loop $ N.shutdown socket N.ShutdownBoth

	-- create receiving queue
	receiveQueue <- newTBQueueIO receiveQueueSize

	-- run receiving thread
	_ <- forkIO $ do
		let work = do
			bytes <- N.recv socket 4096
			unless (B.null bytes) $ do
				atomically $ writeTBQueue receiveQueue bytes
				work
		finally work $ do
			-- signal sending thread to end
			atomically $ writeTQueue sendQueue B.empty
			-- signal receivers about end
			atomically $ writeTBQueue receiveQueue B.empty

	return $ SendReceiveSocket sendQueue receiveQueue

-- | Read specified amount of bytes.
receiveBytes :: (ReceiveSocket s, UnreceiveSocket s) => s B.ByteString -> Int -> STM B.ByteString
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
receiveSerialize :: (ReceiveSocket s, UnreceiveSocket s, S.Serialize a) => s B.ByteString -> S.Get a -> STM (Either String a)
receiveSerialize socket g = loop $ S.runGetPartial g where
	loop parse = do
		bytes <- receive socket
		if B.null bytes then return $ Left "socket ended suddenly"
		else case parse bytes of
			S.Fail err rest -> do
				unless (B.null rest) $ unreceive socket rest
				return $ Left err
			S.Partial nextParse -> loop nextParse
			S.Done result rest -> do
				unless (B.null rest) $ unreceive socket rest
				return $ Right result

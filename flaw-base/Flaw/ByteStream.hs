{-|
Module: Flaw.ByteStream
Description: Helper structure for organizing async byte streams.
License: MIT
-}

module Flaw.ByteStream
	( ByteStream(..)
	, newByteStream
	, byteStreamLength
	, pushByteStream
	, pullByteStream
	, isByteStreamFinished
	, finishByteStream
	) where

import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as BL
import Data.Int

data ByteStream = ByteStream
	{ byteStreamBytesVar :: {-# UNPACK #-} !(TVar BL.ByteString)
	, byteStreamFinishedVar :: {-# UNPACK #-} !(TVar Bool)
	}

newByteStream :: STM ByteStream
newByteStream = ByteStream
	<$> newTVar BL.empty
	<*> newTVar False

byteStreamLength :: ByteStream -> STM Int64
byteStreamLength = (BL.length <$>) . readTVar . byteStreamBytesVar

pushByteStream :: ByteStream -> BL.ByteString -> STM ()
pushByteStream ByteStream
	{ byteStreamBytesVar = bytesVar
	} bytes = modifyTVar' bytesVar (<> bytes)

pullByteStream :: ByteStream -> Int64 -> STM BL.ByteString
pullByteStream ByteStream
	{ byteStreamBytesVar = bytesVar
	} size = do
	bytes <- readTVar bytesVar
	let (pulledBytes, restBytes) = BL.splitAt size bytes
	writeTVar bytesVar restBytes
	return pulledBytes

isByteStreamFinished :: ByteStream -> STM Bool
isByteStreamFinished = readTVar . byteStreamFinishedVar

finishByteStream :: ByteStream -> STM ()
finishByteStream = flip writeTVar True . byteStreamFinishedVar

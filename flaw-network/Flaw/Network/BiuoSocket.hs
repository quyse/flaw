{-|
Module: Flaw.Network.Socket
Description: Bounded-in unbounded-out socket primitive.
License: MIT
-}

module Flaw.Network.BiuoSocket
	( BiuoSocket(..)
	, newBiuoSocket
	) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as B

import Flaw.Network

data BiuoSocket = BiuoSocket
	{ biuoSocketAliveVar :: {-# UNPACK #-} !(TVar Bool)
	, biuoSocketInQueue :: {-# UNPACK #-} !(TBQueue B.ByteString)
	, biuoSocketOutQueue :: {-# UNPACK #-} !(TQueue B.ByteString)
	}

newBiuoSocket :: Int -> STM BiuoSocket
newBiuoSocket inBound = do
	aliveVar <- newTVar True
	inQueue <- newTBQueue inBound
	outQueue <- newTQueue
	return BiuoSocket
		{ biuoSocketAliveVar = aliveVar
		, biuoSocketInQueue = inQueue
		, biuoSocketOutQueue = outQueue
		}

instance Socket BiuoSocket where
	readSocket BiuoSocket
		{ biuoSocketAliveVar = aliveVar
		, biuoSocketInQueue = inQueue
		} = do
		alive <- readTVar aliveVar
		unless alive $ throwSTM SocketEndException
		readTBQueue inQueue
	writeSocket BiuoSocket
		{ biuoSocketOutQueue = outQueue
		} = writeTQueue outQueue
	closeSocket BiuoSocket
		{ biuoSocketAliveVar = aliveVar
		} = writeTVar aliveVar False

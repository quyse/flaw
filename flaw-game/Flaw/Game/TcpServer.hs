{-|
Module: Flaw.Game.TcpServer
Description: TCP network server.
License: MIT
-}

module Flaw.Game.TcpServer
	( runTcpServer
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Network.Socket as N

import Flaw.Exception
import Flaw.Game.Socket

runTcpServer :: String -> Int -> IO (STM (TChan SocketProcess))
runTcpServer host port = do
	-- resolve host name
	addrs <- N.getAddrInfo (Just N.defaultHints
		{ N.addrFlags = [N.AI_NUMERICSERV, N.AI_ADDRCONFIG]
		}) (Just host) (Just $ show port)

	socket <- case addrs of
		(N.AddrInfo
			{ N.addrFamily = addrFamily
			, N.addrProtocol = addrProtocol
			, N.addrAddress = addrAddress
			}) : _restAddrs -> do
			socket <- N.socket addrFamily N.Stream addrProtocol
			N.bind socket addrAddress
			N.listen socket 5
			return socket
		_ -> throwIO $ DescribeFirstException "failed to resolve host name"

	-- create chan for sockets
	chan <- newBroadcastTChanIO

	-- accept clients in separate thread
	let work = do
		(clientSocket, _clientAddr) <- N.accept socket
		process <- processSocket clientSocket
		atomically $ writeTChan chan process
		work
	_ <- forkIO $ onException work (N.close socket)

	return $ dupTChan chan

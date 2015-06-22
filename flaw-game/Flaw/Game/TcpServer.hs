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

runTcpServer :: String -> Int -> IO (STM BoundedReceiveQueueSocket)
runTcpServer host port = do
	-- resolve host name
	addrs <- N.getAddrInfo (Just N.defaultHints
		{ N.addrFlags = [N.AI_NUMERICSERV, N.AI_ADDRCONFIG]
		}) (Just host) (Just $ show port)

	listeningSocket <- case addrs of
		(N.AddrInfo
			{ N.addrFamily = addrFamily
			, N.addrProtocol = addrProtocol
			, N.addrAddress = addrAddress
			}) : _restAddrs -> do
			listeningSocket <- N.socket addrFamily N.Stream addrProtocol
			N.bind listeningSocket addrAddress
			N.listen listeningSocket 5
			return listeningSocket
		_ -> throwIO $ DescribeFirstException "failed to resolve host name"

	-- create queue for sockets
	queue <- newTBQueueIO 16

	-- accept clients in separate thread
	let work = do
		(clientSocket, _clientAddr) <- N.accept listeningSocket
		socket <- processNetworkSocket clientSocket 16
		atomically $ writeTBQueue queue socket
		work
	_ <- forkIO $ finally work $ N.close listeningSocket

	return $ readTBQueue queue

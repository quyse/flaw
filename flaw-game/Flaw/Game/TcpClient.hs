{-|
Module: Flaw.Game.TcpClient
Description: TCP network client.
License: MIT
-}

module Flaw.Game.TcpClient
	( connectTcpClient
	) where

import Control.Exception
import qualified Network.Socket as N

import Flaw.Exception
import Flaw.Game.Socket

connectTcpClient :: String -> Int -> IO NetworkSocket
connectTcpClient host port = do
	-- resolve host name
	addrs <- N.getAddrInfo (Just N.defaultHints
		{ N.addrFlags = [N.AI_NUMERICSERV, N.AI_ADDRCONFIG]
		}) (Just host) (Just $ show port)

	-- try to connect
	let
		tryConnect [] = throwIO $ DescribeFirstException "unable to connect host"
		tryConnect (N.AddrInfo
			{ N.addrFamily = addrFamily
			, N.addrProtocol = addrProtocol
			, N.addrAddress = addrAddress
			} : restAddrInfos) = onException doConnect (tryConnect restAddrInfos) where
			doConnect = do
				socket <- N.socket addrFamily N.Stream addrProtocol
				N.connect socket addrAddress
				return socket

	socket <- tryConnect addrs
	processNetworkSocket socket 16

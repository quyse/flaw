{-|
Module: Flaw.Game.TlsSocket
Description: TLS network socket.
License: MIT
-}

module Flaw.Game.TlsSocket
	( loadTlsServerParams
	, initTlsClientParams
	, TlsSocket
	, runTlsSocket
	) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Data.Default.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import System.X509

import Flaw.Exception
import Flaw.Game.Socket

tlsSupported :: TLS.Supported
tlsSupported = def
	{ TLS.supportedVersions = [TLS.TLS12]
	, TLS.supportedCiphers = TLS.ciphersuite_all
	, TLS.supportedSecureRenegotiation = False
	, TLS.supportedSession = False
	}

loadTlsServerParams :: FilePath -> [FilePath] -> FilePath -> IO TLS.ServerParams
loadTlsServerParams certPath intermediateCertsPaths certKeyPath = do
	eitherCredential <- TLS.credentialLoadX509Chain certPath intermediateCertsPaths certKeyPath
	case eitherCredential of
		Right credential -> return def
			{ TLS.serverWantClientCert = False
			, TLS.serverCACertificates = []
			, TLS.serverDHEParams = Nothing
			, TLS.serverShared = def
				{ TLS.sharedCredentials = TLS.Credentials [credential]
				, TLS.sharedSessionManager = TLS.noSessionManager
				}
			, TLS.serverSupported = tlsSupported
			}
		Left err -> throwIO $ DescribeFirstException err

initTlsClientParams :: IO (String -> TLS.ClientParams)
initTlsClientParams = do
	certificateStore <- getSystemCertificateStore
	return $ \host -> TLS.ClientParams
		{ TLS.clientUseMaxFragmentLength = Nothing
		, TLS.clientServerIdentification = (host, B.empty)
		, TLS.clientUseServerNameIndication = True
		, TLS.clientWantSessionResume = Nothing
		, TLS.clientShared = TLS.Shared
			{ TLS.sharedCredentials = TLS.Credentials []
			, TLS.sharedSessionManager = TLS.noSessionManager
			, TLS.sharedCAStore = certificateStore
			, TLS.sharedValidationCache = def
			}
		, TLS.clientHooks = def
		, TLS.clientSupported = tlsSupported
		}

type TlsSocket = SendReceiveSocket TBQueue TBQueue B.ByteString

runTlsSocket :: (TLS.TLSParams p, SendSocket s, ReceiveSocket s, UnreceiveSocket s) => p -> s B.ByteString -> IO TlsSocket
runTlsSocket params underlyingSocket = do
	-- create backend
	let backend = TLS.Backend
		{ TLS.backendFlush = return ()
		, TLS.backendClose = atomically $ send underlyingSocket B.empty
		, TLS.backendSend = \bytes -> atomically $ send underlyingSocket bytes
		, TLS.backendRecv = \len -> atomically $ receiveBytes underlyingSocket len
		}

	-- create context
	context <- TLS.contextNew backend params
	-- do handshake
	TLS.handshake context

	-- create new sending queue
	sendQueue <- newTBQueueIO 16

	-- run sending thread
	_ <- forkIO $ do
		let loop = do
			bytes <- atomically $ readTBQueue sendQueue
			if B.null bytes then return ()
			else do
				TLS.sendData context $ BL.fromStrict bytes
				loop
		finally loop $ do
			-- shutdown TLS connection
			TLS.bye context
			-- shutdown underlying socket
			atomically $ send underlyingSocket B.empty

	-- create new receiving queue
	receiveQueue <- newTBQueueIO 16

	-- run receiving thread
	_ <- forkIO $ do
		let loop = do
			bytes <- TLS.recvData context
			if B.null bytes then return ()
			else do
				atomically $ writeTBQueue receiveQueue bytes
				loop
		finally loop $ do
			-- make sending thread to finish
			atomically $ writeTBQueue sendQueue B.empty
			-- signal receivers about end
			atomically $ writeTBQueue receiveQueue B.empty

	return $ SendReceiveSocket sendQueue receiveQueue

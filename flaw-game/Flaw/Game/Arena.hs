{-|
Module: Flaw.Game.Arena
Description: Arena server.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies #-}

module Flaw.Game.Arena
	( AccountId
	, Arena(..)
	, serveArena
	, connectArena
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Serialize as S
import Data.Word
import GHC.Generics

import Flaw.Game.Socket
import Flaw.Game.Ticket

newtype ProtocolVersion = ProtocolVersion Word32 deriving Eq

currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = ProtocolVersion 1

instance S.Serialize ProtocolVersion where
	put (ProtocolVersion number) = S.put number
	get = liftM ProtocolVersion S.get

data ClientHandshakePacket
	= ClientHandshakePacket ProtocolVersion B.ByteString
	deriving Generic

instance S.Serialize ClientHandshakePacket

data ServerHandshakePacket
	= ServerFormatErrorPacket
	| ServerVersionMismatchPacket !ProtocolVersion
	| ServerAuthErrorPacket
	| ServerArenaStartedPacket
	deriving Generic

instance S.Serialize ServerHandshakePacket

type AccountId = B.ByteString

class Arena a where
	data ArenaId a :: *
	data ArenaPass a :: *
	-- | Run arena.
	-- Does not return until arena closes.
	runArena :: a -> IO ()
	-- | Join account into arena and run through.
	-- Does not return until account leaves arena.
	playArena :: (SendSocket s, ReceiveSocket s, UnreceiveSocket s) => a -> AccountId -> ArenaPass a -> s B.ByteString -> IO ()
	-- | Kick account.
	kickAccountFromArena :: AccountId -> STM ()

-- | Run arena server.
serveArena ::
	( SendSocket s
	, ReceiveSocket s
	, UnreceiveSocket s
	, ReceiveSocket ss
	, Ticket t
	, Arena a
	, Ord (ArenaId a)
	, S.Serialize (ArenaId a)
	, S.Serialize (ArenaPass a)
	, S.Serialize (t (AccountId, ArenaId a, ArenaPass a))
	)
	=> ss (s B.ByteString)
	-> TicketWitness t
	-> (ArenaId a -> STM a)
	-> IO ()
serveArena socketSocket ticketWitness newArena = do
	-- create map of arenas
	arenasVar <- newTVarIO M.empty -- :: TVar (Map ArenaId Arena)

	-- loop for incoming clients
	forever $ do
		-- get socket
		socket <- atomically $ receive socketSocket

		-- helper function to send packet to client
		let reply message = atomically $ send socket $ S.encode message
		-- helper function to receive packet from client
		let listen = atomically $ receiveSerialize socket S.get

		-- function to join arena by client
		let joinArena (accountId, arenaId, arenaPass) = do
			let play arena = do
				reply ServerArenaStartedPacket
				playArena arena accountId arenaPass socket
			action <- atomically $ do
				-- get arena, create new if needed
				arenas <- readTVar arenasVar
				case M.lookup arenaId arenas of
					Just arena -> return $ play arena
					Nothing -> do
						arena <- newArena arenaId
						writeTVar arenasVar $ M.insert arenaId arena arenas
						return $ do
							_ <- forkIO $ finally (runArena arena) $ do
								atomically $ modifyTVar' arenasVar $ M.delete arenaId
							play arena
			action

		-- fork thread for client
		let work = do
			-- get handshake packet
			eitherHandshakePacket <- listen
			case eitherHandshakePacket of
				Right (ClientHandshakePacket protocolVersion ticketBytes) -> do
					-- check version
					if protocolVersion /= currentProtocolVersion then reply $ ServerVersionMismatchPacket currentProtocolVersion
					else case S.decode ticketBytes of
						Right ticket -> do
							case verifyTicket ticketWitness ticket of
								Just aap -> joinArena aap
								Nothing -> reply ServerAuthErrorPacket
						Left _ -> reply ServerFormatErrorPacket
				Left _ -> reply ServerFormatErrorPacket

		_ <- forkIO $ finally work $ atomically $ send socket B.empty
		return ()

-- | Connect to arena server.
connectArena ::
	( SendSocket s
	, ReceiveSocket s
	, UnreceiveSocket s
	, Ticket t
	, Arena a
	, S.Serialize (t (AccountId, ArenaId a, ArenaPass a))
	)
	=> s B.ByteString
	-> t (AccountId, ArenaId a, ArenaPass a)
	-> IO Bool
connectArena socket ticket = do
	-- send handshake packet
	atomically $ send socket $ S.encode $ ClientHandshakePacket currentProtocolVersion $ S.encode ticket
	-- wait for answer
	serverHandshakePacket <- atomically $ receiveSerialize socket S.get
	case serverHandshakePacket of
		Right ServerArenaStartedPacket -> return True
		_ -> return False

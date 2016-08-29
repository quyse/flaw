{-|
Module: Flaw.Network.Session
Description: Session management.
License: MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flaw.Network.Session
	( SessionManager()
	, SessionId(..)
	, Session(..)
	, newSessionManager
	, newSession
	, getSession
	, deleteSession
	, ensureActiveSession
	, SessionException(..)
	) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Crypto.Random
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S

import Flaw.Book
import Flaw.Flow

-- | Session manager handles user sessions.
-- User authentication is usually rather expensive, as it involves DB access and/or
-- accessing external means such as OAuth requests. Once user authenticity is established
-- (and persistent user id is known), user session can be created and used for the following
-- authentication attempts.
data SessionManager uid = SessionManager
	{ sessionManagerSessionsByIdVar :: {-# UNPACK #-} !(TVar (HM.HashMap SessionId (Session uid)))
	, sessionManagerSessionsByUserIdVar :: {-# UNPACK #-} !(TVar (HM.HashMap uid (Session uid)))
	, sessionManagerNextSessionIdVar :: {-# UNPACK #-} !(TMVar SessionId)
	}

-- | Session id.
newtype SessionId = SessionId B.ByteString deriving (Eq, Hashable, S.Serialize, Show)

-- | Session.
data Session uid = Session
	{ sessionId :: {-# UNPACK #-} !SessionId
	, sessionUserId :: !uid
	-- | Is session active?
	-- Checking value of this var allows to guarantee that
	-- session is active during `STM` transaction, see `ensureActiveSession`.
	, sessionActiveVar :: {-# UNPACK #-} !(TVar Bool)
	}

-- | Initialize session manager.
newSessionManager :: IO (SessionManager uid, IO ())
newSessionManager = withSpecialBook $ \bk -> do
	-- session maps
	sessionsByIdVar <- newTVarIO HM.empty
	sessionsByUserIdVar <- newTVarIO HM.empty

	-- thread generating random session ids
	nextSessionIdVar <- newEmptyTMVarIO
	book bk $ forkFlow $ let
		step drg = do
			let (bytes, newDrg) = randomBytesGenerate 16 drg
			atomically $ putTMVar nextSessionIdVar $ SessionId bytes
			step newDrg
		in step =<< getSystemDRG

	return SessionManager
		{ sessionManagerSessionsByIdVar = sessionsByIdVar
		, sessionManagerSessionsByUserIdVar = sessionsByUserIdVar
		, sessionManagerNextSessionIdVar = nextSessionIdVar
		}

-- | Create session for a user.
-- Removes any existing user's session.
newSession :: (Eq uid, Hashable uid) => SessionManager uid -> uid -> STM (Session uid)
newSession sessionManager@SessionManager
	{ sessionManagerSessionsByIdVar = sessionsByIdVar
	, sessionManagerSessionsByUserIdVar = sessionsByUserIdVar
	, sessionManagerNextSessionIdVar = nextSessionIdVar
	} userId = do
	-- delete existing session
	sessionsByUserId <- readTVar sessionsByUserIdVar
	case HM.lookup userId sessionsByUserId of
		Just session -> deleteSession sessionManager session
		Nothing -> return ()
	-- generate new session
	sid <- takeTMVar nextSessionIdVar
	activeVar <- newTVar True
	let session = Session
		{ sessionId = sid
		, sessionUserId = userId
		, sessionActiveVar = activeVar
		}
	-- add session to manager
	modifyTVar' sessionsByIdVar $ HM.insert sid session
	modifyTVar' sessionsByUserIdVar $ HM.insert userId session
	return session

-- | Try to get session by session id.
getSession :: SessionManager uid -> SessionId -> STM (Maybe (Session uid))
getSession SessionManager
	{ sessionManagerSessionsByIdVar = sessionsByIdVar
	} sid = HM.lookup sid <$> readTVar sessionsByIdVar

-- | Delete session.
deleteSession :: (Eq uid, Hashable uid) => SessionManager uid -> Session uid -> STM ()
deleteSession SessionManager
	{ sessionManagerSessionsByIdVar = sessionsByIdVar
	, sessionManagerSessionsByUserIdVar = sessionsByUserIdVar
	} Session
	{ sessionId = sid
	, sessionUserId = uid
	, sessionActiveVar = activeVar
	} = do
	modifyTVar' sessionsByIdVar $ HM.delete sid
	modifyTVar' sessionsByUserIdVar $ HM.delete uid
	writeTVar activeVar False

-- | Ensure session is active during current STM transaction.
-- Throws `InactiveSessionException` if it's not.
ensureActiveSession :: Session uid -> STM ()
ensureActiveSession Session
	{ sessionActiveVar = activeVar
	} = do
	active <- readTVar activeVar
	unless active $ throwSTM InactiveSessionException

data SessionException
	= InactiveSessionException
	deriving Show

instance Exception SessionException

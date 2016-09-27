{-|
Module: Flaw.Oil.ClientRepo
Description: Oil client repo.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Flaw.Oil.ClientRepo
	( ClientRepo()
	, openClientRepo
	, clientRepoRevision
	, clientRepoGetRevision
	, clientRepoGetRevisionValue
	, clientRepoChange
	, clientRepoGetKeysPrefixed
	, ClientRepoPushState(..)
	, pushClientRepo
	, ClientRepoPullInfo(..)
	, pullClientRepo
	, cleanupClientRepo
	, syncClientRepo
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Foreign.C.Types

import Flaw.Book
import Flaw.Data.Sqlite
import Flaw.Exception
import Flaw.Oil.Repo

data ClientRepo = ClientRepo
	{ clientRepoDb :: !SqliteDb
	, clientRepoStmtManifestGet            :: !SqliteStmt
	, clientRepoStmtManifestSet            :: !SqliteStmt
	, clientRepoStmtGetKeyItems            :: !SqliteStmt
	, clientRepoStmtGetKeyItemsByOneItemId :: !SqliteStmt
	, clientRepoStmtGetKeyItemKey          :: !SqliteStmt
	, clientRepoStmtGetKeyItemRev          :: !SqliteStmt
	, clientRepoStmtGetKeyItemRevValue     :: !SqliteStmt
	, clientRepoStmtAddKeyItem             :: !SqliteStmt
	, clientRepoStmtChangeKeyItemRevStatus :: !SqliteStmt
	, clientRepoStmtChangeKeyItemRevValue  :: !SqliteStmt
	, clientRepoStmtSelectKeysToPush       :: !SqliteStmt
	, clientRepoStmtGetPushLag             :: !SqliteStmt
	, clientRepoStmtMassChangeStatus       :: !SqliteStmt
	, clientRepoStmtEnumerateKeysBegin     :: !SqliteStmt
	, clientRepoStmtEnumerateKeysBeginEnd  :: !SqliteStmt
	, clientRepoStmtAddChunk               :: !SqliteStmt
	, clientRepoStmtPreCutChunks           :: !SqliteStmt
	, clientRepoStmtCutChunks              :: !SqliteStmt
	, clientRepoStmtGetUpperRevision       :: !SqliteStmt
	}

openClientRepo :: T.Text -> IO (ClientRepo, IO ())
openClientRepo fileName = describeException "failed to open oil client repo" $ withSpecialBook $ \bk -> do
	-- open db
	db <- book bk $ openRepoDb fileName clientRepoVersion

	-- enable normal synchronous mode (db correctness is a concern, but durability is not for client repo)
	sqliteExec db $ T.pack "PRAGMA synchronous = NORMAL"

	-- ensure tables and indices exist

	-- manifest table
	sqliteExec db $ T.pack
		"CREATE TABLE IF NOT EXISTS manifest (\
		\key INTEGER PRIMARY KEY, \
		\value ANY NOT NULL)"
	-- items table
	sqliteExec db $ T.pack
		"CREATE TABLE IF NOT EXISTS items (\
		\id INTEGER PRIMARY KEY, \
		\value BLOB NOT NULL, \
		\key BLOB NOT NULL, \
		\rev INTEGER NOT NULL, \
		\status INTEGER NOT NULL)"
	-- items_key_status index
	sqliteExec db $ T.pack
		"CREATE UNIQUE INDEX IF NOT EXISTS items_key_status ON items (key, status)"
	-- items_status_partial index
	sqliteExec db $ T.pack $
		"CREATE INDEX IF NOT EXISTS items_status_partial ON items (status) WHERE\
		\ status = " ++ show (ItemStatusClient :: Int) ++
		" OR status = " ++ show (ItemStatusTransient :: Int) ++
		" OR status = " ++ show (ItemStatusPostponed :: Int)
	-- chunks table
	sqliteExec db $ T.pack
		"CREATE TABLE IF NOT EXISTS chunks (\
		\prerev INTEGER PRIMARY KEY, \
		\postrev INTEGER NOT NULL)"

	-- create statements
	let createStmt str = book bk $ sqliteStmt db $ T.pack str
	stmtManifestGet            <- createStmt "SELECT value FROM manifest WHERE key = ?1"
	stmtManifestSet            <- createStmt "INSERT OR REPLACE INTO manifest (key, value) VALUES (?1, ?2)"
	stmtGetKeyItems            <- createStmt "SELECT id, status FROM items WHERE key = ?1"
	stmtGetKeyItemsByOneItemId <- createStmt "SELECT id, status FROM items WHERE key = (SELECT key FROM items WHERE id = ?1)"
	stmtGetKeyItemKey          <- createStmt "SELECT key FROM items WHERE id = ?1"
	stmtGetKeyItemRev          <- createStmt "SELECT rev FROM items WHERE id = ?1"
	stmtGetKeyItemRevValue     <- createStmt "SELECT rev, value FROM items WHERE id = ?1"
	stmtAddKeyItem             <- createStmt "INSERT OR REPLACE INTO items (key, value, rev, status) VALUES (?1, ?2, ?3, ?4)"
	stmtChangeKeyItemRevStatus <- createStmt "UPDATE OR REPLACE items SET rev = ?2, status = ?3 WHERE id = ?1"
	stmtChangeKeyItemRevValue  <- createStmt "UPDATE items SET rev = ?2, value = ?3 WHERE id = ?1"
	stmtSelectKeysToPush       <- createStmt $ "SELECT id, key, value FROM items WHERE status = " ++ show (ItemStatusClient :: Int) ++ " ORDER BY id LIMIT ?1"
	stmtGetPushLag             <- createStmt $ "SELECT COUNT(*) FROM items WHERE status = " ++ show (ItemStatusClient :: Int)
	stmtMassChangeStatus       <- createStmt "UPDATE OR REPLACE items SET status = ?2 WHERE status = ?1"
	stmtEnumerateKeysBegin     <- createStmt "SELECT DISTINCT key FROM items WHERE key >= ?1 ORDER BY key"
	stmtEnumerateKeysBeginEnd  <- createStmt "SELECT DISTINCT key FROM items WHERE key >= ?1 AND key < ?2 ORDER BY key"
	stmtAddChunk               <- createStmt "INSERT INTO chunks (prerev, postrev) VALUES (?1, ?2)"
	stmtPreCutChunks           <- createStmt "SELECT MAX(postrev) FROM chunks WHERE prerev <= ?1"
	stmtCutChunks              <- createStmt "DELETE FROM chunks WHERE prerev <= ?1"
	stmtGetUpperRevision       <- createStmt "SELECT MIN(prerev) FROM chunks"

	return ClientRepo
		{ clientRepoDb = db
		, clientRepoStmtManifestGet            = stmtManifestGet
		, clientRepoStmtManifestSet            = stmtManifestSet
		, clientRepoStmtGetKeyItems            = stmtGetKeyItems
		, clientRepoStmtGetKeyItemsByOneItemId = stmtGetKeyItemsByOneItemId
		, clientRepoStmtGetKeyItemKey          = stmtGetKeyItemKey
		, clientRepoStmtGetKeyItemRev          = stmtGetKeyItemRev
		, clientRepoStmtGetKeyItemRevValue     = stmtGetKeyItemRevValue
		, clientRepoStmtAddKeyItem             = stmtAddKeyItem
		, clientRepoStmtChangeKeyItemRevStatus = stmtChangeKeyItemRevStatus
		, clientRepoStmtChangeKeyItemRevValue  = stmtChangeKeyItemRevValue
		, clientRepoStmtSelectKeysToPush       = stmtSelectKeysToPush
		, clientRepoStmtGetPushLag             = stmtGetPushLag
		, clientRepoStmtMassChangeStatus       = stmtMassChangeStatus
		, clientRepoStmtEnumerateKeysBegin     = stmtEnumerateKeysBegin
		, clientRepoStmtEnumerateKeysBeginEnd  = stmtEnumerateKeysBeginEnd
		, clientRepoStmtAddChunk               = stmtAddChunk
		, clientRepoStmtPreCutChunks           = stmtPreCutChunks
		, clientRepoStmtCutChunks              = stmtCutChunks
		, clientRepoStmtGetUpperRevision       = stmtGetUpperRevision
		}

-- Item statuses.
-- server version
pattern ItemStatusServer = 0
-- client change based on 'server' in case of no conflict
pattern ItemStatusClient = 1
-- client change based on 'server' which is in process of committing to server
pattern ItemStatusTransient = 2
-- client change based on 'transient', waiting for results of commit of 'transient'
pattern ItemStatusPostponed = 3
-- count
pattern ItemStatusesCount = 4

-- Manifest keys.
pattern ManifestKeyGlobalRevision = 0

type ItemId = Int64

getManifestValue :: ClientRepo -> CInt -> Int64 -> IO Int64
getManifestValue ClientRepo
	{ clientRepoStmtManifestGet = stmtManifestGet
	} key defaultValue = do
	sqliteQuery stmtManifestGet $ \query -> do
		sqliteBind query 1 key
		r <- sqliteStep query
		if r then sqliteColumn query 0
		else return defaultValue

setManifestValue :: ClientRepo -> CInt -> Int64 -> IO ()
setManifestValue ClientRepo
	{ clientRepoStmtManifestSet = stmtManifestSet
	} key value = do
	sqliteQuery stmtManifestSet $ \query -> do
		sqliteBind query 1 key
		sqliteBind query 2 value
		sqliteFinalStep query

-- | Get global revision in client repo.
-- Tries to increase global revision found in manifest, by using chunks, and then removes those chunks.
clientRepoRevision :: ClientRepo -> IO Revision
clientRepoRevision repo@ClientRepo
	{ clientRepoStmtPreCutChunks = stmtPreCutChunks
	, clientRepoStmtCutChunks = stmtCutChunks
	} = do
	-- get global revision from manifest, and try to increase it using chunks
	let preCutChunks globalRevision = do
		preCutRevision <- sqliteQuery stmtPreCutChunks $ \query -> do
			sqliteBind query 1 globalRevision
			r <- sqliteStep query
			unless r $ throwIO $ DescribeFirstException "failed to get pre-cut revision"
			sqliteColumn query 0
		-- try again if it actually increased
		if preCutRevision > globalRevision then preCutChunks preCutRevision
		else return globalRevision
	firstGlobalRevision <- getManifestValue repo ManifestKeyGlobalRevision 0
	globalRevision <- preCutChunks firstGlobalRevision
	-- if global revision has actually incresed, remember it in manifest
	when (globalRevision > firstGlobalRevision) $ setManifestValue repo ManifestKeyGlobalRevision globalRevision
	-- remove chunks behind global revision
	sqliteQuery stmtCutChunks $ \query -> do
		sqliteBind query 1 globalRevision
		sqliteFinalStep query
	return globalRevision

addKeyItem :: ClientRepo -> B.ByteString -> B.ByteString -> Revision -> CInt -> IO ()
addKeyItem ClientRepo
	{ clientRepoStmtAddKeyItem = stmtAddKeyItem
	} key value revision status = sqliteQuery stmtAddKeyItem $ \query -> do
	sqliteBind query 1 key
	sqliteBind query 2 value
	sqliteBind query 3 revision
	sqliteBind query 4 status
	sqliteFinalStep query

changeKeyItemRevisionStatus :: ClientRepo -> ItemId -> Revision -> CInt -> IO ()
changeKeyItemRevisionStatus ClientRepo
	{ clientRepoStmtChangeKeyItemRevStatus = stmtChangeKeyItemRevStatus
	} itemId newRevision newStatus = sqliteQuery stmtChangeKeyItemRevStatus $ \query -> do
	sqliteBind query 1 itemId
	sqliteBind query 2 newRevision
	sqliteBind query 3 newStatus
	sqliteFinalStep query

getKeyItemRevision :: ClientRepo -> ItemId -> IO Revision
getKeyItemRevision ClientRepo
	{ clientRepoStmtGetKeyItemRev = stmtGetKeyItemRev
	} itemId = sqliteQuery stmtGetKeyItemRev $ \query -> do
	sqliteBind query 1 itemId
	r <- sqliteStep query
	unless r $ throwIO $ DescribeFirstException "failed to get key item revision"
	sqliteColumn query 0

getKeyItemRevisionValue :: ClientRepo -> ItemId -> IO (Revision, B.ByteString)
getKeyItemRevisionValue ClientRepo
	{ clientRepoStmtGetKeyItemRevValue = stmtGetKeyItemRevValue
	} itemId = sqliteQuery stmtGetKeyItemRevValue $ \query -> do
	sqliteBind query 1 itemId
	r <- sqliteStep query
	unless r $ throwIO $ DescribeFirstException "failed to get key item revision and value"
	revision <- sqliteColumn query 0
	value <- sqliteColumn query 1
	return (revision, value)

changeKeyItemRevisionValue :: ClientRepo -> ItemId -> Revision -> B.ByteString -> IO ()
changeKeyItemRevisionValue ClientRepo
	{ clientRepoStmtChangeKeyItemRevValue = stmtChangeKeyItemRevValue
	} itemId newRevision newValue = sqliteQuery stmtChangeKeyItemRevValue $ \query -> do
	sqliteBind query 1 itemId
	sqliteBind query 2 newRevision
	sqliteBind query 3 newValue
	sqliteFinalStep query

-- | Collection of items with the same key.
newtype KeyItems = KeyItems (VU.Vector ItemId)

fillKeyItems :: SqliteQuery -> IO KeyItems
fillKeyItems query = do
	v <- VUM.replicate ItemStatusesCount 0
	let step = do
		r <- sqliteStep query
		when r $ do
			itemStatus <- sqliteColumn query 1
			when (itemStatus < 0 || itemStatus >= ItemStatusesCount) $ throwIO $ DescribeFirstException "wrong item status"
			itemId <- sqliteColumn query 0
			VUM.write v (fromIntegral (itemStatus :: CInt)) itemId
			step
	step
	KeyItems <$> VU.unsafeFreeze v

getKeyItems :: ClientRepo -> B.ByteString -> IO KeyItems
getKeyItems ClientRepo
	{ clientRepoStmtGetKeyItems = stmtGetKeyItems
	} key = sqliteQuery stmtGetKeyItems $ \query -> do
	sqliteBind query 1 key
	fillKeyItems query

getKeyItemsByOneItemId :: ClientRepo -> ItemId -> IO KeyItems
getKeyItemsByOneItemId ClientRepo
	{ clientRepoStmtGetKeyItemsByOneItemId = stmtGetKeyItemsByOneItemId
	} itemId = sqliteQuery stmtGetKeyItemsByOneItemId $ \query -> do
	sqliteBind query 1 itemId
	fillKeyItems query

mapKeyRevisionItem :: ClientRepo -> B.ByteString -> a -> (ItemId -> IO a) -> IO a
mapKeyRevisionItem repo key defaultResult f = do
	KeyItems keyItemIds <- getKeyItems repo key
	let
		step (status : restStatuses) = do
			let itemId = keyItemIds VU.! status
			if itemId > 0 then f itemId
			else step restStatuses
		step [] = return defaultResult
	-- check key items in this particular order
	step [ItemStatusPostponed, ItemStatusTransient, ItemStatusClient, ItemStatusServer]

getKeyRevision :: ClientRepo -> B.ByteString -> IO Revision
getKeyRevision repo key = mapKeyRevisionItem repo key 0 $ getKeyItemRevision repo

getKeyRevisionValue :: ClientRepo -> B.ByteString -> IO (Revision, B.ByteString)
getKeyRevisionValue repo key = mapKeyRevisionItem repo key (0, B.empty) $ getKeyItemRevisionValue repo

-- | Get revision by key.
clientRepoGetRevision :: ClientRepo -> B.ByteString -> IO Revision
clientRepoGetRevision repo@ClientRepo
	{ clientRepoDb = db
	} key = sqliteTransaction db $ \_commit -> getKeyRevision repo key

-- | Get revision and value by key.
clientRepoGetRevisionValue :: ClientRepo -> B.ByteString -> IO (Revision, B.ByteString)
clientRepoGetRevisionValue repo@ClientRepo
	{ clientRepoDb = db
	} key = sqliteTransaction db $ \_commit -> getKeyRevisionValue repo key

-- | Change value for given key.
clientRepoChange :: ClientRepo -> B.ByteString -> B.ByteString -> IO ()
clientRepoChange repo@ClientRepo
	{ clientRepoDb = db
	} key value = sqliteTransaction db $ \commit -> do
	KeyItems keyItemIds <- getKeyItems repo key
	let status = if (keyItemIds VU.! ItemStatusTransient) > 0 then ItemStatusPostponed else ItemStatusClient
	let statusItemId = keyItemIds VU.! status
	if statusItemId > 0 then changeKeyItemRevisionValue repo statusItemId 0 value
	else addKeyItem repo key value 0 $ fromIntegral status
	commit

-- | Get all keys having the string given as a prefix.
-- Empty-valued keys are returned too for removed values, for purpose of detecting changes.
clientRepoGetKeysPrefixed :: ClientRepo -> B.ByteString -> IO [B.ByteString]
clientRepoGetKeysPrefixed ClientRepo
	{ clientRepoDb = db
	, clientRepoStmtEnumerateKeysBegin = stmtEnumerateKeysBegin
	, clientRepoStmtEnumerateKeysBeginEnd = stmtEnumerateKeysBeginEnd
	} keyPrefix = sqliteTransaction db $ \_commit -> case maybeUpperBound of
	Just upperBound -> sqliteQuery stmtEnumerateKeysBeginEnd $ \query -> do
		sqliteBind query 1 keyPrefix
		sqliteBind query 2 upperBound
		process query
	Nothing -> sqliteQuery stmtEnumerateKeysBegin $ \query -> do
		sqliteBind query 1 keyPrefix
		process query
	where
		keyPrefixLength = B.length keyPrefix
		-- get upper bound for a query
		-- essentially we need "prefix + 1", i.e. increment first byte from end which is < 0xFF
		-- and set to zero all bytes after it
		-- if the whole prefix looks like "0xFFFFFFFFFF..." then no upper bound is needed
		maybeUpperBound = let
			f i | i >= 0 = let b = keyPrefix `B.index` i in
				if b < 0xFF then Just $ B.take i keyPrefix <> B.singleton (b + 1) <> B.replicate (keyPrefixLength - i - 1) 0
				else f $ i - 1
			f _ = Nothing
			in f $ keyPrefixLength - 1
		process query = let
			step previousKeys = do
				r <- sqliteStep query
				if r then step =<< (: previousKeys) <$> sqliteColumn query 0
				else return previousKeys
			in reverse <$> step []

-- | State of push, needed for pull.
newtype ClientRepoPushState = ClientRepoPushState
	{ clientRepoPushStateTransientIds :: [ItemId]
	}

-- | Perform push.
pushClientRepo :: ClientRepo -> Manifest -> IO (Push, ClientRepoPushState)
pushClientRepo repo@ClientRepo
	{ clientRepoDb = db
	, clientRepoStmtSelectKeysToPush = stmtSelectKeysToPush
	, clientRepoStmtGetUpperRevision = stmtGetUpperRevision
	} Manifest
	{ manifestMaxPushItemsCount = maxPushItemsCount
	, manifestMaxPushValuesTotalSize = maxPushValuesTotalSize
	} = describeException "failed to push client repo" $ sqliteTransaction db $ \commit -> do

	-- get global revision
	clientRevision <- clientRepoRevision repo

	-- get upper revision
	clientUpperRevision <- sqliteQuery stmtGetUpperRevision $ \query -> do
		r <- sqliteStep query
		unless r $ throwIO $ DescribeFirstException "failed to get upper revision"
		sqliteColumn query 0

	-- select keys to push
	(items, transientIds) <- sqliteQuery stmtSelectKeysToPush $ \query -> do
		sqliteBind query 1 (fromIntegral maxPushItemsCount :: Int64)
		let step pushValuesTotalSize = do
			-- get next row
			r <- sqliteStep query
			if r then do
				-- get value
				value <- sqliteColumn query 2
				-- check limits
				let newPushValuesTotalSize = pushValuesTotalSize + B.length value
				if newPushValuesTotalSize > maxPushValuesTotalSize then return ([], [])
				else do
					-- get key
					key <- sqliteColumn query 1
					-- get id of item
					itemId <- sqliteColumn query 0
					-- change status of item to 'transient'
					changeKeyItemRevisionStatus repo itemId 0 ItemStatusTransient
					-- get rest of the items and return
					(restItems, restItemIds) <- step newPushValuesTotalSize
					return ((key, value) : restItems, itemId : restItemIds)
			else return ([], [])
		step 0

	-- commit and return
	commit

	return (Push
		{ pushClientRevision = clientRevision
		, pushClientUpperRevision = clientUpperRevision
		, pushItems = items
		}, ClientRepoPushState
		{ clientRepoPushStateTransientIds = transientIds
		})

data ClientRepoPullInfo = ClientRepoPullInfo
	{ clientRepoPullRevision :: {-# UNPACK #-} !Revision
	, clientRepoPullLag :: {-# UNPACK #-} !Int64
	, clientRepoPullChanges :: [(Revision, B.ByteString, B.ByteString)]
	}

-- | Perform pull, i.e. process answer from server, marking pushed changes and remembering outside changes.
pullClientRepo :: ClientRepo -> Pull -> ClientRepoPushState -> IO ClientRepoPullInfo
pullClientRepo repo@ClientRepo
	{ clientRepoDb = db
	, clientRepoStmtAddChunk = stmtAddChunk
	} Pull
	{ pullLag = lag
	, pullPrePushRevision = prePushRevision
	, pullPostPushRevision = postPushRevision
	, pullItems = itemsToPull
	, pullNewClientRevision = newClientRevision
	} ClientRepoPushState
	{ clientRepoPushStateTransientIds = transientIds
	} = describeException "failed to pull client repo" $ sqliteTransaction db $ \commit -> do

	-- process commited keys
	forM_ (zip [(prePushRevision + 1)..] transientIds) $ \(revision, transientItemId) -> do
		-- get key items
		KeyItems keyItemIds <- getKeyItemsByOneItemId repo transientItemId

		-- 'transient' becomes 'server'
		changeKeyItemRevisionStatus repo transientItemId revision ItemStatusServer
		-- 'postponed' becomes 'client'
		let postponedItemId = keyItemIds VU.! ItemStatusPostponed
		when (postponedItemId > 0) $ changeKeyItemRevisionStatus repo postponedItemId 0 ItemStatusClient

	-- add chunk if something has been committed
	when (prePushRevision < postPushRevision) $ do
		sqliteQuery stmtAddChunk $ \query -> do
			sqliteBind query 1 prePushRevision
			sqliteBind query 2 postPushRevision
			sqliteFinalStep query

	-- pull keys
	forM_ itemsToPull $ \(revision, key, value) -> do
		-- get key items
		KeyItems keyItemIds <- getKeyItems repo key
		-- value always becomes 'server'
		-- see what we need to do
		let serverItemId = keyItemIds VU.! ItemStatusServer
		if serverItemId > 0 then changeKeyItemRevisionValue repo serverItemId revision value
		else when (B.length value > 0) $ addKeyItem repo key value revision ItemStatusServer

	-- set new client revision
	setManifestValue repo ManifestKeyGlobalRevision newClientRevision

	-- commit and return
	commit

	return ClientRepoPullInfo
		{ clientRepoPullRevision = newClientRevision
		, clientRepoPullLag = lag
		, clientRepoPullChanges = itemsToPull
		}

-- | Perform cleanup after interrupted sync (i.e. after push, but without pull).
-- It's harmless to do it without push.
cleanupClientRepo :: ClientRepo -> IO ()
cleanupClientRepo ClientRepo
	{ clientRepoDb = db
	, clientRepoStmtMassChangeStatus = stmtMassChangeStatus
	} = sqliteTransaction db $ \commit -> do
	-- change 'transient' items to 'client'
	sqliteQuery stmtMassChangeStatus $ \query -> do
		sqliteBind query 1 (ItemStatusTransient :: CInt)
		sqliteBind query 2 (ItemStatusClient :: CInt)
		sqliteFinalStep query
	-- change 'postponed' items to 'client' (possibly replacing former 'transient' items)
	sqliteQuery stmtMassChangeStatus $ \query -> do
		sqliteBind query 1 (ItemStatusPostponed :: CInt)
		sqliteBind query 2 (ItemStatusClient :: CInt)
		sqliteFinalStep query
	-- commit
	commit

-- | Helper function to perform sync.
syncClientRepo :: ClientRepo -> Manifest -> (Push -> IO Pull) -> IO ClientRepoPullInfo
syncClientRepo repo manifest sync = flip onException (cleanupClientRepo repo) $ do
	-- perform push on client repo
	(push, pushState) <- pushClientRepo repo manifest
	pull <- sync push
	pullClientRepo repo pull pushState

instance Repo ClientRepo where
	repoDb = clientRepoDb

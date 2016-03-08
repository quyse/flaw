{-|
Module: Flaw.Oil.ClientRepo
Description: Oil client repo.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}

module Flaw.Oil.ClientRepo
	( ClientRepo()
	, openClientRepo
	, clientRepoRevision
	, clientRepoGetValue
	, clientRepoChange
	, ClientRepoPushState(..)
	, pushClientRepo
	, ClientRepoPullInfo(..)
	, pullClientRepo
	, cleanupClientRepo
	, syncClientRepo
	) where

import Control.Exception
import Control.Monad
import Data.Int
import qualified Data.ByteString as B
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
	, clientRepoStmtGetKeyItemValue        :: !SqliteStmt
	, clientRepoStmtGetKeyItemValueLength  :: !SqliteStmt
	, clientRepoStmtAddKeyItem             :: !SqliteStmt
	, clientRepoStmtRemoveKeyItem          :: !SqliteStmt
	, clientRepoStmtChangeKeyItemStatus    :: !SqliteStmt
	, clientRepoStmtChangeKeyItemValue     :: !SqliteStmt
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
	stmtGetKeyItemValue        <- createStmt "SELECT value FROM items WHERE id = ?1"
	stmtGetKeyItemValueLength  <- createStmt "SELECT LENGTH(value) FROM items WHERE id = ?1"
	stmtAddKeyItem             <- createStmt "INSERT OR REPLACE INTO items (key, value, status) VALUES (?1, ?2, ?3)"
	stmtRemoveKeyItem          <- createStmt "DELETE FROM items WHERE id = ?1"
	stmtChangeKeyItemStatus    <- createStmt "UPDATE OR REPLACE items SET status = ?2 WHERE id = ?1"
	stmtChangeKeyItemValue     <- createStmt "UPDATE items SET value = ?2 WHERE id = ?1"
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
		, clientRepoStmtGetKeyItemValue        = stmtGetKeyItemValue
		, clientRepoStmtGetKeyItemValueLength  = stmtGetKeyItemValueLength
		, clientRepoStmtAddKeyItem             = stmtAddKeyItem
		, clientRepoStmtRemoveKeyItem          = stmtRemoveKeyItem
		, clientRepoStmtChangeKeyItemStatus    = stmtChangeKeyItemStatus
		, clientRepoStmtChangeKeyItemValue     = stmtChangeKeyItemValue
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

addKeyItem :: ClientRepo -> B.ByteString -> B.ByteString -> CInt -> IO ()
addKeyItem ClientRepo
	{ clientRepoStmtAddKeyItem = stmtAddKeyItem
	} key value status = sqliteQuery stmtAddKeyItem $ \query -> do
	sqliteBind query 1 key
	sqliteBind query 2 value
	sqliteBind query 3 status
	sqliteFinalStep query

removeKeyItem :: ClientRepo -> ItemId -> IO ()
removeKeyItem ClientRepo
	{ clientRepoStmtRemoveKeyItem = stmtRemoveKeyItem
	} itemId = sqliteQuery stmtRemoveKeyItem $ \query -> do
	sqliteBind query 1 itemId
	sqliteFinalStep query

changeKeyItemStatus :: ClientRepo -> ItemId -> CInt -> IO ()
changeKeyItemStatus ClientRepo
	{ clientRepoStmtChangeKeyItemStatus = stmtChangeKeyItemStatus
	} itemId newStatus = sqliteQuery stmtChangeKeyItemStatus $ \query -> do
	sqliteBind query 1 itemId
	sqliteBind query 2 newStatus
	sqliteFinalStep query

getKeyItemValue :: ClientRepo -> ItemId -> IO B.ByteString
getKeyItemValue ClientRepo
	{ clientRepoStmtGetKeyItemValue = stmtGetKeyItemValue
	} itemId = sqliteQuery stmtGetKeyItemValue $ \query -> do
	sqliteBind query 1 itemId
	r <- sqliteStep query
	unless r $ throwIO $ DescribeFirstException "failed to get key item value"
	sqliteColumn query 0

getKeyItemValueLength :: ClientRepo -> ItemId -> IO Int64
getKeyItemValueLength ClientRepo
	{ clientRepoStmtGetKeyItemValueLength = stmtGetKeyItemValueLength
	} itemId = sqliteQuery stmtGetKeyItemValueLength $ \query -> do
	sqliteBind query 1 itemId
	r <- sqliteStep query
	unless r $ throwIO $ DescribeFirstException "failed to get key item value length"
	sqliteColumn query 0

changeKeyItemValue :: ClientRepo -> ItemId -> B.ByteString -> IO ()
changeKeyItemValue ClientRepo
	{ clientRepoStmtChangeKeyItemValue = stmtChangeKeyItemValue
	} itemId newValue = sqliteQuery stmtChangeKeyItemValue $ \query -> do
	sqliteBind query 1 itemId
	sqliteBind query 2 newValue
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
	fmap KeyItems $ VU.unsafeFreeze v

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

getKeyValue :: ClientRepo -> B.ByteString -> IO B.ByteString
getKeyValue repo key = do
	KeyItems keyItemIds <- getKeyItems repo key
	let
		step (status : restStatuses) = do
			let itemId = keyItemIds VU.! status
			if itemId > 0 then getKeyItemValue repo itemId
			else step restStatuses
		step [] = return B.empty
	-- check key items in this particular order
	step [ItemStatusPostponed, ItemStatusTransient, ItemStatusClient, ItemStatusServer]

-- | Get value by key.
clientRepoGetValue :: ClientRepo -> B.ByteString -> IO B.ByteString
clientRepoGetValue repo@ClientRepo
	{ clientRepoDb = db
	} key = sqliteTransaction db $ \_commit -> getKeyValue repo key

-- | Change value for given key.
clientRepoChange :: ClientRepo -> B.ByteString -> B.ByteString -> IO ()
clientRepoChange repo@ClientRepo
	{ clientRepoDb = db
	} key value = sqliteTransaction db $ \commit -> do
	KeyItems keyItemIds <- getKeyItems repo key
	let status = if (keyItemIds VU.! ItemStatusTransient) > 0 then ItemStatusPostponed else ItemStatusClient
	let statusItemId = keyItemIds VU.! status
	if statusItemId > 0 then changeKeyItemValue repo statusItemId value
	else addKeyItem repo key value $ fromIntegral status
	commit

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
					changeKeyItemStatus repo itemId ItemStatusTransient
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
	, clientRepoPullChanges :: [(B.ByteString, B.ByteString)]
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
	forM_ transientIds $ \transientItemId -> do
		-- get key items
		KeyItems keyItemIds <- getKeyItemsByOneItemId repo transientItemId

		-- 'transient' becomes 'server'
		transientValueLength <- getKeyItemValueLength repo transientItemId
		if transientValueLength > 0 then changeKeyItemStatus repo transientItemId ItemStatusServer
		else do
			removeKeyItem repo transientItemId
			let serverItemId = keyItemIds VU.! ItemStatusServer
			when (serverItemId > 0) $ removeKeyItem repo serverItemId
		-- 'postponed' (if presented) becomes 'client'
		let postponedItemId = keyItemIds VU.! ItemStatusPostponed
		when (postponedItemId > 0) $ changeKeyItemStatus repo postponedItemId ItemStatusClient

	-- add chunk if something has been committed
	when (prePushRevision < postPushRevision) $ do
		sqliteQuery stmtAddChunk $ \query -> do
			sqliteBind query 1 prePushRevision
			sqliteBind query 2 postPushRevision
			sqliteFinalStep query

	-- pull keys
	forM_ itemsToPull $ \(key, value) -> do
		-- get key items
		KeyItems keyItemIds <- getKeyItems repo key
		-- value always becomes 'server'
		-- see what we need to do
		let serverItemId = keyItemIds VU.! ItemStatusServer
		if serverItemId > 0 then
			if B.length value > 0 then
				changeKeyItemValue repo serverItemId value
			else
				removeKeyItem repo serverItemId
		else when (B.length value > 0) $ addKeyItem repo key value ItemStatusServer

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

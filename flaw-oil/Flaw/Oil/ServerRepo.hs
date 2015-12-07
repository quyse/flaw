{-|
Module: Flaw.Oil.ServerRepo
Description: Oil server repo.
License: MIT
-}

module Flaw.Oil.ServerRepo
	( ServerRepo()
	, openServerRepo
	, syncServerRepo
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import qualified Data.Text as T

import Flaw.Book
import Flaw.Data.Sqlite
import Flaw.Exception
import Flaw.Oil.Repo

data ServerRepo = ServerRepo
	{ serverRepoDb :: !SqliteDb
	, serverRepoStmtGetMaxRevision  :: !SqliteStmt
	, serverRepoStmtClearLatest     :: !SqliteStmt
	, serverRepoStmtWrite           :: !SqliteStmt
	, serverRepoStmtPull            :: !SqliteStmt
	, serverRepoStmtGetWeakRevision :: !SqliteStmt
	, serverRepoStmtPullTotalSize   :: !SqliteStmt
	}

openServerRepo :: T.Text -> IO (ServerRepo, IO ())
openServerRepo fileName = describeException "failed to open oil server repo" $ withSpecialBook $ \bk -> do
	-- open db
	db <- book bk $ openRepoDb fileName serverRepoVersion

	-- ensure tables and indices exist

	-- revs table
	sqliteExec db $ T.pack
		"CREATE TABLE IF NOT EXISTS revs (\
		\rev INTEGER PRIMARY KEY AUTOINCREMENT, \
		\date INTEGER NOT NULL, \
		\user INTEGER NOT NULL, \
		\latest INTEGER NOT NULL, \
		\key BLOB NOT NULL, \
		\value BLOB NOT NULL)"
	-- revs_rev__latest_1 index
	sqliteExec db $ T.pack
		"CREATE UNIQUE INDEX IF NOT EXISTS revs_rev__latest_1 ON revs (rev) WHERE latest = 1"
	-- revs_key__latest_1 index
	sqliteExec db $ T.pack
		"CREATE UNIQUE INDEX IF NOT EXISTS revs_key__latest_1 ON revs (key) WHERE latest = 1"

	-- create statements
	let createStmt str = book bk $ sqliteStmt db $ T.pack str
	stmtGetMaxRevision  <- createStmt "SELECT MAX(rev) FROM revs"
	stmtClearLatest     <- createStmt "UPDATE revs SET latest = 0 WHERE key = ?1 AND latest = 1"
	stmtWrite           <- createStmt "INSERT INTO revs (date, user, latest, key, value) VALUES (strftime('%s','now'), ?1, 1, ?2, ?3)"
	stmtPull            <- createStmt "SELECT rev, key, value FROM revs WHERE rev > ?1 AND rev <= ?2 AND latest = 1 ORDER BY rev LIMIT ?3"
	stmtGetWeakRevision <- createStmt "SELECT rev FROM revs WHERE rev > ?1 AND rev <= ?2 AND latest = 1 ORDER BY rev LIMIT 1"
	stmtPullTotalSize   <- createStmt "SELECT COUNT(rev) FROM revs WHERE rev > ?1 AND latest = 1"

	return ServerRepo
		{ serverRepoDb = db
		, serverRepoStmtGetMaxRevision  = stmtGetMaxRevision
		, serverRepoStmtClearLatest     = stmtClearLatest
		, serverRepoStmtWrite           = stmtWrite
		, serverRepoStmtPull            = stmtPull
		, serverRepoStmtGetWeakRevision = stmtGetWeakRevision
		, serverRepoStmtPullTotalSize   = stmtPullTotalSize
		}

serverRepoMaxRevision :: ServerRepo -> IO Int64
serverRepoMaxRevision ServerRepo
	{ serverRepoStmtGetMaxRevision = stmtGetMaxRevision
	} = do
	sqliteQuery stmtGetMaxRevision $ \query -> do
		r <- sqliteStep query
		when (not r) $ throwIO $ DescribeFirstException "failed to get server repo max revision"
		sqliteColumn query 0

-- | Sync operation.
-- Push limits are not checked.
syncServerRepo :: ServerRepo -> Manifest -> Push -> UserId -> IO Pull
syncServerRepo repo@ServerRepo
	{ serverRepoDb = db
	, serverRepoStmtClearLatest     = stmtClearLatest
	, serverRepoStmtWrite           = stmtWrite
	, serverRepoStmtPull            = stmtPull
	, serverRepoStmtGetWeakRevision = stmtGetWeakRevision
	, serverRepoStmtPullTotalSize   = stmtPullTotalSize
	} Manifest
	{ manifestMaxPullItemsCount = maxPullItemsCount
	, manifestMaxPullValuesTotalSize = maxPullValuesTotalSize
	} Push
	{ pushClientRevision = clientRevision
	, pushClientUpperRevision = clientUpperRevisionUncorrected
	, pushItems = itemsToPush
	} userId = describeException "failed to sync server repo" $ sqliteTransaction db $ \commit -> do

	-- get pre-push revision
	prePushRevision <- serverRepoMaxRevision repo

	-- get corrected client upper revision
	let clientUpperRevision = if clientUpperRevisionUncorrected == 0 || clientUpperRevisionUncorrected > prePushRevision
		then prePushRevision
		else clientUpperRevisionUncorrected

	-- determine pull lag
	lag <- sqliteQuery stmtPullTotalSize $ \query -> do
		sqliteBind query 1 clientRevision
		r <- sqliteStep query
		when (not r) $ throwIO $ DescribeFirstException "failed to determine total pull size"
		sqliteColumn query 0

	-- loop for push items
	forM_ itemsToPush $ \(key, value) -> do
		-- clear latest flag for that key
		sqliteQuery stmtClearLatest $ \query -> do
			sqliteBind query 1 key
			sqliteFinalStep query
		-- write key-value pair
		sqliteQuery stmtWrite $ \query -> do
			sqliteBind query 1 userId
			sqliteBind query 2 key
			sqliteBind query 3 value
			sqliteFinalStep query

	-- get post-push revision
	postPushRevision <- serverRepoMaxRevision repo

	-- perform pull
	(itemsToPull, lastKnownClientRevision) <- sqliteQuery stmtPull $ \query -> do
		sqliteBind query 1 clientRevision
		sqliteBind query 2 clientUpperRevision
		sqliteBind query 3 (fromIntegral maxPullItemsCount :: Int64)

		let step valuesTotalSize lastKnownClientRevision = do
			-- get next row
			r <- sqliteStep query
			-- if there's row
			if r then do
				-- get value
				value <- sqliteColumn query 2
				-- if we breach total values limit by adding this row, stop
				let newValuesTotalSize = valuesTotalSize + B.length value
				if newValuesTotalSize > maxPullValuesTotalSize then return ([], lastKnownClientRevision)
				else do
					-- get revision and key
					revision <- sqliteColumn query 0
					key <- sqliteColumn query 1
					-- get rest items and return
					(restItemsToPull, newLastKnownClientRevision) <- step newValuesTotalSize revision
					return ((key, value) : restItemsToPull, newLastKnownClientRevision)
			else return ([], lastKnownClientRevision)
		step 0 clientRevision

	-- get new client revision
	newClientRevision <- sqliteQuery stmtGetWeakRevision $ \query -> do
		sqliteBind query 1 lastKnownClientRevision
		sqliteBind query 2 clientUpperRevision
		r <- sqliteStep query
		if r then liftM (+ (-1)) $ sqliteColumn query 0
		else return clientUpperRevision

	-- commit transaction
	commit

	-- return answer
	return Pull
		{ pullLag = lag
		, pullPrePushRevision = prePushRevision
		, pullPostPushRevision = postPushRevision
		, pullItems = itemsToPull
		, pullNewClientRevision = newClientRevision
		}

instance Repo ServerRepo where
	repoDb = serverRepoDb

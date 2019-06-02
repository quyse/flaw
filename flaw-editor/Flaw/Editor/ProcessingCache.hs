{-|
Module: Flaw.Editor.ProcessingCache
Description: Cache of results of processable entities.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}

module Flaw.Editor.ProcessingCache
  ( ProcessingCache(..)
  , openProcessingCache
  , processingCacheGet
  , processingCacheSet
  , processingCacheClear
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import qualified Data.Text as T
import Foreign.C.Types

import Flaw.Book
import Flaw.Data.Sqlite
import Flaw.Exception

data ProcessingCache = ProcessingCache
  { processingCacheDb :: !SqliteDb
  , processingCacheStmtGetValue :: !SqliteStmt
  , processingCacheStmtTouch :: !SqliteStmt
  , processingCacheStmtSetValue :: !SqliteStmt
  , processingCacheStmtClear :: !SqliteStmt
  }

openProcessingCache :: T.Text -> IO (ProcessingCache, IO ())
openProcessingCache fileName = describeException "failed to open processing cache" $ withSpecialBook $ \bk -> do
  -- open db
  db <- book bk $ sqliteDb fileName

  -- enable exclusive locking mode
  sqliteExec db $ T.pack "PRAGMA locking_mode = EXCLUSIVE"
  -- enable WAL journal mode
  sqliteExec db $ T.pack "PRAGMA journal_mode = WAL"
  -- disable synchronous mode (it's cache after all, we don't care much about it)
  sqliteExec db $ T.pack "PRAGMA synchronous = OFF"

  -- check version
  withBook $ \tempBk -> do
    stmt <- book tempBk $ sqliteStmt db $ T.pack "PRAGMA application_id"
    sqliteQuery stmt $ \query -> do
      r <- sqliteStep query
      unless r $ throwIO $ DescribeFirstException "failed to get application_id"
      currentAppVersion <- sqliteColumn query 0
      -- if version is not set yet
      if currentAppVersion == 0 then
        -- set it
        sqliteExec db $ T.pack $ "PRAGMA application_id = " ++ show REPO_VERSION
      -- else check that version is correct
      else when (currentAppVersion /= REPO_VERSION) $ throwIO $ DescribeFirstException "wrong application_id"

  -- ensure tables and indices exist

  -- items table
  sqliteExec db $ T.pack
    "CREATE TABLE IF NOT EXISTS items (\
    \id INTEGER PRIMARY KEY, \
    \key BLOB NOT NULL, \
    \value BLOB NOT NULL, \
    \lastaccess INTEGER NOT NULL)"
  -- items_key index
  sqliteExec db $ T.pack
    "CREATE UNIQUE INDEX IF NOT EXISTS items_key ON items (key)"
  -- items_lastaccess index
  sqliteExec db $ T.pack
    "CREATE INDEX IF NOT EXISTS items_lastaccess ON items (lastaccess)"

  -- create statements
  let createStmt str = book bk $ sqliteStmt db $ T.pack str
  stmtGetValue <- createStmt "SELECT id, value FROM items WHERE key = ?1"
  stmtTouch <- createStmt "UPDATE items SET lastaccess = strftime('%s','now') WHERE id = ?1"
  stmtSetValue <- createStmt "UPDATE OR REPLACE INTO items (key, value, lastaccess) VALUES (?1, ?2, strftime('%s','now')"
  stmtClear <- createStmt "DELETE FROM items WHERE lastaccess < strftime('%s','now') - ?1"

  return ProcessingCache
    { processingCacheDb = db
    , processingCacheStmtGetValue = stmtGetValue
    , processingCacheStmtTouch = stmtTouch
    , processingCacheStmtSetValue = stmtSetValue
    , processingCacheStmtClear = stmtClear
    }

processingCacheGet :: ProcessingCache -> B.ByteString -> IO (Maybe B.ByteString)
processingCacheGet ProcessingCache
  { processingCacheDb = db
  , processingCacheStmtGetValue = stmtGetValue
  , processingCacheStmtTouch = stmtTouch
  } key = sqliteTransaction db $ \commit -> do
  (row, value) <- sqliteQuery stmtGetValue $ \query -> do
    sqliteBind query 1 key
    r <- sqliteStep query
    if r then do
      row <- sqliteColumn query 0
      value <- sqliteColumn query 1
      return (row :: Int64, value)
    else return (0, B.empty)
  if row > 0 then do
    sqliteQuery stmtTouch $ \query -> do
      sqliteBind query 1 row
      sqliteFinalStep query
    commit
    return $ Just value
  else return Nothing

processingCacheSet :: ProcessingCache -> B.ByteString -> B.ByteString -> IO ()
processingCacheSet ProcessingCache
  { processingCacheDb = db
  , processingCacheStmtSetValue = stmtSetValue
  } key value = sqliteTransaction db $ \commit -> do
  sqliteQuery stmtSetValue $ \query -> do
    sqliteBind query 1 key
    sqliteBind query 2 value
    sqliteFinalStep query
  commit

processingCacheClear
  :: ProcessingCache
  -> Int64 -- ^ Max age in seconds.
  -> IO ()
processingCacheClear ProcessingCache
  { processingCacheDb = db
  , processingCacheStmtClear = stmtClear
  } maxAge = sqliteTransaction db $ \commit -> do
  sqliteQuery stmtClear $ \query -> do
    sqliteBind query 1 maxAge
    sqliteFinalStep query
  commit

pattern REPO_VERSION :: CInt
pattern REPO_VERSION = 0x63706566 -- "fepc"

{-|
Module: Flaw.Data.Sqlite
Description: Simple SQLite Haskell interface.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}

module Flaw.Data.Sqlite
	( SqliteDb()
	, SqliteStmt()
	, SqliteQuery()
	, sqliteDb
	, sqliteExec
	, sqliteStmt
	, sqliteQuery
	, sqliteStep
	, sqliteFinalStep
	, sqliteTransaction
	, SqliteData(..)
	, sqliteLastInsertRowId
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Int
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Flaw.Book

data SqliteDb = SqliteDb
	{ sqliteDbPtr :: !(Ptr C_sqlite3)
	, sqliteDbSavePointStmtPtr :: !(Ptr C_sqlite3_stmt)
	, sqliteDbReleaseStmtPtr :: !(Ptr C_sqlite3_stmt)
	, sqliteDbRollbackToStmtPtr :: !(Ptr C_sqlite3_stmt)
	}

data SqliteStmt = SqliteStmt
	{ sqliteStmtPtr :: !(Ptr C_sqlite3_stmt)
	, sqliteStmtDbPtr :: !(Ptr C_sqlite3)
	}

newtype SqliteQuery = SqliteQuery SqliteStmt

-- | Open SQLite database.
sqliteDb :: T.Text -> IO (SqliteDb, IO ())
sqliteDb fileName = do
	bk <- newBook

	-- open db
	dbPtr <- book bk $ B.useAsCString (T.encodeUtf8 fileName) $ \fileNamePtr -> do
		alloca $ \dbPtrPtr -> do
			r <- sqlite3_open fileNamePtr dbPtrPtr
			dbPtr <- peek dbPtrPtr
			when (r /= SQLITE_OK) $ do
				when (dbPtr /= nullPtr) $ void $ sqlite3_close dbPtr
				throwIO $ SqliteOpenError fileName
			return (dbPtr, void $ sqlite3_close dbPtr)

	-- create transaction statements

	let createStmt str = do
		stmtPtr <- alloca $ \stmtPtrPtr -> do
			sqliteCheckError dbPtr (== SQLITE_OK) $ withCString str $ \strPtr -> sqlite3_prepare_v2 dbPtr strPtr (-1) stmtPtrPtr nullPtr
			peek stmtPtrPtr
		book bk $ return (stmtPtr, void $ sqlite3_finalize stmtPtr)

	savePointStmtPtr <- createStmt "SAVEPOINT T"
	releaseStmtPtr <- createStmt "RELEASE T"
	rollbackToStmtPtr <- createStmt "ROLLBACK TO T"

	return (SqliteDb
		{ sqliteDbPtr = dbPtr
		, sqliteDbSavePointStmtPtr = savePointStmtPtr
		, sqliteDbReleaseStmtPtr = releaseStmtPtr
		, sqliteDbRollbackToStmtPtr = rollbackToStmtPtr
		}, freeBook bk)

-- | Execute one-time query.
sqliteExec :: SqliteDb -> T.Text -> IO ()
sqliteExec SqliteDb
	{ sqliteDbPtr = dbPtr
	} text = do
	sqliteCheckError dbPtr (== SQLITE_OK) $ B.useAsCString (T.encodeUtf8 text) $ \textPtr -> sqlite3_exec dbPtr textPtr nullFunPtr nullPtr nullPtr

-- | Create SQLite statement.
sqliteStmt :: SqliteDb -> T.Text -> IO (SqliteStmt, IO ())
sqliteStmt SqliteDb
	{ sqliteDbPtr = dbPtr
	} text = do
	stmtPtr <- alloca $ \stmtPtrPtr -> do
		sqliteCheckError dbPtr (== SQLITE_OK) $ B.unsafeUseAsCStringLen (T.encodeUtf8 text) $ \(textPtr, textLen) -> sqlite3_prepare_v2 dbPtr textPtr (fromIntegral textLen) stmtPtrPtr nullPtr
		peek stmtPtrPtr
	return (SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		, sqliteStmtDbPtr = dbPtr
		}, void $ sqlite3_finalize stmtPtr)

-- | Get query object from statement.
-- Just to reset statement afterwards.
sqliteQuery :: SqliteStmt -> (SqliteQuery -> IO a) -> IO a
sqliteQuery stmt@SqliteStmt
	{ sqliteStmtPtr = stmtPtr
	} action = finally (action (SqliteQuery stmt)) $ do
	void $ sqlite3_reset stmtPtr
	void $ sqlite3_clear_bindings stmtPtr

-- | Perform query step.
-- Returns True if step succeeded and there's row of data.
-- Returns False if step succeeded, but there's no data anymore.
-- Throws an exception otherwise.
sqliteStep :: SqliteQuery -> IO Bool
sqliteStep (SqliteQuery SqliteStmt
	{ sqliteStmtPtr = stmtPtr
	, sqliteStmtDbPtr = dbPtr
	}) = do
	r <- sqlite3_step stmtPtr
	case r of
		SQLITE_ROW -> return True
		SQLITE_DONE -> return False
		_ -> throwSqliteError dbPtr

-- | Perform query step, and check that it returned SQLITE_DONE.
sqliteFinalStep :: SqliteQuery -> IO ()
sqliteFinalStep query = do
	r <- sqliteStep query
	when r $ throwIO SqliteStepNotFinal

-- | Perform SQLite transaction.
sqliteTransaction :: SqliteDb -> (IO () -> IO a) -> IO a
sqliteTransaction SqliteDb
	{ sqliteDbPtr = dbPtr
	, sqliteDbSavePointStmtPtr = savePointStmtPtr
	, sqliteDbReleaseStmtPtr = releaseStmtPtr
	, sqliteDbRollbackToStmtPtr = rollbackToStmtPtr
	} io = do
	-- save point
	void $ sqlite3_reset savePointStmtPtr
	sqliteCheckError dbPtr (== SQLITE_DONE) $ sqlite3_step savePointStmtPtr
	-- commit function
	finishedRef <- newIORef False
	let commit = do
		-- check that transaction is not finished
		finished <- readIORef finishedRef
		when finished $ throwIO SqliteTransactionAlreadyFinished
		-- commit
		void $ sqlite3_reset releaseStmtPtr
		sqliteCheckError dbPtr (== SQLITE_DONE) $ sqlite3_step releaseStmtPtr
		-- remember
		writeIORef finishedRef True
	finally (io commit) $ do
		-- rollback if not finished
		finished <- readIORef finishedRef
		when (not finished) $ do
			void $ sqlite3_reset rollbackToStmtPtr
			void $ sqlite3_step rollbackToStmtPtr
			void $ sqlite3_reset releaseStmtPtr
			void $ sqlite3_step releaseStmtPtr

-- | Class of data which could be used in statements.
class SqliteData a where
	-- | Bind data into statement.
	sqliteBind :: SqliteQuery -> CInt -> a -> IO ()
	-- | Get data from query.
	sqliteColumn :: SqliteQuery -> CInt -> IO a

instance SqliteData CInt where
	sqliteBind (SqliteQuery SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		, sqliteStmtDbPtr = dbPtr
		}) column value = sqliteCheckError dbPtr (== SQLITE_OK) $ sqlite3_bind_int stmtPtr column value
	sqliteColumn (SqliteQuery SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		}) column = sqlite3_column_int stmtPtr column

instance SqliteData Int64 where
	sqliteBind (SqliteQuery SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		, sqliteStmtDbPtr = dbPtr
		}) column value = sqliteCheckError dbPtr (== SQLITE_OK) $ sqlite3_bind_int64 stmtPtr column value
	sqliteColumn (SqliteQuery SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		}) column = sqlite3_column_int64 stmtPtr column

instance SqliteData B.ByteString where
	sqliteBind (SqliteQuery SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		, sqliteStmtDbPtr = dbPtr
		}) column bytes = do
		sqliteCheckError dbPtr (== SQLITE_OK) $ do
			B.unsafeUseAsCStringLen bytes $ \(ptr, len) -> do
				-- note: we are forcing non-null pointer in case of zero-length bytestring, in order to bind a blob and not a NULL value
				sqlite3_bind_blob stmtPtr column (if len > 0 then castPtr ptr else intPtrToPtr 1) (fromIntegral len) $ castPtrToFunPtr $ intPtrToPtr SQLITE_TRANSIENT
	sqliteColumn (SqliteQuery SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		}) column = do
		ptr <- sqlite3_column_blob stmtPtr column
		len <- sqlite3_column_bytes stmtPtr column
		B.packCStringLen (castPtr ptr, fromIntegral len)

instance SqliteData T.Text where
	sqliteBind (SqliteQuery SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		, sqliteStmtDbPtr = dbPtr
		}) column text = do
		sqliteCheckError dbPtr (== SQLITE_OK) $ do
			B.unsafeUseAsCStringLen (T.encodeUtf8 text) $ \(ptr, len) -> do
				-- note: we are forcing non-null pointer in case of zero-length string, in order to bind a string and not a NULL value
				sqlite3_bind_text stmtPtr column (if len > 0 then ptr else intPtrToPtr 1) (fromIntegral len) $ castPtrToFunPtr $ intPtrToPtr SQLITE_TRANSIENT
	sqliteColumn (SqliteQuery SqliteStmt
		{ sqliteStmtPtr = stmtPtr
		}) column = do
		ptr <- sqlite3_column_text stmtPtr column
		len <- sqlite3_column_bytes stmtPtr column
		liftM T.decodeUtf8 $ B.packCStringLen (ptr, fromIntegral len)

sqliteLastInsertRowId :: SqliteDb -> IO Int64
sqliteLastInsertRowId SqliteDb
	{ sqliteDbPtr = dbPtr
	} = sqlite3_last_insert_rowid dbPtr

throwSqliteError :: Ptr C_sqlite3 -> IO a
throwSqliteError dbPtr = do
	errCode <- sqlite3_errcode dbPtr
	errMsgPtr <- sqlite3_errmsg dbPtr
	errMsgBytes <- B.packCString errMsgPtr
	throwIO $ SqliteError (fromIntegral errCode) (T.decodeUtf8 errMsgBytes)

sqliteCheckError :: Ptr C_sqlite3 -> (CInt -> Bool) -> IO CInt -> IO ()
sqliteCheckError dbPtr cond io = do
	r <- io
	when (not $ cond r) $ throwSqliteError dbPtr

data SqliteError
	= SqliteError {-# UNPACK #-} !Int !T.Text
	| SqliteOpenError !T.Text
	| SqliteStepNotFinal
	| SqliteTransactionAlreadyFinished
	deriving Show

instance Exception SqliteError

-- FFI: types

data C_sqlite3
data C_sqlite3_stmt

-- FFI: functions

foreign import ccall safe sqlite3_open :: Ptr CChar -> Ptr (Ptr C_sqlite3) -> IO CInt
foreign import ccall safe sqlite3_close :: Ptr C_sqlite3 -> IO CInt
foreign import ccall safe sqlite3_prepare_v2 :: Ptr C_sqlite3 -> Ptr CChar -> CInt -> Ptr (Ptr C_sqlite3_stmt) -> Ptr (Ptr CChar) -> IO CInt
foreign import ccall unsafe sqlite3_reset :: Ptr C_sqlite3_stmt -> IO CInt
foreign import ccall safe sqlite3_step :: Ptr C_sqlite3_stmt -> IO CInt
foreign import ccall unsafe sqlite3_clear_bindings :: Ptr C_sqlite3_stmt -> IO CInt
foreign import ccall unsafe sqlite3_finalize :: Ptr C_sqlite3_stmt -> IO CInt
foreign import ccall safe sqlite3_exec :: Ptr C_sqlite3 -> Ptr CChar -> FunPtr (Ptr () -> CInt -> Ptr (Ptr CChar) -> Ptr (Ptr CChar) -> IO CInt) -> Ptr () -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe sqlite3_bind_int :: Ptr C_sqlite3_stmt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe sqlite3_bind_int64 :: Ptr C_sqlite3_stmt -> CInt -> Int64 -> IO CInt
foreign import ccall safe sqlite3_bind_blob :: Ptr C_sqlite3_stmt -> CInt -> Ptr () -> CInt -> FunPtr (Ptr () -> IO ()) -> IO CInt
foreign import ccall unsafe sqlite3_bind_text :: Ptr C_sqlite3_stmt -> CInt -> Ptr CChar -> CInt -> FunPtr (Ptr () -> IO ()) -> IO CInt
--foreign import ccall unsafe sqlite3_bind_null :: Ptr C_sqlite3_stmt -> CInt -> IO CInt

foreign import ccall unsafe sqlite3_column_int :: Ptr C_sqlite3_stmt -> CInt -> IO CInt
foreign import ccall unsafe sqlite3_column_int64 :: Ptr C_sqlite3_stmt -> CInt -> IO Int64
foreign import ccall unsafe sqlite3_column_blob :: Ptr C_sqlite3_stmt -> CInt -> IO (Ptr ())
foreign import ccall unsafe sqlite3_column_bytes :: Ptr C_sqlite3_stmt -> CInt -> IO CInt
foreign import ccall unsafe sqlite3_column_text :: Ptr C_sqlite3_stmt -> CInt -> IO (Ptr CChar)

foreign import ccall unsafe sqlite3_last_insert_rowid :: Ptr C_sqlite3 -> IO Int64

foreign import ccall unsafe sqlite3_errcode :: Ptr C_sqlite3 -> IO CInt
foreign import ccall unsafe sqlite3_errmsg :: Ptr C_sqlite3 -> IO (Ptr CChar)

-- FFI: values

pattern SQLITE_OK = 0
pattern SQLITE_ROW = 100
pattern SQLITE_DONE = 101

pattern SQLITE_TRANSIENT = -1

{-|
Module: Flaw.Data.Lmdb
Description: Simple Lmdb Haskell interface.
License: MIT
-}

{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Flaw.Data.Lmdb
	( Lmdb()
	, LmdbTransaction()
	, lmdbOpen
	, lmdbRead
	, lmdbWrite
	, lmdbCommit
	, lmdbGet
	, lmdbPut
	, lmdbDelete
	) where

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Book
import Flaw.Flow

data Lmdb = Lmdb
	{ lmdbEnvPtr :: {-# UNPACK #-} !(Ptr MDB_env)
	, lmdbDbi :: {-# UNPACK #-} !MDB_dbi
	, lmdbFlow :: !Flow
	}

data LmdbTransaction = LmdbTransaction
	{ lmdbTransactionTxnPtr :: {-# UNPACK #-} !(Ptr MDB_txn)
	, lmdbTransactionDbi :: {-# UNPACK #-} !MDB_dbi
	, lmdbTransactionFinishedRef :: {-# UNPACK #-} !(IORef Bool)
	}

-- | Open LMDB environment.
lmdbOpen :: T.Text -> Word64 -> IO (Lmdb, IO ())
lmdbOpen fileName mapSize = withSpecialBook $ \bk -> do
	-- create env
	envPtr <- alloca $ \envPtrPtr -> do
		lmdbCheckError $ mdb_env_create envPtrPtr
		peek envPtrPtr
	book bk $ return ((), mdb_env_close envPtr)

	-- set memory map size
	lmdbCheckError $ mdb_env_set_mapsize envPtr (fromIntegral mapSize)

	-- open env
	lmdbCheckError $ B.useAsCString (T.encodeUtf8 fileName) $ \fileNamePtr ->
		mdb_env_open envPtr fileNamePtr (MDB_NOSUBDIR .|. MDB_WRITEMAP .|. MDB_NOSYNC .|. MDB_NOTLS) 0o644

	-- open database
	dbi <- let
		acquire = alloca $ \txnPtrPtr -> do
			lmdbCheckError $ mdb_txn_begin envPtr nullPtr MDB_RDONLY txnPtrPtr
			peek txnPtrPtr
		in bracketOnError acquire mdb_txn_abort $ \txnPtr -> do
			dbi <- alloca $ \dbiPtr -> do
				lmdbCheckError $ mdb_dbi_open txnPtr nullPtr MDB_CREATE dbiPtr
				peek dbiPtr
			lmdbCheckError $ mdb_txn_commit txnPtr
			return dbi
	book bk $ return ((), mdb_dbi_close envPtr dbi)

	-- create flow
	flow <- book bk newFlowOS

	return Lmdb
		{ lmdbEnvPtr = envPtr
		, lmdbDbi = dbi
		, lmdbFlow = flow
		}

-- | Run read transaction.
lmdbRead :: Lmdb -> (LmdbTransaction -> IO a) -> IO a
lmdbRead Lmdb
	{ lmdbEnvPtr = envPtr
	, lmdbDbi = dbi
	} io = do
	finishedRef <- newIORef False
	let
		acquire = alloca $ \txnPtrPtr -> do
			lmdbCheckError $ mdb_txn_begin envPtr nullPtr MDB_RDONLY txnPtrPtr
			peek txnPtrPtr
		release txnPtr = do
			finished <- readIORef finishedRef
			unless finished $ mdb_txn_abort txnPtr
	bracket acquire release $ \txnPtr -> io LmdbTransaction
		{ lmdbTransactionTxnPtr = txnPtr
		, lmdbTransactionDbi = dbi
		, lmdbTransactionFinishedRef = finishedRef
		}

-- | Run write transaction.
lmdbWrite :: Lmdb -> (LmdbTransaction -> IO a) -> IO a
lmdbWrite Lmdb
	{ lmdbEnvPtr = envPtr
	, lmdbDbi = dbi
	, lmdbFlow = flow
	} io = runInFlow flow $ do
	finishedRef <- newIORef False
	let
		acquire = alloca $ \txnPtrPtr -> do
			lmdbCheckError $ mdb_txn_begin envPtr nullPtr 0 txnPtrPtr
			peek txnPtrPtr
		release txnPtr = do
			finished <- readIORef finishedRef
			unless finished $ mdb_txn_abort txnPtr
	bracket acquire release $ \txnPtr -> io LmdbTransaction
		{ lmdbTransactionTxnPtr = txnPtr
		, lmdbTransactionDbi = dbi
		, lmdbTransactionFinishedRef = finishedRef
		}

lmdbCommit :: LmdbTransaction -> IO ()
lmdbCommit LmdbTransaction
	{ lmdbTransactionTxnPtr = txnPtr
	, lmdbTransactionFinishedRef = finishedRef
	} = do
	lmdbCheckError $ mdb_txn_commit txnPtr
	writeIORef finishedRef True

lmdbGet :: LmdbTransaction -> B.ByteString -> IO (Maybe B.ByteString)
lmdbGet LmdbTransaction
	{ lmdbTransactionTxnPtr = txnPtr
	, lmdbTransactionDbi = dbi
	} key = B.unsafeUseAsCStringLen key $ \(keyPtr, keyLength) -> allocaArray 2 $ \keyBufPtr -> do
	poke keyBufPtr $ intPtrToPtr $ fromIntegral keyLength
	pokeElemOff keyBufPtr 1 keyPtr
	allocaArray 2 $ \valueBufPtr -> do
		r <- mdb_get txnPtr dbi keyBufPtr valueBufPtr
		if r == MDB_SUCCESS then do
			valueLength <- fromIntegral . ptrToIntPtr <$> peek valueBufPtr
			valuePtr <- peekElemOff valueBufPtr 1
			Just <$> B.packCStringLen (valuePtr, valueLength)
		else if r == MDB_NOTFOUND then return Nothing
		else lmdbThrowError r

lmdbPut :: LmdbTransaction -> B.ByteString -> B.ByteString -> IO ()
lmdbPut LmdbTransaction
	{ lmdbTransactionTxnPtr = txnPtr
	, lmdbTransactionDbi = dbi
	} key value = lmdbCheckError $
	B.unsafeUseAsCStringLen key $ \(keyPtr, keyLength) -> allocaArray 2 $ \keyBufPtr -> do
		poke keyBufPtr $ intPtrToPtr $ fromIntegral keyLength
		pokeElemOff keyBufPtr 1 keyPtr
		B.unsafeUseAsCStringLen value $ \(valuePtr, valueLength) -> allocaArray 2 $ \valueBufPtr -> do
			poke valueBufPtr $ intPtrToPtr $ fromIntegral valueLength
			pokeElemOff valueBufPtr 1 valuePtr
			mdb_put txnPtr dbi keyBufPtr valueBufPtr 0

lmdbDelete :: LmdbTransaction -> B.ByteString -> IO ()
lmdbDelete LmdbTransaction
	{ lmdbTransactionTxnPtr = txnPtr
	, lmdbTransactionDbi = dbi
	} key = lmdbCheckError $
	B.unsafeUseAsCStringLen key $ \(keyPtr, keyLength) -> allocaArray 2 $ \keyBufPtr -> do
		poke keyBufPtr $ intPtrToPtr $ fromIntegral keyLength
		pokeElemOff keyBufPtr 1 keyPtr
		mdb_del txnPtr dbi keyBufPtr nullPtr

lmdbCheckError :: IO CInt -> IO ()
lmdbCheckError io = do
	r <- io
	unless (r == MDB_SUCCESS) $ lmdbThrowError r

lmdbThrowError :: CInt -> IO a
lmdbThrowError r = throwIO . LmdbError r . T.decodeUtf8 =<< B.packCString =<< mdb_strerror r

data LmdbError
	= LmdbError {-# UNPACK #-} !CInt !T.Text
	deriving Show

instance Exception LmdbError

-- FFI: types

data MDB_env
data MDB_txn
type MDB_dbi = CUInt
-- MDB_val is actually struct { size_t, void* } but we use pair of pointers.
type MDB_val = Ptr CChar

-- FFI: functions

foreign import ccall safe mdb_strerror :: CInt -> IO (Ptr CChar)

foreign import ccall safe mdb_env_create :: Ptr (Ptr MDB_env) -> IO CInt
foreign import ccall safe mdb_env_close :: Ptr MDB_env -> IO ()
foreign import ccall safe mdb_env_open :: Ptr MDB_env -> Ptr CChar -> CUInt -> Word32 -> IO CInt
foreign import ccall safe mdb_env_set_mapsize :: Ptr MDB_env -> CSize -> IO CInt

foreign import ccall safe mdb_txn_begin :: Ptr MDB_env -> Ptr MDB_txn -> CUInt -> Ptr (Ptr MDB_txn) -> IO CInt
foreign import ccall safe mdb_txn_commit :: Ptr MDB_txn -> IO CInt
foreign import ccall safe mdb_txn_abort :: Ptr MDB_txn -> IO ()

foreign import ccall safe mdb_dbi_open :: Ptr MDB_txn -> Ptr CChar -> CUInt -> Ptr MDB_dbi -> IO CInt
foreign import ccall safe mdb_dbi_close :: Ptr MDB_env -> MDB_dbi -> IO ()

foreign import ccall safe mdb_get :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import ccall safe mdb_put :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
foreign import ccall safe mdb_del :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt

-- FFI: values

pattern MDB_SUCCESS = 0
pattern MDB_NOTFOUND = (-30798)

pattern MDB_NOSUBDIR = 0x4000
pattern MDB_NOSYNC = 0x10000
pattern MDB_WRITEMAP = 0x80000
pattern MDB_NOTLS = 0x200000

pattern MDB_RDONLY = 0x20000

pattern MDB_CREATE = 0x40000

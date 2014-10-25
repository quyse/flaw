{-# LANGUAGE GADTs, TypeFamilies #-}

module Flaw.Window.Win32
	( Win32WindowSystem()
	) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Data.IORef
import qualified Data.Text as T
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Map.Strict as Map
import System.IO.Unsafe

import Flaw.Window.Internal

data Win32WindowSystem = Win32WindowSystem
	{ wsThreadId :: ThreadId
	, wsShutdownVar :: MVar ()
	-- | Win32 thread handle.
	, wsThreadHandle :: HANDLE
	}

data Win32Window = Win32Window
	{ wWindowSystem :: Win32WindowSystem
	, wHandle :: Ptr ()
	}

instance Window Win32Window where
	setWindowTitle Win32Window { wWindowSystem = ws, wHandle = hwnd } title = invoke_ ws $ do
		withCWString (T.unpack title) $ \titleCString ->
			c_setWin32WindowTitle hwnd titleCString
	closeWindow Win32Window { wWindowSystem = ws, wHandle = hwnd } = invoke_ ws $ c_closeWin32Window hwnd

instance WindowSystem Win32WindowSystem where
	type WindowSystemWindow Win32WindowSystem = Win32Window

	initWindowSystem = do
		threadHandleVar <- newEmptyMVar
		shutdownVar <- newEmptyMVar
		threadId <- forkOS $ do
			-- initialize window system, get a thread handle
			threadHandle <- alloca $ \threadHandlePtr -> do
				c_initWin32WindowSystem threadHandlePtr
				peek threadHandlePtr
			-- send thread handle to the original thread
			putMVar threadHandleVar threadHandle
			-- run window system
			c_runWin32WindowSystem
			-- notify that window system quit
			putMVar shutdownVar ()
		-- wait for thread handle
		threadHandle <- readMVar threadHandleVar
		-- return window system
		return Win32WindowSystem
			{ wsThreadId = threadId
			, wsShutdownVar = shutdownVar
			, wsThreadHandle = threadHandle
			}

	shutdownWindowSystem ws@Win32WindowSystem { wsThreadHandle = threadHandle } = do
		-- send a message to stop window loop
		invoke_ ws c_stopWin32WindowSystem
		-- wait for actual completion
		readMVar $ wsShutdownVar ws
		-- free window resources
		c_freeWin32WindowSystem threadHandle

	createWindow ws title x y width height = invoke ws $ do
		hwnd <- withCWString (T.unpack title) $ \titleCString ->
			c_createWin32Window titleCString x y width height
		if hwnd == nullPtr then
			throwIO CannotCreateWindowException
		else
			return $ Win32Window
				{ wWindowSystem = ws
				, wHandle = hwnd
				}

invokeWithMaybeResultVar :: Maybe (MVar a) -> Win32WindowSystem -> IO a -> IO ()
invokeWithMaybeResultVar maybeResultVar Win32WindowSystem { wsThreadHandle = threadHandle } io = do
	messageId <- modifyMVar invokeMessagesVar $ \(nextMessageId, messages) ->
		return ((nextMessageId + 1, Map.insert nextMessageId (Message io maybeResultVar) messages), nextMessageId)
	c_invokeWin32WindowSystem threadHandle $ intPtrToPtr messageId

-- | Invoke function in window system thread.
-- Do not wait for the result.
invoke_ :: Win32WindowSystem -> IO () -> IO ()
invoke_ = invokeWithMaybeResultVar Nothing

-- | Invoke function in window system thread.
-- Wait for the result.
invoke :: Win32WindowSystem -> IO a -> IO a
invoke ws io = do
	resultVar <- newEmptyMVar
	invokeWithMaybeResultVar (Just resultVar) ws io
	takeMVar resultVar

-- | Function which called from C.
invokeCallback :: Ptr () -> IO ()
invokeCallback messageIdAsPtr = do
	let messageId = ptrToIntPtr messageIdAsPtr
	maybeMessage <- modifyMVar invokeMessagesVar $ \(nextMessageId, messages) -> do
		return ((nextMessageId, Map.delete messageId messages), Map.lookup messageId messages)
	case maybeMessage of
		Just (Message io maybeResultVar) -> do
			-- TODO: propagate exceptions
			result <- io
			case maybeResultVar of
				Just resultVar -> putMVar resultVar result
				Nothing -> return ()
		Nothing -> return ()

data Message where
	Message :: IO a -> Maybe (MVar a) -> Message

-- | Global table of pending messages.
invokeMessagesVar :: MVar (IntPtr, Map.Map IntPtr Message)
invokeMessagesVar = unsafePerformIO $ newMVar (0, Map.empty)

-- foreign imports

type HANDLE = Ptr ()
type LPTSTR = CWString
type HWND = Ptr ()

foreign import ccall unsafe "initWin32WindowSystem" c_initWin32WindowSystem :: Ptr HANDLE -> IO ()
foreign import ccall safe "runWin32WindowSystem" c_runWin32WindowSystem :: IO ()
foreign import ccall unsafe "createWin32Window" c_createWin32Window
	:: LPTSTR -- title
	-> Int -> Int -- x y
	-> Int -> Int -- width height
	-> IO HWND -- HWND
foreign import ccall unsafe "setWin32WindowTitle" c_setWin32WindowTitle :: HWND -> LPTSTR -> IO ()
foreign import ccall unsafe "closeWin32Window" c_closeWin32Window :: HWND -> IO ()
foreign import ccall unsafe "stopWin32WindowSystem" c_stopWin32WindowSystem :: IO ()
foreign import ccall unsafe "freeWin32WindowSystem" c_freeWin32WindowSystem :: HANDLE -> IO ()
foreign import ccall unsafe "invokeWin32WindowSystem" c_invokeWin32WindowSystem :: HANDLE -> Ptr () -> IO ()

-- foreign exports

foreign export ccall "hs_win32InvokeCallback" invokeCallback :: Ptr () -> IO ()

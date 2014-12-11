{-|
Module: Flaw.Window.Win32
Description: Win32 window framework.
License: MIT
-}

{-# LANGUAGE GADTs, TypeFamilies #-}

module Flaw.Window.Win32
	( Win32WindowSystem()
	, Win32Window()
	, initWin32WindowSystem
	, createWin32Window
	, createLayeredWin32Window
	, updateLayeredWin32Window
	) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Fix
import Control.Monad.Trans.Resource
import Data.IORef
import qualified Data.Text as T
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Flaw.Window.Internal

data Win32WindowSystem = Win32WindowSystem
	{ wsHandle :: Ptr () -- ^ Opaque handle for C side.
	, wsThreadId :: ThreadId
	, wsShutdownVar :: MVar ()
	}

data Win32Window = Win32Window
	{ wWindowSystem :: Win32WindowSystem
	, wHandle :: Ptr ()
	, wCallback :: FunPtr WindowCallback
	}

instance Window Win32Window where
	setWindowTitle Win32Window { wWindowSystem = ws, wHandle = hwnd } title = invoke_ ws $ do
		withCWString (T.unpack title) $ \titleCString ->
			c_setWin32WindowTitle hwnd titleCString

initWin32WindowSystem :: MonadResource m => m (ReleaseKey, Win32WindowSystem)
initWin32WindowSystem = allocate init shutdown where
	init = do
		-- create vars
		handleVar <- newEmptyMVar
		shutdownVar <- newEmptyMVar

		-- create OS thread for window loop
		threadId <- forkOS $ do
			-- initialize window system, get a thread handle
			handle <- c_initWin32WindowSystem
			-- send handles to the original thread
			putMVar handleVar handle
			-- run window system
			c_runWin32WindowSystem handle
			-- notify that window system quit
			putMVar shutdownVar ()
		-- wait for handle
		handle <- readMVar handleVar
		-- return window system
		return Win32WindowSystem
			{ wsHandle = handle
			, wsThreadId = threadId
			, wsShutdownVar = shutdownVar
			}
	shutdown ws = do
		-- send a message to stop window loop
		invoke_ ws $ c_stopWin32WindowSystem
		-- wait for actual completion
		readMVar $ wsShutdownVar ws
		-- free resources
		c_shutdownWin32WindowSystem $ wsHandle ws

createWin32Window :: MonadResource m => Win32WindowSystem -> T.Text -> Int -> Int -> Int -> Int -> m (ReleaseKey, Win32Window)
createWin32Window ws title left top width height = internalCreateWin32Window ws title left top width height False

createLayeredWin32Window :: MonadResource m => Win32WindowSystem -> T.Text -> Int -> Int -> Int -> Int -> m (ReleaseKey, Win32Window)
createLayeredWin32Window ws title left top width height = internalCreateWin32Window ws title left top width height True

internalCreateWin32Window :: MonadResource m => Win32WindowSystem -> T.Text -> Int -> Int -> Int -> Int -> Bool -> m (ReleaseKey, Win32Window)
internalCreateWin32Window ws title left top width height layered = allocate create destroy where
	create = invoke ws $ mfix $ \w -> do
		-- create callback
		callback <- wrapWindowCallback $ \msg wParam lParam -> case msg of
			0x0002 -> do -- WM_DESTROY
				-- free callback
				freeHaskellFunPtr $ wCallback w
			_ -> return ()
		-- create window
		hwnd <- withCWString (T.unpack title) $ \titleCString ->
			c_createWin32Window (wsHandle ws) titleCString left top width height callback (if layered then 1 else 0)
		if hwnd == nullPtr then
			error "cannot create Win32Window"
		else
			return $ Win32Window
				{ wWindowSystem = ws
				, wHandle = hwnd
				, wCallback = callback
				}
	destroy Win32Window { wWindowSystem = ws, wHandle = hwnd } = invoke_ ws $ c_destroyWin32Window hwnd

updateLayeredWin32Window :: Win32Window -> IO ()
updateLayeredWin32Window w = invoke_ (wWindowSystem w) $ c_updateLayeredWin32Window $ wHandle w

invokeWithMaybeResultVar :: Maybe (MVar (Either SomeException a)) -> Win32WindowSystem -> IO a -> IO ()
invokeWithMaybeResultVar maybeResultVar ws io = do
	-- create callback
	invokeCallback <- mfix $ \invokeCallback -> wrapInvokeCallback $ do
		-- run IO with exception propagation
		result <- try io
		case maybeResultVar of
			Just resultVar -> putMVar resultVar result
			Nothing -> return ()
		-- free fun ptr
		freeHaskellFunPtr invokeCallback
	-- do invoke
	c_invokeWin32WindowSystem (wsHandle ws) invokeCallback

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
	result <- takeMVar resultVar
	case result of
		Left e -> throw e
		Right r -> return r

-- | Message to invoke.
data Message where
	Message :: IO a -> Maybe (MVar a) -> Message

-- foreign types

type HANDLE = Ptr ()
type LPTSTR = CWString
type HWND = Ptr ()
type WPARAM = CUIntPtr
type LPARAM = CIntPtr

-- foreign imports

foreign import ccall unsafe "initWin32WindowSystem" c_initWin32WindowSystem :: IO (Ptr ())
foreign import ccall safe "runWin32WindowSystem" c_runWin32WindowSystem :: Ptr () -> IO ()
foreign import ccall unsafe "shutdownWin32WindowSystem" c_shutdownWin32WindowSystem :: Ptr () -> IO ()
foreign import ccall unsafe "stopWin32WindowSystem" c_stopWin32WindowSystem :: IO ()
foreign import ccall unsafe "invokeWin32WindowSystem" c_invokeWin32WindowSystem :: Ptr () -> FunPtr InvokeCallback -> IO ()
foreign import ccall unsafe "createWin32Window" c_createWin32Window
	:: Ptr () -- window system handle
	-> LPTSTR -- title
	-> Int -> Int -- x y
	-> Int -> Int -- width height
	-> FunPtr WindowCallback -- callback
	-> Int -- layered
	-> IO HWND -- HWND
foreign import ccall unsafe "setWin32WindowTitle" c_setWin32WindowTitle :: HWND -> LPTSTR -> IO ()
foreign import ccall unsafe "destroyWin32Window" c_destroyWin32Window :: HWND -> IO ()
foreign import ccall unsafe "updateLayeredWin32Window" c_updateLayeredWin32Window :: HWND -> IO ()
foreign import ccall unsafe "getLayeredWin32WindowBitmapData" c_getLayeredWin32WindowBitmapData :: HWND -> Ptr (Ptr CUChar) -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

-- wrappers

type InvokeCallback = IO ()
foreign import ccall "wrapper" wrapInvokeCallback :: InvokeCallback -> IO (FunPtr InvokeCallback)

type WindowCallback = Word -> WPARAM -> LPARAM -> IO ()
foreign import ccall "wrapper" wrapWindowCallback :: WindowCallback -> IO (FunPtr WindowCallback)

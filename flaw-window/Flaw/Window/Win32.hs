{-|
Module: Flaw.Window.Win32
Description: Win32 window framework.
License: MIT
-}

{-# LANGUAGE GADTs, TypeFamilies #-}

module Flaw.Window.Win32
	( Win32WindowSystem()
	, Win32Window(..)
	, initWin32WindowSystem
	, createWin32Window
	, createLayeredWin32Window
	, updateLayeredWin32Window
	, invokeWin32WindowSystem
	, invokeWin32WindowSystem_
	, addWin32WindowCallback
	, chanWin32WindowMessages
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import qualified Data.Text as T
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Flaw.Window
import Flaw.FFI.Win32

data Win32WindowSystem = Win32WindowSystem
	{ wsHandle :: Ptr () -- ^ Opaque handle for C side.
	, wsThreadId :: ThreadId
	, wsShutdownVar :: MVar ()
	}

data Win32Window = Win32Window
	{ wWindowSystem :: Win32WindowSystem
	, wHandle :: Ptr ()
	, wCallback :: FunPtr WindowCallback
	, wUserCallbacksRef :: IORef [WindowCallback]
	, wMessagesChan :: TChan (Word, WPARAM, LPARAM)
	, wEventsChan :: TChan WindowEvent
	}

instance Window Win32Window where
	setWindowTitle Win32Window { wWindowSystem = ws, wHandle = hwnd } title = invokeWin32WindowSystem_ ws $ do
		withCWString (T.unpack title) $ \titleCString ->
			c_setWin32WindowTitle hwnd titleCString
	getWindowClientSize Win32Window
		{ wWindowSystem = ws
		, wHandle = hwnd
		} = invokeWin32WindowSystem ws $ alloca $ \widthPtr -> alloca $ \heightPtr -> do
			c_getWin32WindowClientSize hwnd widthPtr heightPtr
			width <- peek widthPtr
			height <- peek heightPtr
			return (fromIntegral width, fromIntegral height)
	chanWindowEvents Win32Window
		{ wEventsChan = eventsChan
		} = dupTChan eventsChan

initWin32WindowSystem :: IO (Win32WindowSystem, IO ())
initWin32WindowSystem = do
	-- create vars
	handleVar <- newEmptyMVar
	shutdownVar <- newEmptyMVar

	-- create OS thread for window loop
	threadId <- forkOS $ do
		-- initialize window system, get a thread handle
		h <- c_initWin32WindowSystem
		-- send handles to the original thread
		putMVar handleVar h
		-- run window system
		c_runWin32WindowSystem h
		-- notify that window system quit
		putMVar shutdownVar ()
	-- wait for handle
	h <- readMVar handleVar

	let ws = Win32WindowSystem
		{ wsHandle = h
		, wsThreadId = threadId
		, wsShutdownVar = shutdownVar
		}

	let shutdown = do
		-- send a message to stop window loop
		invokeWin32WindowSystem_ ws $ c_stopWin32WindowSystem
		-- wait for actual completion
		readMVar $ wsShutdownVar ws
		-- free resources
		c_shutdownWin32WindowSystem $ wsHandle ws

	return (ws, shutdown)

createWin32Window :: Win32WindowSystem -> T.Text -> Int -> Int -> Int -> Int -> IO (Win32Window, IO ())
createWin32Window ws title left top width height = internalCreateWin32Window ws title left top width height False

createLayeredWin32Window :: Win32WindowSystem -> T.Text -> Int -> Int -> Int -> Int -> IO (Win32Window, IO ())
createLayeredWin32Window ws title left top width height = internalCreateWin32Window ws title left top width height True

internalCreateWin32Window :: Win32WindowSystem -> T.Text -> Int -> Int -> Int -> Int -> Bool -> IO (Win32Window, IO ())
internalCreateWin32Window ws title left top width height layered = do
	w <- invokeWin32WindowSystem ws $ mfix $ \w -> do
		-- create callback
		userCallbacksRef <- newIORef []
		messagesChan <- newBroadcastTChanIO
		eventsChan <- newBroadcastTChanIO
		callback <- wrapWindowCallback $ \msg wParam lParam -> do
			-- run sync callbacks
			userCallbacks <- readIORef userCallbacksRef
			forM_ userCallbacks $ \callback -> callback msg wParam lParam
			-- dispatch raw message
			atomically $ writeTChan messagesChan (msg, wParam, lParam)
			-- dispatch event
			let maybeEvent = case msg of
				0x0002 {- WM_DESTROY -} -> Just DestroyWindowEvent
				0x0005 {- WM_SIZE -} -> Just $ ResizeWindowEvent (fromIntegral $ loWord lParam) (fromIntegral $ hiWord lParam)
				0x0010 {- WM_CLOSE -} -> Just CloseWindowEvent
				_ -> Nothing
			case maybeEvent of
				Just event -> atomically $ writeTChan eventsChan event
				Nothing -> return ()
			-- process some other messages
			case msg of
				0x0002 -> do -- WM_DESTROY
					-- free callback
					freeHaskellFunPtr $ wCallback w
				_ -> return ()
		-- create window
		hwnd <- withCWString (T.unpack title) $ \titleCString ->
			c_createWin32Window (wsHandle ws) titleCString left top width height callback (if layered then 1 else 0)
		if hwnd == nullPtr then error "cannot create Win32Window"
		else return Win32Window
			{ wWindowSystem = ws
			, wHandle = hwnd
			, wCallback = callback
			, wUserCallbacksRef = userCallbacksRef
			, wMessagesChan = messagesChan
			, wEventsChan = eventsChan
			}
	return (w, invokeWin32WindowSystem_ ws $ c_destroyWin32Window $ wHandle w)

updateLayeredWin32Window :: Win32Window -> IO ()
updateLayeredWin32Window w = invokeWin32WindowSystem_ (wWindowSystem w) $ c_updateLayeredWin32Window $ wHandle w

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
invokeWin32WindowSystem_ :: Win32WindowSystem -> IO () -> IO ()
invokeWin32WindowSystem_ = invokeWithMaybeResultVar Nothing

-- | Invoke function in window system thread.
-- Wait for the result.
invokeWin32WindowSystem :: Win32WindowSystem -> IO a -> IO a
invokeWin32WindowSystem ws io = do
	resultVar <- newEmptyMVar
	invokeWithMaybeResultVar (Just resultVar) ws io
	result <- takeMVar resultVar
	case result of
		Left e -> throwIO e
		Right r -> return r

addWin32WindowCallback :: Win32Window -> WindowCallback -> IO ()
addWin32WindowCallback Win32Window
	{ wWindowSystem = ws
	, wUserCallbacksRef = userCallbacksRef
	} callback = do
	invokeWin32WindowSystem ws $ do
		callbacks <- readIORef userCallbacksRef
		writeIORef userCallbacksRef $ callback : callbacks

chanWin32WindowMessages :: Win32Window -> STM (TChan (Word, WPARAM, LPARAM))
chanWin32WindowMessages Win32Window
	{ wMessagesChan = messagesChan
	} = dupTChan messagesChan

-- foreign imports

foreign import ccall unsafe "initWin32WindowSystem" c_initWin32WindowSystem :: IO (Ptr ())
foreign import ccall safe "runWin32WindowSystem" c_runWin32WindowSystem :: Ptr () -> IO ()
foreign import ccall unsafe "shutdownWin32WindowSystem" c_shutdownWin32WindowSystem :: Ptr () -> IO ()
foreign import ccall unsafe "stopWin32WindowSystem" c_stopWin32WindowSystem :: IO ()
foreign import ccall unsafe "invokeWin32WindowSystem" c_invokeWin32WindowSystem :: Ptr () -> FunPtr InvokeCallback -> IO ()
foreign import ccall unsafe "createWin32Window" c_createWin32Window
	:: Ptr () -- window system handle
	-> LPWSTR -- title
	-> Int -> Int -- x y
	-> Int -> Int -- width height
	-> FunPtr WindowCallback -- callback
	-> Int -- layered
	-> IO HWND -- HWND
foreign import ccall unsafe "setWin32WindowTitle" c_setWin32WindowTitle :: HWND -> LPWSTR -> IO ()
foreign import ccall unsafe "getWin32WindowClientSize" c_getWin32WindowClientSize :: HWND -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "destroyWin32Window" c_destroyWin32Window :: HWND -> IO ()
foreign import ccall unsafe "updateLayeredWin32Window" c_updateLayeredWin32Window :: HWND -> IO ()
foreign import ccall unsafe "getLayeredWin32WindowBitmapData" c_getLayeredWin32WindowBitmapData :: HWND -> Ptr (Ptr CUChar) -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- wrappers

type InvokeCallback = IO ()
foreign import ccall "wrapper" wrapInvokeCallback :: InvokeCallback -> IO (FunPtr InvokeCallback)

type WindowCallback = Word -> WPARAM -> LPARAM -> IO ()
foreign import ccall "wrapper" wrapWindowCallback :: WindowCallback -> IO (FunPtr WindowCallback)

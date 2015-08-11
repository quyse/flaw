{-|
Module: Flaw.Window.Sdl
Description: SDL window framework.
License: MIT
-}

module Flaw.Window.Sdl
	( SdlWindowSystem(..)
	, SdlWindow(..)
	, initSdlWindowSystem
	, createSdlWindow
	, invokeSdlWindowSystem_
	, invokeSdlWindowSystem
	, addSdlWindowCallback
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as B
import Data.Int
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.UI.SDL.Enum as SDL
import qualified Graphics.UI.SDL.Event as SDL
import qualified Graphics.UI.SDL.Types as SDL
import qualified Graphics.UI.SDL.Video as SDL

import Flaw.Sdl
import Flaw.Window

data SdlWindowSystem = SdlWindowSystem
	{ swsWindows :: TVar (HashMap.HashMap Word32 SdlWindow)
	, swsInvokeUserEventCode :: !Int32
	}

data SdlWindow = SdlWindow
	{ swSystem :: SdlWindowSystem
	, swHandle :: SDL.Window
	, swEventsChan :: TChan WindowEvent
	, swClientSizeVar :: TVar (Int, Int)
	, swUserCallbacksRef :: IORef [SDL.Event -> IO ()]
	}

instance Window SdlWindow where
	setWindowTitle SdlWindow
		{ swHandle = h
		} title = do
		B.useAsCString (T.encodeUtf8 title) $ \titlePtr -> do
			SDL.setWindowTitle h titlePtr
	getWindowClientSize SdlWindow
		{ swClientSizeVar = sizeVar
		} = atomically $ readTVar sizeVar
	chanWindowEvents SdlWindow
		{ swEventsChan = chan
		} = dupTChan chan

initSdlWindowSystem :: IO (SdlWindowSystem, IO ())
initSdlWindowSystem = do

	windowsVar <- newTVarIO HashMap.empty

	userEventCodes <- SDL.registerEvents 2
	let invokeUserEventCode = fromIntegral userEventCodes
	let quitUserEventCode = invokeUserEventCode + 1

	-- run window thread
	_ <- forkOS $ do

		-- quit flag
		quitRef <- newIORef False

		let loop = do
			-- wait for event
			event <- alloca $ \eventPtr -> do
				checkSdlError (== 1) $ SDL.waitEvent eventPtr
				peek eventPtr

			-- select by event
			case event of
				SDL.WindowEvent
					{ SDL.windowEventWindowID = windowId
					, SDL.windowEventEvent = eventType
					, SDL.windowEventData1 = data1
					, SDL.windowEventData2 = data2
					} -> do
					windows <- atomically $ readTVar windowsVar
					case HashMap.lookup windowId windows of
						Just SdlWindow
							{ swEventsChan = eventsChan
							, swClientSizeVar = clientSizeVar
							} -> case eventType of
							SDL.SDL_WINDOWEVENT_RESIZED -> atomically $ do
								let width = fromIntegral data1
								let height = fromIntegral data2
								writeTVar clientSizeVar (width, height)
								writeTChan eventsChan $ ResizeWindowEvent width height
							SDL.SDL_WINDOWEVENT_CLOSE -> atomically $ writeTChan eventsChan CloseWindowEvent
							_ -> return ()
						Nothing -> return ()

				SDL.UserEvent
					{ SDL.userEventCode = eventCode
					, SDL.userEventData1 = eventData
					} -> do
					if eventCode == invokeUserEventCode then do
						let funPtr = castPtrToFunPtr eventData
						unwrapInvokeCallback funPtr
						freeHaskellFunPtr funPtr
					else if eventCode == quitUserEventCode then writeIORef quitRef True
					else return ()
				_ -> return ()

			-- check if quit flag is set
			quit <- readIORef quitRef
			if quit then return () else loop

		loop

	let quit = with SDL.UserEvent
		{ SDL.eventType = SDL.SDL_USEREVENT
		, SDL.eventTimestamp = 0
		, SDL.userEventWindowID = 0
		, SDL.userEventCode = quitUserEventCode
		, SDL.userEventData1 = nullPtr
		, SDL.userEventData2 = nullPtr
		} $ checkSdlError (== 1) . SDL.pushEvent

	return (SdlWindowSystem
		{ swsWindows = windowsVar
		, swsInvokeUserEventCode = invokeUserEventCode
		}, quit)

createSdlWindow :: SdlWindowSystem -> T.Text -> Int -> Int -> Int -> Int -> IO (SdlWindow, IO ())
createSdlWindow ws@SdlWindowSystem
	{ swsWindows = windowsVar
	} title x y width height = invokeSdlWindowSystem ws $ do
	-- create SDL window
	windowHandle <- checkSdlResult $ withCString (T.unpack title) $ \titlePtr -> do
		SDL.createWindow titlePtr
			(fromIntegral x)
			(fromIntegral y)
			(fromIntegral width)
			(fromIntegral height)
			(SDL.SDL_WINDOW_RESIZABLE + SDL.SDL_WINDOW_OPENGL)

	windowID <- SDL.getWindowID windowHandle

	eventsChan <- newBroadcastTChanIO
	clientSizeVar <- newTVarIO (0, 0)
	userCallbacksRef <- newIORef []

	let window = SdlWindow
		{ swSystem = ws
		, swHandle = windowHandle
		, swEventsChan = eventsChan
		, swClientSizeVar = clientSizeVar
		, swUserCallbacksRef = userCallbacksRef
		}

	-- add window into list
	atomically $ modifyTVar windowsVar $ HashMap.insert windowID window

	return (window, SDL.destroyWindow windowHandle)

addSdlWindowCallback :: SdlWindow -> (SDL.Event -> IO ()) -> IO ()
addSdlWindowCallback SdlWindow
	{ swSystem = ws
	, swUserCallbacksRef = userCallbacksRef
	} callback = invokeSdlWindowSystem ws $ modifyIORef userCallbacksRef (callback :)

invokeWithMaybeResultVar :: Maybe (MVar (Either SomeException a)) -> SdlWindowSystem -> IO a -> IO ()
invokeWithMaybeResultVar maybeResultVar SdlWindowSystem
	{ swsInvokeUserEventCode = eventCode
	} io = do
	invokeCallback <- wrapInvokeCallback $ do
		result <- try io
		case maybeResultVar of
			Just resultVar -> putMVar resultVar result
			Nothing -> return ()
	with SDL.UserEvent
		{ SDL.eventType = SDL.SDL_USEREVENT
		, SDL.eventTimestamp = 0
		, SDL.userEventWindowID = 0
		, SDL.userEventCode = eventCode
		, SDL.userEventData1 = castFunPtrToPtr invokeCallback
		, SDL.userEventData2 = nullPtr
		} $ checkSdlError (== 1) . SDL.pushEvent

invokeSdlWindowSystem_ :: SdlWindowSystem -> IO () -> IO ()
invokeSdlWindowSystem_ = invokeWithMaybeResultVar Nothing

invokeSdlWindowSystem :: SdlWindowSystem -> IO a -> IO a
invokeSdlWindowSystem ws io = do
	resultVar <- newEmptyMVar
	invokeWithMaybeResultVar (Just resultVar) ws io
	result <- takeMVar resultVar
	case result of
		Left e -> throwIO e
		Right r -> return r

type InvokeCallback = IO ()
foreign import ccall "wrapper" wrapInvokeCallback :: InvokeCallback -> IO (FunPtr InvokeCallback)
foreign import ccall "dynamic" unwrapInvokeCallback :: FunPtr InvokeCallback -> InvokeCallback

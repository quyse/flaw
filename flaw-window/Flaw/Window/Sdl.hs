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
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified SDL.Raw.Basic as SDL
import qualified SDL.Raw.Enum as SDL
import qualified SDL.Raw.Event as SDL
import qualified SDL.Raw.Thread as SDL
import qualified SDL.Raw.Types as SDL
import qualified SDL.Raw.Video as SDL

import Flaw.Book
import Flaw.Sdl
import Flaw.Window

data SdlWindowSystem = SdlWindowSystem
	{ swsThreadId :: {-# UNPACK #-} !SDL.ThreadID
	, swsWindows :: {-# UNPACK #-} !(TVar (HashMap.HashMap Word32 SdlWindow))
	, swsInvokeUserEventCode :: {-# UNPACK #-} !Int32
	, swsMouseCursors :: {-# UNPACK #-} !(V.Vector SDL.Cursor)
	}

data SdlWindow = SdlWindow
	{ swSystem :: !SdlWindowSystem
	, swHandle :: {-# UNPACK #-} !SDL.Window
	, swEventsChan :: {-# UNPACK #-} !(TChan WindowEvent)
	, swUserCallbacksRef :: {-# UNPACK #-} !(IORef [SDL.Event -> IO ()])
	}

instance Window SdlWindow where
	setWindowTitle SdlWindow
		{ swHandle = h
		} title = do
		B.useAsCString (T.encodeUtf8 title) $ \titlePtr -> do
			SDL.setWindowTitle h titlePtr
	getWindowClientSize SdlWindow
		{ swHandle = h
		} = alloca $ \widthPtr -> alloca $ \heightPtr -> do
		SDL.glGetDrawableSize h widthPtr heightPtr
		width <- peek widthPtr
		height <- peek heightPtr
		return (fromIntegral width, fromIntegral height)
	chanWindowEvents SdlWindow
		{ swEventsChan = chan
		} = dupTChan chan
	getWindowClipboardText _ = do
		cStr <- SDL.getClipboardText
		if cStr == nullPtr then return T.empty
		else do
			bytes <- B.packCString cStr
			SDL.free $ castPtr cStr
			return $ T.decodeUtf8 bytes
	setWindowClipboardText _ text = do
		B.useAsCString (T.encodeUtf8 text) $ void . SDL.setClipboardText
	setWindowMouseCursor SdlWindow
		{ swSystem = SdlWindowSystem
			{ swsMouseCursors = mouseCursors
			}
		} mouseCursor = SDL.setCursor $ mouseCursors V.! fromEnum mouseCursor
	setWindowMouseLock _ mouseLock = checkSdlError (== 0) $ SDL.setRelativeMouseMode mouseLock

initSdlWindowSystem :: Bool -> IO (SdlWindowSystem, IO ())
initSdlWindowSystem debug = withSpecialBook $ \bk -> do

	-- var for returning initialization result from SDL thread
	initResultVar <- newEmptyMVar

	-- run window thread
	_ <- forkOS $ do

		-- initialize

		book bk initSdlVideo

		windowsVar <- newTVarIO HashMap.empty

		userEventCodes <- SDL.registerEvents 2
		let invokeUserEventCode = fromIntegral userEventCodes
		let quitUserEventCode = invokeUserEventCode + 1

		let shutdown = with SDL.UserEvent
			{ SDL.eventType = SDL.SDL_USEREVENT
			, SDL.eventTimestamp = 0
			, SDL.userEventWindowID = 0
			, SDL.userEventCode = quitUserEventCode
			, SDL.userEventData1 = nullPtr
			, SDL.userEventData2 = nullPtr
			} $ checkSdlError (== 1) . SDL.pushEvent

		book bk $ return ((), shutdown)

		-- mouse cursors
		mouseCursors <- V.forM (V.fromList [minBound .. maxBound]) $ \mouseCursor -> do
			let sdlSystemCursor = case mouseCursor of
				MouseCursorArrow -> SDL.SDL_SYSTEM_CURSOR_ARROW
				MouseCursorWait -> SDL.SDL_SYSTEM_CURSOR_WAIT
				MouseCursorWaitArrow -> SDL.SDL_SYSTEM_CURSOR_WAITARROW
				MouseCursorIBeam -> SDL.SDL_SYSTEM_CURSOR_IBEAM
				MouseCursorSizeNWSE -> SDL.SDL_SYSTEM_CURSOR_SIZENWSE
				MouseCursorSizeNESW -> SDL.SDL_SYSTEM_CURSOR_SIZENESW
				MouseCursorSizeWE -> SDL.SDL_SYSTEM_CURSOR_SIZEWE
				MouseCursorSizeNS -> SDL.SDL_SYSTEM_CURSOR_SIZENS
				MouseCursorSizeAll -> SDL.SDL_SYSTEM_CURSOR_SIZEALL
				MouseCursorHand -> SDL.SDL_SYSTEM_CURSOR_HAND
			sdlCursor <- SDL.createSystemCursor sdlSystemCursor
			book bk $ return (sdlCursor, SDL.freeCursor sdlCursor)

		-- return result into initial thread
		threadId <- SDL.threadID
		putMVar initResultVar SdlWindowSystem
			{ swsThreadId = threadId
			, swsWindows = windowsVar
			, swsInvokeUserEventCode = invokeUserEventCode
			, swsMouseCursors = mouseCursors
			}

		-- set common attributes
		mapM_ (\(attr, value) -> checkSdlError (== 0) $ SDL.glSetAttribute attr value)
			[ (SDL.SDL_GL_CONTEXT_MAJOR_VERSION, 3)
			, (SDL.SDL_GL_CONTEXT_MINOR_VERSION, 3)
			, (SDL.SDL_GL_DOUBLEBUFFER, 1)
			, (SDL.SDL_GL_FRAMEBUFFER_SRGB_CAPABLE, 1)
			, (SDL.SDL_GL_CONTEXT_FLAGS, if debug then SDL.SDL_GL_CONTEXT_DEBUG_FLAG else 0)
			]

		-- quit flag
		quitRef <- newIORef False

		let loop = do
			-- wait for event
			event <- alloca $ \eventPtr -> do
				checkSdlError (== 1) $ SDL.waitEvent eventPtr
				peek eventPtr

			-- get window id
			let maybeWindowId = case event of
				SDL.WindowEvent
					{ SDL.windowEventWindowID = windowId
					} -> Just windowId
				SDL.KeyboardEvent
					{ SDL.keyboardEventWindowID = windowId
					} -> Just windowId
				SDL.TextEditingEvent
					{ SDL.textEditingEventWindowID = windowId
					} -> Just windowId
				SDL.TextInputEvent
					{ SDL.textInputEventWindowID = windowId
					} -> Just windowId
				SDL.MouseMotionEvent
					{ SDL.mouseMotionEventWindowID = windowId
					} -> Just windowId
				SDL.MouseButtonEvent
					{ SDL.mouseButtonEventWindowID = windowId
					} -> Just windowId
				SDL.MouseWheelEvent
					{ SDL.mouseWheelEventWindowID = windowId
					} -> Just windowId
				SDL.UserEvent
					{ SDL.userEventWindowID = windowId
					} -> Just windowId
				_ -> Nothing

			-- get window
			maybeWindow <- case maybeWindowId of
				Just windowId -> fmap (HashMap.lookup windowId) $ atomically $ readTVar windowsVar
				Nothing -> return Nothing

			-- if there's a window, call user callbacks
			case maybeWindow of
				Just SdlWindow
					{ swUserCallbacksRef = userCallbacksRef
					} -> mapM_ ($ event) =<< readIORef userCallbacksRef
				Nothing -> return ()

			-- process some events
			case event of
				SDL.WindowEvent
					{ SDL.windowEventEvent = eventType
					, SDL.windowEventData1 = data1
					, SDL.windowEventData2 = data2
					} -> do
					case maybeWindow of
						Just SdlWindow
							{ swEventsChan = eventsChan
							} -> case eventType of
							SDL.SDL_WINDOWEVENT_RESIZED -> atomically $ do
								let width = fromIntegral data1
								let height = fromIntegral data2
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
			unless quit loop

		loop

	-- wait for initialization and return result from thread
	takeMVar initResultVar

createSdlWindow :: SdlWindowSystem -> T.Text -> Maybe (Int, Int) -> Maybe (Int, Int) -> Bool -> IO (SdlWindow, IO ())
createSdlWindow ws@SdlWindowSystem
	{ swsWindows = windowsVar
	} title maybePosition maybeSize needDepth = invokeSdlWindowSystem ws $ do
	-- set up attributes
	checkSdlError (== 0) $ SDL.glSetAttribute SDL.SDL_GL_RED_SIZE 8
	checkSdlError (== 0) $ SDL.glSetAttribute SDL.SDL_GL_GREEN_SIZE 8
	checkSdlError (== 0) $ SDL.glSetAttribute SDL.SDL_GL_BLUE_SIZE 8
	checkSdlError (== 0) $ SDL.glSetAttribute SDL.SDL_GL_ALPHA_SIZE 8
	checkSdlError (== 0) $ SDL.glSetAttribute SDL.SDL_GL_DEPTH_SIZE (if needDepth then 16 else 0)
	checkSdlError (== 0) $ SDL.glSetAttribute SDL.SDL_GL_STENCIL_SIZE (if needDepth then 8 else 0)
	-- position and size
	let (x, y) = fromMaybe (SDL.SDL_WINDOWPOS_UNDEFINED, SDL.SDL_WINDOWPOS_UNDEFINED) maybePosition
	let (width, height) = fromMaybe (800, 600) maybeSize
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
	userCallbacksRef <- newIORef []

	let window = SdlWindow
		{ swSystem = ws
		, swHandle = windowHandle
		, swEventsChan = eventsChan
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
	{ swsThreadId = threadId
	, swsInvokeUserEventCode = eventCode
	} io = do
	sync <- if isNothing maybeResultVar then return False else do
		currentThreadId <- SDL.threadID
		return $ threadId == currentThreadId
	let callback = do
		result <- try io
		case maybeResultVar of
			Just resultVar -> putMVar resultVar result
			Nothing -> return ()
	if sync then callback else do
		invokeCallback <- wrapInvokeCallback callback
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

{-|
Module: Flaw.UI.Window
Description: Window is a thing running UI processing on top of the native window.
It's not an element.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, GADTs #-}

module Flaw.UI.Window
	( Window(..)
	, newWindow
	, queueWindowAction
	, setWindowCloseHandler
	, processWindow
	, renderWindow
	, getWindowSize
	) where

import Control.Concurrent.STM
import Control.Monad

import Flaw.Graphics
import Flaw.Input
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer
import qualified Flaw.Window as W

data Window = Window
	{ windowNativeWindow :: !SomeNativeWindow
	, windowEventsChan :: {-# UNPACK #-} !(TChan W.WindowEvent)
	, windowKeyboardEventsChan :: {-# UNPACK #-} !(TChan KeyboardEvent)
	, windowKeyboardState :: !KeyboardState
	, windowMouseEventsChan :: {-# UNPACK #-} !(TChan MouseEvent)
	, windowMouseState :: !MouseState
	, windowElement :: !SomeElement
	, windowSizeVar :: {-# UNPACK #-} !(TVar Int2)
	, windowCloseHandlerVar :: {-# UNPACK #-} !(TVar (STM ()))
	, windowDestroyHandlerVar :: {-# UNPACK #-} !(TVar (STM ()))
	, windowActionsQueue :: {-# UNPACK #-} !(TQueue (IO ()))
	, windowMouseCursorVar :: {-# UNPACK #-} !(TVar MouseCursor)
	}

data SomeNativeWindow where
	SomeNativeWindow :: W.Window w => w -> SomeNativeWindow

newWindow :: (W.Window w, InputManager im KeyboardEvent, InputManager im MouseEvent, Element e) => w -> im -> e -> STM Window
newWindow nativeWindow inputManager element = do
	-- get window events channel
	eventsChan <- W.chanWindowEvents nativeWindow
	-- get keyboard events channel and state
	keyboardEventsChan <- chanInputEvents inputManager
	keyboardState <- initialInputState
	-- get mouse events channel and state
	mouseEventsChan <- chanInputEvents inputManager
	mouseState <- initialInputState

	-- current size
	sizeVar <- newTVar $ Vec2 0 0

	-- handlers
	closeHandlerVar <- newTVar $ return ()
	destroyHandlerVar <- newTVar $ return ()

	-- queue for delayed IO actions
	actionsQueue <- newTQueue

	-- current mouse cursor
	mouseCursorVar <- newTVar MouseCursorArrow

	-- return window
	return Window
		{ windowNativeWindow = SomeNativeWindow nativeWindow
		, windowEventsChan = eventsChan
		, windowKeyboardEventsChan = keyboardEventsChan
		, windowKeyboardState = keyboardState
		, windowMouseEventsChan = mouseEventsChan
		, windowMouseState = mouseState
		, windowElement = SomeElement element
		, windowSizeVar = sizeVar
		, windowCloseHandlerVar = closeHandlerVar
		, windowDestroyHandlerVar = destroyHandlerVar
		, windowActionsQueue = actionsQueue
		, windowMouseCursorVar = mouseCursorVar
		}

-- | Enqueue an IO action, which will be run in a window loop.
queueWindowAction :: Window -> IO () -> STM ()
queueWindowAction Window
	{ windowActionsQueue = actionsQueue
	} action = writeTQueue actionsQueue action

-- | Set window close handler.
setWindowCloseHandler :: Window -> STM () -> STM ()
setWindowCloseHandler Window
	{ windowCloseHandlerVar = closeHandlerVar
	} closeHandler = writeTVar closeHandlerVar closeHandler

-- | Run normal window processing.
-- It's necessary to run this method regularly (usually in a frame update function).
-- Also it's critical to run it before 'renderWindow'.
processWindow :: Window -> IO ()
processWindow Window
	{ windowNativeWindow = SomeNativeWindow nativeWindow
	, windowEventsChan = eventsChan
	, windowKeyboardEventsChan = keyboardEventsChan
	, windowKeyboardState = keyboardState
	, windowMouseEventsChan = mouseEventsChan
	, windowMouseState = mouseState
	, windowElement = SomeElement element
	, windowSizeVar = sizeVar
	, windowCloseHandlerVar = closeHandlerVar
	, windowDestroyHandlerVar = destroyHandlerVar
	, windowActionsQueue = actionsQueue
	, windowMouseCursorVar = mouseCursorVar
	} = do
	-- compose input state
	let inputState = InputState
		{ inputStateKeyboard = keyboardState
		, inputStateMouse = mouseState
		, inputStateGetClipboardText = \callback -> writeTQueue actionsQueue $ (atomically . callback) =<< W.getWindowClipboardText nativeWindow
		, inputStateSetClipboardText = writeTQueue actionsQueue . W.setWindowClipboardText nativeWindow
		}
	-- update layout
	let updateLayout newSize = do
		size <- readTVar sizeVar
		when (size /= newSize) $ do
			layoutElement element newSize
			writeTVar sizeVar newSize
	-- process input and window events
	let process = do
		-- native window events
		let processWindowEvent = do
			event <- readTChan eventsChan
			case event of
				W.CloseWindowEvent -> join $ readTVar closeHandlerVar
				W.DestroyWindowEvent -> join $ readTVar destroyHandlerVar
				W.ResizeWindowEvent width height -> updateLayout $ Vec2 width height
				_ -> return ()
		-- event handling
		let handleEvent event = void $ processInputEvent element event inputState
		-- keyboard events
		let processKeyboardEvent = do
			keyboardEvent <- readTChan keyboardEventsChan
			handleEvent (KeyboardInputEvent keyboardEvent)
			applyInputEvent keyboardState keyboardEvent
		-- mouse events
		let processMouseEvent = do
			mouseEvent <- readTChan mouseEventsChan
			handleEvent (MouseInputEvent mouseEvent)
			applyInputEvent mouseState mouseEvent
		let processSomeEvent = orElse processWindowEvent (orElse processKeyboardEvent processMouseEvent)
		join $ atomically $ orElse (processSomeEvent >> return process) (return $ return ())
	process

	-- update mouse cursor if needed
	atomically $ do
		mouseCursor <- elementMouseCursor element
		currentMouseCursor <- readTVar mouseCursorVar
		when (mouseCursor /= currentMouseCursor) $ do
			writeTQueue actionsQueue $ W.setWindowMouseCursor nativeWindow mouseCursor
			writeTVar mouseCursorVar mouseCursor

	-- process IO actions
	let processActions = do
		maybeAction <- atomically $ tryReadTQueue actionsQueue
		case maybeAction of
			Just action -> do
				action
				processActions
			Nothing -> return ()
	processActions

	-- update layout one more time, just in case there was no resize events
	(clientWidth, clientHeight) <- W.getWindowClientSize nativeWindow
	atomically $ updateLayout $ Vec2 clientWidth clientHeight

-- | Render window.
renderWindow :: Context c d => Window -> Drawer d -> STM (Render c ())
renderWindow Window
	{ windowElement = SomeElement element
	} drawer = do
	renderElement element drawer $ Vec2 0 0

-- | Get window size.
getWindowSize :: Window -> STM Size
getWindowSize Window
	{ windowSizeVar = sizeVar
	} = readTVar sizeVar

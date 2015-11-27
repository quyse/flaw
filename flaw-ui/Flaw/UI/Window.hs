{-|
Module: Flaw.UI.Window
Description: Window is a thing running UI processing on top of the native window.
It's not an element.
License: MIT
-}

{-# LANGUAGE FlexibleContexts #-}

module Flaw.UI.Window
	( Window(..)
	, newWindow
	, processWindow
	) where

import Flaw.Input
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.UI
import qualified Flaw.Window as W

import Control.Concurrent.STM
import Control.Monad

data Window = Window
	{ windowEventsChan :: !(TChan W.WindowEvent)
	, windowKeyboardEventsChan :: !(TChan KeyboardEvent)
	, windowKeyboardState :: !KeyboardState
	, windowMouseEventsChan :: !(TChan MouseEvent)
	, windowMouseState :: !MouseState
	, windowElement :: !SomeElement
	, windowCloseHandlerVar :: !(TVar (STM ()))
	, windowDestroyHandlerVar :: !(TVar (STM ()))
	}

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

	-- refs
	closeHandlerVar <- newTVar $ return ()
	destroyHandlerVar <- newTVar $ return ()

	-- return window
	return Window
		{ windowEventsChan = eventsChan
		, windowKeyboardEventsChan = keyboardEventsChan
		, windowKeyboardState = keyboardState
		, windowMouseEventsChan = mouseEventsChan
		, windowMouseState = mouseState
		, windowElement = SomeElement element
		, windowCloseHandlerVar = closeHandlerVar
		, windowDestroyHandlerVar = destroyHandlerVar
		}

-- | Run normal window processing.
-- It's necessary to run this method regularly (usually in a frame update function).
-- Nothing works without it.
processWindow :: Window -> IO ()
processWindow Window
	{ windowEventsChan = eventsChan
	, windowKeyboardEventsChan = keyboardEventsChan
	, windowKeyboardState = keyboardState
	, windowMouseEventsChan = mouseEventsChan
	, windowMouseState = mouseState
	, windowElement = SomeElement element
	, windowCloseHandlerVar = closeHandlerVar
	, windowDestroyHandlerVar = destroyHandlerVar
	} = do
	-- process input and window events
	let process = do
		-- native window events
		let processWindowEvent = do
			event <- readTChan eventsChan
			case event of
				W.CloseWindowEvent -> join $ readTVar closeHandlerVar
				W.DestroyWindowEvent -> join $ readTVar destroyHandlerVar
				_ -> return ()
		-- keyboard events
		let processKeyboardEvent = do
			keyboardEvent <- readTChan keyboardEventsChan
			_ <- processInputEvent element $ KeyboardInputEvent keyboardEvent keyboardState
			applyInputEvent keyboardState keyboardEvent
		-- mouse events
		let processMouseEvent = do
			mouseEvent <- readTChan mouseEventsChan
			_ <- processInputEvent element $ MouseInputEvent mouseEvent mouseState
			applyInputEvent mouseState mouseEvent
		let processSomeEvent = orElse processWindowEvent (orElse processKeyboardEvent processMouseEvent)
		join $ atomically $ orElse (processSomeEvent >> return process) (return $ return ())
	process

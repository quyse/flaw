{-|
Module: Flaw.Input.Web
Description: User input for web.
License: MIT
-}

module Flaw.Input.Web
	( WebInputManager()
	, initWebInput
	) where

import Control.Concurrent.STM
import Data.Char
import GHCJS.Foreign.Callback
import GHCJS.Marshal.Pure
import GHCJS.Types

import qualified Flaw.Window.Web.Canvas as Web
import Flaw.Input.Basic
import Flaw.Input.Mouse
import Flaw.Input.Keyboard

type WebInputManager = BasicInputManager

initWebInput :: Web.Canvas -> IO WebInputManager
initWebInput Web.Canvas
	{ Web.canvasElement = jsCanvas
	} = do
	-- init basic manager
	inputManager@BasicInputManager
		{ mKeyboardChan = keyboardChan
		, mMouseChan = mouseChan
		} <- initBasicInputManager

	-- helper routines
	let addKeyboardEvent event = atomically $ writeTChan keyboardChan event
	let addMouseEvent event = atomically $ writeTChan mouseChan event

	-- register callbacks
	keydownCallback <- syncCallback1 ThrowWouldBlock $ addKeyboardEvent . KeyDownEvent . convertKeyCode
	keyupCallback <- syncCallback1 ThrowWouldBlock $ addKeyboardEvent . KeyUpEvent . convertKeyCode
	keypressCallback <- syncCallback1 ThrowWouldBlock $ addKeyboardEvent . CharEvent . chr . pFromJSVal
	mousedownCallback <- syncCallback1 ThrowWouldBlock $ addMouseEvent . MouseDownEvent . convertMouseButton
	mouseupCallback <- syncCallback1 ThrowWouldBlock $ addMouseEvent . MouseUpEvent . convertMouseButton
	rawMouseMoveCallback <- syncCallback3 ThrowWouldBlock $ \jsX jsY jsZ -> addMouseEvent $ RawMouseMoveEvent (pFromJSVal jsX) (pFromJSVal jsY) (pFromJSVal jsZ)
	cursorMoveCallback <- syncCallback2 ThrowWouldBlock $ \jsX jsY -> addMouseEvent $ CursorMoveEvent (pFromJSVal jsX) (pFromJSVal jsY)

	js_registerEvents jsCanvas
		keydownCallback keyupCallback keypressCallback
		mousedownCallback mouseupCallback rawMouseMoveCallback cursorMoveCallback

	return inputManager

convertKeyCode :: JSVal -> Key
convertKeyCode jsKeyCode = case (pFromJSVal jsKeyCode :: Int) of
	37 -> KeyLeft
	38 -> KeyUp
	39 -> KeyRight
	40 -> KeyDown
	65 -> KeyA
	66 -> KeyB
	67 -> KeyC
	68 -> KeyD
	69 -> KeyE
	70 -> KeyF
	71 -> KeyG
	72 -> KeyH
	73 -> KeyI
	74 -> KeyJ
	75 -> KeyK
	76 -> KeyL
	77 -> KeyM
	78 -> KeyN
	79 -> KeyO
	80 -> KeyP
	81 -> KeyQ
	82 -> KeyR
	83 -> KeyS
	84 -> KeyT
	85 -> KeyU
	86 -> KeyV
	87 -> KeyW
	88 -> KeyX
	89 -> KeyY
	90 -> KeyZ
	_ -> KeyUnknown

convertMouseButton :: JSVal -> MouseButton
convertMouseButton jsButton = case (pFromJSVal jsButton :: Int) of
	0 -> LeftMouseButton
	1 -> MiddleMouseButton
	2 -> RightMouseButton
	_ -> LeftMouseButton

foreign import javascript unsafe "h$flaw_input_register_events($1, $2, $3, $4, $5, $6, $7, $8)" js_registerEvents
	:: JSVal
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> JSVal -> JSVal -> IO ())
	-> Callback (JSVal -> JSVal -> IO ())
	-> IO ()

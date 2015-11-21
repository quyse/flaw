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
import Data.Maybe
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types

import qualified Flaw.Window.Web.Canvas as Web
import Flaw.Input.Basic
import Flaw.Input.Mouse
import Flaw.Input.Keyboard

type WebInputManager = BasicInputManager

initWebInput :: Web.Canvas -> IO WebInputManager
initWebInput Web.Canvas
	{ Web.canvasElement = domCanvas
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
	keydownCallback <- syncCallback1 ThrowWouldBlock $ \jsKeyCode -> do
		keyCode <- convertKeyCode jsKeyCode
		addKeyboardEvent $ KeyDownEvent keyCode
	keyupCallback <- syncCallback1 ThrowWouldBlock $ \jsKeyCode -> do
		keyCode <- convertKeyCode jsKeyCode
		addKeyboardEvent $ KeyUpEvent keyCode
	keypressCallback <- syncCallback1 ThrowWouldBlock $ \jsCharCode -> do
		charCode <- convertCharCode jsCharCode
		addKeyboardEvent $ CharEvent charCode
	mousedownCallback <- syncCallback1 ThrowWouldBlock $ \jsButton -> do
		button <- convertMouseButton jsButton
		addMouseEvent $ MouseDownEvent button
	mouseupCallback <- syncCallback1 ThrowWouldBlock $ \jsButton -> do
		button <- convertMouseButton jsButton
		addMouseEvent $ MouseUpEvent button
	mousemoveCallback <- syncCallback2 ThrowWouldBlock $ \jsX jsY -> do
		maybeX <- fromJSVal jsX
		maybeY <- fromJSVal jsY
		addMouseEvent $ CursorMoveEvent (fromJust maybeX) (fromJust maybeY)
	mousewheelCallback <- syncCallback1 ThrowWouldBlock $ \jsZ -> do
		maybeZ <- fromJSVal jsZ
		addMouseEvent $ RawMouseMoveEvent 0 0 $ fromJust maybeZ

	jsCanvas <- toJSVal domCanvas
	js_registerEvents jsCanvas
		keydownCallback keyupCallback keypressCallback
		mousedownCallback mouseupCallback mousemoveCallback mousewheelCallback

	return inputManager

convertKeyCode :: JSVal -> IO Key
convertKeyCode jsKeyCode = do
	maybeKeyCode <- fromJSVal jsKeyCode :: IO (Maybe Int)
	return $ case fromJust maybeKeyCode of
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

convertCharCode :: JSVal -> IO Char
convertCharCode jsCharCode = do
	maybeCharCode <- fromJSVal jsCharCode
	return $ chr $ fromJust maybeCharCode

convertMouseButton :: JSVal -> IO MouseButton
convertMouseButton jsButton = do
	maybeButton <- fromJSVal jsButton :: IO (Maybe Int)
	return $ case fromJust maybeButton of
		0 -> LeftMouseButton
		1 -> MiddleMouseButton
		2 -> RightMouseButton
		_ -> LeftMouseButton

foreign import javascript unsafe " \
	\ document.addEventListener('keydown', function(e) { \
	\ $2(e.which); \
	\ }, false); \
	\ document.addEventListener('keyup', function(e) { \
	\ $3(e.which); \
	\ }, false); \
	\ document.addEventListener('keypress', function(e) { \
	\ $4(e.which); \
	\ }, false); \
	\ $1.addEventListener('mousedown', function(e) { \
	\ $5(e.button); \
	\ }, false); \
	\ $1.addEventListener('mouseup', function(e) { \
	\ $6(e.button); \
	\ }, false); \
	\ $1.addEventListener('mousemove', function(e) { \
	\ $7(e.clientX, e.clientY); \
	\ }, false); \
	\ $1.addEventListener('wheel', function(e) { \
	\ $8(e.wheelDelta || ((e.deltaY || 0) * 40)); \
	\ }, false); \
	\ " js_registerEvents
	:: JSVal
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> Callback (JSVal -> JSVal -> IO ())
	-> Callback (JSVal -> IO ())
	-> IO ()

{-|
Module: Flaw.Input.Web
Description: User input for web.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Input.Web
	( WebInputManager()
	, initWebInputManager
	) where

import Data.Char
import Data.Maybe
import qualified GHCJS.DOM.Element as DOM
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import qualified Flaw.Window.Web.Canvas as Web
import Flaw.Input
import Flaw.Input.Basic

data WebInputManager = WebInputManager
	{ mFramePair :: BasicFramePair
	}

instance Manager WebInputManager where
	type ManagerFrame WebInputManager = BasicFrame
	nextInputFrame WebInputManager
		{ mFramePair = framePair
		} = nextBasicFrame framePair

initWebInputManager :: Web.Canvas -> IO WebInputManager
initWebInputManager (Web.Canvas domCanvas) = do
	-- create frame pair
	framePair <- initBasicFramePair

	-- register callbacks
	keydownCallback <- syncCallback1 AlwaysRetain False $ \jsKeyCode -> do
		keyCode <- convertKeyCode jsKeyCode
		addEventToBasicFrame framePair $ EventKeyboard $ KeyDownEvent keyCode
	keyupCallback <- syncCallback1 AlwaysRetain False $ \jsKeyCode -> do
		keyCode <- convertKeyCode jsKeyCode
		addEventToBasicFrame framePair $ EventKeyboard $ KeyUpEvent keyCode
	keypressCallback <- syncCallback1 AlwaysRetain False $ \jsCharCode -> do
		charCode <- convertCharCode jsCharCode
		addEventToBasicFrame framePair $ EventKeyboard $ CharEvent charCode
	mousedownCallback <- syncCallback1 AlwaysRetain False $ \jsButton -> do
		button <- convertMouseButton jsButton
		addEventToBasicFrame framePair $ EventMouse $ MouseDownEvent button
	mouseupCallback <- syncCallback1 AlwaysRetain False $ \jsButton -> do
		button <- convertMouseButton jsButton
		addEventToBasicFrame framePair $ EventMouse $ MouseUpEvent button
	mousemoveCallback <- syncCallback2 AlwaysRetain False $ \jsX jsY -> do
		maybeX <- fromJSRef jsX
		maybeY <- fromJSRef jsY
		addEventToBasicFrame framePair $ EventMouse $ CursorMoveEvent (fromJust maybeX) (fromJust maybeY)
	mousewheelCallback <- syncCallback1 AlwaysRetain False $ \jsZ -> do
		maybeZ <- fromJSRef jsZ
		addEventToBasicFrame framePair $ EventMouse $ RawMouseMoveEvent 0 0 $ fromJust maybeZ

	jsCanvas <- toJSRef domCanvas
	js_registerEvents jsCanvas
		keydownCallback keyupCallback keypressCallback
		mousedownCallback mouseupCallback mousemoveCallback mousewheelCallback

	return WebInputManager
		{ mFramePair = framePair
		}

convertKeyCode :: JSRef Int -> IO Key
convertKeyCode jsKeyCode = do
	maybeKeyCode <- fromJSRef jsKeyCode
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

convertCharCode :: JSRef Int -> IO Char
convertCharCode jsCharCode = do
	maybeCharCode <- fromJSRef jsCharCode
	return $ chr $ fromJust maybeCharCode

convertMouseButton :: JSRef Int -> IO MouseButton
convertMouseButton jsButton = do
	maybeButton <- fromJSRef jsButton
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
	\ $8(e.wheelDelta); \
	\ }, false); \
	\ " js_registerEvents
	:: JSRef DOM.Element
	-> JSFun (JSRef Int -> IO ())
	-> JSFun (JSRef Int -> IO ())
	-> JSFun (JSRef Int -> IO ())
	-> JSFun (JSRef Int -> IO ())
	-> JSFun (JSRef Int -> IO ())
	-> JSFun (JSRef Int -> JSRef Int -> IO ())
	-> JSFun (JSRef Float -> IO ())
	-> IO ()

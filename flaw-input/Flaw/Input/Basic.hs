{-|
Module: Flaw.Input.Basic
Description: Helper classes for implementation of input managers.
License: MIT
-}

module Flaw.Input.Basic
	( BasicFrame(..)
	, BasicFramePair
	, initBasicFramePair
	, addEventToBasicFrame
	, nextBasicFrame
	) where

import Control.Monad
import Data.Array.IO
import Data.IORef

import Flaw.Input

data BasicFrame = BasicFrame
	{ fKeys :: IOUArray Key Bool
	, fMouseButtons :: IOUArray MouseButton Bool
	, fCursor :: IORef (Int, Int)
	, fEvents :: IORef [Event]
	}

instance State BasicFrame where
	getKeyState BasicFrame
		{ fKeys = keysArray
		} key = readArray keysArray key
	getMouseButtonState BasicFrame
		{ fMouseButtons = mouseButtonsArray
		} mouseButton = readArray mouseButtonsArray mouseButton
	getMouseCursor BasicFrame
		{ fCursor = cursorRef
		} = readIORef cursorRef

instance Frame BasicFrame where
	nextInputEvent frame@BasicFrame
		{ fEvents = eventsRef
		} = do
		events <- readIORef eventsRef
		case events of
			e:es -> do
				applyEventToBasicFrame frame e
				writeIORef eventsRef es
				return $ Just e
			[] -> return Nothing

applyEventToBasicFrame :: BasicFrame -> Event -> IO ()
applyEventToBasicFrame BasicFrame
	{ fKeys = keysArray
	} (EventKeyboard keyboardEvent) = do
	case keyboardEvent of
		KeyDownEvent key -> writeArray keysArray key True
		KeyUpEvent key -> writeArray keysArray key False
		CharEvent _char -> return ()
applyEventToBasicFrame BasicFrame
	{ fMouseButtons = mouseButtonsArray
	, fCursor = cursorRef
	} (EventMouse mouseEvent) = do
	case mouseEvent of
		MouseDownEvent mouseButton -> writeArray mouseButtonsArray mouseButton True
		MouseUpEvent mouseButton -> writeArray mouseButtonsArray mouseButton False
		RawMouseMoveEvent _x _y _z -> return ()
		CursorMoveEvent x y -> writeIORef cursorRef (x, y)

type BasicFramePair = IORef (BasicFrame, BasicFrame)

initBasicFramePair :: IO BasicFramePair
initBasicFramePair = do
	let initBasicFrame = do
		keys <- newArray (minBound, maxBound) False
		mouseButtons <- newArray (minBound, maxBound) False
		cursor <- newIORef (0, 0)
		events <- newIORef []
		return BasicFrame
			{ fKeys = keys
			, fMouseButtons = mouseButtons
			, fCursor = cursor
			, fEvents = events
			}
	currentFrame <- initBasicFrame
	internalFrame <- initBasicFrame
	newIORef (currentFrame, internalFrame)

addEventToBasicFrame :: BasicFramePair -> Event -> IO ()
addEventToBasicFrame framesRef event = do
	(_currentFrame, BasicFrame
		{ fEvents = eventsRef
		}) <- readIORef framesRef
	modifyIORef eventsRef (++ [event])

nextBasicFrame :: BasicFramePair -> IO BasicFrame
nextBasicFrame framesRef = do
	-- get frames
	(currentFrame@BasicFrame
		{ fEvents = eventsRef
		}, internalFrame) <- readIORef framesRef
	-- rewind current frame (if there's some events left)
	events <- readIORef eventsRef
	forM_ events $ applyEventToBasicFrame currentFrame
	writeIORef eventsRef []
	-- swap frames
	writeIORef framesRef (internalFrame, currentFrame)
	-- return new current frame
	return internalFrame

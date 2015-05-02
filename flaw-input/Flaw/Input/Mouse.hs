{-|
Module: Flaw.Input.Mouse
Description: Mouse user input.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Flaw.Input.Mouse
	( MouseState(..)
	, MouseEvent(..)
	, MouseButton(..)
	, getMouseButtonState
	, getMouseCursor
	) where

import Control.Concurrent.STM
import Data.Array.MArray

import Flaw.Input

data MouseState = MouseState
	{ mouseStateButtons :: TArray MouseButton Bool
	, mouseStateCursor :: TVar (Int, Int)
	}

data MouseEvent
	= MouseDownEvent !MouseButton
	| MouseUpEvent !MouseButton
	| RawMouseMoveEvent !Float !Float !Float
	| CursorMoveEvent !Int !Int
	deriving Show

data MouseButton
	= LeftMouseButton
	| RightMouseButton
	| MiddleMouseButton
	deriving (Eq, Ord, Ix, Bounded, Enum, Show)

instance InputState MouseState where
	initialInputState = do
		buttonsArray <- newArray (minBound, maxBound) False
		cursorVar <- newTVar (0, 0)
		return MouseState
			{ mouseStateButtons = buttonsArray
			, mouseStateCursor = cursorVar
			}

instance InputDevice MouseState MouseEvent where
	applyInputEvent MouseState
		{ mouseStateButtons = buttonsArray
		, mouseStateCursor = cursorVar
		} event = do
		case event of
			MouseDownEvent button -> writeArray buttonsArray button True
			MouseUpEvent button -> writeArray buttonsArray button False
			RawMouseMoveEvent _ _ _ -> return ()
			CursorMoveEvent x y -> writeTVar cursorVar (x, y)

getMouseButtonState :: MouseState -> MouseButton -> STM Bool
getMouseButtonState MouseState
	{ mouseStateButtons = buttonsArray
	} button = readArray buttonsArray button

getMouseCursor :: MouseState -> STM (Int, Int)
getMouseCursor MouseState
	{ mouseStateCursor = cursorVar
	} = readTVar cursorVar

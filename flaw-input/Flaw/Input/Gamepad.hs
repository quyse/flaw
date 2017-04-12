{-|
Module: Flaw.Input.Gamepad
Description: Gamepad user input.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Flaw.Input.Gamepad
	( GamepadState(..)
	, GamepadEvent(..)
	, GamepadButton(..)
	, GamepadAxis(..)
	, getGamepadButtonState
	, getGamepadAxisState
	) where

import Control.Concurrent.STM
import Data.Array.MArray

import Flaw.Input

-- | Gamepad state.
data GamepadState = GamepadState
	{ gamepadStateButtons :: {-# UNPACK #-} !(TArray GamepadButton Bool)
	, gamepadStateAxes :: {-# UNPACK #-} !(TArray GamepadAxis Float)
	}

-- | Gamepad event.
data GamepadEvent
	= GamepadButtonDownEvent !GamepadButton
	| GamepadButtonUpEvent !GamepadButton
	| GamepadAxisEvent !GamepadAxis {-# UNPACK #-} !Float
	deriving Show

-- | Gamepad button.
data GamepadButton
	= GamepadButtonUnknown
	| GamepadButtonA
	| GamepadButtonB
	| GamepadButtonX
	| GamepadButtonY
	| GamepadButtonBack
	| GamepadButtonGuide
	| GamepadButtonStart
	| GamepadButtonLeftStick
	| GamepadButtonRightStick
	| GamepadButtonLeftShoulder
	| GamepadButtonRightShoulder
	| GamepadButtonDPadUp
	| GamepadButtonDPadDown
	| GamepadButtonDPadLeft
	| GamepadButtonDPadRight
	deriving (Eq, Ord, Ix, Bounded, Enum, Show)

-- | Gamepad axis.
data GamepadAxis
	= GamepadAxisUnknown
	| GamepadAxisLeftX
	| GamepadAxisLeftY
	| GamepadAxisRightX
	| GamepadAxisRightY
	| GamepadAxisTriggerLeft
	| GamepadAxisTriggerRight
	deriving (Eq, Ord, Ix, Bounded, Enum, Show)

instance InputState GamepadState where
	initialInputState = do
		buttonsArray <- newArray (minBound, maxBound) False
		axesArray <- newArray (minBound, maxBound) 0
		return GamepadState
			{ gamepadStateButtons = buttonsArray
			, gamepadStateAxes = axesArray
			}

instance InputDevice GamepadState GamepadEvent where
	applyInputEvent GamepadState
		{ gamepadStateButtons = buttonsArray
		, gamepadStateAxes = axesArray
		} event = case event of
		GamepadButtonDownEvent button -> writeArray buttonsArray button True
		GamepadButtonUpEvent button -> writeArray buttonsArray button False
		GamepadAxisEvent axis value -> writeArray axesArray axis value

getGamepadButtonState :: GamepadState -> GamepadButton -> STM Bool
getGamepadButtonState GamepadState
	{ gamepadStateButtons = buttonsArray
	} = readArray buttonsArray

getGamepadAxisState :: GamepadState -> GamepadAxis -> STM Float
getGamepadAxisState GamepadState
	{ gamepadStateAxes = axesArray
	} = readArray axesArray

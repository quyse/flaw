{-|
Module: Flaw.Input.Keyboard
Description: Keyboard user input.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Flaw.Input.Keyboard
	( KeyboardState(..)
	, KeyboardEvent(..)
	, Key(..)
	, getKeyState
	) where

import Control.Concurrent.STM
import Data.Array.MArray

import Flaw.Input

data KeyboardState = KeyboardState
	{ keyboardStateKeys :: TArray Key Bool
	}

data KeyboardEvent
	= KeyDownEvent !Key
	| KeyUpEvent !Key
	| CharEvent !Char
	deriving Show

data Key
	= KeyUnknown
	| KeyBackSpace
	| KeyTab
	| KeyLineFeed
	| KeyClear
	| KeyReturn
	| KeyPause
	| KeyScrollLock
	| KeySysReq
	| KeyEscape
	| KeyInsert
	| KeyDelete
	| KeyHome
	| KeyLeft
	| KeyUp
	| KeyRight
	| KeyDown
	| KeyPageUp
	| KeyPageDown
	| KeyEnd
	| KeyBegin
	| KeyNumLock
	| KeyPadSpace
	| KeyPadTab
	| KeyPadEnter
	| KeyPadF1
	| KeyPadF2
	| KeyPadF3
	| KeyPadF4
	| KeyPadHome
	| KeyPadLeft
	| KeyPadUp
	| KeyPadRight
	| KeyPadDown
	| KeyPadPageUp
	| KeyPadPageDown
	| KeyPadEnd
	| KeyPadBegin
	| KeyPadInsert
	| KeyPadDelete
	| KeyPadEqual
	| KeyPadMultiply
	| KeyPadAdd
	| KeyPadSeparator
	| KeyPadSubtract
	| KeyPadDecimal
	| KeyPadDivide
	| KeyPad0
	| KeyPad1
	| KeyPad2
	| KeyPad3
	| KeyPad4
	| KeyPad5
	| KeyPad6
	| KeyPad7
	| KeyPad8
	| KeyPad9
	| KeyF1
	| KeyF2
	| KeyF3
	| KeyF4
	| KeyF5
	| KeyF6
	| KeyF7
	| KeyF8
	| KeyF9
	| KeyF10
	| KeyF11
	| KeyF12
	| KeyShiftL
	| KeyShiftR
	| KeyControlL
	| KeyControlR
	| KeyCapsLock
	| KeyShiftLock
	| KeyMetaL
	| KeyMetaR
	| KeyAltL
	| KeyAltR
	| KeySuperL
	| KeySuperR
	| KeyHyperL
	| KeyHyperR
	| KeySpace
	| Key0
	| Key1
	| Key2
	| Key3
	| Key4
	| Key5
	| Key6
	| Key7
	| Key8
	| Key9
	| KeyA
	| KeyB
	| KeyC
	| KeyD
	| KeyE
	| KeyF
	| KeyG
	| KeyH
	| KeyI
	| KeyJ
	| KeyK
	| KeyL
	| KeyM
	| KeyN
	| KeyO
	| KeyP
	| KeyQ
	| KeyR
	| KeyS
	| KeyT
	| KeyU
	| KeyV
	| KeyW
	| KeyX
	| KeyY
	| KeyZ
	deriving (Eq, Ord, Ix, Bounded, Enum, Show)

instance InputState KeyboardState where
	initialInputState = do
		keysArray <- newArray (minBound, maxBound) False
		return KeyboardState
			{ keyboardStateKeys = keysArray
			}

instance InputDevice KeyboardState KeyboardEvent where
	applyInputEvent KeyboardState
		{ keyboardStateKeys = keysArray
		} event = do
		case event of
			KeyDownEvent key -> writeArray keysArray key True
			KeyUpEvent key -> writeArray keysArray key False
			CharEvent _ -> return ()

getKeyState :: KeyboardState -> Key -> STM Bool
getKeyState KeyboardState
	{ keyboardStateKeys = keysArray
	} key = readArray keysArray key

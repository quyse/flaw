{-|
Module: Flaw.Input
Description: User input base definitions.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Input
	( Event(..)
	, KeyboardEvent(..)
	, MouseEvent(..)
	, Key(..)
	, MouseButton(..)
	, State(..)
	, Frame(..)
	, Manager(..)
	) where

import Data.Ix

data Event
	= EventKeyboard !KeyboardEvent
	| EventMouse !MouseEvent
	deriving Show

data KeyboardEvent
	= KeyDownEvent !Key
	| KeyUpEvent !Key
	| CharEvent !Char
	deriving Show

data MouseEvent
	= MouseDownEvent !MouseButton
	| MouseUpEvent !MouseButton
	| RawMouseMoveEvent !Float !Float !Float
	| CursorMoveEvent !Int !Int
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

data MouseButton
	= LeftMouseButton
	| RightMouseButton
	| MiddleMouseButton
	deriving (Eq, Ord, Ix, Bounded, Enum, Show)

-- | Class of input state.
class State a where
	getKeyState :: a -> Key -> IO Bool
	getMouseButtonState :: a -> MouseButton -> IO Bool
	getMouseCursor :: a -> IO (Int, Int)

-- | Class of input frame.
class State a => Frame a where
	nextInputEvent :: a -> IO (Maybe Event)

-- | Class of input manager.
class Manager a where
	type ManagerFrame a :: *
	nextInputFrame :: a -> IO (ManagerFrame a)

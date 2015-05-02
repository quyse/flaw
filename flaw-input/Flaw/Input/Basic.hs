{-|
Module: Flaw.Input.Basic
Description: Helper classes for implementation of input managers.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Flaw.Input.Basic
	( BasicInputManager(..)
	, initBasicInputManager
	) where

import Control.Concurrent.STM

import Flaw.Input
import Flaw.Input.Mouse
import Flaw.Input.Keyboard

data BasicInputManager = BasicInputManager
	{ mKeyboardChan :: TChan KeyboardEvent
	, mMouseChan :: TChan MouseEvent
	}

instance InputManager BasicInputManager KeyboardEvent where
	chanInputEvents BasicInputManager
		{ mKeyboardChan = keyboardChan
		} = dupTChan keyboardChan

instance InputManager BasicInputManager MouseEvent where
	chanInputEvents BasicInputManager
		{ mMouseChan = mouseChan
		} = dupTChan mouseChan

initBasicInputManager :: IO BasicInputManager
initBasicInputManager = do
	keyboardChan <- newBroadcastTChanIO
	mouseChan <- newBroadcastTChanIO
	return BasicInputManager
		{ mKeyboardChan = keyboardChan
		, mMouseChan = mouseChan
		}

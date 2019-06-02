{-|
Module: Flaw.Input
Description: User input base definitions.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Flaw.Input
  ( InputState(..)
  , InputDevice(..)
  , InputManager(..)
  ) where

import Control.Concurrent.STM

class InputState s where
  initialInputState :: STM s

class InputState s => InputDevice s e where
  applyInputEvent :: s -> e -> STM ()

class InputManager m e where
  chanInputEvents :: m -> STM (TChan e)

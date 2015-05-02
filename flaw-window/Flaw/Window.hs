{-|
Module: Flaw.Window
Description: General Window framework.
License: MIT
-}

{-# LANGUAGE GADTs, TypeFamilies, DeriveDataTypeable #-}

module Flaw.Window
	( Window(..)
	, WindowEvent(..)
	) where

import Control.Concurrent.STM
import qualified Data.Text as T

-- | Class of window.
class Window w where
	-- | Set title of the window.
	setWindowTitle :: w -> T.Text -> IO ()
	-- | Get window client size.
	getWindowClientSize :: w -> IO (Int, Int)
	-- | Get chan of window messages.
	chanWindowEvents :: w -> STM (TChan WindowEvent)

data WindowEvent
	= CloseWindowEvent
	| DestroyWindowEvent
	| ResizeWindowEvent Int Int

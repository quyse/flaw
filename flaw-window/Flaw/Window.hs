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

import qualified Data.Text as T

-- | Class of window.
class Window w where
	-- | Set title of the window.
	setWindowTitle :: w -> T.Text -> IO ()
	-- | Get window client size.
	getWindowClientSize :: w -> IO (Int, Int)
	-- | Add window callback.
	addWindowCallback :: w -> (WindowEvent -> IO ()) -> IO ()

data WindowEvent
	= CloseWindowEvent
	| DestroyWindowEvent
	| ResizeWindowEvent Int Int

{-|
Module: Flaw.Window
Description: General Window framework.
License: MIT
-}

{-# LANGUAGE GADTs, TypeFamilies #-}

module Flaw.Window
	( Window(..)
	, WindowEvent(..)
	, MouseCursor(..)
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

	-- Clipboard functions.
	-- | Get clipboard contents as a text.
	getWindowClipboardText :: w -> IO T.Text
	-- | Set text as a content of clipboard.
	setWindowClipboardText :: w -> T.Text -> IO ()

	-- | Set mouse cursor.
	setWindowMouseCursor :: w -> MouseCursor -> IO ()
	-- | Set mouse lock mode.
	setWindowMouseLock :: w -> Bool -> IO ()

data WindowEvent
	-- | User is trying to close window.
	= CloseWindowEvent
	-- | Window is destroyed.
	| DestroyWindowEvent
	-- | Window is resized.
	| ResizeWindowEvent !Int !Int
	-- | Window is activated or deactivated.
	| ActivateWindowEvent !Bool

data MouseCursor
	= MouseCursorArrow
	| MouseCursorWait
	| MouseCursorWaitArrow
	| MouseCursorIBeam
	| MouseCursorSizeNWSE
	| MouseCursorSizeNESW
	| MouseCursorSizeWE
	| MouseCursorSizeNS
	| MouseCursorSizeAll
	| MouseCursorHand
	deriving (Eq, Bounded, Enum)

module Flaw.Window
	( WindowSystem(..)
	, Window(..)
	, WindowHandle()
	, CannotCreateWindowException()
	, setWindowTitle
	) where

import qualified Data.Text as T

import Flaw.Window.Internal

-- | Set window title.
setWindowTitle :: WindowHandle -> T.Text -> IO ()
setWindowTitle (WindowHandle w) title = windowSetTitle w title

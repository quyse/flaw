{-# LANGUAGE GADTs, TypeFamilies, DeriveDataTypeable #-}

module Flaw.Window.Internal
	( WindowSystem(..)
	, Window(..)
	) where

import Control.Exception
import qualified Data.Text as T
import Data.Typeable

-- | Class of window system.
class WindowSystem ws where
	-- | Initialize window system.
	initWindowSystem :: IO ws
	-- | Shutdown window system.
	shutdownWindowSystem :: ws -> IO ()

-- | Class of window.
class Window w where
	-- | Set title of the window.
	setWindowTitle :: w -> T.Text -> IO ()
	-- | Close window.
	closeWindow :: w -> IO ()
	-- | Wait until window is closed.
	waitForWindowClose :: w -> IO ()

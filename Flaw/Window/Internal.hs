{-# LANGUAGE GADTs, DeriveDataTypeable #-}

module Flaw.Window.Internal
	( WindowSystem(..)
	, Window(..)
	, WindowHandle(..)
	, CannotCreateWindowException(..)
	) where

import Control.Exception
import qualified Data.Text as T
import Data.Typeable

data WindowHandle where
	WindowHandle :: Window w => w -> WindowHandle

-- | Class of window system.
class WindowSystem ws where
	-- | Initialize window system.
	initWindowSystem :: IO ws
	-- | Shutdown window system.
	shutdownWindowSystem :: ws -> IO ()
	-- | Create window.
	createWindow :: ws -> T.Text -> Int -> Int -> Int -> Int -> IO WindowHandle

-- | Class of window.
class Window w where
	windowSetTitle :: w -> T.Text -> IO ()

data CannotCreateWindowException = CannotCreateWindowException
	deriving (Typeable, Show)

instance Exception CannotCreateWindowException

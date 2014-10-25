{-# LANGUAGE GADTs, TypeFamilies, DeriveDataTypeable #-}

module Flaw.Window.Internal
	( WindowSystem(..)
	, Window(..)
	, CannotCreateWindowException(..)
	) where

import Control.Exception
import qualified Data.Text as T
import Data.Typeable

-- | Class of window system.
class WindowSystem ws where
	type WindowSystemWindow ws :: *
	-- | Initialize window system.
	initWindowSystem :: IO ws
	-- | Shutdown window system.
	shutdownWindowSystem :: ws -> IO ()
	-- | Create window.
	createWindow :: ws -> T.Text -> Int -> Int -> Int -> Int -> IO (WindowSystemWindow ws)

-- | Class of window.
class Window w where
	-- | Set title of the window.
	setWindowTitle :: w -> T.Text -> IO ()
	-- | Close window.
	closeWindow :: w -> IO ()

data CannotCreateWindowException = CannotCreateWindowException
	deriving (Typeable, Show)

instance Exception CannotCreateWindowException

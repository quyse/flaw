{-# LANGUAGE GADTs, TypeFamilies, DeriveDataTypeable #-}

module Flaw.Window.Internal
	( Window(..)
	) where

import qualified Data.Text as T

-- | Class of window.
class Window w where
	-- | Set title of the window.
	setWindowTitle :: w -> T.Text -> IO ()
	-- | Get window client size.
	getWindowClientSize :: w -> IO (Int, Int)

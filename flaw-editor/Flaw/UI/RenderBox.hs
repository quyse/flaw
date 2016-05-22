{-|
Module: Flaw.UI.RenderBox
Description: Element rendering 3D graphics.
License: MIT
-}

{-# LANGUAGE RankNTypes #-}

module Flaw.UI.RenderBox
	( RenderBox(..)
	, newRenderBox
	) where

import Control.Concurrent.STM

import Flaw.Graphics
import Flaw.Math
import Flaw.UI

-- | UI element displaying 3D graphics.
data RenderBox = RenderBox
	{ renderBoxRenderCallback :: !RenderCallback
	, renderBoxInputCallback :: !InputCallback
	, renderBoxSizeVar :: !(TVar Size)
	, renderBoxFocusedVar :: !(TVar Bool)
	}

type RenderCallback = forall c d. Context c d => Position -> Size -> STM (Render c ())

type InputCallback = InputEvent -> InputState -> STM Bool

newRenderBox :: RenderCallback -> InputCallback -> STM RenderBox
newRenderBox renderCallback inputCallback = do
	sizeVar <- newTVar $ Vec2 0 0
	focusedVar <- newTVar False
	return RenderBox
		{ renderBoxRenderCallback = renderCallback
		, renderBoxInputCallback = inputCallback
		, renderBoxSizeVar = sizeVar
		, renderBoxFocusedVar = focusedVar
		}

instance Element RenderBox where

	layoutElement RenderBox
		{ renderBoxSizeVar = sizeVar
		} size = writeTVar sizeVar size

	dabElement RenderBox
		{ renderBoxSizeVar = sizeVar
		} (Vec2 x y) = do
		if x < 0 || y < 0 then return False
		else do
			Vec2 sx sy <- readTVar sizeVar
			return $ x < sx && y < sy

	renderElement RenderBox
		{ renderBoxRenderCallback = renderCallback
		, renderBoxSizeVar = sizeVar
		} _drawer position = do
		size <- readTVar sizeVar
		renderCallback position size

	processInputEvent RenderBox
		{ renderBoxInputCallback = inputCallback
		} = inputCallback

	focusElement RenderBox
		{ renderBoxFocusedVar = focusedVar
		} = do
		writeTVar focusedVar True
		return True

	unfocusElement RenderBox
		{ renderBoxFocusedVar = focusedVar
		} = writeTVar focusedVar False

instance Focusable RenderBox where
	isFocused RenderBox
		{ renderBoxFocusedVar = focusedVar
		} = readTVar focusedVar

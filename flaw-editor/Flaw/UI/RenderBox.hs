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
	}

type RenderCallback = forall c d. Context c d => Position -> Size -> STM (Render c ())

type InputCallback = InputEvent -> InputState -> STM Bool

newRenderBox :: RenderCallback -> InputCallback -> STM RenderBox
newRenderBox renderCallback inputCallback = do
	sizeVar <- newTVar $ Vec2 0 0
	return RenderBox
		{ renderBoxRenderCallback = renderCallback
		, renderBoxInputCallback = inputCallback
		, renderBoxSizeVar = sizeVar
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

	focusElement _ = return True

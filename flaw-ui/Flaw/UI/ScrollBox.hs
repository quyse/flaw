{-|
Module: Flaw.UI.ScrollBox
Description: Scroll box.
License: MIT
-}

module Flaw.UI.ScrollBox
	( ScrollBox(..)
	, newScrollBox
	) where

import Control.Concurrent.STM

import Flaw.Graphics
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI

data ScrollBox = ScrollBox
	{ scrollBoxElement :: !SomeScrollable
	-- | Position of left-top corner of child element
	-- relative to scroll box' left-top corner, i.e. <= 0.
	, scrollBoxScrollVar :: {-# UNPACK #-} !(TVar Position)
	, scrollBoxSizeVar :: {-# UNPACK #-} !(TVar Size)
	}

newScrollBox :: Scrollable e => e -> STM ScrollBox
newScrollBox element = do
	scrollVar <- newTVar $ Vec2 0 0
	sizeVar <- newTVar $ Vec2 0 0
	return ScrollBox
		{ scrollBoxElement = SomeScrollable element
		, scrollBoxScrollVar = scrollVar
		, scrollBoxSizeVar = sizeVar
		}

instance Element ScrollBox where

	layoutElement ScrollBox
		{ scrollBoxElement = SomeScrollable element
		, scrollBoxSizeVar = sizeVar
		} size = do
		writeTVar sizeVar size
		layoutElement element size

	dabElement ScrollBox
		{ scrollBoxElement = SomeScrollable element
		, scrollBoxScrollVar = scrollVar
		, scrollBoxSizeVar = sizeVar
		} (Vec2 x y) = if x < 0 || y < 0 then return False else do
		Vec2 sx sy <- readTVar sizeVar
		if x < sx && y < sy then do
			Vec2 ox oy <- readTVar scrollVar
			dabElement element $ Vec2 (x - ox) (y - oy)
		else return False

	elementMouseCursor ScrollBox
		{ scrollBoxElement = SomeScrollable element
		} = elementMouseCursor element

	renderElement ScrollBox
		{ scrollBoxElement = SomeScrollable element
		, scrollBoxScrollVar = scrollVar
		, scrollBoxSizeVar = sizeVar
		} drawer position@(Vec2 px py) = do
		scroll@(Vec2 ox oy) <- readTVar scrollVar
		Vec2 sx sy <- readTVar sizeVar
		r <- renderScrollableElement element drawer (position + scroll) (Vec4 (-ox) (-oy) (sx - ox) (sy - oy))
		return $ do
			renderIntersectScissor $ Vec4 px py (px + sx) (py + sy)
			r

	processInputEvent ScrollBox
		{ scrollBoxElement = SomeScrollable element
		, scrollBoxScrollVar = scrollVar
		} inputEvent inputState = case inputEvent of
		MouseInputEvent mouseEvent -> case mouseEvent of
			CursorMoveEvent x y -> do
				Vec2 ox oy <- readTVar scrollVar
				processInputEvent element (MouseInputEvent (CursorMoveEvent (x - ox) (y - oy))) inputState
			_ -> processInputEvent element inputEvent inputState
		_ -> processInputEvent element inputEvent inputState

	focusElement ScrollBox
		{ scrollBoxElement = SomeScrollable element
		} = focusElement element

	unfocusElement ScrollBox
		{ scrollBoxElement = SomeScrollable element
		} = unfocusElement element

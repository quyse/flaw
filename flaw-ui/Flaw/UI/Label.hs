{-|
Module: Flaw.UI.Label
Description: One-line centered text label.
License: MIT
-}

module Flaw.UI.Label
	( Label(..)
	, newLabel
	) where

import Control.Concurrent.STM
import qualified Data.Text as T

import Flaw.Graphics.Font.Render
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer

data Label = Label
	{ labelTextVar :: !(TVar T.Text)
	, labelSizeVar :: !(TVar Size)
	}

newLabel :: STM Label
newLabel = do
	textVar <- newTVar T.empty
	sizeVar <- newTVar $ Vec2 0 0
	return Label
		{ labelTextVar = textVar
		, labelSizeVar = sizeVar
		}

instance Visual Label where
	layoutVisual Label
		{ labelSizeVar = sizeVar
		} size = writeTVar sizeVar size
	renderVisual Label
		{ labelTextVar = textVar
		, labelSizeVar = sizeVar
		} Drawer
		{ drawerGlyphRenderer = glyphRenderer
		, drawerNormalFontShaper = SomeFontShaper normalFontShaper
		, drawerNormalRenderableFont = normalRenderableFont
		} position Style
		{ styleTextColor = textColor
		} = do
		text <- readTVar textVar
		size <- readTVar sizeVar
		return $
			renderGlyphs glyphRenderer normalRenderableFont $
			renderText normalFontShaper text (fmap fromIntegral $ position + fmap (`div` 2) size) RenderTextCursorCenter RenderTextCursorMiddle textColor

instance HasText Label where
	setText Label
		{ labelTextVar = textVar
		} text = writeTVar textVar text

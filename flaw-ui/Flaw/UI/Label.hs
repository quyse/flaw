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

import Flaw.Graphics.Font
import Flaw.Graphics.Font.Render
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer

data Label = Label
	{ labelTextVar :: !(TVar T.Text)
	, labelTextScriptVar :: !(TVar FontScript)
	}

newLabel :: STM Label
newLabel = do
	textVar <- newTVar T.empty
	textScriptVar <- newTVar fontScriptUnknown
	return Label
		{ labelTextVar = textVar
		, labelTextScriptVar = textScriptVar
		}

instance Visual Label where
	renderVisual Label
		{ labelTextVar = textVar
		, labelTextScriptVar = textScriptVar
		} Drawer
		{ drawerGlyphRenderer = glyphRenderer
		, drawerStyles = DrawerStyles
			{ drawerLabelFont = DrawerFont
				{ drawerFontRenderableFont = renderableFont
				, drawerFontShaper = SomeFontShaper fontShaper
				}
			}
		} (Vec2 px py) (Vec2 sx sy) Style
		{ styleTextColor = textColor
		} = do
		text <- readTVar textVar
		textScript <- readTVar textScriptVar
		return $ do
			renderGlyphs glyphRenderer renderableFont $ do
				let position = Vec2
					(fromIntegral $ px + sx `div` 2)
					(fromIntegral $ py + sy `div` 2)
				renderTexts fontShaper [(text, textColor)] textScript position RenderTextCursorCenter RenderTextCursorMiddle

instance HasText Label where
	setText Label
		{ labelTextVar = textVar
		} text = writeTVar textVar text
	setTextScript Label
		{ labelTextScriptVar = textScriptVar
		} textScript = writeTVar textScriptVar textScript

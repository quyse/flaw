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
	, labelAlignmentVar :: !(TVar (AlignX, AlignY))
	}

newLabel :: STM Label
newLabel = do
	textVar <- newTVar T.empty
	textScriptVar <- newTVar fontScriptUnknown
	alignmentVar <- newTVar (AlignLeft, AlignMiddle)
	return Label
		{ labelTextVar = textVar
		, labelTextScriptVar = textScriptVar
		, labelAlignmentVar = alignmentVar
		}

instance Visual Label where
	renderVisual Label
		{ labelTextVar = textVar
		, labelTextScriptVar = textScriptVar
		, labelAlignmentVar = alignmentVar
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
		(alignmentX, alignmentY) <- readTVar alignmentVar
		return $ do
			renderGlyphs glyphRenderer renderableFont $ do
				let (x, cursorX) = case alignmentX of
					AlignLeft -> (px, RenderTextCursorLeft)
					AlignCenter -> (px + sx `div` 2, RenderTextCursorCenter)
					AlignRight -> (px + sx, RenderTextCursorRight)
				let (y, cursorY) = case alignmentY of
					AlignTop -> (py, RenderTextCursorTop)
					AlignMiddle -> (py + sy `div` 2, RenderTextCursorMiddle)
					AlignBottom -> (py + sy, RenderTextCursorBottom)
				renderTexts fontShaper [(text, textColor)] textScript (Vec2 (fromIntegral x) (fromIntegral y)) cursorX cursorY

instance HasText Label where
	setText Label
		{ labelTextVar = textVar
		} text = writeTVar textVar text
	setTextScript Label
		{ labelTextScriptVar = textScriptVar
		} textScript = writeTVar textScriptVar textScript

instance HasAlignment Label where
	setAlignment Label
		{ labelAlignmentVar = alignmentVar
		} alignmentX alignmentY = writeTVar alignmentVar (alignmentX, alignmentY)

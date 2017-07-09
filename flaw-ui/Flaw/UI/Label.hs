{-|
Module: Flaw.UI.Label
Description: One-line centered text label.
License: MIT
-}

module Flaw.UI.Label
	( Label(..)
	, LabelStyle(..)
	, newLabel
	, newTextLabel
	, newTitleLabel
	, renderLabel
	) where

import Control.Concurrent.STM
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Font
import Flaw.Graphics.Font.Render
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer

data Label = Label
	{ labelTextVar :: !(TVar T.Text)
	, labelTextScriptVar :: !(TVar FontScript)
	, labelStyle :: !LabelStyle
	}

data LabelStyle
	= LabelStyleText
	| LabelStyleButton
	| LabelStyleTitle

newLabel :: LabelStyle -> STM Label
newLabel style = do
	textVar <- newTVar T.empty
	textScriptVar <- newTVar fontScriptUnknown
	return Label
		{ labelTextVar = textVar
		, labelTextScriptVar = textScriptVar
		, labelStyle = style
		}

newTextLabel :: STM Label
newTextLabel = newLabel LabelStyleText

newTitleLabel :: STM Label
newTitleLabel = newLabel LabelStyleTitle

instance Visual Label where
	renderVisual Label
		{ labelTextVar = textVar
		, labelTextScriptVar = textScriptVar
		, labelStyle = lstyle
		} drawer position size style = do
		text <- readTVar textVar
		textScript <- readTVar textScriptVar
		return $ renderLabel text textScript lstyle drawer position size style

instance HasText Label where
	setText Label
		{ labelTextVar = textVar
		} = writeTVar textVar
	setTextScript Label
		{ labelTextScriptVar = textScriptVar
		} = writeTVar textScriptVar
	getText Label
		{ labelTextVar = textVar
		} = readTVar textVar

{-# INLINABLE renderLabel #-}
renderLabel :: Context c d => T.Text -> FontScript -> LabelStyle -> Drawer d -> Position -> Size -> Style -> Render c ()
renderLabel text textScript style Drawer
	{ drawerGlyphRenderer = glyphRenderer
	, drawerStyles = styles
	} (Vec2 px py) (Vec2 sx sy) Style
	{ styleTextColor = color
	} = do
	(DrawerFont
		{ drawerFontRenderableFontCache = renderableFontCache
		, drawerFontShaper = SomeFontShaper fontShaper
		}, alignmentX, alignmentY) <- return $ case style of
		LabelStyleText -> (drawerLabelFont styles, AlignLeft, AlignMiddle)
		LabelStyleButton -> (drawerLabelFont styles, AlignCenter, AlignMiddle)
		LabelStyleTitle -> (drawerTitleFont styles, AlignLeft, AlignMiddle)
	let (x, cursorX) = case alignmentX of
		AlignLeft -> (fromIntegral px, RenderTextCursorLeft)
		AlignCenter -> (fromIntegral px + fromIntegral sx * 0.5, RenderTextCursorCenter)
		AlignRight -> (fromIntegral px + fromIntegral sx, RenderTextCursorRight)
	let (y, cursorY) = case alignmentY of
		AlignTop -> (fromIntegral py, RenderTextCursorTop)
		AlignMiddle -> (fromIntegral py + fromIntegral sy * 0.5, RenderTextCursorMiddle)
		AlignBottom -> (fromIntegral py + fromIntegral sy, RenderTextCursorBottom)
	renderGlyphs glyphRenderer renderableFontCache $
		renderTexts fontShaper [(text, color)] textScript (Vec2 x y) cursorX cursorY

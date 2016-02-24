{-|
Module: Flaw.UI.EditBox
Description: One-line edit box.
License: MIT
-}

module Flaw.UI.EditBox
	( EditBox(..)
	, newEditBox
	) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Graphics.Font
import Flaw.Graphics.Font.Render
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer

-- | Edit box.
data EditBox = EditBox
	{ editBoxTextVar :: !(TVar T.Text)
	, editBoxTextScriptVar :: !(TVar FontScript)
	, editBoxPasswordModeVar :: !(TVar Bool)
	-- | Start and end position of selection.
	, editBoxSelectionVar :: !(TVar (Int, Int))
	-- | Scroll offset in pixels. Positive means rendered text shifted to the left.
	, editBoxScrollVar :: !(TVar Float)
	, editBoxSizeVar :: !(TVar Size)
	, editBoxMousedVar :: !(TVar Bool)
	, editBoxFocusedVar :: !(TVar Bool)
	, editBoxBlinkVar :: !(TVar Float)
	-- | Render-delayed commands
	--, editBoxRenderDelayedCommands :: !(TQueue EditBoxRender
	}

newEditBox :: STM EditBox
newEditBox = do
	textVar <- newTVar T.empty
	textScriptVar <- newTVar fontScriptUnknown
	passwordModeVar <- newTVar False
	selectionVar <- newTVar (0, 0)
	scrollVar <- newTVar 0
	sizeVar <- newTVar $ Vec2 0 0
	mousedVar <- newTVar False
	focusedVar <- newTVar False
	blinkVar <- newTVar 0
	return EditBox
		{ editBoxTextVar = textVar
		, editBoxTextScriptVar = textScriptVar
		, editBoxPasswordModeVar = passwordModeVar
		, editBoxSelectionVar = selectionVar
		, editBoxScrollVar = scrollVar
		, editBoxSizeVar = sizeVar
		, editBoxMousedVar = mousedVar
		, editBoxFocusedVar = focusedVar
		, editBoxBlinkVar = blinkVar
		}

instance Element EditBox where

	layoutElement EditBox
		{ editBoxSizeVar = sizeVar
		} size = writeTVar sizeVar size

	dabElement EditBox
		{ editBoxSizeVar = sizeVar
		} (Vec2 x y) = do
		if x < 0 || y < 0 then return False
		else do
			Vec2 sx sy <- readTVar sizeVar
			return $ x < sx && y < sy

	elementMouseCursor _ = return MouseCursorIBeam

	renderElement EditBox
		{ editBoxTextVar = textVar
		, editBoxTextScriptVar = textScriptVar
		, editBoxPasswordModeVar = passwordModeVar
		, editBoxSelectionVar = selectionVar
		, editBoxScrollVar = scrollVar
		, editBoxSizeVar = sizeVar
		, editBoxMousedVar = mousedVar
		, editBoxFocusedVar = focusedVar
		, editBoxBlinkVar = blinkVar
		} Drawer
		{ drawerCanvas = canvas
		, drawerGlyphRenderer = glyphRenderer
		, drawerFrameTimeVar = frameTimeVar
		, drawerStyles = DrawerStyles
			{ drawerEditFont = DrawerFont
				{ drawerFontRenderableFont = renderableFont@RenderableFont
					{ renderableFontMaxGlyphBox = Vec4 _boxLeft boxTop _boxRight boxBottom
					}
				, drawerFontShaper = SomeFontShaper fontShaper
				}
			, drawerLoweredStyleVariant = StyleVariant
				{ styleVariantNormalStyle = normalStyle
				, styleVariantMousedStyle = mousedStyle
				, styleVariantSelectedFocusedStyle = selectedFocusedStyle
				, styleVariantSelectedUnfocusedStyle = selectedUnfocusedStyle
				}
			}
		} (Vec2 px py) = do
		passwordMode <- readTVar passwordModeVar
		text <- liftM (\text -> if passwordMode then T.map (const '●') text else text) $ readTVar textVar
		textScript <- readTVar textScriptVar
		Vec2 sx sy <- readTVar sizeVar
		moused <- readTVar mousedVar
		focused <- readTVar focusedVar
		let style = if moused || focused then mousedStyle else normalStyle

		-- split text according to selection
		(selectionStart, selectionEnd) <- readTVar selectionVar
		let selectionMin = min selectionStart selectionEnd
		let selectionMax = max selectionStart selectionEnd
		let (textBefore, textSelected, textAfter) = splitTextBySelection text selectionMin selectionMax

		let selectedStyle = if focused then selectedFocusedStyle else selectedUnfocusedStyle

		-- update blinking phase (only do calculations if we are focused)
		blink <- do
			if focused then do
				frameTime <- readTVar frameTimeVar
				let blinkPeriod = 1
				oldBlink <- readTVar blinkVar
				let blink = snd (properFraction $ oldBlink + frameTime / blinkPeriod :: (Int, Float))
				writeTVar blinkVar blink
				return blink
			else return 0

		return $ renderScope $ do

			-- draw edit box
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + sx - 1) (px + sx))
				(Vec4 py (py + 1) (py + sy - 1) (py + sy))
				(styleFillColor style) (styleBorderColor style)

			-- constrain further rendering
			renderIntersectScissor $ Vec4 (px + 1) (py + 1) (px + sx - 1) (py + sy - 1)

			-- manually shape glyphs
			runs@[beforeRun, selectedRun, _afterRun] <- liftIO $ shapeText fontShaper [textBefore, textSelected, textAfter] textScript

			-- special offset for cursor and selection relative to text
			let hackyOffsetX = 1
			let selectionMinX = hackyOffsetX + x_ (snd beforeRun)
			let selectionMaxX = hackyOffsetX + x_ (snd selectedRun)
			let cursorX = if selectionStart < selectionEnd then selectionMaxX else selectionMinX

			-- offset from left side
			let textOffsetX = 2

			-- calculate scroll
			scroll <- liftIO $ atomically $ do
				scroll <- readTVar scrollVar
				let border = 3
				-- so this should be true: border < textPreX + cursorX < sx - 2 - border
				-- which means: border < textOffsetX - scroll + cursorX < sx - 2 - border
				-- border - textOffsetX - cursorX < -scroll < sx - textOffsetX - cursorX - border - 2
				-- cursorX + textOffsetX - sx + border + 2 < scroll < cursorX + textOffsetX - border
				let minScroll = cursorX + textOffsetX - fromIntegral sx + border + 2
				let maxScroll = cursorX + textOffsetX - border
				do
					if scroll <= minScroll then do
						writeTVar scrollVar minScroll
						return minScroll
					else if scroll >= maxScroll then do
						let newScroll = max 0 $ maxScroll - fromIntegral sx / 3
						writeTVar scrollVar newScroll
						return newScroll
					else return scroll

			let textXY@(Vec2 textX _textY) = Vec2 (fromIntegral px + 1 + textOffsetX - scroll) (fromIntegral py + 1 + ((fromIntegral $ sy - 2) - boxTop - boxBottom) * 0.5)
			let selectionTop = fromIntegral (py + 1) + (fromIntegral (sy - 2) - (boxBottom - boxTop)) * 0.5
			let selectionBottom = fromIntegral (py + 1) + (fromIntegral (sy - 2) + (boxBottom - boxTop)) * 0.5

			-- draw selection
			when (not $ T.null textSelected) $ do
				drawBorderedRectangle canvas
					(Vec4 (floor $ textX + selectionMinX - 1) (floor $ textX + selectionMinX) (floor $ textX + selectionMaxX + 1) (floor $ textX + selectionMaxX + 2))
					(Vec4 (floor selectionTop) (floor $ selectionTop + 1) (floor $ selectionBottom - 1) (floor selectionBottom))
					(styleFillColor selectedStyle) (styleBorderColor selectedStyle)

			-- draw blinking cursor
			when (blink * 2 < 1) $ do
				drawBorderedRectangle canvas
					(Vec4 (floor $ textX + cursorX) (floor $ textX + cursorX + 1) (floor $ textX + cursorX + 1) (floor $ textX + cursorX + 1))
					(Vec4 (floor selectionTop) (floor $ selectionTop + 1) (floor $ selectionBottom - 1) (floor selectionBottom))
					(styleFillColor selectedStyle) (styleBorderColor selectedStyle)

			-- render glyphs
			renderGlyphs glyphRenderer renderableFont $ do
				forM_ (zip runs [styleTextColor style, styleTextColor selectedStyle, styleTextColor style]) $ \((positionsAndIndices, _advance), color) -> do
					renderTextRun positionsAndIndices textXY color

	processInputEvent EditBox
		{ editBoxTextVar = textVar
		, editBoxSelectionVar = selectionVar
		, editBoxMousedVar = mousedVar
		, editBoxBlinkVar = blinkVar
		} inputEvent InputState
		{ inputStateKeyboard = keyboardState
		, inputStateGetClipboardText = getClipboardText
		, inputStateSetClipboardText = setClipboardText
		} = case inputEvent of
		KeyboardInputEvent keyboardEvent -> case keyboardEvent of
			KeyDownEvent key -> case key of
				KeyBackSpace -> do
					text <- readTVar textVar
					(selectionStart, selectionEnd) <- readTVar selectionVar
					if selectionStart == selectionEnd then do
						let (textBefore, textAfter) = T.splitAt selectionEnd text
						when (T.length textBefore > 0) $ do
							writeTVar textVar $ mappend (T.init textBefore) textAfter
							writeTVar selectionVar (selectionEnd - 1, selectionEnd - 1)
							dontBlink
					else replaceSelection T.empty
					return True
				KeyDelete -> do
					text <- readTVar textVar
					(selectionStart, selectionEnd) <- readTVar selectionVar
					if selectionStart == selectionEnd then do
						let (textBefore, textAfter) = T.splitAt selectionEnd text
						when (T.length textAfter > 0) $ do
							writeTVar textVar $ mappend textBefore (T.tail textAfter)
							dontBlink
					else do
						shiftPressed <- isShiftPressed
						-- Shift+Del - cut to clipboard
						when shiftPressed $ setClipboardText =<< getSelectedText
						replaceSelection T.empty
					return True
				KeyLeft -> do
					(_selectionStart, selectionEnd) <- readTVar selectionVar
					moveCursor $ selectionEnd - 1
					return True
				KeyRight -> do
					(_selectionStart, selectionEnd) <- readTVar selectionVar
					moveCursor $ selectionEnd + 1
					return True
				KeyHome -> do
					moveCursor 0
					return True
				KeyEnd -> do
					text <- readTVar textVar
					moveCursor $ T.length text
					return True
				KeyInsert -> do
					controlPressed <- isControlPressed
					shiftPressed <- isShiftPressed
					if controlPressed then
						if shiftPressed then return False
						else do
							-- Ctrl+Ins - copy to clipboard
							setClipboardText =<< getSelectedText
							return True
					else
						if shiftPressed then do
							-- Shift+Ins - paste from clipboard
							getClipboardText replaceSelection
							return True
						else return False
				KeyA -> do
					controlPressed <- isControlPressed
					if controlPressed then do
						-- select all
						text <- readTVar textVar
						writeTVar selectionVar (0, T.length text)
						dontBlink
						return True
					else return False
				KeyC -> do
					controlPressed <- isControlPressed
					if controlPressed then do
						-- Ctrl+C - copy to clipboard
						setClipboardText =<< getSelectedText
						return True
					else return False
				KeyV -> do
					controlPressed <- isControlPressed
					if controlPressed then do
						-- Ctrl+V - paste from clipboard
						getClipboardText replaceSelection
						return True
					else return False
				KeyX -> do
					controlPressed <- isControlPressed
					if controlPressed then do
						-- Ctrl+X - cut to clipboard
						setClipboardText =<< getSelectedText
						replaceSelection T.empty
						return True
					else return False
				_ -> return False
			CharEvent char -> do
				-- ignore control characters
				if char > '\x1f' then do
					replaceSelection $ T.singleton char
					return True
				else return False
			_ -> return False
		MouseInputEvent mouseEvent -> case mouseEvent of
			CursorMoveEvent _x _y -> do
				writeTVar mousedVar True
				return True
			_ -> return False
		MouseLeaveEvent -> do
			writeTVar mousedVar False
			return True
		where
			replaceSelection replacementText = do
				text <- readTVar textVar
				(selectionStart, selectionEnd) <- readTVar selectionVar
				let (textBefore, _textSelected, textAfter) = splitTextBySelection text selectionStart selectionEnd
				writeTVar textVar $ mconcat [textBefore, replacementText, textAfter]
				let newSelection = min selectionStart selectionEnd + T.length replacementText
				writeTVar selectionVar (newSelection, newSelection)
				dontBlink
			moveCursor position = do
				(selectionStart, _selectionEnd) <- readTVar selectionVar
				text <- readTVar textVar
				let newSelectionEnd = max 0 $ min (T.length text) position
				shiftPressed <- isShiftPressed
				let newSelectionStart = if shiftPressed then selectionStart else newSelectionEnd
				writeTVar selectionVar (newSelectionStart, newSelectionEnd)
				dontBlink
			dontBlink = writeTVar blinkVar 0
			isControlPressed = do
				controlLPressed <- getKeyState keyboardState KeyControlL
				controlRPressed <- getKeyState keyboardState KeyControlR
				return $ controlLPressed || controlRPressed
			isShiftPressed = do
				shiftLPressed <- getKeyState keyboardState KeyShiftL
				shiftRPressed <- getKeyState keyboardState KeyShiftR
				return $ shiftLPressed || shiftRPressed
			getSelectedText = do
				text <- readTVar textVar
				(selectionStart, selectionEnd) <- readTVar selectionVar
				let (_textBefore, textSelected, _textAfter) = splitTextBySelection text selectionStart selectionEnd
				return textSelected

	focusElement EditBox
		{ editBoxFocusedVar = focusedVar
		, editBoxBlinkVar = blinkVar
		} = do
		writeTVar focusedVar True
		-- reset blinking (it's just more pleasant to see cursor immediately)
		writeTVar blinkVar 0
		return True

	unfocusElement EditBox
		{ editBoxFocusedVar = focusedVar
		} = writeTVar focusedVar False

instance HasText EditBox where
	setText EditBox
		{ editBoxTextVar = textVar
		, editBoxSelectionVar = selectionVar
		} text = do
		writeTVar textVar text
		writeTVar selectionVar (0, 0)
	setTextScript EditBox
		{ editBoxTextScriptVar = textScriptVar
		} textScript = writeTVar textScriptVar textScript

instance HasPassword EditBox where
	setPasswordMode EditBox
		{ editBoxPasswordModeVar = passwordModeVar
		} passwordMode = writeTVar passwordModeVar passwordMode

splitTextBySelection :: T.Text -> Int -> Int -> (T.Text, T.Text, T.Text)
splitTextBySelection text start end = (before, selected, after) where
	(before, selectedAndAfter) = T.splitAt (min start end) text
	(selected, after) = T.splitAt (abs $ start - end) selectedAndAfter

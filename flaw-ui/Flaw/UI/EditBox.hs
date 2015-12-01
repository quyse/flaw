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
	-- | Start and end position of selection.
	, editBoxSelectionVar :: !(TVar (Int, Int))
	-- | Scroll offset in pixels. Positive means rendered text shifted to the left.
	, editBoxScrollVar :: !(TVar Float)
	, editBoxSizeVar :: !(TVar Size)
	, editBoxMousedVar :: !(TVar Bool)
	, editBoxFocusedVar :: !(TVar Bool)
	-- | Render-delayed commands
	--, editBoxRenderDelayedCommands :: !(TQueue EditBoxRender
	}

newEditBox :: STM EditBox
newEditBox = do
	textVar <- newTVar T.empty
	textScriptVar <- newTVar fontScriptUnknown
	selectionVar <- newTVar (0, 0)
	scrollVar <- newTVar 0
	sizeVar <- newTVar $ Vec2 0 0
	mousedVar <- newTVar False
	focusedVar <- newTVar False
	return EditBox
		{ editBoxTextVar = textVar
		, editBoxTextScriptVar = textScriptVar
		, editBoxSelectionVar = selectionVar
		, editBoxScrollVar = scrollVar
		, editBoxSizeVar = sizeVar
		, editBoxMousedVar = mousedVar
		, editBoxFocusedVar = focusedVar
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

	renderElement EditBox
		{ editBoxTextVar = textVar
		, editBoxTextScriptVar = textScriptVar
		, editBoxSelectionVar = selectionVar
		, editBoxScrollVar = scrollVar
		, editBoxSizeVar = sizeVar
		, editBoxMousedVar = mousedVar
		, editBoxFocusedVar = focusedVar
		} Drawer
		{ drawerCanvas = canvas
		, drawerGlyphRenderer = glyphRenderer
		, drawerEditFont = DrawerFont
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
		} (Vec2 px py) = do
		text <- readTVar textVar
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

		return $ renderScope $ do
			-- draw edit box
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + sx - 1) (px + sx))
				(Vec4 py (py + 1) (py + sy - 1) (py + sy))
				(styleFillColor style) (styleBorderColor style)

			-- constrain further rendering
			renderViewport $ Vec4 (px + 1) (py + 1) (px + sx - 1) (py + sy - 1)

			-- manually shape glyphs
			(runs@[beforeRun, selectedRun, afterRun], _advance) <- liftIO $ shapeText fontShaper [textBefore, textSelected, textAfter] textScript

			-- calculate some bounds of runs
			let selectionLeftRight = foldrTextBounds renderableFont
				(\(Vec4 left _top right _bottom) (Vec2 minLeft maxRight) -> Vec2 (min left minLeft) (max right maxRight)) (Vec2 1e9 (-1e9)) selectedRun
			let beforeRight = foldrTextBounds renderableFont
				(\(Vec4 _left _top right _bottom) maxRight -> max right maxRight) (-1e9) beforeRun
			let afterLeft = foldrTextBounds renderableFont
				(\(Vec4 left _top _right _bottom) minLeft -> min left minLeft) 1e9 afterRun

			-- get bounds for selection or cursor
			let Vec2 selectionMinX selectionMaxX = if T.null textSelected then
				let selectionCenterX =
					if T.null textBefore then 
						if T.null textAfter then 0 else afterLeft
					else
						if T.null textAfter then beforeRight else (beforeRight + afterLeft) * 0.5
				in Vec2 selectionCenterX selectionCenterX
				else selectionLeftRight

			-- offset from left side
			let textOffsetX = 2

			-- calculate current text offset
			scroll <- liftIO $ atomically $ do
				scroll <- readTVar scrollVar
				-- calculate where is cursor
				let cursorOffsetPreX = if selectionStart < selectionEnd then selectionMaxX else selectionMinX
				let border = 3
				-- so this should be true: border < textPreX + cursorOffsetPreX < sx - 2 - border
				-- which means: border < textOffsetX - scroll + cursorOffsetPreX < sx - 2 - border
				-- border - textOffsetX - cursorOffsetPreX < -scroll < sx - textOffsetX - cursorOffsetPreX - border - 2
				-- cursorOffsetPreX + textOffsetX - sx + border + 2 < scroll < cursorOffsetPreX + textOffsetX - border
				let minScroll = cursorOffsetPreX + textOffsetX - fromIntegral sx + border + 2
				let maxScroll = cursorOffsetPreX + textOffsetX - border
				do
					if scroll <= minScroll then do
						writeTVar scrollVar minScroll
						return minScroll
					else if scroll >= maxScroll then do
						let newScroll = max 0 $ maxScroll - fromIntegral sx / 3
						writeTVar scrollVar newScroll
						return newScroll
					else return scroll

			-- draw selection
			let selectionTop = (fromIntegral (sy - 2) - (boxBottom - boxTop)) * 0.5
			let selectionBottom = (fromIntegral (sy - 2) + (boxBottom - boxTop)) * 0.5
			let textXY@(Vec2 textX _textY) = Vec2 (2 - scroll) (1 + ((fromIntegral $ sy - 2) - boxTop - boxBottom) * 0.5)
			drawBorderedRectangle canvas
				(Vec4 (floor $ textX + selectionMinX - 1) (floor $ textX + selectionMinX) (floor $ textX + selectionMaxX) (floor $ textX + selectionMaxX + 1))
				(Vec4 (floor selectionTop) (floor $ selectionTop + 1) (floor $ selectionBottom - 1) (floor selectionBottom))
				(styleFillColor selectedStyle) (styleBorderColor selectedStyle)

			-- render glyphs
			renderGlyphs glyphRenderer renderableFont $ do
				forM_ (zip runs [styleTextColor style, styleTextColor selectedStyle, styleTextColor style]) $ \(positionsAndIndices, color) -> do
					renderTextRun positionsAndIndices textXY color

	processInputEvent EditBox
		{ editBoxTextVar = textVar
		, editBoxSelectionVar = selectionVar
		, editBoxMousedVar = mousedVar
		} inputEvent InputState
		{ inputStateKeyboard = keyboardState
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
					else replaceSelection T.empty
					return True
				KeyDelete -> do
					text <- readTVar textVar
					(selectionStart, selectionEnd) <- readTVar selectionVar
					if selectionStart == selectionEnd then do
						let (textBefore, textAfter) = T.splitAt selectionEnd text
						when (T.length textAfter > 0) $ do
							writeTVar textVar $ mappend textBefore (T.tail textAfter)
					else replaceSelection T.empty
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
				KeyA -> do
					controlLPressed <- getKeyState keyboardState KeyControlL
					controlRPressed <- getKeyState keyboardState KeyControlR
					if controlLPressed || controlRPressed then do
						-- select all
						text <- readTVar textVar
						writeTVar selectionVar (0, T.length text)
						return True
					else return False
				_ -> return False
			CharEvent char -> do
				replaceSelection $ T.singleton char
				return True
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
			moveCursor position = do
				(selectionStart, _selectionEnd) <- readTVar selectionVar
				text <- readTVar textVar
				let newSelectionEnd = max 0 $ min (T.length text) position
				shiftLPressed <- getKeyState keyboardState KeyShiftL
				shiftRPressed <- getKeyState keyboardState KeyShiftR
				let newSelectionStart = if shiftLPressed || shiftRPressed then selectionStart else newSelectionEnd
				writeTVar selectionVar (newSelectionStart, newSelectionEnd)

	focusElement EditBox
		{ editBoxFocusedVar = focusedVar
		} = do
		writeTVar focusedVar True
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

splitTextBySelection :: T.Text -> Int -> Int -> (T.Text, T.Text, T.Text)
splitTextBySelection text start end = (before, selected, after) where
	(before, selectedAndAfter) = T.splitAt (min start end) text
	(selected, after) = T.splitAt (abs $ start - end) selectedAndAfter

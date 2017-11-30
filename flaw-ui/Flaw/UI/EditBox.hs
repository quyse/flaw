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
import Data.List
import Data.Maybe
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
import Flaw.UI.Metrics

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
	, editBoxLastMousePositionVar :: !(TVar (Maybe Position))
	, editBoxMousePressedVar :: !(TVar Bool)
	, editBoxFocusedVar :: !(TVar Bool)
	, editBoxBlinkVar :: !(TVar Float)
	, editBoxDelayedOpVar :: !(TVar DelayedOp)
	}

-- | Operation delayed to rendering time.
data DelayedOp
	= EmptyDelayedOp
	| SetSelectionEndDelayedOp
	| SetSelectionDelayedOp
	deriving (Eq, Ord)

newEditBox :: STM EditBox
newEditBox = do
	textVar <- newTVar T.empty
	textScriptVar <- newTVar fontScriptUnknown
	passwordModeVar <- newTVar False
	selectionVar <- newTVar (0, 0)
	scrollVar <- newTVar 0
	sizeVar <- newTVar $ Vec2 0 0
	lastMousePositionVar <- newTVar Nothing
	mousePressedVar <- newTVar False
	focusedVar <- newTVar False
	blinkVar <- newTVar 0
	delayedOpVar <- newTVar EmptyDelayedOp
	return EditBox
		{ editBoxTextVar = textVar
		, editBoxTextScriptVar = textScriptVar
		, editBoxPasswordModeVar = passwordModeVar
		, editBoxSelectionVar = selectionVar
		, editBoxScrollVar = scrollVar
		, editBoxSizeVar = sizeVar
		, editBoxLastMousePositionVar = lastMousePositionVar
		, editBoxMousePressedVar = mousePressedVar
		, editBoxFocusedVar = focusedVar
		, editBoxBlinkVar = blinkVar
		, editBoxDelayedOpVar = delayedOpVar
		}

instance Element EditBox where

	layoutElement EditBox
		{ editBoxSizeVar = sizeVar
		} = writeTVar sizeVar

	dabElement EditBox
		{ editBoxSizeVar = sizeVar
		} (Vec2 x y) =
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
		, editBoxLastMousePositionVar = lastMousePositionVar
		, editBoxFocusedVar = focusedVar
		, editBoxBlinkVar = blinkVar
		, editBoxDelayedOpVar = delayedOpVar
		} Drawer
		{ drawerCanvas = canvas
		, drawerGlyphRenderer = glyphRenderer
		, drawerFrameTimeVar = frameTimeVar
		, drawerStyles = DrawerStyles
			{ drawerEditFont = DrawerFont
				{ drawerFontRenderableFontCache = renderableFontCache@RenderableFontCache
					{ renderableFontCacheMaybeFontVar = maybeFontVar
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
		text <- (\text -> if passwordMode then T.map (const '●') text else text) <$> readTVar textVar
		textScript <- readTVar textScriptVar
		Vec2 sx sy <- readTVar sizeVar
		maybeLastMousePosition <- readTVar lastMousePositionVar
		let moused = isJust maybeLastMousePosition
		focused <- readTVar focusedVar
		let style = if moused || focused then mousedStyle else normalStyle

		-- split text according to selection
		(selectionStart, selectionEnd) <- readTVar selectionVar
		let selectionMin = min selectionStart selectionEnd
		let selectionMax = max selectionStart selectionEnd
		let (textBefore, textSelected, textAfter) = splitTextBySelection text selectionMin selectionMax

		let selectedStyle = if focused then selectedFocusedStyle else selectedUnfocusedStyle

		-- update blinking phase (only do calculations if we are focused)
		blink <-
			if focused then do
				frameTime <- readTVar frameTimeVar
				let blinkPeriod = 1
				oldBlink <- readTVar blinkVar
				let blink = snd (properFraction $ oldBlink + frameTime / blinkPeriod :: (Int, Float))
				writeTVar blinkVar blink
				return blink
			else return 0

		return $ do

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
			let textOffsetX = 1

			-- try to get font metrics
			maybeFont <- liftIO $ atomically $ readTMVar maybeFontVar
			let Vec4 _boxLeft boxTop _boxRight boxBottom = case maybeFont of
				Just RenderableFont
					{ renderableFontMaxGlyphBox = maxGlyphBox
					} -> maxGlyphBox
				Nothing -> Vec4 0 0 0 0

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
				if scroll <= minScroll then do
					writeTVar scrollVar minScroll
					return minScroll
				else if scroll >= maxScroll then do
					let newScroll = max 0 $ maxScroll - fromIntegral sx / 3
					writeTVar scrollVar newScroll
					return newScroll
				else return scroll

			let textXY@(Vec2 textX _textY) = Vec2 (fromIntegral px + 1 + textOffsetX - scroll) (fromIntegral py + 1 + (fromIntegral (sy - 2) - boxTop - boxBottom) * 0.5)
			let selectionTop = py + 2
			let selectionBottom = py + sy - 2

			-- function calculating best text split for a given cursor position
			let splitTextByX x = do
				let
					calc m = do
						let (a, b) = T.splitAt m text
						[(_, Vec2 cx _cy), _] <- shapeText fontShaper [a, b] textScript
						return cx
					step l r = if l + 1 >= r then return (l, r) else do
						let m = (l + r) `quot` 2
						cx <- calc m
						if x >= cx then step m r else step l m
					len = T.length text
				(l, r) <- step 0 len
				(minimumBy (\a b -> compare (abs $ x - fst a) (abs $ x - fst b)) <$>) . forM (filter (\i -> i >= 0 && i <= len) [(l - 1)..(r + 1)]) $ \i -> do
					cx <- calc i
					return (cx, i)

			-- get position of floating cursor
			maybeFloatingCursor <- case maybeLastMousePosition of
				Just (Vec2 qx _qy) -> (Just <$>) . liftIO $ splitTextByX $ fromIntegral (px + qx) - textX
				Nothing -> return Nothing

			-- process delayed op
			liftIO $ atomically $ do
				delayedOp <- readTVar delayedOpVar
				case delayedOp of
					EmptyDelayedOp -> return ()
					SetSelectionEndDelayedOp -> do
						case maybeFloatingCursor of
							Just (_, floatingCursor) -> do
								writeTVar selectionVar (selectionStart, floatingCursor)
								writeTVar blinkVar 0 -- reset blink
							Nothing -> return ()
						writeTVar delayedOpVar EmptyDelayedOp
					SetSelectionDelayedOp -> do
						case maybeFloatingCursor of
							Just (_, floatingCursor) -> do
								writeTVar selectionVar (floatingCursor, floatingCursor)
								writeTVar blinkVar 0 -- reset blink
							Nothing -> return ()
						writeTVar delayedOpVar EmptyDelayedOp

			-- draw selection
			unless (T.null textSelected) $ drawBorderedRectangle canvas
				(Vec4 (floor $ textX + selectionMinX - 1) (floor $ textX + selectionMinX) (floor $ textX + selectionMaxX + 1) (floor $ textX + selectionMaxX + 2))
				(Vec4 selectionTop (selectionTop + 1) (selectionBottom - 1) selectionBottom)
				(styleFillColor selectedStyle) (styleBorderColor selectedStyle)

			-- draw blinking cursor
			when (blink * 2 < 1) $ drawBorderedRectangle canvas
				(Vec4 (floor $ textX + cursorX) (floor $ textX + cursorX + 1) (floor $ textX + cursorX + 1) (floor $ textX + cursorX + 1))
				(Vec4 selectionTop (selectionTop + 1) (selectionBottom - 1) selectionBottom)
				(styleFillColor selectedStyle) (styleBorderColor selectedStyle)

			-- draw floating cursor
			case maybeFloatingCursor of
				Just (floatingCursorX, _) -> drawBorderedRectangle canvas
					(Vec4 (floor $ textX + floatingCursorX) (floor $ textX + floatingCursorX + 1) (floor $ textX + floatingCursorX + 1) (floor $ textX + floatingCursorX + 1))
					(Vec4 selectionTop (selectionTop + 1) (selectionBottom - 1) selectionBottom)
					(styleFillColor selectedStyle) (styleFillColor selectedStyle)
				Nothing -> return ()

			-- render glyphs
			renderGlyphs glyphRenderer renderableFontCache $
				forM_ (zip runs [styleTextColor style, styleTextColor selectedStyle, styleTextColor style]) $ \((shapedGlyphs, _advance), color) ->
					renderTextRun shapedGlyphs textXY color

	processInputEvent EditBox
		{ editBoxTextVar = textVar
		, editBoxSelectionVar = selectionVar
		, editBoxLastMousePositionVar = lastMousePositionVar
		, editBoxMousePressedVar = mousePressedVar
		, editBoxBlinkVar = blinkVar
		, editBoxDelayedOpVar = delayedOpVar
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
			CharEvent char ->
				-- ignore control characters
				if char > '\x1f' then do
					replaceSelection $ T.singleton char
					return True
				else return False
			_ -> return False
		MouseInputEvent mouseEvent -> case mouseEvent of
			MouseDownEvent LeftMouseButton -> do
				maybeLastMousePosition <- readTVar lastMousePositionVar
				if isJust maybeLastMousePosition then do
					setDelayedOp SetSelectionDelayedOp
					writeTVar mousePressedVar True
					return True
				else return False
			MouseUpEvent LeftMouseButton -> do
				writeTVar mousePressedVar False
				return True
			CursorMoveEvent x y -> do
				let mousePosition = Vec2 x y
				writeTVar lastMousePositionVar $ Just mousePosition
				mousePressed <- readTVar mousePressedVar
				when mousePressed $ setDelayedOp SetSelectionEndDelayedOp
				return True
			_ -> return False
		MouseLeaveEvent -> do
			writeTVar lastMousePositionVar Nothing
			writeTVar mousePressedVar False
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
			setDelayedOp newOp = do
				oldOp <- readTVar delayedOpVar
				when (newOp > oldOp) $ writeTVar delayedOpVar newOp

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
		} = writeTVar textScriptVar
	getText EditBox
		{ editBoxTextVar = textVar
		} = readTVar textVar

instance HasPassword EditBox where
	setPasswordMode EditBox
		{ editBoxPasswordModeVar = passwordModeVar
		} = writeTVar passwordModeVar

instance HasPreferredSize EditBox where
	preferredSize Metrics
		{ metricsMainWidth = width
		, metricsEditBoxHeight = height
		} _ = Vec2 width height

splitTextBySelection :: T.Text -> Int -> Int -> (T.Text, T.Text, T.Text)
splitTextBySelection text start end = (before, selected, after) where
	(before, selectedAndAfter) = T.splitAt (min start end) text
	(selected, after) = T.splitAt (abs $ start - end) selectedAndAfter

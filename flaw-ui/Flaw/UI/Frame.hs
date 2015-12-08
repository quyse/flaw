{-|
Module: Flaw.UI.Frame
Description: Sub-window with header, may be moved and resized by mouse.
License: MIT
-}

module Flaw.UI.Frame
	( Frame(..)
	, newFrame
	) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Graphics.Font
import Flaw.Graphics.Font.Render
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer
import Flaw.UI.Panel

data Frame = Frame
	{ frameElement :: !SomeElement
	, framePanel :: !Panel
	, frameTextVar :: !(TVar T.Text)
	, frameTextScriptVar :: !(TVar FontScript)
	, frameFocusedVar :: !(TVar Bool)
	, frameFreeChildVar :: !(TVar (Maybe SomeFreeChild))
	}

-- TODO: do something with hardcoded metrics
frameClientLeft :: Int
frameClientLeft = 5
frameClientTop :: Int
frameClientTop = 25
frameClientRight :: Int
frameClientRight = 5
frameClientBottom :: Int
frameClientBottom = 5
frameTopBorder :: Int
frameTopBorder = 5

-- | Create frame.
-- Internally frame uses panel, and places multiple special elements on it.
newFrame :: Element e => e -> STM Frame
newFrame element = do
	-- create panel
	panel@Panel
		{ panelSizeVar = pnlSizeVar
		} <- newPanel

	-- create vars
	textVar <- newTVar T.empty
	textScriptVar <- newTVar fontScriptUnknown
	focusedVar <- newTVar False
	freeChildVar <- newTVar Nothing

	-- create frame
	let frame = Frame
		{ frameElement = SomeElement element
		, framePanel = panel
		, frameTextVar = textVar
		, frameTextScriptVar = textScriptVar
		, frameFocusedVar = focusedVar
		, frameFreeChildVar = freeChildVar
		}

	-- add element to panel
	elementChild <- addFreeChild panel element

	-- create resize elements
	let createFRE (kx, ky, kw, kh) mouseCursor = do
		lastMousePositionVar <- newTVar Nothing
		let processInput inputEvent InputState
			{ inputStateMouse = mouseState
			} = case inputEvent of
			MouseInputEvent mouseEvent -> case mouseEvent of
				MouseDownEvent LeftMouseButton -> do
					(x, y) <- getMouseCursor mouseState
					writeTVar lastMousePositionVar $ Just $ Vec2 x y
					return True
				CursorMoveEvent _x _y -> do
					lastMousePosition <- readTVar lastMousePositionVar
					case lastMousePosition of
						Just (Vec2 lx ly) -> do
							(x, y) <- getMouseCursor mouseState
							let dx = x - lx
							let dy = y - ly
							-- change position if needed
							when (kx /= 0 || ky /= 0) $ do
								maybeSomeFreeChild <- readTVar freeChildVar
								case maybeSomeFreeChild of
									Just (SomeFreeChild freeContainer freeChild) -> placeFreeChildRelatively freeContainer freeChild $ Vec2 (dx * kx) (dy * ky)
									Nothing -> return ()
							-- change size if needed
							when (kw /= 0 || kh /= 0) $ do
								Vec2 sx sy <- readTVar pnlSizeVar
								layoutElement frame $ Vec2 (sx + dx * kw) (sy + dy * kh)
							-- remember new coordinates
							writeTVar lastMousePositionVar $ Just $ Vec2 x y
							return True
						Nothing -> return False
				MouseUpEvent LeftMouseButton -> do
					writeTVar lastMousePositionVar Nothing
					return True
				_ -> return False
			MouseLeaveEvent -> do
				writeTVar lastMousePositionVar Nothing
				return True
			_ -> return False

		sizeVar <- newTVar $ Vec2 0 0
		let fre = FrameResizeElement
			{ freProcessInput = processInput
			, freMouseCursor = mouseCursor
			, freSizeVar = sizeVar
			}
		freeChild <- addFreeChild panel fre
		return (fre, freeChild)

	(freNW, freNWChild) <- createFRE (1, 1, -1, -1) MouseCursorSizeNWSE
	(freW, freWChild) <- createFRE (1, 0, -1, 0) MouseCursorSizeWE
	(freSW, freSWChild) <- createFRE (1, 0, -1, 1) MouseCursorSizeNESW
	(freN, freNChild) <- createFRE (0, 1, 0, -1) MouseCursorSizeNS
	(freM, freMChild) <- createFRE (1, 1, 0, 0) MouseCursorSizeAll
	(freS, freSChild) <- createFRE (0, 0, 0, 1) MouseCursorSizeNS
	(freNE, freNEChild) <- createFRE (0, 1, 1, -1) MouseCursorSizeNESW
	(freE, freEChild) <- createFRE (0, 0, 1, 0) MouseCursorSizeWE
	(freSE, freSEChild) <- createFRE (0, 0, 1, 1) MouseCursorSizeNWSE

	-- set layout function
	setLayoutHandler panel $ \(Vec2 sx sy) -> do
		-- element
		placeFreeChild panel elementChild $ Vec2 frameClientLeft frameClientTop
		layoutElement element $ Vec2 (sx - frameClientLeft - frameClientRight) (sy - frameClientTop - frameClientBottom)
		-- NW
		placeFreeChild panel freNWChild $ Vec2 0 0
		layoutElement freNW $ Vec2 frameClientLeft frameTopBorder
		-- W
		placeFreeChild panel freWChild $ Vec2 0 frameTopBorder
		layoutElement freW $ Vec2 frameClientLeft (sy - frameTopBorder - frameClientBottom)
		-- SW
		placeFreeChild panel freSWChild $ Vec2 0 (sy - frameClientBottom)
		layoutElement freSW $ Vec2 frameClientLeft frameClientBottom
		-- N
		placeFreeChild panel freNChild $ Vec2 frameClientLeft 0
		layoutElement freN $ Vec2 (sx - frameClientLeft - frameClientRight) frameTopBorder
		-- M
		placeFreeChild panel freMChild $ Vec2 frameClientLeft frameTopBorder
		layoutElement freM $ Vec2 (sx - frameClientLeft - frameClientRight) (frameClientTop - frameTopBorder)
		-- S
		placeFreeChild panel freSChild $ Vec2 frameClientLeft (sy - frameClientBottom)
		layoutElement freS $ Vec2 (sx - frameClientLeft - frameClientRight) frameClientBottom
		-- NE
		placeFreeChild panel freNEChild $ Vec2 (sx - frameClientRight) 0
		layoutElement freNE $ Vec2 frameClientRight frameTopBorder
		-- E
		placeFreeChild panel freEChild $ Vec2 (sx - frameClientRight) frameTopBorder
		layoutElement freE $ Vec2 frameClientRight (sy - frameTopBorder - frameClientBottom)
		-- SE
		placeFreeChild panel freSEChild $ Vec2 (sx - frameClientRight) (sy - frameClientBottom)
		layoutElement freSE $ Vec2 frameClientRight frameClientBottom

	return frame

data FrameResizeElement = FrameResizeElement
	{ freProcessInput :: !(InputEvent -> InputState -> STM Bool)
	, freMouseCursor :: !MouseCursor
	, freSizeVar :: !(TVar (Vec2 Int))
	}

instance Element FrameResizeElement where
	layoutElement FrameResizeElement
		{ freSizeVar = sizeVar
		} size = writeTVar sizeVar size

	dabElement FrameResizeElement
		{ freSizeVar = sizeVar
		} (Vec2 x y) = do
		if x < 0 || y < 0 then return False
		else do
			Vec2 sx sy <- readTVar sizeVar
			return $ x < sx && y < sy

	elementMouseCursor FrameResizeElement
		{ freMouseCursor = mouseCursor
		} = return mouseCursor

	renderElement _ _ _ = return $ return ()

	processInputEvent FrameResizeElement
		{ freProcessInput = processInput
		} inputEvent inputState = processInput inputEvent inputState

instance Element Frame where

	layoutElement Frame
		{ framePanel = panel
		} size = layoutElement panel size

	dabElement Frame
		{ framePanel = Panel
			{ panelSizeVar = sizeVar
			}
		} (Vec2 x y) = do
		if x < 0 || y < 0 then return False
		else do
			Vec2 sx sy <- readTVar sizeVar
			return $ x < sx && y < sy

	elementMouseCursor Frame
		{ framePanel = panel
		} = elementMouseCursor panel

	renderElement Frame
		{ framePanel = panel@Panel
			{ panelSizeVar = sizeVar
			}
		, frameTextVar = textVar
		, frameTextScriptVar = textScriptVar
		, frameFocusedVar = focusedVar
		} drawer@Drawer
		{ drawerCanvas = canvas
		, drawerGlyphRenderer = glyphRenderer
		, drawerStyles = DrawerStyles
			{ drawerTitleFont = DrawerFont
				{ drawerFontRenderableFont = renderableFont
				, drawerFontShaper = SomeFontShaper fontShaper
				}
			, drawerFrameOuterNormalStyle = outerNormalStyle
			, drawerFrameOuterFocusedStyle = outerFocusedStyle
			, drawerFrameInnerStyle = innerStyle
			}
		} (Vec2 px py) = do
		text <- readTVar textVar
		textScript <- readTVar textScriptVar
		focused <- readTVar focusedVar
		Vec2 sx sy <- readTVar sizeVar
		let outerStyle = if focused then outerFocusedStyle else outerNormalStyle
		panelRender <- renderElement panel drawer $ Vec2 px py
		return $ renderScope $ do
			-- draw outer frame
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + sx - 1) (px + sx))
				(Vec4 py (py + 1) (py + sy - 1) (py + sy))
				(styleFillColor outerStyle) (styleBorderColor outerStyle)

			-- render text
			renderGlyphs glyphRenderer renderableFont $ do
				renderTexts fontShaper [(text, styleTextColor outerStyle)] textScript
					(Vec2 (fromIntegral $ px + (sx - frameClientLeft - frameClientRight) `div` 2) (fromIntegral $ py + frameTopBorder + (frameClientTop - frameTopBorder) `div` 2))
					RenderTextCursorCenter RenderTextCursorMiddle

			-- draw inner frame
			drawBorderedRectangle canvas
				(Vec4 (px + frameClientLeft - 1) (px + frameClientLeft) (px + sx - frameClientRight) (px + sx - frameClientRight + 1))
				(Vec4 (py + frameClientTop - 1) (py + frameClientTop) (py + sy - frameClientBottom) (py + sy - frameClientBottom + 1))
				(styleFillColor innerStyle) (styleFillColor innerStyle)

			-- render panel
			panelRender

	processInputEvent Frame
		{ framePanel = panel
		} inputEvent inputState = processInputEvent panel inputEvent inputState

	focusElement Frame
		{ framePanel = panel
		, frameFocusedVar = focusedVar
		} = do
		writeTVar focusedVar True
		void $ focusElement panel
		return True

	unfocusElement Frame
		{ framePanel = panel
		, frameFocusedVar = focusedVar
		} = do
		unfocusElement panel
		writeTVar focusedVar False

instance HasText Frame where
	setText Frame
		{ frameTextVar = textVar
		} text = writeTVar textVar text
	setTextScript Frame
		{ frameTextScriptVar = textScriptVar
		} textScript = writeTVar textScriptVar textScript

instance DraggableInFreeContainer Frame where
	setSelfFreeChild Frame
		{ frameFreeChildVar = freeChildVar
		} container freeChild = writeTVar freeChildVar $ Just $ SomeFreeChild container freeChild

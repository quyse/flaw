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
import Data.Maybe
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Graphics.Font
import Flaw.Graphics.Font.Render
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer
import Flaw.UI.Metrics
import Flaw.UI.Panel

data Frame = Frame
	{ frameElement :: !SomeElement
	, framePanel :: !Panel
	, frameTextVar :: !(TVar T.Text)
	, frameTextScriptVar :: !(TVar FontScript)
	, frameFocusedVar :: !(TVar Bool)
	, frameFreeChildVar :: !(TVar (Maybe SomeFreeChild))
	, frameResizableVar :: !(TVar Bool)
	}

-- | Create frame.
-- Internally frame uses panel, and places multiple special elements on it.
newFrame :: Element e => e -> Metrics -> STM Frame
newFrame element Metrics
	{ metricsFrameClient = Vec4 clientLeft clientTop clientRight clientBottom
	, metricsFrameTopBorder = topBorder
	} = do
	-- create panel
	panel@Panel
		{ panelSizeVar = pnlSizeVar
		} <- newPanel True

	-- create vars
	textVar <- newTVar T.empty
	textScriptVar <- newTVar fontScriptUnknown
	focusedVar <- newTVar False
	freeChildVar <- newTVar Nothing
	resizableVar <- newTVar False

	-- create frame
	let frame = Frame
		{ frameElement = SomeElement element
		, framePanel = panel
		, frameTextVar = textVar
		, frameTextScriptVar = textScriptVar
		, frameFocusedVar = focusedVar
		, frameFreeChildVar = freeChildVar
		, frameResizableVar = resizableVar
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
					-- if we don't have free child, don't even remember last mouse position
					maybeSomeFreeChild <- readTVar freeChildVar
					when (isJust maybeSomeFreeChild) $ do
						(x, y) <- getMouseCursor mouseState
						writeTVar lastMousePositionVar $ Just $ Vec2 x y
					return True
				CursorMoveEvent _x _y -> do
					lastMousePosition <- readTVar lastMousePositionVar
					case lastMousePosition of
						Just (Vec2 lx ly) -> do
							maybeSomeFreeChild <- readTVar freeChildVar
							case maybeSomeFreeChild of
								Just (SomeFreeChild freeContainer freeChild) -> do
									-- check that we are resizable (or we don't need to resize anything)
									resizable <- readTVar resizableVar
									let needSizeChange = kw /= 0 || kh /= 0
									when (resizable || not needSizeChange) $ do
										(x, y) <- getMouseCursor mouseState
										let dx = x - lx
										let dy = y - ly
										-- change position if needed
										when (kx /= 0 || ky /= 0) $
											placeFreeChildRelatively freeContainer freeChild $ Vec2 (dx * kx) (dy * ky)
										-- change size if needed
										when needSizeChange $ do
											Vec2 sx sy <- readTVar pnlSizeVar
											layoutElement frame $ Vec2 (sx + dx * kw) (sy + dy * kh)
										-- remember new coordinates
										writeTVar lastMousePositionVar $ Just $ Vec2 x y
								Nothing -> return ()
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
			, freShowCursorExp = if kw == 0 && kh == 0 then isJust <$> readTVar freeChildVar else readTVar resizableVar
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
		placeFreeChild panel elementChild $ Vec2 clientLeft clientTop
		layoutElement element $ Vec2 (sx - clientLeft - clientRight) (sy - clientTop - clientBottom)
		-- NW
		placeFreeChild panel freNWChild $ Vec2 0 0
		layoutElement freNW $ Vec2 clientLeft topBorder
		-- W
		placeFreeChild panel freWChild $ Vec2 0 topBorder
		layoutElement freW $ Vec2 clientLeft (sy - topBorder - clientBottom)
		-- SW
		placeFreeChild panel freSWChild $ Vec2 0 (sy - clientBottom)
		layoutElement freSW $ Vec2 clientLeft clientBottom
		-- N
		placeFreeChild panel freNChild $ Vec2 clientLeft 0
		layoutElement freN $ Vec2 (sx - clientLeft - clientRight) topBorder
		-- M
		placeFreeChild panel freMChild $ Vec2 clientLeft topBorder
		layoutElement freM $ Vec2 (sx - clientLeft - clientRight) (clientTop - topBorder)
		-- S
		placeFreeChild panel freSChild $ Vec2 clientLeft (sy - clientBottom)
		layoutElement freS $ Vec2 (sx - clientLeft - clientRight) clientBottom
		-- NE
		placeFreeChild panel freNEChild $ Vec2 (sx - clientRight) 0
		layoutElement freNE $ Vec2 clientRight topBorder
		-- E
		placeFreeChild panel freEChild $ Vec2 (sx - clientRight) topBorder
		layoutElement freE $ Vec2 clientRight (sy - topBorder - clientBottom)
		-- SE
		placeFreeChild panel freSEChild $ Vec2 (sx - clientRight) (sy - clientBottom)
		layoutElement freSE $ Vec2 clientRight clientBottom

	return frame

data FrameResizeElement = FrameResizeElement
	{ freProcessInput :: !(InputEvent -> InputState -> STM Bool)
	, freMouseCursor :: !MouseCursor
	, freSizeVar :: !(TVar Int2)
	, freShowCursorExp :: !(STM Bool)
	}

instance Element FrameResizeElement where
	layoutElement FrameResizeElement
		{ freSizeVar = sizeVar
		} = writeTVar sizeVar

	dabElement FrameResizeElement
		{ freSizeVar = sizeVar
		} (Vec2 x y) =
		if x < 0 || y < 0 then return False
		else do
			Vec2 sx sy <- readTVar sizeVar
			return $ x < sx && y < sy

	elementMouseCursor FrameResizeElement
		{ freMouseCursor = mouseCursor
		, freShowCursorExp = showCursorExp
		} = do
		showCursor <- showCursorExp
		return $ if showCursor then mouseCursor else MouseCursorArrow

	renderElement _ _ _ = return $ return ()

	processInputEvent FrameResizeElement
		{ freProcessInput = processInput
		} = processInput

instance Element Frame where

	layoutElement Frame
		{ framePanel = panel
		} = layoutElement panel

	dabElement Frame
		{ framePanel = Panel
			{ panelSizeVar = sizeVar
			}
		} (Vec2 x y) =
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
			{ drawerMetrics = Metrics
				{ metricsFrameClient = Vec4 clientLeft clientTop clientRight clientBottom
				, metricsFrameTopBorder = topBorder
				}
			, drawerTitleFont = DrawerFont
				{ drawerFontRenderableFontCache = renderableFontCache
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
		return $ do
			-- draw outer frame
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + sx - 1) (px + sx))
				(Vec4 py (py + 1) (py + sy - 1) (py + sy))
				(styleFillColor outerStyle) (styleBorderColor outerStyle)

			-- render text
			renderGlyphs glyphRenderer renderableFontCache $
				renderTexts fontShaper [(text, styleTextColor outerStyle)] textScript
					(Vec2 (fromIntegral $ px + (sx - clientLeft - clientRight) `quot` 2) (fromIntegral $ py + topBorder + (clientTop - topBorder) `quot` 2))
					RenderTextCursorCenter RenderTextCursorMiddle

			-- draw inner frame
			drawBorderedRectangle canvas
				(Vec4 (px + clientLeft - 1) (px + clientLeft) (px + sx - clientRight) (px + sx - clientRight + 1))
				(Vec4 (py + clientTop - 1) (py + clientTop) (py + sy - clientBottom) (py + sy - clientBottom + 1))
				(styleFillColor innerStyle) (styleFillColor innerStyle)

			-- render panel
			renderScope panelRender

	processInputEvent Frame
		{ framePanel = panel
		} = processInputEvent panel

	focusElement Frame
		{ framePanel = panel
		, frameFocusedVar = focusedVar
		, frameFreeChildVar = freeChildVar
		} = do
		writeTVar focusedVar True
		void $ focusElement panel
		-- try to bring frame to top
		maybeSomeFreeChild <- readTVar freeChildVar
		case maybeSomeFreeChild of
			Just (SomeFreeChild container freeChild) -> bringFreeChildOnTop container freeChild
			Nothing -> return ()
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
		} = writeTVar textVar
	setTextScript Frame
		{ frameTextScriptVar = textScriptVar
		} = writeTVar textScriptVar
	getText Frame
		{ frameTextVar = textVar
		} = readTVar textVar

instance DraggableInFreeContainer Frame where
	setSelfFreeChild Frame
		{ frameFreeChildVar = freeChildVar
		, frameResizableVar = resizableVar
		} container freeChild resizable = do
		writeTVar freeChildVar $ Just $ SomeFreeChild container freeChild
		writeTVar resizableVar resizable

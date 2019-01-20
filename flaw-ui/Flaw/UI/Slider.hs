{-|
Module: Flaw.UI.Slider
Description: Slider is an element with value.
License: MIT
-}

module Flaw.UI.Slider
	( Slider(..)
	, newSlider
	) where

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

import Flaw.Graphics.Canvas
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer
import Flaw.UI.Metrics

data Slider = Slider
	{ sliderValueVar :: !(TVar Float)
	, sliderSizeVar :: !(TVar Size)
	, sliderFocusedVar :: !(TVar Bool)
	, sliderLastMousePositionVar :: !(TVar (Maybe Position))
	, sliderFirstPressedPositionValueVar :: !(TVar (Maybe (Position, Float)))
	, sliderChangeHandlerVar :: !(TVar (STM ()))
	, sliderPieceWidth :: {-# UNPACK #-} !Metric
	, sliderValueStep :: {-# UNPACK #-} !Float
	}

newSlider :: Metrics -> Float -> STM Slider
newSlider Metrics
	{ metricsSliderPieceWidth = pieceWidth
	} valueStep = do
	valueVar <- newTVar 0
	sizeVar <- newTVar $ Vec2 0 0
	focusedVar <- newTVar False
	lastMousePositionVar <- newTVar Nothing
	firstPressedPositionValueVar <- newTVar Nothing
	changeHandlerVar <- newTVar $ return ()
	return Slider
		{ sliderValueVar = valueVar
		, sliderSizeVar = sizeVar
		, sliderFocusedVar = focusedVar
		, sliderLastMousePositionVar = lastMousePositionVar
		, sliderFirstPressedPositionValueVar = firstPressedPositionValueVar
		, sliderChangeHandlerVar = changeHandlerVar
		, sliderPieceWidth = pieceWidth
		, sliderValueStep = valueStep
		}

instance Element Slider where

	layoutElement Slider
		{ sliderSizeVar = sizeVar
		} = writeTVar sizeVar

	dabElement Slider
		{ sliderSizeVar = sizeVar
		} (Vec2 x y) =
		if x < 0 || y < 0 then return False
		else do
			size <- readTVar sizeVar
			let Vec2 sx sy = size
			return $ x < sx && y < sy

	renderElement Slider
		{ sliderValueVar = valueVar
		, sliderSizeVar = sizeVar
		, sliderFocusedVar = focusedVar
		, sliderLastMousePositionVar = lastMousePositionVar
		, sliderFirstPressedPositionValueVar = firstPressedPositionValueVar
		, sliderPieceWidth = pieceWidth
		} Drawer
		{ drawerCanvas = canvas
		, drawerStyles = DrawerStyles
			{ drawerLoweredStyleVariant = loweredStyleVariant
			, drawerRaisedStyleVariant = raisedStyleVariant
			}
		} (Vec2 px py) = do
		-- get state
		value <- readTVar valueVar
		size <- readTVar sizeVar
		let Vec2 sx sy = size
		focused <- readTVar focusedVar
		moused <- isJust <$> readTVar lastMousePositionVar
		pressed <- isJust <$> readTVar firstPressedPositionValueVar
		-- get style
		let styleVariant
			| pressed = styleVariantPressedStyle
			| moused || focused = styleVariantMousedStyle
			| otherwise = styleVariantNormalStyle
		let loweredStyle = styleVariant loweredStyleVariant
		let raisedStyle = styleVariant raisedStyleVariant
		-- return rendering
		return $ do
			let my = py + sy `quot` 2
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + sx - 1) (px + sx))
				(Vec4 (my - 2) (my - 1) (my + 1) (my + 2))
				(styleFillColor loweredStyle) (styleBorderColor loweredStyle)
			let x = px + floor (value * fromIntegral (sx - pieceWidth))
			drawBorderedRectangle canvas
				(Vec4 x (x + 1) (x + pieceWidth - 1) (x + pieceWidth))
				(Vec4 py (py + 1) (py + sy - 1) (py + sy))
				(styleFillColor raisedStyle) (styleBorderColor raisedStyle)

	processInputEvent Slider
		{ sliderValueVar = valueVar
		, sliderSizeVar = sizeVar
		, sliderLastMousePositionVar = lastMousePositionVar
		, sliderFirstPressedPositionValueVar = firstPressedPositionValueVar
		, sliderChangeHandlerVar = changeHandlerVar
		, sliderPieceWidth = pieceWidth
		, sliderValueStep = valueStep
		} inputEvent _inputState = case inputEvent of
		KeyboardInputEvent keyboardEvent -> case keyboardEvent of
			KeyDownEvent KeyLeft -> do
				value <- readTVar valueVar
				changeValue $ max 0 $ value - valueStep
				return True
			KeyDownEvent KeyRight -> do
				value <- readTVar valueVar
				changeValue $ min 1 $ value + valueStep
				return True
			_ -> return False
		MouseInputEvent mouseEvent -> case mouseEvent of
			MouseDownEvent LeftMouseButton -> do
				maybeLastMousePosition <- readTVar lastMousePositionVar
				writeTVar firstPressedPositionValueVar =<< case maybeLastMousePosition of
					Just lastMousePosition@(Vec2 x _y) -> do
						firstValue <- readTVar valueVar
						-- check if click hit slider
						size <- readTVar sizeVar
						let Vec2 sx _sy = size
						let l = floor $ firstValue * fromIntegral (sx - pieceWidth)
						if x >= l && x < l + pieceWidth then return $ Just (lastMousePosition, firstValue)
						else do
							let newValue = max 0 $ min 1 $ fromIntegral (x - pieceWidth `quot` 2) / fromIntegral (sx - pieceWidth)
							changeValue newValue
							return $ Just (lastMousePosition, newValue)
					Nothing -> return Nothing
				return True
			MouseUpEvent LeftMouseButton -> do
				writeTVar firstPressedPositionValueVar Nothing
				return True
			CursorMoveEvent x y -> do
				maybeFirstPressedPositionValue <- readTVar firstPressedPositionValueVar
				case maybeFirstPressedPositionValue of
					Just (Vec2 fx _fy, firstValue) -> do
						size <- readTVar sizeVar
						let Vec2 sx _sy = size
						changeValue $ max 0 $ min 1 $ firstValue + fromIntegral (x - fx) / fromIntegral (sx - pieceWidth)
					Nothing -> return ()
				writeTVar lastMousePositionVar $ Just $ Vec2 x y
				return True
			_ -> return False
		MouseLeaveEvent -> do
			writeTVar lastMousePositionVar Nothing
			writeTVar firstPressedPositionValueVar Nothing
			return True
		where
		changeValue newValue = do
			writeTVar valueVar newValue
			join $ readTVar changeHandlerVar

	focusElement Slider
		{ sliderFocusedVar = focusedVar
		} = do
		writeTVar focusedVar True
		return True

	unfocusElement Slider
		{ sliderFocusedVar = focusedVar
		} = writeTVar focusedVar False

instance HasFloatValue Slider where
	setFloatValue Slider
		{ sliderValueVar = valueVar
		} = writeTVar valueVar
	getFloatValue Slider
		{ sliderValueVar = valueVar
		}	= readTVar valueVar

instance HasChangeHandler Slider where
	setChangeHandler Slider
		{ sliderChangeHandlerVar = changeHandlerVar
		} = writeTVar changeHandlerVar

instance HasPreferredSize Slider where
	preferredSize Metrics
		{ metricsMainWidth = width
		, metricsSliderHeight = height
		} _ = Vec2 width height

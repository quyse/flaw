{-|
Module: Flaw.UI.CheckBox
Description: Check box.
License: MIT
-}

module Flaw.UI.CheckBox
	( CheckBox(..)
	, newCheckBox
	, newLabeledCheckBox
	) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer
import Flaw.UI.Label
import Flaw.UI.Metrics

data CheckBox = CheckBox
	{ checkBoxVisual :: !SomeVisual
	, checkBoxSizeVar :: !(TVar Size)
	, checkBoxCheckedVar :: !(TVar Bool)
	, checkBoxMousedVar :: !(TVar Bool)
	, checkBoxFocusedVar :: !(TVar Bool)
	, checkBoxChangeHandlerVar :: !(TVar (STM ()))
	}

newCheckBox :: Visual v => v -> STM CheckBox
newCheckBox visual = do
	sizeVar <- newTVar $ Vec2 0 0
	checkedVar <- newTVar False
	mousedVar <- newTVar False
	focusedVar <- newTVar False
	changeHandlerVar <- newTVar $ return ()
	return CheckBox
		{ checkBoxVisual = SomeVisual visual
		, checkBoxSizeVar = sizeVar
		, checkBoxCheckedVar = checkedVar
		, checkBoxMousedVar = mousedVar
		, checkBoxFocusedVar = focusedVar
		, checkBoxChangeHandlerVar = changeHandlerVar
		}

newLabeledCheckBox :: T.Text -> STM CheckBox
newLabeledCheckBox text = do
	label <- newLabel LabelStyleText
	setText label text
	newCheckBox label

instance Element CheckBox where

	layoutElement CheckBox
		{ checkBoxSizeVar = sizeVar
		} = writeTVar sizeVar

	dabElement CheckBox
		{ checkBoxSizeVar = sizeVar
		} (Vec2 x y) =
		if x < 0 || y < 0 then return False
		else do
			Vec2 sx sy <- readTVar sizeVar
			return $ x < sx && y < sy

	renderElement CheckBox
		{ checkBoxVisual = SomeVisual visual
		, checkBoxSizeVar = sizeVar
		, checkBoxCheckedVar = checkedVar
		, checkBoxMousedVar = mousedVar
		, checkBoxFocusedVar = focusedVar
		} drawer@Drawer
		{ drawerCanvas = canvas
		, drawerStyles = DrawerStyles
			{ drawerLoweredStyleVariant = loweredStyleVariant
			, drawerRaisedStyleVariant = raisedStyleVariant
			}
		} (Vec2 px py) = do
		Vec2 sx sy <- readTVar sizeVar
		checked <- readTVar checkedVar
		moused <- readTVar mousedVar
		focused <- readTVar focusedVar
		let loweredStyle = (if moused || focused then styleVariantMousedStyle else styleVariantNormalStyle) loweredStyleVariant
		let raisedStyle = (if moused then styleVariantMousedStyle else styleVariantNormalStyle) raisedStyleVariant
		let s = min sx sy
		let gap = s `quot` 3
		visualRender <- renderVisual visual drawer (Vec2 (px + s + gap) py) (Vec2 (sx - s - gap) sy) loweredStyle
		return $ do
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + s - 1) (px + s))
				(Vec4 py (py + 1) (py + s - 1) (py + s))
				(styleFillColor loweredStyle) (styleBorderColor loweredStyle)
			when checked $ drawBorderedRectangle canvas
				(Vec4 (px + gap) (px + gap + 1) (px + s - gap - 1) (px + s - gap))
				(Vec4 (py + gap) (py + gap + 1) (py + s - gap - 1) (py + s - gap))
				(styleTextColor raisedStyle) (styleBorderColor raisedStyle)
			renderIntersectScissor $ Vec4 (px + s + gap) py (px + sx) (py + sy)
			visualRender

	processInputEvent CheckBox
		{ checkBoxCheckedVar = checkedVar
		, checkBoxMousedVar = mousedVar
		, checkBoxChangeHandlerVar = changeHandlerVar
		} inputEvent _inputState = case inputEvent of
		KeyboardInputEvent keyboardEvent -> case keyboardEvent of
			KeyDownEvent KeyReturn -> toggle
			KeyDownEvent KeySpace -> toggle
			_ -> return False
		MouseInputEvent (MouseDownEvent LeftMouseButton) -> toggle
		MouseInputEvent CursorMoveEvent {} -> do
			writeTVar mousedVar True
			return True
		MouseLeaveEvent -> do
			writeTVar mousedVar False
			return True
		_ -> return False
		where toggle = do
			modifyTVar' checkedVar not
			join $ readTVar changeHandlerVar
			return True

	focusElement CheckBox
		{ checkBoxFocusedVar = focusedVar
		} = do
		writeTVar focusedVar True
		return True

	unfocusElement CheckBox
		{ checkBoxFocusedVar = focusedVar
		} = writeTVar focusedVar False

instance HasChecked CheckBox where
	setChecked CheckBox
		{ checkBoxCheckedVar = checkedVar
		} = writeTVar checkedVar
	getChecked CheckBox
		{ checkBoxCheckedVar = checkedVar
		} = readTVar checkedVar

instance HasChangeHandler CheckBox where
	setChangeHandler CheckBox
		{ checkBoxChangeHandlerVar = changeHandlerVar
		} = writeTVar changeHandlerVar

instance HasPreferredSize CheckBox where
	preferredSize Metrics
		{ metricsLabelSize = Vec2 _sx sy
		} _ = Vec2 sy sy

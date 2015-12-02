{-|
Module: Flaw.UI.Button
Description: Button.
License: MIT
-}

module Flaw.UI.Button
	( Button(..)
	, newButton
	) where

import Control.Concurrent.STM
import Control.Monad

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer

data Button = Button
	{ buttonVisual :: !SomeVisual
	, buttonSizeVar :: !(TVar Size)
	, buttonFocusedVar :: !(TVar Bool)
	, buttonMousedVar :: !(TVar Bool)
	, buttonPressedVar :: !(TVar Bool)
	, buttonClickHandlerVar :: !(TVar (STM ()))
	}

newButton :: Visual v => v -> STM Button
newButton visual = do
	sizeVar <- newTVar $ Vec2 0 0
	focusedVar <- newTVar False
	mousedVar <- newTVar False
	pressedVar <- newTVar False
	clickHandlerVar <- newTVar $ return ()
	return Button
		{ buttonVisual = SomeVisual visual
		, buttonSizeVar = sizeVar
		, buttonFocusedVar = focusedVar
		, buttonMousedVar = mousedVar
		, buttonPressedVar = pressedVar
		, buttonClickHandlerVar = clickHandlerVar
		}

instance Element Button where

	layoutElement Button
		{ buttonSizeVar = sizeVar
		} size = writeTVar sizeVar size

	dabElement Button
		{ buttonSizeVar = sizeVar
		} (Vec2 x y) = do
		if x < 0 || y < 0 then return False else do
			Vec2 sx sy <- readTVar sizeVar
			return $ x < sx && y < sy

	renderElement Button
		{ buttonVisual = SomeVisual visual
		, buttonSizeVar = sizeVar
		, buttonFocusedVar = focusedVar
		, buttonMousedVar = mousedVar
		, buttonPressedVar = pressedVar
		} drawer@Drawer
		{ drawerCanvas = canvas
		, drawerStyles = DrawerStyles
			{ drawerRaisedStyleVariant = StyleVariant
				{ styleVariantNormalStyle = normalStyle
				, styleVariantMousedStyle = mousedStyle
				, styleVariantPressedStyle = pressedStyle
				}
			}
		} (Vec2 px py) = do
		-- get state
		size@(Vec2 sx sy) <- readTVar sizeVar
		focused <- readTVar focusedVar
		moused <- readTVar mousedVar
		pressed <- readTVar pressedVar
		-- get style
		let style = if pressed then pressedStyle else if moused || focused then mousedStyle else normalStyle
		-- calculate visual rendering
		visualRender <- renderVisual visual drawer (Vec2 0 0) size style
		-- return rendering monad
		return $ renderScope $ do
			drawBorderedRectangle canvas
				(Vec4 px (px + 1) (px + sx - 1) (px + sx))
				(Vec4 py (py + 1) (py + sy - 1) (py + sy))
				(styleFillColor style) (styleBorderColor style)
			renderViewport $ Vec4 (px + 1) (py + 1) (px + sx - 2) (py + sy - 2)
			visualRender
			when focused $ drawBorderedRectangle canvas
				(Vec4 2 3 (sx - 5) (sx - 4))
				(Vec4 2 3 (sy - 5) (sy - 4))
				(Vec4 0 0 0 0) (Vec4 1 1 1 0.5)

	processInputEvent Button
		{ buttonMousedVar = mousedVar
		, buttonPressedVar = pressedVar
		, buttonClickHandlerVar = clickHandlerVar
		} inputEvent _inputState = case inputEvent of
		KeyboardInputEvent keyboardEvent -> case keyboardEvent of
			KeyDownEvent key -> do
				if isPressKey key then do
					writeTVar pressedVar True
					return True
				else return False
			KeyUpEvent key -> do
				if isPressKey key then do
					pressed <- readTVar pressedVar
					when pressed $ do
						click
						writeTVar pressedVar False
					return True
				else return False
			_ -> return False
		MouseInputEvent mouseEvent -> case mouseEvent of
			MouseDownEvent LeftMouseButton -> do
				writeTVar pressedVar True
				return True
			MouseUpEvent LeftMouseButton -> do
				pressed <- readTVar pressedVar
				when pressed $ do
					click
					writeTVar pressedVar False
				return True
			CursorMoveEvent _x _y -> do
				writeTVar mousedVar True
				return True
			_ -> return False
		MouseLeaveEvent -> do
			writeTVar mousedVar False
			writeTVar pressedVar False
			return True
		where
			click = join $ readTVar clickHandlerVar
			isPressKey key = case key of
				KeyReturn -> True
				KeySpace -> True
				_ -> False

	focusElement Button
		{ buttonFocusedVar = focusedVar
		} = do
		writeTVar focusedVar True
		return True

	unfocusElement Button
		{ buttonFocusedVar = focusedVar
		, buttonPressedVar = pressedVar
		} = do
		writeTVar focusedVar False
		writeTVar pressedVar False

instance HasClickHandler Button where
	setClickHandler Button
		{ buttonClickHandlerVar = clickHandlerVar
		} clickHandler = writeTVar clickHandlerVar clickHandler

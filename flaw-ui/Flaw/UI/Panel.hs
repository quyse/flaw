{-|
Module: Flaw.UI.Panel
Description: Panel is a free container element.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.UI.Panel
	( Panel(..)
	, newPanel
	) where

import Control.Monad
import Control.Concurrent.STM
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer

data Panel = Panel
	{ panelChildrenVar :: !(TVar (S.Set PanelChild))
	, panelChildIndexVar :: !(TVar Int)
	, panelLayoutHandlerVar :: !(TVar (Size -> STM ()))
	, panelSizeVar :: !(TVar Size)
	, panelFocusedChildVar :: !(TVar (Maybe PanelChild))
	, panelLastMouseCursorVar :: !(TVar (Maybe Position))
	, panelLastMousedChildVar :: !(TVar (Maybe PanelChild))
	}

data PanelChild = PanelChild
	{ panelChildIndex :: !Int
	, panelChildElement :: !SomeElement
	, panelChildPositionVar :: !(TVar Position)
	}

newPanel :: STM Panel
newPanel = do
	childrenVar <- newTVar S.empty
	childIndexVar <- newTVar 0
	layoutHandlerVar <- newTVar $ \_ -> return ()
	sizeVar <- newTVar $ Vec2 0 0
	focusedChildVar <- newTVar Nothing
	lastMouseCursorVar <- newTVar Nothing
	lastMousedChildVar <- newTVar Nothing
	return Panel
		{ panelChildrenVar = childrenVar
		, panelChildIndexVar = childIndexVar
		, panelLayoutHandlerVar = layoutHandlerVar
		, panelSizeVar = sizeVar
		, panelFocusedChildVar = focusedChildVar
		, panelLastMouseCursorVar = lastMouseCursorVar
		, panelLastMousedChildVar = lastMousedChildVar
		}

instance Eq PanelChild where
	child1 == child2 = panelChildIndex child1 == panelChildIndex child2

instance Ord PanelChild where
	compare child1 child2 = compare (panelChildIndex child1) (panelChildIndex child2)

instance Element Panel where

	layoutElement Panel
		{ panelLayoutHandlerVar = layoutHandlerVar
		, panelSizeVar = sizeVar
		} size = do
		layoutHandler <- readTVar layoutHandlerVar
		layoutHandler size
		writeTVar sizeVar size

	dabElement Panel
		{ panelChildrenVar = childrenVar
		, panelSizeVar = sizeVar
		} point@(Vec2 px py) = do
		if px < 0 || py < 0 then return False
		else do
			Vec2 sx sy <- readTVar sizeVar
			if px >= sx || py >= sy then return False
			else do
				children <- readTVar childrenVar
				let
					dabChildren (PanelChild
						{ panelChildElement = SomeElement element
						, panelChildPositionVar = childPositionVar
						} : restChildren) = do
						childPosition <- readTVar childPositionVar
						r <- dabElement element $ point - childPosition
						if r then return True else dabChildren restChildren
					dabChildren [] = return False
				dabChildren $ S.toDescList children

	renderElement Panel
		{ panelChildrenVar = childrenVar
		, panelSizeVar = sizeVar
		} drawer@Drawer
		{ drawerCanvas = canvas
		, drawerStyles = DrawerStyles
			{ drawerFlatStyleVariant = StyleVariant
				{ styleVariantNormalStyle = Style
					{ styleFillColor = fillColor
					, styleBorderColor = borderColor
					}
				}
			}
		} position@(Vec2 px py) = do
		Vec2 sx sy <- readTVar sizeVar
		-- compose rendering of children
		children <- readTVar childrenVar
		let drawChild PanelChild
			{ panelChildElement = SomeElement element
			, panelChildPositionVar = childPositionVar
			} = do
			childPosition <- readTVar childPositionVar
			renderElement element drawer $ position + childPosition
		renderChildren <- foldrM (\a b -> liftM (>> b) a) (return ()) $ map drawChild $ S.toAscList children
		-- return
		return $ renderScope $ do
			renderViewport $ Vec4 px py (px + sx) (py + sy)
			drawBorderedRectangle canvas (Vec4 0 1 (sx - 1) sx) (Vec4 0 1 (sy - 1) sy) fillColor borderColor
			renderChildren

	processInputEvent panel@Panel
		{ panelChildrenVar = childrenVar
		, panelFocusedChildVar = focusedChildVar
		, panelLastMousedChildVar = lastMousedChildVar
		} inputEvent inputState@InputState
		{ inputStateKeyboard = keyboardState
		} = case inputEvent of

		KeyboardInputEvent keyboardEvent -> do
			-- own processing: handle tab-moving focus
			let ownProcessEvent = case keyboardEvent of
				KeyDownEvent KeyTab -> do
					focusedChild <- readTVar focusedChildVar
					children <- readTVar childrenVar
					keyShiftLPressed <- getKeyState keyboardState KeyShiftL
					keyShiftRPressed <- getKeyState keyboardState KeyShiftR
					let keyShiftPressed = keyShiftLPressed || keyShiftRPressed
					case focusedChild of
						Just child@PanelChild
							{ panelChildElement = SomeElement focusedElement
							} -> do
							let (before, after) = S.split child children
							focusedNewChild <- focusSomeChild panel $ if keyShiftPressed then S.toDescList before ++ S.toDescList after else S.toAscList after ++ S.toAscList before
							when focusedNewChild $ unfocusElement focusedElement
							return focusedNewChild
						Nothing -> focusSomeChild panel $ (if keyShiftPressed then S.toDescList else S.toAscList) children
				_ -> return False

			-- send keyboard event to focused element
			focusedChild <- readTVar focusedChildVar
			case focusedChild of
				Just PanelChild
					{ panelChildElement = SomeElement element
					} -> do
					processed <- processInputEvent element inputEvent inputState
					if processed then return True else ownProcessEvent
				Nothing -> ownProcessEvent

		MouseInputEvent mouseEvent -> do
			children <- readTVar childrenVar
			-- send event to last moused child (without any correction)
			let sendToLastChild = do
				lastMousedChild <- readTVar lastMousedChildVar
				case lastMousedChild of
					Just PanelChild
						{ panelChildElement = SomeElement lastMousedChildElement
						} -> processInputEvent lastMousedChildElement inputEvent inputState
					Nothing -> return False
			-- select by mouse event
			case mouseEvent of
				MouseDownEvent _mouseButton -> do
					-- focus-by-click
					lastMousedChild <- readTVar lastMousedChildVar
					case lastMousedChild of
						Just PanelChild
							{ panelChildElement = SomeElement lastMousedChildElement
							} -> do
							-- get currently focused child
							focusedChild <- readTVar focusedChildVar
							-- if it's not the same one
							when (lastMousedChild /= focusedChild) $ do
								-- try to focus element under mouse
								focusAccepted <- focusElement lastMousedChildElement
								when focusAccepted $ do
									writeTVar focusedChildVar lastMousedChild
									-- unfocus previously focused child
									case focusedChild of
										Just PanelChild
											{ panelChildElement = SomeElement focusedElement
											} -> unfocusElement focusedElement
										Nothing -> return ()
							-- send mouse event in any case
							processInputEvent lastMousedChildElement inputEvent inputState
						Nothing -> return False
				MouseUpEvent _mouseButton -> sendToLastChild
				RawMouseMoveEvent _dx _dy _dz -> sendToLastChild
				CursorMoveEvent x y -> do
					-- determine child with the mouse on it
					let
						pickChild (child@PanelChild
							{ panelChildElement = SomeElement element
							, panelChildPositionVar = childPositionVar
							} : restChildren) point = do
							-- correct position and ask child element
							childPosition <- readTVar childPositionVar
							r <- dabElement element $ point - childPosition
							if r then return $ Just child else pickChild restChildren point
						pickChild [] _point = return Nothing
					mousedChild <- pickChild (S.toDescList children) $ Vec2 x y
					-- update last moused child
					lastMousedChild <- readTVar lastMousedChildVar
					when (mousedChild /= lastMousedChild) $ do
						writeTVar lastMousedChildVar mousedChild
						case lastMousedChild of
							Just PanelChild
								{ panelChildElement = SomeElement lastMousedChildElement
								} -> void $ processInputEvent lastMousedChildElement MouseLeaveEvent inputState
							Nothing -> return ()
					-- if mouse points to some element now
					case mousedChild of
						Just PanelChild
							{ panelChildElement = SomeElement childElement
							, panelChildPositionVar = childPositionVar
							} -> do
							-- correct coordinates and send event
							Vec2 px py <- readTVar childPositionVar
							processInputEvent childElement (MouseInputEvent (CursorMoveEvent (x - px) (y - py))) inputState
						Nothing -> return False
		MouseLeaveEvent -> do
			lastMousedChild <- readTVar lastMousedChildVar
			case lastMousedChild of
				Just PanelChild
					{ panelChildElement = SomeElement element
					} -> processInputEvent element MouseLeaveEvent inputState
				Nothing -> return False

	focusElement panel@Panel
		{ panelChildrenVar = childrenVar
		, panelFocusedChildVar = focusedChildVar
		} = do
		focusedChild <- readTVar focusedChildVar
		if isNothing focusedChild then do
			children <- readTVar childrenVar
			focusSomeChild panel $ S.toAscList children
		else return True

	unfocusElement Panel
		{ panelFocusedChildVar = focusedChildVar
		} = do
		focusedChild <- readTVar focusedChildVar
		case focusedChild of
			Just PanelChild
				{ panelChildElement = SomeElement element
				} -> do
				unfocusElement element
				writeTVar focusedChildVar Nothing
			Nothing -> return ()

instance FreeContainer Panel where

	type FreeContainerChild Panel = PanelChild

	setLayoutHandler Panel
		{ panelLayoutHandlerVar = layoutHandlerVar
		} layoutHandler = writeTVar layoutHandlerVar layoutHandler

	addFreeChild Panel
		{ panelChildrenVar = childrenVar
		, panelChildIndexVar = childIndexVar
		} element = do
		-- get index for new child
		childIndex <- readTVar childIndexVar
		writeTVar childIndexVar $ childIndex + 1
		-- create child
		positionVar <- newTVar $ Vec2 0 0
		let child = PanelChild
			{ panelChildIndex = childIndex
			, panelChildElement = SomeElement element
			, panelChildPositionVar = positionVar
			}
		-- add it to map
		children <- readTVar childrenVar
		writeTVar childrenVar $ S.insert child children
		-- return
		return child

	removeFreeChild panel@Panel
		{ panelChildrenVar = childrenVar
		, panelFocusedChildVar = focusedChildVar
		} child@PanelChild
		{ panelChildElement = SomeElement element
		} = do
		children <- readTVar childrenVar
		-- remove from children
		let newChildren = S.delete child children
		-- if this element is focused
		focusedChild <- readTVar focusedChildVar
		when (focusedChild == Just child) $ do
			-- unfocus it
			unfocusElement element
			writeTVar focusedChildVar Nothing
			-- try to focus some other child, starting from next one
			let (childrenBefore, childrenAfter) = S.split child newChildren
			_ <- focusSomeChild panel $ S.toAscList childrenAfter ++ S.toAscList childrenBefore
			return ()
		-- write new children
		writeTVar childrenVar newChildren

	placeFreeChild _panel PanelChild
		{ panelChildPositionVar = childPositionVar
		} position = writeTVar childPositionVar position

-- | Helper function, trying to focus first child in a list accepting the focus.
-- Writes index of a child accepted focus to panel.
focusSomeChild :: Panel -> [PanelChild] -> STM Bool
focusSomeChild Panel
	{ panelFocusedChildVar = focusedChildVar
	} = tryToFocus where
	tryToFocus (child@PanelChild
		{ panelChildElement = SomeElement element
		} : restChildren) = do
		focusAccepted <- focusElement element
		if focusAccepted then do
			writeTVar focusedChildVar $ Just child
			return True
		else tryToFocus restChildren
	tryToFocus [] = do
		return False

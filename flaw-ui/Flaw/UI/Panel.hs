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

import Flaw.Input.Keyboard
import Flaw.Math
import Flaw.UI

import Control.Monad
import Control.Concurrent.STM
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S

data Panel = Panel
	{ panelChildrenVar :: !(TVar (S.Set PanelChild))
	, panelChildIndexVar :: !(TVar Int)
	, panelLayoutHandlerVar :: !(TVar (Size -> STM ()))
	, panelFocusedChildVar :: !(TVar (Maybe PanelChild))
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
	focusedChildVar <- newTVar Nothing
	lastMousedChildVar <- newTVar Nothing
	return Panel
		{ panelChildrenVar = childrenVar
		, panelChildIndexVar = childIndexVar
		, panelLayoutHandlerVar = layoutHandlerVar
		, panelFocusedChildVar = focusedChildVar
		, panelLastMousedChildVar = lastMousedChildVar
		}

instance Eq PanelChild where
	child1 == child2 = panelChildIndex child1 == panelChildIndex child2

instance Ord PanelChild where
	compare child1 child2 = compare (panelChildIndex child1) (panelChildIndex child2)

instance Visual Panel where

	layout Panel
		{ panelLayoutHandlerVar = layoutHandlerVar
		} size = do
		layoutHandler <- readTVar layoutHandlerVar
		layoutHandler size

	draw Panel
		{ panelChildrenVar = childrenVar
		} = do
		-- TODO: set scissors
		-- draw children
		children <- readTVar childrenVar
		let drawChild PanelChild
			{ panelChildElement = SomeElement element
			} = draw element
		foldrM (\a b -> liftM (>> b) a) (return ()) $ map drawChild $ S.toAscList children

instance Element Panel where

	processInputEvent panel@Panel
		{ panelChildrenVar = childrenVar
		, panelFocusedChildVar = focusedChildVar
		, panelLastMousedChildVar = lastMousedChildVar
		} inputEvent = case inputEvent of

		KeyboardInputEvent keyboardEvent keyboardState -> do
			-- own processing: handle tab-moving focus
			let ownProcessEvent = case keyboardEvent of
				KeyDownEvent KeyTab -> do
					focusedChild <- readTVar focusedChildVar
					children <- readTVar childrenVar
					keyShiftLPressed <- getKeyState keyboardState KeyShiftL
					keyShiftRPressed <- getKeyState keyboardState KeyShiftR
					let keyShiftPressed = keyShiftLPressed || keyShiftRPressed
					case focusedChild of
						Just child -> do
							let (before, after) = S.split child children
							focusSomeChild panel (if keyShiftPressed then S.toDescList before else S.toAscList after)
						Nothing -> focusSomeChild panel $ (if keyShiftPressed then S.toDescList else S.toAscList) children
				_ -> return False

			-- send keyboard event to focused element
			focusedChild <- readTVar focusedChildVar
			case focusedChild of
				Just PanelChild
					{ panelChildElement = SomeElement element
					} -> do
					processed <- processInputEvent element inputEvent
					if processed then return True else ownProcessEvent
				Nothing -> ownProcessEvent

		MouseInputEvent _ _ -> do
			children <- readTVar childrenVar
			-- try to send mouse event to first (in reverse order) child accepting it
			let
				trySendMouse (child@PanelChild
					{ panelChildElement = SomeElement element
					} : restChildren) = do
					processed <- processInputEvent element inputEvent
					if processed then return $ Just child
					else trySendMouse restChildren
				trySendMouse [] = return Nothing
			child <- trySendMouse (S.toDescList children)
			-- check if it's not the same child as last time
			lastMousedChild <- readTVar lastMousedChildVar
			when (lastMousedChild /= child) $ do
				-- remember new last-moused child
				writeTVar lastMousedChildVar child
				-- send mouse leave event to old one
				case lastMousedChild of
					Just PanelChild
						{ panelChildElement = SomeElement element
						} -> void $ processInputEvent element MouseLeaveEvent
					Nothing -> return ()
			return $ isJust child
		MouseLeaveEvent -> do
			lastMousedChild <- readTVar lastMousedChildVar
			case lastMousedChild of
				Just PanelChild
					{ panelChildElement = SomeElement element
					} -> processInputEvent element MouseLeaveEvent
				Nothing -> return False

	focus panel@Panel
		{ panelChildrenVar = childrenVar
		, panelFocusedChildVar = focusedChildVar
		} = do
		focusedChild <- readTVar focusedChildVar
		if isNothing focusedChild then do
			children <- readTVar childrenVar
			focusSomeChild panel $ S.toAscList children
		else return True

	unfocus Panel
		{ panelFocusedChildVar = focusedChildVar
		} = do
		focusedChild <- readTVar focusedChildVar
		case focusedChild of
			Just PanelChild
				{ panelChildElement = SomeElement element
				} -> do
				unfocus element
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
			unfocus element
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
		focusAccepted <- focus element
		if focusAccepted then do
			writeTVar focusedChildVar $ Just child
			return True
		else tryToFocus restChildren
	tryToFocus [] = do
		writeTVar focusedChildVar Nothing
		return False

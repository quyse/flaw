{-|
Module: Flaw.UI
Description: Basic user interface definitions.
License: MIT
-}

{-# LANGUAGE GADTs, TypeFamilies #-}

module Flaw.UI
	( Metric
	, Position
	, Size
	, Visual(..)
	, SomeVisual(..)
	, Element(..)
	, InputEvent(..)
	, InputState(..)
	, SomeElement(..)
	, HasText(..)
	, HasClickHandler(..)
	, HasChecked(..)
	, HasProgress(..)
	, Progress(..)
	, FreeContainer(..)
	, DraggableInFreeContainer(..)
	, SomeFreeChild(..)
	, MouseCursor(..) -- re-export from Flaw.Window
	) where

import Control.Concurrent.STM
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Font
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI.Drawer
import Flaw.Window

-- | Base type for length values.
type Metric = Int
-- | Base type for two-dimensional position values.
type Position = Vec2 Metric
-- | Base type for two-dimensional length values.
type Size = Vec2 Metric

-- | Visual is a paintable thing with layout.
class Visual a where
	-- | Render visual.
	renderVisual :: Context c d => a -> Drawer d -> Position -> Size -> Style -> STM (Render c ())

-- | Any visual.
data SomeVisual where
	SomeVisual :: Visual a => !a -> SomeVisual

-- | Element is a content able to react to input events.
class Element a where
	-- | Set size of element.
	-- If (and only if) size has changed, element should re-calculate
	-- layout and call this method for nested visuals/elements.
	layoutElement :: a -> Size -> STM ()
	-- | Check that point is in element.
	-- Visual can return False for "holes".
	dabElement :: a -> Position -> STM Bool
	-- | Get mouse cursor element wants to show over.
	elementMouseCursor :: a -> STM MouseCursor
	elementMouseCursor _ = return MouseCursorArrow
	-- | Render element.
	-- Size is set by previous call to 'layout'. It's parent element's responsibility
	-- to correctly constrain viewport.
	renderElement :: Context c d => a -> Drawer d -> Position -> STM (Render c ())
	-- | Process input event addressed to the content.
	-- Element may return False which means it wants to return event
	-- back to container. It should be used for passing mouse events
	-- in transparent areas, and for skipping non-processed keyboard
	-- events, so container has chance to implement focus control with keys.
	processInputEvent :: a -> InputEvent -> InputState -> STM Bool
	-- | Container gives child keyboard focus.
	-- Child returns True if it accepts focus;
	-- in case of False container may try to give focus to
	-- another element.
	focusElement :: a -> STM Bool
	focusElement _ = return False
	-- | Container takes keyboard focus back.
	-- Element has to release focus.
	unfocusElement :: a -> STM ()
	unfocusElement _ = return ()

-- | Input events.
-- State of keyboard/mouse is provided before applying event.
data InputEvent
	-- | Keyboard event. Sent to focused control.
	= KeyboardInputEvent !KeyboardEvent
	-- | Mouse event. CursorMoveEvent is adjusted to element's coordinates.
	| MouseInputEvent !MouseEvent
	-- | Mouse left the element.
	| MouseLeaveEvent

-- | Input state.
data InputState = InputState
	{ inputStateKeyboard :: !KeyboardState
	, inputStateMouse :: !MouseState
	-- | Function to get (asynchronously) clipboard text.
	-- Text will be returned to the callback in separate transaction.
	, inputStateGetClipboardText :: (T.Text -> STM ()) -> STM ()
	-- | Function to set (asynchronously) clipboard text.
	, inputStateSetClipboardText :: T.Text -> STM ()
	}

-- | Any element.
data SomeElement where
	SomeElement :: Element a => !a -> SomeElement

class HasText a where
	setText :: a -> T.Text -> STM ()
	setTextScript :: a -> FontScript -> STM ()

class HasClickHandler a where
	setClickHandler :: a -> STM () -> STM ()

class HasChecked a where
	setChecked :: a -> Bool -> STM ()

class HasProgress a where
	setProgress :: a -> Progress -> STM ()

data Progress = Progress Float | IndeterminateProgress

-- | Free container is an element able to place other elements
-- freely with explicit positions.
class Element a => FreeContainer a where
	-- | Handle of child element added to free container.
	type FreeContainerChild a :: *
	-- | Set free container handler of 'layout' method.
	setLayoutHandler :: a -> (Size -> STM ()) -> STM ()
	-- | Add child element to free container.
	addFreeChild :: Element e => a -> e -> STM (FreeContainerChild a)
	-- | Remove child element from free container.
	removeFreeChild :: a -> FreeContainerChild a -> STM ()
	-- | Set position of child element in free container.
	placeFreeChild :: a -> FreeContainerChild a -> Position -> STM ()
	-- | Move child element relatively its current position.
	placeFreeChildRelatively :: a -> FreeContainerChild a -> Vec2 Int -> STM ()

-- | Class of element which could be moved by mouse.
class Element a => DraggableInFreeContainer a where
	-- | Tell element a "free child" object used for placement this element.
	-- After that element becomes movable.
	setSelfFreeChild :: FreeContainer fc => a -> fc -> FreeContainerChild fc -> STM ()

data SomeFreeChild where
	SomeFreeChild :: FreeContainer fc => fc -> FreeContainerChild fc -> SomeFreeChild

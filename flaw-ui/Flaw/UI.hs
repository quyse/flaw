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
	, SomeElement(..)
	, FreeContainer(..)
	, InputEvent(..)
	, InputState(..)
	, HasText(..)
	, HasClickHandler(..)
	, HasChecked(..)
	, HasProgress(..)
	, Progress(..)
	) where

import Control.Concurrent.STM
import qualified Data.Text as T

import Flaw.Graphics
import Flaw.Graphics.Font
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.UI.Drawer

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
	-- | Container takes keyboard focus back.
	-- Element has to release focus.
	unfocusElement :: a -> STM ()

-- | Any element.
data SomeElement where
	SomeElement :: Element a => !a -> SomeElement

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
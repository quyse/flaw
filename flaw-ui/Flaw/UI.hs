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
	, Rect
	, Visual(..)
	, SomeVisual(..)
	, Element(..)
	, InputEvent(..)
	, InputState(..)
	, SomeElement(..)
	, HasText(..)
	, AlignX(..), AlignY(..)
	, HasAlignment(..)
	, HasPassword(..)
	, HasClickHandler(..)
	, HasChangeHandler(..)
	, HasChecked(..)
	, HasFloatValue(..)
	, HasProgress(..)
	, Progress(..)
	, FreeContainer(..)
	, DraggableInFreeContainer(..)
	, SomeFreeChild(..)
	, DefaultActionRedirector(..)
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
import Flaw.UI.Metrics
import Flaw.Window

-- | Visual is a paintable thing with layout.
class Visual a where
	-- | Render visual.
	-- Style is set by parent element, so visual may react on mouse or pressed state.
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

class HasText a => HasPassword a where
	setPasswordMode :: a -> Bool -> STM ()

data AlignX = AlignLeft | AlignCenter | AlignRight
data AlignY = AlignTop | AlignMiddle | AlignBottom

class HasAlignment a where
	setAlignment :: a -> AlignX -> AlignY -> STM ()

class HasClickHandler a where
	setClickHandler :: a -> STM () -> STM ()

class HasChangeHandler a where
	setChangeHandler :: a -> STM () -> STM ()

class HasChecked a where
	setChecked :: a -> Bool -> STM ()

class HasFloatValue a where
	setFloatValue :: a -> Float -> STM ()
	getFloatValue :: a -> STM Float

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
	placeFreeChildRelatively :: a -> FreeContainerChild a -> Int2 -> STM ()
	-- | Bring element to the end of render list (in order to render on top of everything).
	bringFreeChildOnTop :: a -> FreeContainerChild a -> STM ()

-- | Class of element which could be moved by mouse.
class Element a => DraggableInFreeContainer a where
	-- | Tell element a "free child" object used for placement this element.
	-- After that element becomes movable and resizable (if flag is set).
	setSelfFreeChild :: FreeContainer fc
		=> a -- ^ Element.
		-> fc -- ^ Container.
		-> FreeContainerChild fc -- ^ Free child in container.
		-> Bool -- ^ Resizable?
		-> STM ()

data SomeFreeChild where
	SomeFreeChild :: FreeContainer fc => fc -> FreeContainerChild fc -> SomeFreeChild

class Element a => DefaultActionRedirector a where
	setDefaultElement :: Element e => a -> e -> STM ()
	setCancelElement :: Element e => a -> e -> STM ()

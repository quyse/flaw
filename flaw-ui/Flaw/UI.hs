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
	, Element(..)
	, SomeElement(..)
	, FreeContainer(..)
	, InputEvent(..)
	, HasContent(..)
	, HasTitle(..)
	, HasClickHandler(..)
	, HasChecked(..)
	, HasProgress(..)
	, Progress(..)
	) where

import Flaw.Graphics
import Flaw.Graphics.Canvas
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math

import Control.Concurrent.STM
import qualified Data.Text as T

-- | Base type for length values.
type Metric = Int
-- | Base type for two-dimensional position values.
type Position = Vec2 Metric
-- | Base type for two-dimensional length values.
type Size = Vec2 Metric

-- | Visual is a paintable thing with layout.
class Visual a where
	-- | Set size of content.
	-- If (and only if) size has changed, content should re-calculate
	-- layout and call this method for nested content.
	layout :: a -> Size -> STM ()
	-- | Draw content.
	-- Left-top corner should be (0, 0), and size is set by previous
	-- call to 'layout'. It's parent content's responsibility to correctly
	-- set current transform within 'Draw' monad, and optionally
	-- scissors region, etc.
	draw :: Context c d => a -> STM (CanvasM c d ())

-- | Element is a content able to react to input events.
class Visual a => Element a where
	-- | Process input event addressed to the content.
	-- Element may return False which means it wants to return event
	-- back to container. It should be used for passing mouse events
	-- in transparent areas, and for skipping non-processed keyboard
	-- events, so container has chance to implement focus control with keys.
	processInputEvent :: a -> InputEvent -> STM Bool
	-- | Container gives child keyboard focus.
	-- Child returns True if it accepts focus;
	-- in case of False container may try to give focus to
	-- another element.
	focus :: a -> STM Bool
	-- | Container takes keyboard focus back.
	-- Element has to release focus.
	unfocus :: a -> STM ()

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
	= KeyboardInputEvent !KeyboardEvent !KeyboardState
	-- | Mouse event. CursorMoveEvent is adjusted to element's coordinates.
	| MouseInputEvent !MouseEvent !MouseState
	-- | Mouse left the element.
	| MouseLeaveEvent

class Element a => HasContent a where
	setContent :: Visual c => a -> c -> STM ()

class Element a => HasTitle a where
	setTitle :: a -> T.Text -> STM ()

class Element a => HasClickHandler a where
	setClickHandler :: a -> STM () -> STM ()

class Element a => HasChecked a where
	setChecked :: a -> Bool -> STM ()

class Element a => HasProgress a where
	setProgress :: a -> Progress -> STM ()

data Progress = Progress Float | IndeterminateProgress

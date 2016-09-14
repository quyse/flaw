{-|
Module: Flaw.UI.VisualElement
Description: Adapter for Visual to use it as Element.
License: MIT
-}

module Flaw.UI.VisualElement
	( VisualElement(..)
	, newVisualElement
	) where

import Control.Concurrent.STM

import Flaw.Math
import Flaw.UI
import Flaw.UI.Drawer

data VisualElement = VisualElement
	{ visualElementVisual :: !SomeVisual
	, visualElementSizeVar :: !(TVar Size)
	}

newVisualElement :: Visual v => v -> STM VisualElement
newVisualElement visual = do
	sizeVar <- newTVar $ Vec2 0 0
	return VisualElement
		{ visualElementVisual = SomeVisual visual
		, visualElementSizeVar = sizeVar
		}

instance Element VisualElement where
	layoutElement VisualElement
		{ visualElementSizeVar = sizeVar
		} = writeTVar sizeVar
	dabElement _ _ = return False
	elementMouseCursor _ = return MouseCursorArrow
	renderElement VisualElement
		{ visualElementVisual = SomeVisual visual
		, visualElementSizeVar = sizeVar
		} drawer@Drawer
		{ drawerStyles = DrawerStyles
			{ drawerFlatStyleVariant = StyleVariant
				{ styleVariantNormalStyle = style
				}
			}
		} position = do
		size <- readTVar sizeVar
		renderVisual visual drawer position size style
	processInputEvent _ _ _ = return False

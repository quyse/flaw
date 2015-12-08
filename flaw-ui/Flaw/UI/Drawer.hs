{-|
Module: Flaw.UI.Drawer
Description: UI drawer object.
License: MIT
-}

{-# LANGUAGE GADTs #-}

module Flaw.UI.Drawer
	( Drawer(..)
	, initDrawer
	, setDrawerFrameTime
	, DrawerStyles(..)
	, StyleVariant(..)
	, Style(..)
	, DrawerFont(..)
	, SomeFontShaper(..)
	) where

import Control.Concurrent.STM

import Flaw.Graphics.Canvas
import Flaw.Graphics.Font
import Flaw.Graphics.Font.Render
import Flaw.Math

-- | Drawer data.
data Drawer d = Drawer
	{
	-- | Canvas.
	  drawerCanvas :: !(Canvas d)
	-- | Glyph renderer for all fonts.
	, drawerGlyphRenderer :: !(GlyphRenderer d)
	-- | Length of previous frame in seconds.
	-- Use wisely in UI controls! As control not always gets rendered it may miss some time.
	, drawerFrameTimeVar :: !(TVar Float)
	-- | Style information.
	, drawerStyles :: DrawerStyles d
	}

-- | Create drawer.
initDrawer :: Canvas d -> GlyphRenderer d -> DrawerStyles d -> STM (Drawer d)
initDrawer canvas glyphRenderer styles = do
	frameTimeVar <- newTVar 1
	return Drawer
		{ drawerCanvas = canvas
		, drawerGlyphRenderer = glyphRenderer
		, drawerFrameTimeVar = frameTimeVar
		, drawerStyles = styles
		}

-- | Update drawer with time passed.
setDrawerFrameTime :: Drawer d -> Float -> STM ()
setDrawerFrameTime Drawer
	{ drawerFrameTimeVar = frameTimeVar
	} frameTime = writeTVar frameTimeVar frameTime

-- | Style information.
data DrawerStyles d = DrawerStyles
	{
	-- | Label font for UI.
	-- For normal labels, buttons, etc.
	  drawerLabelFont :: !(DrawerFont d)
	-- | Edit font for UI.
	-- For text entered by user.
	, drawerEditFont :: !(DrawerFont d)
	-- | Title font for UI.
	, drawerTitleFont :: !(DrawerFont d)

	-- | "Flat" style variant.
	-- Used for things like labels and checkboxes.
	, drawerFlatStyleVariant :: !StyleVariant
	-- | "Lowered" style variant.
	-- Mostly used for text fields, list boxes, etc.
	, drawerLoweredStyleVariant :: !StyleVariant
	-- | "Raised" style variant.
	-- Used for buttons.
	, drawerRaisedStyleVariant :: !StyleVariant

	, drawerFrameOuterNormalStyle :: !Style
	, drawerFrameOuterFocusedStyle :: !Style
	, drawerFrameInnerStyle :: !Style
	}

data StyleVariant = StyleVariant
	{ styleVariantNormalStyle :: !Style
	, styleVariantMousedStyle :: !Style
	, styleVariantPressedStyle :: !Style
	, styleVariantSelectedFocusedStyle :: !Style
	, styleVariantSelectedUnfocusedStyle :: !Style
	}

data Style = Style
	{ styleTextColor :: !Vec4f
	, styleFillColor :: !Vec4f
	, styleBorderColor :: !Vec4f
	}

data DrawerFont d = DrawerFont
	{ drawerFontRenderableFont :: !(RenderableFont d)
	, drawerFontShaper :: !SomeFontShaper
	}

data SomeFontShaper where
	SomeFontShaper :: FontShaper s => !s -> SomeFontShaper

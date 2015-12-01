{-|
Module: Flaw.UI.Drawer
Description: UI drawer object.
License: MIT
-}

{-# LANGUAGE GADTs #-}

module Flaw.UI.Drawer
	( Drawer(..)
	, StyleVariant(..)
	, Style(..)
	, DrawerFont(..)
	, SomeFontShaper(..)
	) where

import Flaw.Graphics.Canvas
import Flaw.Graphics.Font
import Flaw.Graphics.Font.Render
import Flaw.Math

-- | Drawer data.
-- Currently contains style things (fonts, colors, etc).
-- TODO: Probably need to move them to separate struct/class/etc.
data Drawer d = Drawer
	{
	-- | Canvas.
	  drawerCanvas :: !(Canvas d)
	-- | Glyph renderer for all fonts.
	, drawerGlyphRenderer :: !(GlyphRenderer d)
	-- | Label font for UI.
	-- For normal labels, buttons, etc.
	, drawerLabelFont :: !(DrawerFont d)
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

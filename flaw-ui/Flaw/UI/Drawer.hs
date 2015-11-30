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
	-- | Normal font for UI.
	, drawerNormalRenderableFont :: !(RenderableFont d)
	-- | Normal font shaper.
	, drawerNormalFontShaper :: !SomeFontShaper
	-- | Title font for UI.
	, drawerTitleRenderableFont :: !(RenderableFont d)
	-- | Title font shaper.
	, drawerTitleFontShaper :: !SomeFontShaper

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
	, styleVariantSelectedStyle :: !Style
	}

data Style = Style
	{ styleTextColor :: !Vec4f
	, styleFillColor :: !Vec4f
	, styleBorderColor :: !Vec4f
	}

data SomeFontShaper where
	SomeFontShaper :: FontShaper s => !s -> SomeFontShaper

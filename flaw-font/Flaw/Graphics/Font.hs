{-|
Module: Flaw.Graphics.Font
Description: Fonts.
License: MIT
-}

module Flaw.Graphics.Font
	( Glyphs(..)
	, GlyphInfo(..)
	, FontFace(..)
	) where

import Codec.Picture
import qualified Data.Vector as V

data Glyphs = Glyphs
	{ glyphsImage :: !(Image Pixel8)
	, glyphsInfos :: !(V.Vector GlyphInfo)
	, glyphsScaleX :: !Int
	, glyphsScaleY :: !Int
	}

data GlyphInfo = GlyphInfo
	{ glyphWidth :: !Int
	, glyphHeight :: !Int
	, glyphLeftTopX :: !Int
	, glyphLeftTopY :: !Int
	, glyphOffsetX :: !Int
	, glyphOffsetY :: !Int
	}

class FontFace f where
	createGlyphs :: f
		-> Int -- ^ Size in pixels.
		-> Int -- ^ Half scale for X. Real scale is (1 + halfScale * 2).
		-> Int -- ^ Half scale for Y. Real scale is (1 + halfScale * 2).
		-> IO Glyphs

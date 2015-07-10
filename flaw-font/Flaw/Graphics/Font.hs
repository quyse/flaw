{-|
Module: Flaw.Graphics.Font
Description: Fonts.
License: MIT
-}

module Flaw.Graphics.Font
	( Glyphs(..)
	, GlyphInfo(..)
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

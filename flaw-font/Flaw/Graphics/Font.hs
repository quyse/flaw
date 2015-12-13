{-|
Module: Flaw.Graphics.Font
Description: Fonts.
License: MIT
-}

module Flaw.Graphics.Font
	( Glyphs(..)
	, GlyphInfo(..)
	, FontShaper(..)
	, FontScript(..)
	, fontScriptUnknown
	) where

import Codec.Picture
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word

import Flaw.Math

data Glyphs = Glyphs
	{ glyphsImage :: !(Image Pixel8)
	, glyphsInfos :: !(V.Vector GlyphInfo)
	, glyphsScaleX :: !Int
	, glyphsScaleY :: !Int
	}

data GlyphInfo = GlyphInfo
	{
	-- Size of glyph on texture.
	  glyphWidth :: !Int
	, glyphHeight :: !Int
	-- Coordinates of left-top corner on texture.
	, glyphLeftTopX :: !Int
	, glyphLeftTopY :: !Int
	-- Offset from pen point to left-top corner.
	, glyphOffsetX :: !Int
	, glyphOffsetY :: !Int
	}

class FontShaper a where
	-- | Shape multiple text parts and return lists of glyphs (position, glyph index), and final position.
	shapeText :: a -> [T.Text] -> FontScript -> IO ([V.Vector (Float2, Int)], Float2)

-- | Font script.
-- Contains 4-letter script code (according to http://unicode.org/iso15924/iso15924-codes.html),
-- big-endian encoded.
newtype FontScript = FontScript Word32

fontScriptUnknown :: FontScript
fontScriptUnknown = FontScript 0

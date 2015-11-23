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
	{ glyphWidth :: !Int
	, glyphHeight :: !Int
	, glyphLeftTopX :: !Int
	, glyphLeftTopY :: !Int
	, glyphOffsetX :: !Int
	, glyphOffsetY :: !Int
	}

class FontShaper a where
	shapeText :: a -> T.Text -> FontScript -> IO (V.Vector (Vec2f, Int), Vec2f)

-- | Font script.
-- Contains 4-letter script code (according to http://unicode.org/iso15924/iso15924-codes.html),
-- big-endian encoded.
newtype FontScript = FontScript Word32

fontScriptUnknown :: FontScript
fontScriptUnknown = FontScript 0

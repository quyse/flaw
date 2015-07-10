{-|
Module: Flaw.Graphics.Font
Description: Fonts.
License: MIT
-}

module Flaw.Graphics.Font
	( Glyphs(..)
	, GlyphInfo(..)
	, FontShaper(..)
	) where

import Codec.Picture
import qualified Data.Text as T
import qualified Data.Vector as V

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
	shapeText :: a -> T.Text -> IO (V.Vector (Vec2f, Int), Vec2f)

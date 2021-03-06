{-|
Module: Flaw.Graphics.Font
Description: Fonts.
License: MIT
-}

module Flaw.Graphics.Font
  ( Glyphs(..)
  , GlyphInfo(..)
  , FontShaper(..)
  , ShapedGlyph(..)
  , FontScript(..)
  , fontScriptUnknown
  ) where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word

import Flaw.Graphics.Texture
import Flaw.Math

data Glyphs = Glyphs
  { glyphsTextureInfo :: !TextureInfo
  , glyphsTextureData :: !B.ByteString
  , glyphsInfos :: !(HM.HashMap Int GlyphInfo)
  , glyphsScaleX :: {-# UNPACK #-} !Int
  , glyphsScaleY :: {-# UNPACK #-} !Int
  }

data GlyphInfo = GlyphInfo
  {
  -- Size of glyph on texture.
    glyphWidth :: {-# UNPACK #-} !Int
  , glyphHeight :: {-# UNPACK #-} !Int
  -- Coordinates of left-top corner on texture.
  , glyphLeftTopX :: {-# UNPACK #-} !Int
  , glyphLeftTopY :: {-# UNPACK #-} !Int
  -- Offset from pen point to left-top corner.
  , glyphOffsetX :: {-# UNPACK #-} !Int
  , glyphOffsetY :: {-# UNPACK #-} !Int
  }

class FontShaper a where
  -- | Shape multiple text parts and return lists of glyphs (position, glyph index) with final positions.
  shapeText :: a -> [T.Text] -> FontScript -> IO [(V.Vector ShapedGlyph, Float2)]

data ShapedGlyph = ShapedGlyph
  { shapedGlyphPosition :: {-# UNPACK #-} !Float2
  , shapedGlyphIndex :: {-# UNPACK #-} !Int
  }

-- | Font script.
-- Contains 4-letter script code (according to http://unicode.org/iso15924/iso15924-codes.html),
-- big-endian encoded.
newtype FontScript = FontScript Word32

fontScriptUnknown :: FontScript
fontScriptUnknown = FontScript 0

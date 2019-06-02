{-|
Module: Flaw.Graphics.Font.Harfbuzz
Description: Harfbuzz text shaping.
License: MIT
-}

module Flaw.Graphics.Font.Harfbuzz
  ( createHarfbuzzShaper
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.State.Strict
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import qualified Data.Vector as V
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Graphics.Font
import Flaw.Graphics.Font.FreeType
import Flaw.Graphics.Font.FreeType.FFI
import Flaw.Math

data HarfbuzzShaper = HarfbuzzShaper
  {-# UNPACK #-} !(Ptr Hb_font_t)
  {-# UNPACK #-} !(Ptr Hb_buffer_t)
  !FreeTypeFont

createHarfbuzzShaper :: FreeTypeFont -> IO (HarfbuzzShaper, IO ())
createHarfbuzzShaper font@FreeTypeFont
  { ftFontFaceVar = ftFaceVar
  } = bracket (atomically $ takeTMVar ftFaceVar) (atomically . putTMVar ftFaceVar) $ \ftFace -> do
  hbFont <- hb_ft_font_create ftFace nullPtr
  hbBuffer <- hb_buffer_create
  let
    destroy = do
      hb_font_destroy hbFont
      hb_buffer_destroy hbBuffer
  return (HarfbuzzShaper hbFont hbBuffer font, destroy)

instance FontShaper HarfbuzzShaper where
  shapeText (HarfbuzzShaper hbFont hbBuffer FreeTypeFont
    { ftFontFaceVar = ftFaceVar
    }) texts (FontScript script) = bracket (atomically $ takeTMVar ftFaceVar) (atomically . putTMVar ftFaceVar) $ \_ftFace -> do
    -- calculate offsets and lengths
    let
      calcOffsetsAndLengths (t : ts) ll = (ll, l) : calcOffsetsAndLengths ts (ll + l) where l = T.length t
      calcOffsetsAndLengths [] _ = []
      offsetsAndLengths = calcOffsetsAndLengths texts 0

    -- loop for texts (inside single call to useAsPtr, as it does copying)
    -- state monad contains advance vector
    T.useAsPtr (mconcat texts) $ \unitedTextPtr unitedTextLen -> flip evalStateT (Vec2 0 0) $ forM offsetsAndLengths $ \(offset, len) -> do
      -- set script and direction
      liftIO $ hb_buffer_set_script hbBuffer script
      liftIO $ hb_buffer_set_direction hbBuffer $ hb_script_get_horizontal_direction script

      -- add total text (specifying context)
      liftIO $ hb_buffer_add_utf16 hbBuffer unitedTextPtr (fromIntegral unitedTextLen) (fromIntegral offset) (fromIntegral len)

      -- shape
      liftIO $ hb_shape hbFont hbBuffer nullPtr 0

      -- get glyphs
      (hbGlyphInfos, hbGlyphPositions, glyphCount) <- liftIO $ alloca $ \glyphCountPtr -> do
        hbGlyphInfos <- hb_buffer_get_glyph_infos hbBuffer glyphCountPtr
        hbGlyphPositions <- hb_buffer_get_glyph_positions hbBuffer glyphCountPtr
        glyphCount <- peek glyphCountPtr
        return (hbGlyphInfos, hbGlyphPositions, fromIntegral glyphCount)

      liftIO $ hb_buffer_clear_contents hbBuffer

      shapedGlyphs <- V.generateM glyphCount $ \i -> do
        let
          hbGlyphPositionPtr = plusPtr hbGlyphPositions $ i * hb_glyph_position_t_size
        xOffset <- liftIO $ peek $ hb_x_offset hbGlyphPositionPtr
        yOffset <- liftIO $ peek $ hb_y_offset hbGlyphPositionPtr
        xAdvance <- liftIO $ peek $ hb_x_advance hbGlyphPositionPtr
        yAdvance <- liftIO $ peek $ hb_y_advance hbGlyphPositionPtr
        codepoint <- liftIO $ peek $ hb_codepoint $ plusPtr hbGlyphInfos $ i * hb_glyph_info_t_size

        position <- state $ \position -> (position, position + Vec2 (fromIntegral xAdvance / 64) (fromIntegral yAdvance / 64))

        return ShapedGlyph
          { shapedGlyphPosition = position + Vec2 (fromIntegral xOffset / 64) (fromIntegral yOffset / 64)
          , shapedGlyphIndex = fromIntegral codepoint
          }

      lastPosition <- get
      return (shapedGlyphs, lastPosition)

data Hb_font_t
data Hb_buffer_t
data Hb_glyph_info_t
data Hb_glyph_position_t
type Hb_position_t = Int32
type Hb_codepoint_t = Word32

foreign import ccall unsafe hb_ft_font_create :: FT_Face -> Ptr () -> IO (Ptr Hb_font_t)
foreign import ccall unsafe hb_font_destroy :: Ptr Hb_font_t -> IO ()
foreign import ccall unsafe hb_buffer_create :: IO (Ptr Hb_buffer_t)
foreign import ccall unsafe hb_buffer_destroy :: Ptr Hb_buffer_t -> IO ()
foreign import ccall unsafe hb_buffer_set_script :: Ptr Hb_buffer_t -> Word32 -> IO ()
foreign import ccall unsafe hb_buffer_set_direction :: Ptr Hb_buffer_t -> Int -> IO ()
foreign import ccall unsafe hb_buffer_add_utf16 :: Ptr Hb_buffer_t -> Ptr Word16 -> CInt -> CUInt -> CInt -> IO ()
foreign import ccall unsafe hb_buffer_clear_contents :: Ptr Hb_buffer_t -> IO ()
foreign import ccall unsafe hb_shape :: Ptr Hb_font_t -> Ptr Hb_buffer_t -> Ptr () -> CUInt -> IO ()
foreign import ccall unsafe hb_buffer_get_glyph_infos :: Ptr Hb_buffer_t -> Ptr CUInt -> IO (Ptr Hb_glyph_info_t)
foreign import ccall unsafe hb_buffer_get_glyph_positions :: Ptr Hb_buffer_t -> Ptr CUInt -> IO (Ptr Hb_glyph_position_t)
foreign import ccall unsafe hb_script_get_horizontal_direction :: Word32 -> Int

hb_x_advance :: Ptr Hb_glyph_position_t -> Ptr Hb_position_t
hb_x_advance = castPtr
hb_y_advance :: Ptr Hb_glyph_position_t -> Ptr Hb_position_t
hb_y_advance p = advancePtr (castPtr p) 1
hb_x_offset :: Ptr Hb_glyph_position_t -> Ptr Hb_position_t
hb_x_offset p = advancePtr (castPtr p) 2
hb_y_offset :: Ptr Hb_glyph_position_t -> Ptr Hb_position_t
hb_y_offset p = advancePtr (castPtr p) 3
hb_codepoint :: Ptr Hb_glyph_info_t -> Ptr Hb_codepoint_t
hb_codepoint = castPtr

hb_glyph_position_t_size :: Int
hb_glyph_position_t_size = 20

hb_glyph_info_t_size :: Int
hb_glyph_info_t_size = 20

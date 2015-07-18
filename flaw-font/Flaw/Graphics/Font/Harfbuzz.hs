{-|
Module: Flaw.Graphics.Font.Harfbuzz
Description: Harfbuzz text shaping.
License: MIT
-}

module Flaw.Graphics.Font.Harfbuzz
	( createHarfbuzzShaper
	) where

import Control.Monad.State.Strict
import qualified Data.ByteString.Unsafe as B
import Data.Int
import qualified Data.Text.Encoding as T
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

data HarfbuzzShaper = HarfbuzzShaper (Ptr Hb_font_t) (Ptr Hb_buffer_t)

createHarfbuzzShaper :: FreeTypeFont -> Int -> IO (HarfbuzzShaper, IO ())
createHarfbuzzShaper FreeTypeFont
	{ ftFontFace = ftFace
	} size = do
	ftErrorCheck "ft_Set_Pixel_Sizes" =<< ft_Set_Pixel_Sizes ftFace (fromIntegral size) (fromIntegral size)
	hbFont <- hb_ft_font_create ftFace nullPtr
	hbBuffer <- hb_buffer_create
	let destroy = do
		hb_font_destroy hbFont
		hb_buffer_destroy hbBuffer
	return (HarfbuzzShaper hbFont hbBuffer, destroy)

instance FontShaper HarfbuzzShaper where
	shapeText (HarfbuzzShaper hbFont hbBuffer) text = do
		hb_buffer_set_direction hbBuffer hB_DIRECTION_LTR
		B.unsafeUseAsCStringLen (T.encodeUtf8 text) $ \(textPtr, textLen) -> do
			hb_buffer_add_utf8 hbBuffer textPtr (fromIntegral textLen) 0 (fromIntegral textLen)
		hb_shape hbFont hbBuffer nullPtr 0

		(hbGlyphInfos, hbGlyphPositions, glyphCount) <- alloca $ \glyphCountPtr -> do
			hbGlyphInfos <- hb_buffer_get_glyph_infos hbBuffer glyphCountPtr
			hbGlyphPositions <- hb_buffer_get_glyph_positions hbBuffer glyphCountPtr
			glyphCount <- peek glyphCountPtr
			return (hbGlyphInfos, hbGlyphPositions, fromIntegral glyphCount)

		let f = V.generateM glyphCount $ \i -> do
			let hbGlyphPositionPtr = plusPtr hbGlyphPositions $ i * hb_glyph_position_t_size
			xOffset <- liftIO $ peek $ hb_x_offset hbGlyphPositionPtr
			yOffset <- liftIO $ peek $ hb_y_offset hbGlyphPositionPtr
			xAdvance <- liftIO $ peek $ hb_x_advance hbGlyphPositionPtr
			yAdvance <- liftIO $ peek $ hb_y_advance hbGlyphPositionPtr
			codepoint <- liftIO $ peek $ hb_codepoint $ plusPtr hbGlyphInfos $ i * hb_glyph_info_t_size

			position <- get
			put $ position + Vec2 (fromIntegral xAdvance / 64) (fromIntegral yAdvance / 64)

			return (position + Vec2 (fromIntegral xOffset / 64) (fromIntegral yOffset / 64), fromIntegral codepoint)

		r <- runStateT f (Vec2 0 0)

		hb_buffer_clear_contents hbBuffer

		return r

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
foreign import ccall unsafe hb_buffer_set_direction :: Ptr Hb_buffer_t -> Int -> IO ()
foreign import ccall unsafe hb_buffer_add_utf8 :: Ptr Hb_buffer_t -> Ptr CChar -> CInt -> CUInt -> CInt -> IO ()
foreign import ccall unsafe hb_buffer_clear_contents :: Ptr Hb_buffer_t -> IO ()
foreign import ccall unsafe hb_shape :: Ptr Hb_font_t -> Ptr Hb_buffer_t -> Ptr () -> CUInt -> IO ()
foreign import ccall unsafe hb_buffer_get_glyph_infos :: Ptr Hb_buffer_t -> Ptr CUInt -> IO (Ptr Hb_glyph_info_t)
foreign import ccall unsafe hb_buffer_get_glyph_positions :: Ptr Hb_buffer_t -> Ptr CUInt -> IO (Ptr Hb_glyph_position_t)

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

hB_DIRECTION_LTR :: Int
hB_DIRECTION_LTR = 4
hB_DIRECTION_RTL :: Int
hB_DIRECTION_RTL = 5

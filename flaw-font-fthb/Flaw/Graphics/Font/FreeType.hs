{-|
Module: Flaw.Graphics.Font.FreeType
Description: FreeType fonts.
License: MIT
-}

module Flaw.Graphics.Font.FreeType
	( ftErrorCheck
	, FreeTypeLibrary(..)
	, initFreeType
	, FreeTypeFont(..)
	, loadFreeTypeFont
	, createFreeTypeGlyphs
	) where

import Codec.Picture
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.Graphics.Font
import Flaw.Graphics.Font.FreeType.FFI

ftErrorCheck :: String -> FT_Error -> IO ()
ftErrorCheck errorMessage errorCode = do
	if errorCode == 0 then return ()
	else fail $ show ("FreeType error", errorMessage, errorCode)

newtype FreeTypeLibrary = FreeTypeLibrary FT_Library

initFreeType :: IO (FreeTypeLibrary, IO ())
initFreeType = do
	ftLibrary <- alloca $ \ptrLibrary -> do
		ftErrorCheck "FT_Init_FreeType" =<< ft_Init_FreeType ptrLibrary
		peek ptrLibrary
	let destroy = do
		_ <- ft_Done_FreeType ftLibrary
		return ()
	return (FreeTypeLibrary ftLibrary, destroy)

data FreeTypeFont = FreeTypeFont
	{ ftFontFace :: !FT_Face
	, ftFontFaceMemory :: !(Ptr CUChar)
	, ftFontFaceSize :: !Int
	}

loadFreeTypeFont :: FreeTypeLibrary -> Int -> B.ByteString -> IO (FreeTypeFont, IO ())
loadFreeTypeFont (FreeTypeLibrary ftLibrary) size bytes = do
	-- copy bytes into new buffer, as FT_New_Memory_Face keeps pointer to memory given
	(memory, memoryLen) <- B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
		memory <- mallocArray bytesLen
		copyArray memory bytesPtr bytesLen
		return (castPtr memory, fromIntegral bytesLen)

	-- create freetype face
	ftFace <- alloca $ \ptrFtFace -> do
		ftErrorCheck "FT_New_Memory_Face" =<< ft_New_Memory_Face ftLibrary memory memoryLen 0 ptrFtFace
		peek ptrFtFace

	-- set pixel size
	ftErrorCheck "FT_Set_Pixel_Sizes" =<< ft_Set_Pixel_Sizes ftFace (fromIntegral size) (fromIntegral size)

	let destroy = do
		_ <- ft_Done_Face ftFace
		free memory

	return (FreeTypeFont
		{ ftFontFace = ftFace
		, ftFontFaceMemory = memory
		, ftFontFaceSize = size
		}, destroy)

createFreeTypeGlyphs :: FreeTypeFont -> Int -> Int -> IO (V.Vector (Image Pixel8, GlyphInfo))
createFreeTypeGlyphs FreeTypeFont
	{ ftFontFace = ftFace
	, ftFontFaceSize = size
	} halfScaleX halfScaleY = do

	-- set pixel size with scale
	if halfScaleX > 0 || halfScaleY > 0 then
		ftErrorCheck "FT_Set_Pixel_Sizes" =<< ft_Set_Pixel_Sizes ftFace
			(fromIntegral $ size * (halfScaleX * 2 + 1))
			(fromIntegral $ size * (halfScaleY * 2 + 1))
	else return ()

	-- get number of glyphs
	glyphsCount <- flaw_ft_get_num_glyphs ftFace

	-- create glyph images and infos
	imagesAndInfos <- V.generateM (fromIntegral glyphsCount) $ \glyphIndex -> do

		-- load and render glyph
		do
			ftErrorCheck "FT_Load_Glyph" =<< ft_Load_Glyph ftFace (fromIntegral glyphIndex) FT_LOAD_NO_HINTING
			ftGlyphSlot <- flaw_ft_get_glyph_slot ftFace
			ftErrorCheck "ft_Render_Glyph" =<< ft_Render_Glyph ftGlyphSlot FT_RENDER_MODE_NORMAL

		-- read bitmap info
		ftGlyphSlot <- flaw_ft_get_glyph_slot ftFace
		FT_Bitmap
			{ f_FT_Bitmap_rows = bitmapRowsCInt
			, f_FT_Bitmap_width = bitmapWidthCInt
			, f_FT_Bitmap_pitch = bitmapPitchCInt
			, f_FT_Bitmap_buffer = bitmapBuffer
			} <- peek =<< flaw_ft_get_bitmap ftGlyphSlot
		let bitmapRows = fromIntegral bitmapRowsCInt
		let bitmapWidth = fromIntegral bitmapWidthCInt
		let bitmapPitch = fromIntegral bitmapPitchCInt

		-- make copy of pixels
		pixels <- VSM.new $ bitmapWidth * bitmapRows :: IO (VSM.IOVector Word8)
		VSM.unsafeWith pixels $ \pixelsPtr -> do
			forM_ [0..(bitmapRows - 1)] $ \i -> do
				copyArray
					(advancePtr pixelsPtr (i * bitmapWidth)) -- destination
					(plusPtr bitmapBuffer ((if bitmapPitch >= 0 then i else i + 1 - bitmapRows) * bitmapPitch)) -- source
					bitmapWidth -- count

		-- perform blur if needed
		let width = bitmapWidth + halfScaleX * 2
		let height = bitmapRows + halfScaleY * 2
		blurredPixels <- do
			if halfScaleX > 0 || halfScaleY > 0 then do
				blurredPixels <- VSM.new $ width * height
				let fullScale = (halfScaleX * 2 + 1) * (halfScaleY * 2 + 1)
				forM_ [0..(height - 1)] $ \i -> do
					forM_ [0..(width - 1)] $ \j -> do
						let mini = max (i - halfScaleY * 2) 0
						let maxi = min (i + 1) bitmapRows
						let minj = max (j - halfScaleX * 2) 0
						let maxj = min (j + 1) bitmapWidth
						pixelSum <- liftM sum $ forM [mini..(maxi - 1)] $ \ii -> do
							liftM sum $ forM [minj..(maxj - 1)] $ \jj -> do
								liftM fromIntegral $ VSM.read pixels $ ii * bitmapWidth + jj :: IO Int
						VSM.write blurredPixels (i * width + j) $ fromIntegral $ pixelSum `div` fullScale;
				return blurredPixels
			else return pixels
		freezedPixels <- VS.unsafeFreeze blurredPixels

		-- return glyph
		bitmapLeft <- flaw_ft_get_bitmap_left ftGlyphSlot
		bitmapTop <- flaw_ft_get_bitmap_top ftGlyphSlot
		return
			( Image
				{ imageWidth = width
				, imageHeight = height
				, imageData = freezedPixels
				}
			, GlyphInfo
				{ glyphWidth = width
				, glyphHeight = height
				, glyphLeftTopX = 0
				, glyphLeftTopY = 0
				, glyphOffsetX = halfScaleX + fromIntegral bitmapLeft
				, glyphOffsetY = halfScaleY - (fromIntegral bitmapTop)
				}
			)

	-- restore pixel size
	if halfScaleX > 0 || halfScaleY > 0 then
		ftErrorCheck "FT_Set_Pixel_Sizes" =<< ft_Set_Pixel_Sizes ftFace (fromIntegral size) (fromIntegral size)
	else return ()

	return imagesAndInfos

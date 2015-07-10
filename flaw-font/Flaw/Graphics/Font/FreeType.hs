{-|
Module: Flaw.Graphics.Font.FreeType
Description: FreeType fonts.
License: MIT
-}

{-# LANGUAGE FlexibleContexts #-}

module Flaw.Graphics.Font.FreeType
	( FreeTypeLibrary
	, initFreeType
	, FreeTypeFont
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
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.Rendering.FreeType.Internal as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as FT
import qualified Graphics.Rendering.FreeType.Internal.Face as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as FT
import qualified Graphics.Rendering.FreeType.Internal.Library as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT

import Flaw.Graphics.Font
import Flaw.Resource

ftErrorCheck :: String -> FT.FT_Error -> IO ()
ftErrorCheck errorMessage errorCode = do
	if errorCode == 0 then return ()
	else fail $ show ("FreeType error", errorMessage, errorCode)

newtype FreeTypeLibrary = FreeTypeLibrary FT.FT_Library

initFreeType :: ResourceIO m => m (ReleaseKey, FreeTypeLibrary)
initFreeType = allocate create destroy where
	create = alloca $ \ptrLibrary -> do
		ftErrorCheck "ft_Init_FreeType" =<< FT.ft_Init_FreeType ptrLibrary
		liftM FreeTypeLibrary $ peek ptrLibrary
	destroy (FreeTypeLibrary ftLibrary) = do
		_ <- FT.ft_Done_FreeType ftLibrary
		return ()

data FreeTypeFont = FreeTypeFont
	{ ftFontFace :: !FT.FT_Face
	, ftFontFaceMemory :: !(Ptr FT.FT_Byte)
	}

loadFreeTypeFont :: ResourceIO m => FreeTypeLibrary -> B.ByteString -> m (ReleaseKey, FreeTypeFont)
loadFreeTypeFont (FreeTypeLibrary ftLibrary) bytes = allocate create destroy where
	create = alloca $ \ptrFtFace -> do

		-- copy bytes into new buffer, as FT_New_Memory_Face keeps pointer to memory given
		(memory, memoryLen) <- B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> do
			memory <- mallocArray bytesLen
			copyArray memory bytesPtr bytesLen
			return (castPtr memory, fromIntegral bytesLen)

		-- create freetype face
		ftErrorCheck "ft_New_Memory_Face" =<< FT.ft_New_Memory_Face ftLibrary memory memoryLen 0 ptrFtFace
		ftFace <- peek ptrFtFace

		return $ FreeTypeFont
			{ ftFontFace = ftFace
			, ftFontFaceMemory = memory
			}

	destroy (FreeTypeFont ftFace memory) = do
		_ <- FT.ft_Done_Face ftFace
		free memory
		return ()

createFreeTypeGlyphs :: FreeTypeFont -> Int -> Int -> Int -> IO (V.Vector (Image Pixel8, GlyphInfo))
createFreeTypeGlyphs (FreeTypeFont ftFace _memory) size halfScaleX halfScaleY = do

	-- set pixel size with scale
	ftErrorCheck "ft_Set_Pixel_Sizes" =<< FT.ft_Set_Pixel_Sizes ftFace
		(fromIntegral $ size * (halfScaleX * 2 + 1))
		(fromIntegral $ size * (halfScaleY * 2 + 1))

	-- get number of glyphs
	glyphsCount <- peek $ FT.num_glyphs ftFace

	-- create glyph images and infos
	V.generateM (fromIntegral glyphsCount) $ \glyphIndex -> do

		-- load and render glyph
		do
			ftErrorCheck (show ("ft_Load_Glyph", glyphIndex)) =<< FT.ft_Load_Glyph ftFace (fromIntegral glyphIndex) FT.ft_LOAD_NO_HINTING
			glyphSlot <- peek $ FT.glyph ftFace
			ftErrorCheck "ft_Render_Glyph" =<< FT.ft_Render_Glyph glyphSlot FT.ft_RENDER_MODE_NORMAL

		-- read bitmap info
		glyphSlot <- peek $ FT.glyph ftFace
		FT.FT_Bitmap
			{ FT.rows = bitmapRowsCInt
			, FT.width = bitmapWidthCInt
			, FT.pitch = bitmapPitchCInt
			, FT.buffer = bitmapBuffer
			} <- peek $ FT.bitmap glyphSlot
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
		bitmapLeft <- peek $ FT.bitmap_left glyphSlot
		bitmapTop <- peek $ FT.bitmap_top glyphSlot
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

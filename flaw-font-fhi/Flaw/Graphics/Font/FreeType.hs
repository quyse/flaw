{-|
Module: Flaw.Graphics.Font.FreeType
Description: FreeType fonts.
License: MIT
-}

{-# LANGUAGE BangPatterns #-}

module Flaw.Graphics.Font.FreeType
	( ftErrorCheck
	, FreeTypeLibrary(..)
	, initFreeType
	, FreeTypeFont(..)
	, loadFreeTypeFont
	, createFreeTypeGlyphs
	) where

import Control.Concurrent.STM
import Control.Exception
import Codec.Picture
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.HashMap.Strict as HM
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
ftErrorCheck errorMessage errorCode =
	unless (errorCode == 0) $ fail $ show ("FreeType error", errorMessage, errorCode)

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
	{ ftFontFaceVar :: {-# UNPACK #-} !(TMVar FT_Face)
	, ftFontFaceMemory :: {-# UNPACK #-} !(Ptr CUChar)
	, ftFontFaceSize :: {-# UNPACK #-} !Int
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

	ftFaceVar <- newTMVarIO ftFace

	return (FreeTypeFont
		{ ftFontFaceVar = ftFaceVar
		, ftFontFaceMemory = memory
		, ftFontFaceSize = size
		}, destroy)

createFreeTypeGlyphs :: FreeTypeFont -> Int -> Int -> [Int] -> IO (HM.HashMap Int (Image Pixel8, GlyphInfo))
createFreeTypeGlyphs FreeTypeFont
	{ ftFontFaceVar = ftFaceVar
	, ftFontFaceSize = size
	} halfScaleX halfScaleY glyphsIndices = bracket acquire release $ \ftFace -> foldM (foldImage ftFace) HM.empty glyphsIndices

	where

	acquire = do
		ftFace <- atomically $ takeTMVar ftFaceVar
		-- set pixel size with scale
		when (halfScaleX > 0 || halfScaleY > 0) $
			ftErrorCheck "FT_Set_Pixel_Sizes" =<< ft_Set_Pixel_Sizes ftFace
				(fromIntegral $ size * (halfScaleX * 2 + 1))
				(fromIntegral $ size * (halfScaleY * 2 + 1))
		return ftFace

	release ftFace = do
		-- restore pixel size
		when (halfScaleX > 0 || halfScaleY > 0) $
			ftErrorCheck "FT_Set_Pixel_Sizes" =<< ft_Set_Pixel_Sizes ftFace (fromIntegral size) (fromIntegral size)
		atomically $ putTMVar ftFaceVar ftFace

	foldImage :: FT_Face -> HM.HashMap Int (Image Pixel8, GlyphInfo) -> Int -> IO (HM.HashMap Int (Image Pixel8, GlyphInfo))
	foldImage ftFace restImages glyphIndex = do -- handle handleError $ do

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
		VSM.unsafeWith pixels $ \pixelsPtr ->
			forn_ bitmapRows $ \i ->
				copyArray
					(advancePtr pixelsPtr (i * bitmapWidth)) -- destination
					(plusPtr bitmapBuffer ((if bitmapPitch >= 0 then i else i + 1 - bitmapRows) * bitmapPitch)) -- source
					bitmapWidth -- count

		-- perform blur if needed
		let width = bitmapWidth + halfScaleX * 2
		let height = bitmapRows + halfScaleY * 2

		blurredPixels <-
			if halfScaleX > 0 || halfScaleY > 0 then do
				blurredPixels <- VSM.new $ width * height
				let fullScale = (halfScaleX * 2 + 1) * (halfScaleY * 2 + 1)
				forn_ height $ \i -> do
					let mini = max (i - halfScaleY * 2) 0
					let maxi = min (i + 1) bitmapRows
					forn_ width $ \j -> do
						let minj = max (j - halfScaleX * 2) 0
						let maxj = min (j + 1) bitmapWidth
						pixelSum <- foldab (+) mini maxi 0 $ \ii ->
							foldab (+) minj maxj 0 $ \jj ->
								fmap fromIntegral $ VSM.unsafeRead pixels $ ii * bitmapWidth + jj
						VSM.unsafeWrite blurredPixels (i * width + j) $ fromIntegral $ pixelSum `quot` fullScale;
				return blurredPixels
			else return pixels
		freezedPixels <- VS.unsafeFreeze blurredPixels

		-- return glyph
		bitmapLeft <- flaw_ft_get_bitmap_left ftGlyphSlot
		bitmapTop <- flaw_ft_get_bitmap_top ftGlyphSlot
		return $ HM.insert glyphIndex
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
				, glyphOffsetY = halfScaleY - fromIntegral bitmapTop
				}
			) restImages

	-- helper functions
	-- forM_ [0..(n - 1)] q
	forn_ n q = let
		forin_ i = when (i < n) $ q i >> forin_ (i + 1)
		in forin_ 0
	-- foldl f z <$> forM [a..(b - 1)] q
	foldab f a b z q = let
		foldiab i !s = if i < b then do
			r <- f s <$> q i
			foldiab (i + 1) r
			else return s
		in foldiab a z

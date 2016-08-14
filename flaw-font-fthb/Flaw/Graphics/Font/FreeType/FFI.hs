{-|
Module: Flaw.Graphics.Font.FreeType.FFI
Description: FreeType FFI.
License: MIT
-}

{-# LANGUAGE PatternSynonyms, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Flaw.Graphics.Font.FreeType.FFI
	( FT_Error
	, FT_Library
	, FT_Face
	, FT_GlyphSlot
	, ft_Init_FreeType
	, ft_Done_FreeType
	, ft_New_Memory_Face
	, ft_Done_Face
	, ft_Set_Pixel_Sizes
	, ft_Load_Glyph
	, ft_Render_Glyph
	, pattern FT_LOAD_NO_HINTING
	, pattern FT_RENDER_MODE_NORMAL
	, FT_Bitmap(..)
	, flaw_ft_get_glyph_slot
	, flaw_ft_get_num_glyphs
	, flaw_ft_get_bitmap
	, flaw_ft_get_bitmap_left
	, flaw_ft_get_bitmap_top
	) where

import Data.Int
import Foreign.C.Types
import Foreign.Ptr

import Flaw.FFI

type FT_Error = CInt

data FT_LibraryRec
type FT_Library = Ptr FT_LibraryRec

data FT_FaceRec
type FT_Face = Ptr FT_FaceRec

data FT_GlyphSlotRec
type FT_GlyphSlot = Ptr FT_GlyphSlotRec

foreign import ccall unsafe "FT_Init_FreeType" ft_Init_FreeType :: Ptr FT_Library -> IO FT_Error
foreign import ccall unsafe "FT_Done_FreeType" ft_Done_FreeType :: FT_Library -> IO FT_Error
foreign import ccall unsafe "FT_New_Memory_Face" ft_New_Memory_Face :: FT_Library -> Ptr CUChar -> CLong -> CLong -> Ptr FT_Face -> IO FT_Error
foreign import ccall unsafe "FT_Done_Face" ft_Done_Face :: FT_Face -> IO FT_Error
foreign import ccall unsafe "FT_Set_Pixel_Sizes" ft_Set_Pixel_Sizes :: FT_Face -> CUInt -> CUInt -> IO FT_Error
foreign import ccall unsafe "FT_Load_Glyph" ft_Load_Glyph :: FT_Face -> CUInt -> Int32 -> IO FT_Error
foreign import ccall unsafe "FT_Render_Glyph" ft_Render_Glyph :: FT_GlyphSlot -> CInt -> IO FT_Error

pattern FT_LOAD_NO_HINTING = 2

pattern FT_RENDER_MODE_NORMAL = 0

genStruct "FT_Bitmap"
	[ ([t| CUInt |], "rows")
	, ([t| CUInt |], "width")
	, ([t| CInt |], "pitch")
	, ([t| Ptr CUChar |], "buffer")
	, ([t| CUShort |], "num_grays")
	, ([t| CUChar |], "pixel_mode")
	, ([t| CUChar |], "palette_mode")
	, ([t| Ptr () |], "palette")
	]

foreign import ccall unsafe flaw_ft_get_glyph_slot :: FT_Face -> IO FT_GlyphSlot
foreign import ccall unsafe flaw_ft_get_num_glyphs :: FT_Face -> IO CLong
foreign import ccall unsafe flaw_ft_get_bitmap :: FT_GlyphSlot -> IO (Ptr FT_Bitmap)
foreign import ccall unsafe flaw_ft_get_bitmap_left :: FT_GlyphSlot -> IO CInt
foreign import ccall unsafe flaw_ft_get_bitmap_top :: FT_GlyphSlot -> IO CInt

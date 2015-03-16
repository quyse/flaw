{-|
Module: Flaw.Graphics.Texture
Description: Abstract texture things.
License: MIT
-}

module Flaw.Graphics.Texture
	( TextureFormat(..)
	, PixelComponents(..)
	, PixelValueType(..)
	, PixelSize(..)
	, TextureCompression(..)
	, ColorSpace(..)
	, TextureInfo(..)
	, TextureMetrics(..)
	, TextureMipMetrics(..)
	, calcTextureMetrics
	, pixelSizeByteSize
	, compressed4x4BlockSize
	) where

import Data.Bits

-- | Texture formats.
data TextureFormat
	= UncompressedTextureFormat
		{ textureFormatComponents :: PixelComponents
		, textureFormatValueType :: PixelValueType
		, textureFormatPixelSize :: PixelSize
		, textureFormatColorSpace :: ColorSpace
		}
	| CompressedTextureFormat
		{ textureFormatCompression :: TextureCompression
		, textureFormatColorSpace :: ColorSpace
		}
	deriving Show

-- | Pixel components for texture format.
data PixelComponents
	= PixelR
	| PixelRG
	| PixelRGB
	| PixelRGBA
	deriving Show

-- | Pixel value type.
data PixelValueType
	= PixelUntyped
	| PixelUint
	| PixelFloat
	deriving Show

-- | Pixel size.
data PixelSize
	= Pixel8bit
	| Pixel16bit
	| Pixel24bit
	| Pixel32bit
	| Pixel64bit
	| Pixel96bit
	| Pixel128bit
	deriving Show

-- | Texture compression.
data TextureCompression
	= TextureCompressionBC1
	| TextureCompressionBC1Alpha
	| TextureCompressionBC2
	| TextureCompressionBC3
	| TextureCompressionBC4
	| TextureCompressionBC4Signed
	| TextureCompressionBC5
	| TextureCompressionBC5Signed
	deriving Show

-- | Color space.
data ColorSpace
	= LinearColorSpace
	| StandardColorSpace
	deriving Show

-- | Texture information.
{- Acceptable combinations:
width height depth
W     0      0      1D texture with width W
W     1      0      2D texture Wx1
W     H      0      2D texture WxH
W     H      1      3D texture WxHx1
W     H      L      3D texture WxHxL
-}
data TextureInfo = TextureInfo
	{ textureWidth :: Int
	, textureHeight :: Int
	, textureDepth :: Int
	-- | Number of MIP levels, should be >= 1.
	, textureMips :: Int
	, textureFormat :: TextureFormat
	-- | Number of textures in array.
	-- Zero means non-array.
	, textureCount :: Int
	} deriving Show

-- | Various byte metrics of texture.
data TextureMetrics = TextureMetrics
	{
	-- | Size of one image in array.
	  textureImageSize :: Int
	-- | Byte offsets from the beginning of image to mip levels.
	, textureMipsMetrics :: [TextureMipMetrics]
	}

-- | Various byte metrics of texture mip level.
-- All numbers >= 1, like even for 1D texture height is 1,
-- as opposite to 0 in TextureInfo.
data TextureMipMetrics = TextureMipMetrics
	{
	-- | Number of pixels in a row.
	  textureMipWidth :: Int
	-- | Number of lines. >= 1.
	, textureMipHeight :: Int
	-- | Number of slices. >= 1.
	, textureMipDepth :: Int
	-- | Byte length of one line.
	-- In case of compressed textures (4x4 blocks coded),
	-- it's a size of one line of 4x4 blocks.
	, textureMipLinePitch :: Int
	-- | Byte length of 2D slice.
	, textureMipSlicePitch :: Int
	-- | Byte offset to mip data from the beginning of an image.
	, textureMipOffset :: Int
	-- | Byte size of mip.
	, textureMipSize :: Int
	}

-- | Calculate byte metrics for texture.
calcTextureMetrics :: TextureInfo -> TextureMetrics
calcTextureMetrics TextureInfo
	{ textureWidth = width
	, textureHeight = height
	, textureDepth = depth
	, textureMips = mips
	, textureFormat = format
	} = tm where
	tm = TextureMetrics
		{ textureImageSize = sum $ map textureMipSize mm
		, textureMipsMetrics = mm
		}
	mm = calcMipMetrics 0 0
	calcMipMetrics mip prevOffset = mipMetrics where
		mipMetrics = if mip >= mips then [] else TextureMipMetrics
			{ textureMipWidth = mipWidth
			, textureMipHeight = mipHeight
			, textureMipDepth = mipDepth
			, textureMipLinePitch = mipLinePitch
			, textureMipSlicePitch = mipSlicePitch
			, textureMipOffset = prevOffset
			, textureMipSize = mipSize
			} : calcMipMetrics (mip + 1) nextOffset
		mipWidth = max 1 $ width `shiftR` mip
		mipHeight = max 1 $ height `shiftR` mip
		mipDepth = max 1 $ depth `shiftR` mip
		mipLinePitch = case format of
			UncompressedTextureFormat
				{ textureFormatPixelSize = ps
				} -> mipWidth * (pixelSizeByteSize ps)
			CompressedTextureFormat
				{ textureFormatCompression = c
				} -> ((mipWidth + 3) `shiftR` 2) * (compressed4x4BlockSize c)
		mipSlicePitch = case format of
			UncompressedTextureFormat {} -> mipHeight * mipLinePitch
			CompressedTextureFormat {} -> ((mipHeight + 3) `shiftR` 2) * mipLinePitch
		mipSize = mipDepth * mipSlicePitch
		nextOffset = prevOffset + mipSize

-- | Convert PixelSize to actual number of bytes.
pixelSizeByteSize :: PixelSize -> Int
pixelSizeByteSize ps = case ps of
	Pixel8bit -> 1
	Pixel16bit -> 2
	Pixel24bit -> 3
	Pixel32bit -> 4
	Pixel64bit -> 8
	Pixel96bit -> 12
	Pixel128bit -> 16

-- | Get size of 4x4 compressed pixel block.
compressed4x4BlockSize :: TextureCompression -> Int
compressed4x4BlockSize c = case c of
	TextureCompressionBC1 -> 8
	TextureCompressionBC1Alpha -> 8
	TextureCompressionBC2 -> 16
	TextureCompressionBC3 -> 16
	TextureCompressionBC4 -> 8
	TextureCompressionBC4Signed -> 8
	TextureCompressionBC5 -> 16
	TextureCompressionBC5Signed -> 16
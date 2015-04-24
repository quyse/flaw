{-|
Module: Flaw.Asset.Texture
Description: Texture support.
License: MIT
-}

{-# LANGUAGE FlexibleContexts #-}

module Flaw.Asset.Texture
	( loadTexture
	) where

import Codec.Picture
import Codec.Picture.Types
import qualified Data.ByteString as B
import Foreign.Storable

import Flaw.Build
import Flaw.Graphics.Texture

loadDynamicImageWithFormat :: Storable (PixelBaseComponent a) => Image a -> TextureFormat -> IO (TextureInfo, B.ByteString)
loadDynamicImageWithFormat Image
	{ imageWidth = width
	, imageHeight = height
	, imageData = dataVector
	} format = do
	bytes <- packVector dataVector
	return (TextureInfo
		{ textureWidth = width
		, textureHeight = height
		, textureDepth = 0
		, textureMips = 1
		, textureFormat = format
		, textureCount = 0
		}, bytes)

loadDynamicImage :: DynamicImage -> IO (TextureInfo, B.ByteString)
loadDynamicImage dynamicImage = case dynamicImage of
	ImageY8 pixel8Image -> loadDynamicImageWithFormat pixel8Image UncompressedTextureFormat
		{ textureFormatComponents = PixelR
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel8bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageY16 pixel16Image -> loadDynamicImageWithFormat pixel16Image UncompressedTextureFormat
		{ textureFormatComponents = PixelR
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel16bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageYF pixelFImage -> loadDynamicImageWithFormat pixelFImage UncompressedTextureFormat
		{ textureFormatComponents = PixelR
		, textureFormatValueType = PixelFloat
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageYA8 pixelYA8Image -> loadDynamicImage $ ImageRGBA8 $ promoteImage pixelYA8Image
	ImageYA16 pixelYA16Image -> loadDynamicImage $ ImageRGBA16 $ promoteImage pixelYA16Image
	ImageRGB8 pixelRGB8Image -> loadDynamicImage $ ImageRGBA8 $ promoteImage pixelRGB8Image
	ImageRGB16 pixelRGB16Image -> loadDynamicImage $ ImageRGBA16 $ promoteImage pixelRGB16Image
	ImageRGBF pixelRGBFImage -> loadDynamicImageWithFormat pixelRGBFImage UncompressedTextureFormat
		{ textureFormatComponents = PixelRGB
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel96bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageRGBA8 pixelRGBA8Image -> loadDynamicImageWithFormat pixelRGBA8Image UncompressedTextureFormat
		{ textureFormatComponents = PixelRGBA
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel32bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageRGBA16 pixelRGBA16Image -> loadDynamicImageWithFormat pixelRGBA16Image UncompressedTextureFormat
		{ textureFormatComponents = PixelRGBA
		, textureFormatValueType = PixelUint
		, textureFormatPixelSize = Pixel64bit
		, textureFormatColorSpace = StandardColorSpace
		}
	ImageYCbCr8 pixelYCbCr8Image -> loadDynamicImage $ ImageRGB8 $ convertImage pixelYCbCr8Image
	ImageCMYK8 pixelCMYK8Image -> loadDynamicImage $ ImageRGB8 $ convertImage pixelCMYK8Image
	ImageCMYK16 pixelCMYK16Image -> loadDynamicImage $ ImageRGB16 $ convertImage pixelCMYK16Image

loadTexture :: B.ByteString -> IO (TextureInfo, B.ByteString)
loadTexture bytes = do
	case decodeImage bytes of
		Right dynamicImage -> loadDynamicImage dynamicImage
		Left err -> fail err

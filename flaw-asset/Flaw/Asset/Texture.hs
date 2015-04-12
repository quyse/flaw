{-|
Module: Flaw.Asset.Texture
Description: Texture support.
License: MIT
-}

module Flaw.Asset.Texture
	( loadPngTexture
	) where

import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V

import Flaw.Build
import Flaw.Graphics.Texture

convertDynamicImage :: DynamicImage -> IO (TextureInfo, B.ByteString)
convertDynamicImage dynamicImage = case dynamicImage of
	ImageRGBA8 Image
		{ imageWidth = width
		, imageHeight = height
		, imageData = dataVector
		} -> do
		bytes <- packList $ V.toList dataVector
		let textureInfo = TextureInfo
			{ textureWidth = width
			, textureHeight = height
			, textureDepth = 0
			, textureMips = 1
			, textureFormat = UncompressedTextureFormat
				{ textureFormatComponents = PixelRGBA
				, textureFormatValueType = PixelUint
				, textureFormatPixelSize = Pixel32bit
				, textureFormatColorSpace = StandardColorSpace
				}
			, textureCount = 0
			}
		return (textureInfo, bytes)
	ImageRGB8 Image
		{ imageWidth = width
		, imageHeight = height
		, imageData = dataVector
		} -> do
		let
			insertGap (d0:d1:d2:ds) = d0:d1:d2:255:insertGap ds
			insertGap [] = []
			insertGap _ = undefined
		bytes <- packList $ insertGap $ V.toList dataVector
		let textureInfo = TextureInfo
			{ textureWidth = width
			, textureHeight = height
			, textureDepth = 0
			, textureMips = 1
			, textureFormat = UncompressedTextureFormat
				{ textureFormatComponents = PixelRGBA
				, textureFormatValueType = PixelUint
				, textureFormatPixelSize = Pixel32bit
				, textureFormatColorSpace = StandardColorSpace
				}
			, textureCount = 0
			}
		return (textureInfo, bytes)
	_ -> fail "unsupported dynamic image format"

loadPngTexture :: B.ByteString -> IO (TextureInfo, B.ByteString)
loadPngTexture bytes = do
	case decodePng bytes of
		Right dynamicImage -> convertDynamicImage dynamicImage
		Left err -> fail err
